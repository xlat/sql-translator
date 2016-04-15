package SQL::Translator::Producer::SQLServer;
#largely inspired on SQL::Translator::Producer::MySQL
use strict;
use warnings;
our ( $DEBUG, $WARN );
our $VERSION = '1.59.1';

my $DEFAULT_MAX_ID_LENGTH = 128;
$DEBUG = 1 unless defined $DEBUG;

use base qw(SQL::Translator::Producer);
use SQL::Translator::Schema::Constants;
use SQL::Translator::Utils qw(
    debug
    header_comment
    normalize_quote_options
    batch_alter_table_statements
    truncate_id_uniquely
  );
use SQL::Translator::Generator::DDL::SQLServer;

#
# Use only lowercase for the keys (e.g. "long" and not "LONG")
#
my %translate  = (
    #
    # Oracle types
    #
    varchar2   => 'varchar',
    long       => 'text',
    clob       => 'longtext',

    #
    # Sybase types
    #
    #int        => 'integer',
    #money      => 'float',
    #real       => 'double',
    comment    => 'text',
    #bit        => 'tinyint',

    #
    # Access types
    #
    'long integer' => 'int',
    'text'         => 'text',
    'datetime'     => 'datetime2',

    #
    # PostgreSQL types
    #
    bytea => 'binary',
);

my @no_length_attr = qw/ date time timestamp datetime year /;
  
{
  my ($quoting_generator, $nonquoting_generator);
  sub _generator {
    my $options = shift;
    return $options->{generator} if exists $options->{generator};

    return normalize_quote_options($options)
      ? $quoting_generator ||= SQL::Translator::Generator::DDL::SQLServer->new
      : $nonquoting_generator ||= SQL::Translator::Generator::DDL::SQLServer->new(
        quote_chars => [],
      );
  }
}

sub produce {
  my $translator = shift;
  SQL::Translator::Generator::DDL::SQLServer->new(
    add_comments    => !$translator->no_comments,
    add_drop_table => $translator->add_drop_table,
  )->schema($translator->schema)
}

sub alter_table {
  #TODO at alter_table level: check for multiple identities or an identity property transfered from one column to another one!
  #if($identities>1){
  #  #MSSQL: Multiple identity columns specified for table '$TABLE'. Only one identity column per table is allowed.
  #  #-> So if an identity column lost its identity property and another one will 
  #}

  ()
} # Noop

#sub generate_table_options
#{
#  my ($table, $options) = @_;
#  my $create;
#
#  my $table_type_defined = 0;
#  my $generator        = _generator($options);
#  my $charset          = ''; #$table->extra('mysql_charset');
#  my $collate          = ''; #$table->extra('mysql_collate');
#  my $union            = undef;
#  for my $t1_option_ref ( $table->options ) {
#    my($key, $value) = %{$t1_option_ref};
#    $table_type_defined = 1
#      if uc $key eq 'ENGINE' or uc $key eq 'TYPE';
#    if (uc $key eq 'CHARACTER SET') {
#      $charset = $value;
#      next;
#    } elsif (uc $key eq 'COLLATE') {
#      $collate = $value;
#      next;
#    } elsif (uc $key eq 'UNION') {
#      $union = '(' . join(', ', map { $generator->quote($_) } @$value) . ')';
#      next;
#    }
#    $create .= " $key=$value";
#  }
#
#  #my $mysql_table_type = $table->extra('mysql_table_type');
#  #$create .= " ENGINE=$mysql_table_type"
#  #  if $mysql_table_type && !$table_type_defined;
#  my $comments         = $table->comments;
#
#  $create .= " DEFAULT CHARACTER SET $charset" if $charset;
#  $create .= " COLLATE $collate" if $collate;
#  $create .= " UNION=$union" if $union;
#  $create .= qq[ comment='$comments'] if $comments;
#  return $create;
#}
#
#sub alter_table
#{
#    my ($to_table, $options) = @_;
#
#    my $table_options = generate_table_options($to_table, $options) || '';
#    my $table_name = _generator($options)->quote($to_table->name);
#    my $out = sprintf('/* HACK42 */ ALTER TABLE %s%s',
#                      $table_name,
#                      $table_options);
#
#    return $out;
#}
sub normalize_prop($);
sub normalize_prop($){
  return normalize_prop(\@_) if @_>1;
  my $v = shift;
  if(my $ref = ref $v){
    if($ref eq 'ARRAY'){
      $v = '['.join(',',map{ normalize_prop($_) } @$v).']';
    }
    elsif($ref eq 'HASH'){
      $v = '{'.join(',',map{ normalize_prop($_) } %$v).'}';
    }
    elsif($ref eq 'REGEXP'){
      $v = "$v";
    }
    else{
      $v = normalize_prop($$v);
    }
  }
  $v//'';
}

sub cmp_prop($$$){
  my ($from, $to, $prop) = @_;
  my $a = normalize_prop( $from->$prop() );
  my $b = normalize_prop( $to->$prop() );
  $a cmp $b;
}

sub full_data_type{
    my $field = shift;
    # data type and size
    my $data_type = $field->data_type;
    my @size      = $field->size;
    my %extra     = $field->extra;
    my $list      = $extra{'list'} || [];
    my $commalist = join( ', ', map { __PACKAGE__->_quote_string($_) } @$list );
    #
    #my $mysql_version = $options->{mysql_version} || 0;
    #
    # Oracle "number" type -- figure best SQLServer type
    #
    if ( lc $data_type eq 'number' ) {
        # not an integer
        if ( scalar @size > 1 ) {
            $data_type = 'decimal';
        }
        elsif ( $size[0] && $size[0] >= 12 ) {
            $data_type = 'bigint';
        }
        elsif ( $size[0] && $size[0] <= 1 ) {
            $data_type = 'tinyint';
        }
        else {
            $data_type = 'int';
        }
    }
    #
    # Convert a large Oracle varchar to "text"
    # (not necessary as of 5.0.3 http://dev.mysql.com/doc/refman/5.0/en/char.html)
    #
    elsif ( $data_type =~ /char/i && $size[0] > 255 ) {
        unless ($size[0] <= 65535 ) {
            $data_type = ($data_type=~/^n/i ? 'n' : '') . 'varchar(max)';
            @size      = ();
        }
    }
    elsif ( $data_type =~ /boolean/i ) {
        $data_type = 'bit';
    }
    elsif ( exists $translate{ lc $data_type } ) {
        $data_type = $translate{ lc $data_type };
    }

    @size = () if $data_type =~ /(text|blob|binary)/i;

    if ( $data_type =~ /(decimal|float)/ && scalar @size == 1 ) {
        push @size, '0';
    }

    if ( lc($data_type) eq 'enum' || lc($data_type) eq 'set') {
        $data_type .= '(' . $commalist . ')';
    }
    elsif (
        defined $size[0] && $size[0] > 0
        &&
        ! grep lc($data_type) eq $_, @no_length_attr
    ) {
        $data_type .= '(' . join( ', ', @size ) . ')';
    }
    return $data_type;
}

sub add_field {
  my ($field) = @_;

  return sprintf("ALTER TABLE %s ADD %s",
      _generator()->quote($field->table->name), _generator()->field($field))
}

sub drop_field{
    my ($old_field, $options) = @_;

    my $generator  = _generator($options);
    my $table_name = $generator->quote($old_field->table->name);

    #TODO:
    # Error => ALTER TABLE DROP COLUMN **column name** failed because one or more objects access this column.
    # This can append when trying to drop a column with a default value (eg: getdate() ) that
    # is handled in the form of a constraint.
    #http://stackoverflow.com/questions/8641954/how-to-drop-column-with-constraint
    
    my $out = sprintf('ALTER TABLE %s DROP COLUMN %s',
                      $table_name,
                      $generator->quote($old_field->name));

    return $out;

}

sub alter_field
{
    my ($from_field, $to_field, $options) = @_;

    my $generator  = _generator($options);
    my $table_name = $generator->quote($to_field->table->name);
    my $from_name = $generator->quote($from_field->name);
    my $to_name   = $generator->quote($to_field->name);

    my @out;
    my $upgrade_required;
    my $datatype_changed;
    
    PROPERTY:
    for(qw(is_auto_increment is_primary_key size is_nullable data_type order comments default_value)){
      if(cmp_prop($from_field,$to_field,$_)){
        debug("alter_field $table_name.$from_name: $_ changed".
              " from ". normalize_prop($from_field->$_()).
              " to "  . normalize_prop($to_field  ->$_()));
        if(/order|comments|is_primary_key/){
          debug("  - modification on property $_ are ignored (yet)");
          next PROPERTY;
        }
        $datatype_changed++ if /data_type|size/;
        $upgrade_required++;
      }
    }
        
    if($upgrade_required){
      #Need to copy data from OLD to NEW column,
      #but if a rename was needed, don't rename
      
      #1) rename $from_field->name as 'Tmp'.$from_to->name
      my $to_drop_column = $from_field->name;
      if($from_name eq $to_name){
        #TODO: look for possible colision of existing column
        $to_drop_column .= '#';
      }      
      push @out, qq{EXECUTE sp_rename N'$table_name.$from_name', N'$to_drop_column', 'COLUMN'};
      $to_drop_column = $generator->quote($to_drop_column);
      #2) create $to_field->name
      push @out, qq{ALTER TABLE $table_name ADD } . create_field($to_field, $options, { force_default_on_not_null => 1 });
      #3) copy (& cast if required) data
      push @out, qq{SET IDENTITY_INSERT $table_name ON}
        if $to_field->is_auto_increment;
      my $to_expr   = !$to_field->is_nullable && $to_field->default_value
                    ? "ISNULL($to_drop_column,".$to_field->default_value.")"
                    : $to_drop_column;
      my $from_expr = $datatype_changed
                  ? "cast($to_expr as ". full_data_type( $to_field ) .")"
                  : $to_drop_column;
      push @out, qq{UPDATE $table_name SET $to_field = $from_expr};
      push @out, qq{SET IDENTITY_INSERT $table_name OFF}
        if $to_field->is_auto_increment;
      #4) drop Tmp column
      push @out, qq{ALTER TABLE $table_name DROP COLUMN $to_drop_column};
    }
    elsif($from_name ne $to_name){
      #only rename field
      push @out, qq{EXECUTE sp_rename N'$table_name.$from_name', N'$to_name', 'COLUMN'};
    }

    return join("\n", map{ "$_;" } @out) // '';
}

sub alter_drop_index
{
    my ($index, $options) = @_;
    my $generator  = _generator($options);
    my $table_name = $index->table->name;
    my $index_name = $index->name || die(" Can't drop unnamed index on table $table_name with fields:", join ',', $index->fields);
    $index_name =~ s/^[^.]+\.//g;

    return join( ' ',
                 'DROP',
                 'INDEX',
                 $generator->quote($index_name),
                 'ON',
                 $generator->quote($table_name),
                 );

}

sub alter_create_index{
    return 'CREATE ' . create_index(@_);
}

sub create_index{
    my ( $index, $options ) = @_;
    my $generator = _generator($options);
    my $table_name = $index->table->name;
    my $index_name = $index->name
        ? truncate_id_uniquely(
                $index->name,
                $options->{max_id_length} || $DEFAULT_MAX_ID_LENGTH
          )
        : '';
    $index_name =~ s/^[^.]+\.//g;

    return join(
        ' ',
        map { $_ || () }
        lc $index->type eq 'normal' ? 'INDEX' : $index->type . ' INDEX',
        $generator->quote($index_name),
        'ON', 
        $generator->quote($table_name),
        '(' . join( ', ', map { $generator->quote($_) } $index->fields ) . ')'
        #TODO: add ASC/DESC field order
        #TODO: add WITH (DROP_EXISTING = ON)
    );
}

sub alter_drop_constraint{
    my ($c, $options) = @_;

    my $generator = _generator($options);
    my $table_name = $c->table->name;
    $DB::single = 1 if $table_name =~ /parameter$/ and $c->type eq PRIMARY_KEY;
    my @fk_constraints;
    TABLE:
    for my $table (@{ $c->table->schema->get_tables // [] }){
      next TABLE if $table->name eq $table_name;
      CONSTRAINT:
      for(@{$table->get_constraints // []}){
            next CONSTRAINT unless $_->reference_table eq $table_name;
            next CONSTRAINT if exists $options->{dropped}{$_->name};
              #must check for $table_name related FK that are not already dropped,
              # so we should DROP them, then append FK re-create in the $options->{stmts_queue}
              #then mark it as dropped.
            $options->{dropped}{$_->name}++;
            push @fk_constraints, $_;
      }
    }
    
    $table_name = $generator->quote($table_name);
    
    my @out;
    if(@fk_constraints){
      push @{$options->{stmts_queue}},  batch_alter_table_statements(
              { alter_create_constraint => [ @fk_constraints ] },
              $options );
      push @out, batch_alter_table_statements(
              { alter_drop_constraint => [ @fk_constraints ] },
              $options );
    }
    
    if($c->type eq PRIMARY_KEY) {
      push @out, <<DROP_PK;
--Drop PK for table $table_name
/*KEEP BEGIN*/BEGIN
	DECLARE \@SQL VARCHAR(MAX)
	SET \@SQL = REPLACE('ALTER TABLE $table_name DROP CONSTRAINT |ConstraintName| ', '|ConstraintName|', ( SELECT name FROM sysobjects WHERE xtype = 'PK' AND parent_obj = OBJECT_ID('$table_name')) )
	EXEC (\@SQL)
END
DROP_PK
    }
    else {
        my $c_name = $generator->quote($c->name);
        $c_name =~ s/^.*\.//;
        push @out, join(' ', 'ALTER','TABLE',$table_name,'DROP', $c_name);
    }
    return join(";\n",@out);
}

sub alter_create_constraint{
    my ($index, $options) = @_;

    my $table_name = _generator($options)->quote($index->table->name);
    my $def = join( ' ',
                 'ALTER TABLE',
                 $table_name,
                 'ADD',
                 create_constraint(@_) );
    if($index->type eq FOREIGN_KEY){
      push @{$options->{stmts_queue}}, $def;
      return '';
    }
    return $def;
}

sub create_constraint{
    my ($c, $options) = @_;

    my $generator       = _generator($options);
    my $leave_name      = $options->{leave_name} || undef;

    my $reference_table_name = $generator->quote($c->reference_table);

    my @fields = $c->fields or return;

    if ( $c->type eq PRIMARY_KEY ) {
        return 'PRIMARY KEY (' . join(", ", map { $generator->quote($_) } @fields) . ')';
    }
    elsif ( $c->type eq UNIQUE ) {
        my $c_name = defined $c->name && $c->name
              ? truncate_id_uniquely( $c->name,
                                      $options->{max_id_length} || $DEFAULT_MAX_ID_LENGTH )
              : truncate_id_uniquely( join('_',$c->reference_table, @fields),
                                      $options->{max_id_length} || $DEFAULT_MAX_ID_LENGTH ).'uq'
              ;
        $c_name = $generator->quote( $c_name );
        $c_name =~ s/^.*\.//;
        return sprintf 'CONSTRAINT %s UNIQUE (%s)',
          $c_name,
          ( join ', ', map { $generator->quote($_) } @fields ),
        ;
    }
    elsif ( $c->type eq FOREIGN_KEY ) {
        #
        # Make sure FK field is indexed or MySQL complains.
        #

        my $table = $c->table;
        my $c_name = truncate_id_uniquely( $c->name, $options->{max_id_length} || $DEFAULT_MAX_ID_LENGTH );
        if($c_name){
          $c_name = $generator->quote($c_name);
          $c_name =~ s/^.*\.//;
        }
        my $def = join(' ',
                         'CONSTRAINT',
                         ($c_name ? $c_name : () ),
                         'FOREIGN KEY'
                      );


        $def .= ' ('. join( ', ', map { $generator->quote($_) } @fields ) . ')';

        $def .= ' REFERENCES ' . $reference_table_name;

        my @rfields = map { $_ || () } $c->reference_fields;
        unless ( @rfields ) {
            my $rtable_name = $c->reference_table;
            if ( my $ref_table = $table->schema->get_table( $rtable_name ) ) {
                push @rfields, $ref_table->primary_key;
            }
            else {
                warn "Can't find reference table '$rtable_name' " .
                    "in schema\n" if $options->{show_warnings};
            }
        }

        if ( @rfields ) {
            $def .= ' (' . join( ', ', map { $generator->quote($_) } @rfields ) . ')';
        }
        else {
            warn "FK constraint on " . $table->name . '.' .
                join('', @fields) . " has no reference fields\n"
                if $options->{show_warnings};
        }

        if ( $c->match_type ) {
            $def .= ' MATCH ' .
                ( $c->match_type =~ /full/i ) ? 'FULL' : 'PARTIAL';
        }

        if ( $c->on_delete ) {
            $def .= ' ON DELETE '. $c->on_delete;
        }

        if ( $c->on_update ) {
            $def .= ' ON UPDATE '. $c->on_update;
        }
        
        return $def;
        #push @{$options->{stmts_queue}}, $def;
        #return undef;
    }

    return undef;
}

sub batch_alter_table_end{
  my ($diffs, $options) = @_;
  return @{ delete($options->{stmts_queue}) // [] };
}

sub batch_alter_table {
  my ($table, $diff_hash, $options) = @_;
  my @post_stmts;
  $options->{post_stmts} = \@post_stmts;

  my %constraints_to_alter;
  my %constraints_to_drop = map {
    ( $_->name => $_ )
  } @{$diff_hash->{alter_drop_constraint} };

  my %constraints_to_create = map {
    $constraints_to_alter{$_->name} = $constraints_to_drop{$_->name}
      if $constraints_to_drop{$_->name};
    ( $_->name => $_ );
  } @{$diff_hash->{alter_create_constraint} };

  ##move FK create statement into POST statements queue, that will be
  ##processed after all tables
  #push @{$options->{stmts_queue}}, batch_alter_table_statements(
  #                  { alter_create_constraint => [
  #                      grep{ $_->type eq FOREIGN_KEY }
  #                      @{$diff_hash->{alter_create_constraint}}
  #                    ]
  #                  },
  #                  $options
  #                );
  #@{$diff_hash->{alter_create_constraint}} = grep{ $_->type ne FOREIGN_KEY } @{$diff_hash->{alter_create_constraint}};
  
  my @drop_stmt;
  if (scalar keys %constraints_to_alter) {
    $diff_hash->{alter_drop_constraint} = [
      grep { !$constraints_to_alter{$_->name} } @{ $diff_hash->{alter_drop_constraint} }
    ];

    @drop_stmt = batch_alter_table(
                    $table,
                    { alter_drop_constraint => [ values %constraints_to_alter ] },
                    $options
                  );

  }
  
  TABLE:
  for my $table_fields(@{ $diff_hash->{alter_field} }){
    FIELD:
    for my $field (@$table_fields){
      #alter column need to drop/create constraint (pk, fk, uniq, index)
      CONSTRAINT:
      for(@{$field->table->{_constraints}//[]}){
        unless(exists $constraints_to_drop{ $_->name }){
          push @drop_stmt, batch_alter_table(
              $_->table->name,
              { alter_drop_constraint => [ $_ ] },
              $options
            );
          $constraints_to_drop{ $_->name } = $_;
          #recreate constraint
          unless(exists $constraints_to_create{ $_->name }){
            push @post_stmts, batch_alter_table(
                $_->table->name,
                { alter_create_constraint => [ $_ ] },
                $options
              );
          }
        }
      }
    }
  }
  
  my @stmts = batch_alter_table_statements($diff_hash, $options);
  
  #quote
  my $generator = _generator($options);
  delete $options->{post_stmts};
  return @drop_stmt, @stmts, @post_stmts;

  ## Just zero or one stmts. return now
  #return (@drop_stmt,@stmts) unless @stmts > 1;
  #
  ## rename_table makes things a bit more complex
  #my $renamed_from = "";
  #$renamed_from = $generator->quote($diff_hash->{rename_table}[0][0]->name)
  #  if $diff_hash->{rename_table} && @{$diff_hash->{rename_table}};
  #  
  ## Now strip off the 'ALTER TABLE xyz' of all but the first one
  #my $table_name = $generator->quote($table->name);
  #
  #my $re = $renamed_from
  #       ? qr/^ALTER TABLE (?:\Q$table_name\E|\Q$renamed_from\E) /
  #          : qr/^ALTER TABLE \Q$table_name\E /;
  #
  #my $first = shift  @stmts;
  #my ($alter_table) = $first =~ /($re)/;
  #
  #my $padd = " " x length($alter_table);
  #
  #return @drop_stmt, join( ",\n", $first, map { s/$re//; $padd . $_ } @stmts);

}

sub drop_table {
  my ($table, $options) = @_;

          # Drop (foreign key) constraints so table drops cleanly
  my @sql;# = batch_alter_table($table, { alter_drop_constraint => [ grep { $_->type eq 'FOREIGN KEY' } $table->get_constraints ] }, $options);

  my $table_name = _generator($options)->quote($table);
  return (@sql, "DROP TABLE $table_name");

}

sub create_field{
    my ($field, $options, $args) = @_;

    my $generator = _generator($options);

    my $field_name = $field->name;
    debug("PKG: Looking at field '$field_name'\n");
    my $field_def = $generator->quote($field_name);

    my %extra     = $field->extra;
    my $charset = undef; #$extra{'mysql_charset'};
    my $collate = undef; #$extra{'mysql_collate'};
    
    # data type
    my $data_type = full_data_type( $field );
    $field_def .= " $data_type";
    
    # char sets
    $field_def .= " CHARACTER SET $charset" if $charset;
    $field_def .= " COLLATE $collate" if $collate;

    # MySQL qualifiers
    for my $qual ( qw[ binary unsigned zerofill ] ) {
        my $val = $extra{ $qual } || $extra{ uc $qual } or next;
        $field_def .= " $qual";
    }
    for my $qual ( 'character set', 'collate', 'on update' ) {
        my $val = $extra{ $qual } || $extra{ uc $qual } or next;
        if ( ref $val ) {
            $field_def .= " $qual ${$val}";
        }
        else {
            $field_def .= " $qual $val";
        }
    }

    # Null?
    if ( $field->is_nullable ) {
        $field_def .= ' NULL';
    }
    else {
        $field_def .= ' NOT NULL';
    }

    # Default?
    __PACKAGE__->_apply_default_value(
      $field,
      \$field_def,
      [
        'NULL'       => \'NULL',
      ],
    );
    
    if( !$field->is_nullable
       and !defined($field->default_value)
       and $args->{force_default_on_not_null}){
      $field_def .= ' DEFAULT ';
      if($data_type =~ /char|text|binary|image/){
        $field_def .= "''";
      }
      else{
        $field_def .= '0';
      }
    }

    if ( my $comments = $field->comments ) {
        #MSSQL could not add comment while creating column, but in a separate statement:
        #syntax is: COMMENT ON COLUMN product.product_description IS 'This is comment for the column';
        #TODO...
        #$comments = __PACKAGE__->_quote_string($comments);
        #$field_def .= qq[ comment $comments];
        $field_def .= "/*TODO: append comment: $comments*/";
    }

    # auto_increment?
    $field_def .= " identity" if $field->is_auto_increment;

    return $field_def;
}

sub _quote_string {
    my ($self, $string) = @_;

    $string =~ s/([\\'])/$1$1/g;
    return qq{'$string'};
}

1;

=head1 NAME

SQL::Translator::Producer::SQLServer - MS SQLServer producer for SQL::Translator

=head1 SYNOPSIS

  use SQL::Translator;

  my $t = SQL::Translator->new( parser => '...', producer => 'SQLServer' );
  $t->translate;

=head1 DESCRIPTION

This is currently a thin wrapper around the nextgen
L<SQL::Translator::Generator::DDL::SQLServer> DDL maker.

=head1 Extra Attributes

=over 4

=item field.list

List of values for an enum field.

=back

=head1 TODO

 * !! Write some tests !!
 * Reserved words list needs updating to SQLServer.
 * Triggers, Procedures and Views DO NOT WORK


    # Text of view is already a 'create view' statement so no need to
    # be fancy
    foreach ( $schema->get_views ) {
        my $name = $_->name();
        $output .= "\n\n";
        $output .= "--\n-- View: $name\n--\n\n" unless $no_comments;
        my $text = $_->sql();
        $text =~ s/\r//g;
        $output .= "$text\nGO\n";
    }

    # Text of procedure already has the 'create procedure' stuff
    # so there is no need to do anything fancy. However, we should
    # think about doing fancy stuff with granting permissions and
    # so on.
    foreach ( $schema->get_procedures ) {
        my $name = $_->name();
        $output .= "\n\n";
        $output .= "--\n-- Procedure: $name\n--\n\n" unless $no_comments;
        my $text = $_->sql();
      $text =~ s/\r//g;
        $output .= "$text\nGO\n";
    }

=head1 SEE ALSO

L<SQL::Translator>

=head1 AUTHORS

See the included AUTHORS file:
L<http://search.cpan.org/dist/SQL-Translator/AUTHORS>

=head1 COPYRIGHT

Copyright (c) 2012 the SQL::Translator L</AUTHORS> as listed above.

=head1 LICENSE

This code is free software and may be distributed under the same terms as Perl
itself.

=cut
