#!/usr/bin/perl
# vim:set ft=perl:

$| = 1;

use strict;
use Test::More tests => 157;
use SQL::Translator::Schema::Constants;

require_ok( 'SQL::Translator::Schema' );

{
    #
    # Schema
    #
    my $schema = SQL::Translator::Schema->new( 
        name => 'foo',
        database => 'MySQL',
    );
    isa_ok( $schema, 'SQL::Translator::Schema' );

    is( $schema->name, 'foo', 'Schema name is "foo"' );
    is( $schema->name('bar'), 'bar', 'Schema name changed to "bar"' );

    is( $schema->database, 'MySQL', 'Schema database is "MySQL"' );
    is( $schema->database('PostgreSQL'), 'PostgreSQL', 
        'Schema database changed to "PostgreSQL"' );

    is( $schema->is_valid, undef, 'Schema not valid...' );
    like( $schema->error, qr/no tables/i, '...because there are no tables' );

    #
    # $schema->add_*
    #
    my $foo_table = $schema->add_table(name => 'foo') or warn $schema->error;
    isa_ok( $foo_table, 'SQL::Translator::Schema::Table', 'Table "foo"' );

    my $bar_table = SQL::Translator::Schema::Table->new( name => 'bar' ) or 
        warn SQL::Translator::Schema::Table->error;
    $bar_table = $schema->add_table( $bar_table );
    isa_ok( $bar_table, 'SQL::Translator::Schema::Table', 'Table "bar"' );
    is( $bar_table->name, 'bar', 'Add table "bar"' );

    $schema = $bar_table->schema( $schema );
    isa_ok( $schema, 'SQL::Translator::Schema', 'Schema' );

    is( $bar_table->name('foo'), undef, 
        q[Can't change name of table "bar" to "foo"...]);
    like( $bar_table->error, qr/can't use table name/i, 
        q[...because "foo" exists] );

    my $redundant_table = $schema->add_table(name => 'foo');
    is( $redundant_table, undef, qq[Can't create another "foo" table...] );
    like( $schema->error, qr/can't use table name/i, 
        '... because "foo" exists' );

    #
    # Table default new
    #
    is( $foo_table->name, 'foo', 'Table name is "foo"' );
    is( $foo_table->is_valid, undef, 'Table "foo" is not yet valid' );

    my $fields = $foo_table->get_fields;
    is( scalar @{ $fields || [] }, 0, 'Table "foo" has no fields' );
    like( $foo_table->error, qr/no fields/i, 'Error for no fields' );

    is( $foo_table->comments, '', 'No comments' );

    #
    # New table with args
    #
    my $person_table = $schema->add_table( 
        name     => 'person', 
        comments => 'foo',
    );
    is( $person_table->name, 'person', 'Table name is "person"' );
    is( $person_table->is_valid, undef, 'Table is not yet valid' );
    is( $person_table->comments, 'foo', 'Comments = "foo"' );
    is( join(',', $person_table->comments('bar')), 'foo,bar', 
        'Table comments = "foo,bar"' );
    is( $person_table->comments, "foo\nbar", 'Table comments = "foo,bar"' );

    #
    # Field default new
    #
    my $f1 = $person_table->add_field(name => 'foo') or 
        warn $person_table->error;
    isa_ok( $f1, 'SQL::Translator::Schema::Field', 'Field' );
    is( $f1->name, 'foo', 'Field name is "foo"' );
    is( $f1->data_type, '', 'Field data type is blank' );
    is( $f1->size, 0, 'Field size is "0"' );
    is( $f1->is_primary_key, '0', 'Field is_primary_key is false' );
    is( $f1->is_nullable, 1, 'Field can be NULL' );
    is( $f1->default_value, undef, 'Field default is undefined' );
    is( $f1->comments, '', 'No comments' );

    my $f2 = SQL::Translator::Schema::Field->new (
        name     => 'f2',
        comments => 'foo',
    ) or warn SQL::Translator::Schema::Field->error;
    $f2 = $person_table->add_field( $f2 );
    isa_ok( $f1, 'SQL::Translator::Schema::Field', 'f2' );
    is( $f2->name, 'f2', 'Add field "f2"' );
    is( $f2->is_nullable(0), 0, 'Field cannot be NULL' );
    is( $f2->is_nullable(''), 0, 'Field cannot be NULL' );
    is( $f2->is_nullable('0'), 0, 'Field cannot be NULL' );
    is( $f2->default_value(''), '', 'Field default is empty string' );
    is( $f2->comments, 'foo', 'Field comment = "foo"' );
    is( join(',', $f2->comments('bar')), 'foo,bar', 
        'Field comment = "foo,bar"' );
    is( $f2->comments, "foo\nbar", 'Field comment = "foo,bar"' );

    $person_table = $f2->table( $person_table );
    isa_ok( $person_table, 'SQL::Translator::Schema::Table', 'person_table' );

    is( $f2->name('foo'), undef, q[Can't set field name of "f2" to "foo"...] );
    like( $f2->error, qr/can't use field name/i, '...because name exists' );

    my $redundant_field = $person_table->add_field(name => 'f2');
    is( $redundant_field, undef, qq[Didn't create another "f2" field...] );
    like( $person_table->error, qr/can't use field/i, 
        '... because it exists' );

    my @fields = $person_table->get_fields;
    is( scalar @fields, 2, 'Table "foo" has 2 fields' );

    is( $fields[0]->name, 'foo', 'First field is "foo"' );
    is( $fields[1]->name, 'f2', 'Second field is "f2"' );

    #
    # Field methods
    #
    is( $f1->name('person_name'), 'person_name', 
        'Field name is "person_name"' );
    is( $f1->data_type('varchar'), 'varchar', 'Field data type is "varchar"' );
    is( $f1->size('30'), '30', 'Field size is "30"' );
    is( $f1->is_primary_key(0), '0', 'Field is_primary_key is negative' );

    $f1->extra( foo => 'bar' );
    $f1->extra( { baz => 'quux' } );
    my %extra = $f1->extra;
    is( $extra{'foo'}, 'bar', 'Field extra "foo" is "bar"' );
    is( $extra{'baz'}, 'quux', 'Field extra "baz" is "quux"' );

    #
    # New field with args
    #
    my $age       = $person_table->add_field(
        name      => 'age',
        data_type => 'float',
        size      => '10,2',
    );
    is( $age->name, 'age', 'Field name is "age"' );
    is( $age->data_type, 'float', 'Field data type is "float"' );
    is( $age->size, '10,2', 'Field size is "10,2"' );
    is( $age->size(10,2), '10,2', 'Field size still "10,2"' );
    is( $age->size([10,2]), '10,2', 'Field size still "10,2"' );
    is( $age->size(qw[ 10 2 ]), '10,2', 'Field size still "10,2"' );
    is( join(':', $age->size), '10:2', 'Field size returns array' );

    #
    # Index
    #
    my @indices = $person_table->get_indices;
    is( scalar @indices, 0, 'No indices' );
    like( $person_table->error, qr/no indices/i, 'Error for no indices' );
    my $index1 = $person_table->add_index( name => "foo" ) 
        or warn $person_table->error;
    isa_ok( $index1, 'SQL::Translator::Schema::Index', 'Index' );
    is( $index1->name, 'foo', 'Index name is "foo"' );

    is( $index1->is_valid, undef, 'Index name is not valid...' );
    like( $index1->error, qr/no fields/i, '...because it has no fields' );

    is( join(':', $index1->fields('foo,bar')), 'foo:bar', 
        'Index accepts fields');

    is( $index1->is_valid, undef, 'Index name is not valid...' );
    like( $index1->error, qr/does not exist in table/i, 
        '...because it used fields not in the table' );

    is( join(':', $index1->fields(qw[foo age])), 'foo:age', 
        'Index accepts fields');
    is( $index1->is_valid, 1, 'Index name is now valid' );

    is( $index1->type, NORMAL, 'Index type is "normal"' );

    my $index2 = SQL::Translator::Schema::Index->new( name => "bar" ) 
        or warn SQL::Translator::Schema::Index->error;
    $index2    = $person_table->add_index( $index2 );
    isa_ok( $index2, 'SQL::Translator::Schema::Index', 'Index' );
    is( $index2->name, 'bar', 'Index name is "bar"' );

    my $indices = $person_table->get_indices;
    is( scalar @$indices, 2, 'Two indices' );
    is( $indices->[0]->name, 'foo', '"foo" index' );
    is( $indices->[1]->name, 'bar', '"bar" index' );

    #
    # Constraint
    #
    my @constraints = $person_table->get_constraints;
    is( scalar @constraints, 0, 'No constraints' );
    like( $person_table->error, qr/no constraints/i, 
        'Error for no constraints' );
    my $constraint1 = $person_table->add_constraint( name => 'foo' ) 
        or warn $person_table->error;
    isa_ok( $constraint1, 'SQL::Translator::Schema::Constraint', 'Constraint' );
    is( $constraint1->name, 'foo', 'Constraint name is "foo"' );

    $fields = join(',', $constraint1->fields('id') );
    is( $fields, 'id', 'Constraint field = "id"' );

    $fields = join(',', $constraint1->fields('id,id') );
    is( $fields, 'id', 'Constraint field = "id"' );

    $fields = join(',', $constraint1->fields('id', 'name') );
    is( $fields, 'id,name', 'Constraint field = "id,name"' );

    $fields = join(',', $constraint1->fields( 'id,name,id' ) );
    is( $fields, 'id,name', 'Constraint field = "id,name"' );

    $fields = join(',', $constraint1->fields( 'id, name' ) );
    is( $fields, 'id,name', 'Constraint field = "id,name"' );

    $fields = join(',', $constraint1->fields( [ 'id', 'name' ] ) );
    is( $fields, 'id,name', 'Constraint field = "id,name"' );

    $fields = join(',', $constraint1->fields( qw[ id name ] ) );
    is( $fields, 'id,name', 'Constraint field = "id,name"' );

    is( $constraint1->match_type, '', 'Constraint match type is empty' );
    is( $constraint1->match_type('foo'), undef, 
        'Constraint match type rejects bad arg...' );
    like( $constraint1->error, qr/invalid match type/i,
        '...because it is invalid');
    is( $constraint1->match_type('FULL'), 'full', 
        'Constraint match type = "full"' );

    my $constraint2 = SQL::Translator::Schema::Constraint->new( name => 'bar' );
    $constraint2    = $person_table->add_constraint( $constraint2 );
    isa_ok( $constraint2, 'SQL::Translator::Schema::Constraint', 'Constraint' );
    is( $constraint2->name, 'bar', 'Constraint name is "bar"' );

    my $constraints = $person_table->get_constraints;
    is( scalar @$constraints, 2, 'Two constraints' );
    is( $constraints->[0]->name, 'foo', '"foo" constraint' );
    is( $constraints->[1]->name, 'bar', '"bar" constraint' );

    #
    # View
    #
    my $view = $schema->add_view( name => 'view1' ) or warn $schema->error;
    isa_ok( $view, 'SQL::Translator::Schema::View', 'View' );
    my $view_sql = 'select * from table';
    is( $view->sql( $view_sql ), $view_sql, 'View SQL is good' );

    my $view2 = SQL::Translator::Schema::View->new(name => 'view2') or
        warn SQL::Translator::Schema::View->error;
    my $check_view = $schema->add_view( $view2 );
    is( $check_view->name, 'view2', 'Add view "view2"' );

    my $redundant_view = $schema->add_view(name => 'view2');
    is( $redundant_view, undef, qq[Didn't create another "view2" view...] );
    like( $schema->error, qr/can't create view/i, '... because it exists' );

    #
    # $schema->get_*
    #
    my $bad_table = $schema->get_table;
    like( $schema->error, qr/no table/i, 'Error on no arg to get_table' );

    $bad_table = $schema->get_table('baz');
    like( $schema->error, qr/does not exist/i, 
        'Error on bad arg to get_table' );

    my $bad_view = $schema->get_view;
    like( $schema->error, qr/no view/i, 'Error on no arg to get_view' );

    $bad_view = $schema->get_view('bar');
    like( $schema->error, qr/does not exist/i, 
        'Error on bad arg to get_view' );

    my $good_table = $schema->get_table('foo');
    isa_ok( $good_table, 'SQL::Translator::Schema::Table', 'Table "foo"' );

    my $good_view = $schema->get_view('view1');
    isa_ok( $good_view, 'SQL::Translator::Schema::View', 'View "view1"' );
    is( $view->sql( $view_sql ), $view_sql, 'View SQL is good' );

    #
    # $schema->get_*s
    #
    my @tables = $schema->get_tables;
    is( scalar @tables, 3, 'Found 2 tables' );

    my @views = $schema->get_views;
    is( scalar @views, 2, 'Found 1 view' );
}

#
# Test ability to introspect some values
#
{ 
    my $s        =  SQL::Translator::Schema->new( 
        name     => 'foo',
        database => 'PostgreSQL',
    );
    my $t = $s->add_table( name => 'person' ) or warn $s->erro;
    my $f = $t->add_field( name => 'person_id' ) or warn $t->error;
    $f->data_type('serial');

    my $c          = $t->add_constraint(
        type       => PRIMARY_KEY,
        fields     => 'person_id',
    ) or warn $t->error;

    is( $f->is_primary_key, 1, 'Field is PK' );
    is( $f->is_auto_increment, 1, 'Field is auto inc' );
}

#
# FK constraint validity
#
{
    my $s = SQL::Translator::Schema->new;
    my $t = $s->add_table( name => 'person' ) or warn $s->error;
    my $c = $t->add_constraint or warn $t->error;

    is( $c->is_valid, undef, 'Constraint on "person" not valid...');
    like( $c->error, qr/no type/i, '...because it has no type' );

    is( $c->type( FOREIGN_KEY ), FOREIGN_KEY, 'Constraint type now a FK' );

    is( $c->is_valid, undef, 'Constraint on "person" not valid...');
    like( $c->error, qr/no fields/i, '...because it has no fields' );

    is( join('', $c->fields('foo')), 'foo', 'Fields now = "foo"' );

    is( $c->is_valid, undef, 'Constraint on "person" not valid...');
    like( $c->error, qr/non-existent field/i, 
        q[...because field "foo" doesn't exist] );

    my $fk = $t->add_field( name => 'pet_id' );
    is( $fk->name, 'pet_id', 'Added field "pet_id"' );
    is( join('', $c->fields('pet_id')), 'pet_id', 'Fields now = "pet_id"' );

    $t->add_field( name => 'f1' );
    $t->add_field( name => 'f2' );
    is( join(',', $c->fields('f1,f2')), 'f1,f2', 'Fields now = "f1,f2"' );
    is( $c->is_valid, undef, 'Constraint on "person" not valid...');
    like( $c->error, qr/only one field/i, 
        q[...because too many fields for FK] );

    $c->fields('f1');

    is( $c->is_valid, undef, 'Constraint on "person" not valid...');
    like( $c->error, qr/no reference table/i, 
        q[...because there's no reference table] );

    is( $c->reference_table('foo'), 'foo', 'Reference table now = "foo"' );
    is( $c->is_valid, undef, 'Constraint on "person" not valid...');
    like( $c->error, qr/no table named/i, 
        q[...because reference table "foo" doesn't exist] );

    my $t2 = $s->add_table( name => 'pet' );
    is( $t2->name, 'pet', 'Added "pet" table' );

    is( $c->reference_table('pet'), 'pet', 'Reference table now = "pet"' );

    is( $c->is_valid, undef, 'Constraint on "person" not valid...');
    like( $c->error, qr/no reference fields/i,
        q[...because there're no reference fields]);

    is( join('', $c->reference_fields('pet_id')), 'pet_id', 
        'Reference fields = "pet_id"' );

    is( $c->is_valid, undef, 'Constraint on "person" not valid...');
    like( $c->error, qr/non-existent field/i,
        q[...because there's no "pet_id" field in "pet"]);

    my $pet_id = $t2->add_field( name => 'pet_id' );
    is( $pet_id->name, 'pet_id', 'Added field "pet_id"' );
    
    is( $c->is_valid, 1, 'Constraint now valid' );
}

#
# $table->primary_key test
#
{
    my $s = SQL::Translator::Schema->new;
    my $t = $s->add_table( name => 'person' );

    is( $t->primary_key, undef, 'No primary key' );

    is( $t->primary_key('person_id'), undef, 
            q[Can't make PK on "person_id"...] );
    like( $t->error, qr/invalid field/i, "...because it doesn't exist" );

    $t->add_field( name => 'person_id' );
    my $c = $t->primary_key('person_id');

    isa_ok( $c, 'SQL::Translator::Schema::Constraint', 'Constraint' );

    is( join('', $c->fields), 'person_id', 'Constraint now on "person_id"' );

    $t->add_field( name => 'name' );
    $c = $t->primary_key('name');
    is( join(',', $c->fields), 'person_id,name', 
        'Constraint now on "person_id" and "name"' );

    is( scalar @{ $t->get_constraints }, 1, 'Found 1 constraint' );
}

#
# FK finds PK
#
{
    my $s  = SQL::Translator::Schema->new;
    my $t1 = $s->add_table( name => 'person' );
    my $t2 = $s->add_table( name => 'pet' );
    $t1->add_field( name => 'id' );
    my $c1 = $t1->primary_key( 'id' ) or warn $t1->error;
    is( $c1->type, PRIMARY_KEY, 'Made "person_id" PK on "person"' );

    $t2->add_field( name => 'person_id' );
    my $c2 = $t2->add_constraint(
        type            => PRIMARY_KEY,
        fields          => 'person_id',
        reference_table => 'person',
    );

    is( join('', $c2->reference_fields), 'id', 'FK found PK "person.id"' );
}

#
# View
#
{
    my $name   = 'foo_view';
    my $sql    = 'select name, age from person';
    my $fields = 'name, age';
    my $s      = SQL::Translator::Schema->new;
    my $v      = $s->add_view(
        name   => $name,
        sql    => $sql,
        fields => $fields,
    );

    isa_ok( $v, 'SQL::Translator::Schema::View', 'View' );
    is( $v->name, $name, qq[Name is "$name"] );
    is( $v->sql, $sql, qq[Name is "$sql"] );
    is( join(':', $v->fields), 'name:age', qq[Fields are "$fields"] );
}