package SQL::Translator::Producer::Diagram;

# -------------------------------------------------------------------
# $Id: Diagram.pm,v 1.3 2003-06-09 04:40:50 kycl4rk Exp $
# -------------------------------------------------------------------
# Copyright (C) 2003 Ken Y. Clark <kclark@cpan.org>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; version 2.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
# 02111-1307  USA
# -------------------------------------------------------------------

use strict;
use GD;
use Data::Dumper;
use SQL::Translator::Schema::Constants;
use SQL::Translator::Utils qw(debug);

use vars qw[ $VERSION $DEBUG ];
$VERSION = sprintf "%d.%02d", q$Revision: 1.3 $ =~ /(\d+)\.(\d+)/;
$DEBUG   = 0 unless defined $DEBUG;

use constant VALID_FONT_SIZE => {
    small  => 1,
    medium => 1,
    large  => 1,
    huge   => 1,
};

use constant VALID_IMAGE_TYPE => {
    png  => 1,
    jpeg => 1,
};

sub produce {
    my $t          = shift;
    my $schema     = $t->schema;
    my $args       = $t->producer_args;
    local $DEBUG   = $t->debug;
    debug("Schema =\n", Dumper( $schema ));
    debug("Producer args =\n", Dumper( $args ));

    my $out_file     = $args->{'out_file'}   || '';
    my $image_type   = $args->{'image_type'} || 'png';
    my $title        = $args->{'title'}      || $t->filename;
    my $font_size    = $args->{'font_size'}  || 'medium';
    my $no_columns   = $args->{'no_columns'};
    my $no_lines     = $args->{'no_lines'};
    my $add_color    = $args->{'add_color'};
    my $show_fk_only = $args->{'show_fk_only'};
    my $join_pk_only = $args->{'join_pk_only'};
    my $natural_join = $args->{'natural_join'} || $join_pk_only;
    my $skip_fields  = $args->{'skip_fields'};
    my %skip         = map { s/^\s+|\s+$//g; $_,1 } split (/,/, $skip_fields);

    $schema->make_natural_joins(
        join_pk_only => $join_pk_only,
        skip_fields  => $args->{'skip_fields'},
    ) if $natural_join;

    die "Invalid image type '$image_type'"
        unless VALID_IMAGE_TYPE ->{ $image_type  };
    die "Invalid font size '$font_size'"
        unless VALID_FONT_SIZE->{ $font_size };

    #
    # Layout the image.
    #
    my $font         = 
        $font_size eq 'small'  ? gdTinyFont  :
        $font_size eq 'medium' ? gdSmallFont :
        $font_size eq 'large'  ? gdLargeFont : gdGiantFont;
    my @tables       = $schema->get_tables;
    my $no_tables    = scalar @tables;
    $no_columns      = 0 unless $no_columns =~ /^\d+$/;
    $no_columns    ||= sprintf( "%.0f", sqrt( $no_tables ) + .5 );
    $no_columns    ||= .5;
    my $no_per_col   = sprintf( "%.0f", $no_tables/$no_columns + .5 );

    my @shapes;            
    my ( $max_x, $max_y );          # the furthest x and y used
    my $orig_y      = 40;           # used to reset y for each column
    my ( $x, $y )   = (30,$orig_y); # where to start
    my $cur_col     = 1;            # the current column
    my $no_this_col = 0;            # number of tables in current column
    my $this_col_x  = $x;           # current column's x
    my $gutter      = 30;           # distance b/w columns
    my %nj_registry;                # for locations of fields for natural joins
    my @fk_registry;                # for locations of fields for foreign keys
    my %table_x;                    # for max x of each table
    my $field_no;                   # counter to give distinct no. to each field
    my %coords;                     # holds fields coordinates
    my %legend;

    for my $table ( @tables ) {
        my $table_name = $table->name;
        my $top        = $y;
        push @shapes, 
            [ 'string', $font, $this_col_x, $y, $table_name, 'black' ];

        $y                   += $font->height + 2;
        my $below_table_name  = $y;
        $y                   += 2;
        my $this_max_x        = 
            $this_col_x + ($font->width * length($table_name));

        debug("Processing table '$table_name'");

        my @fields = $table->get_fields;
        debug("Fields = ", join(', ', map { $_->name } @fields));

        my ( @fld_desc, $max_name );
        for my $f ( @fields ) {
            my $name  = $f->name or next;
            my $is_pk = $f->is_primary_key;

            #
            # Decide if we should skip this field.
            #
            if ( $show_fk_only ) {
                next unless $is_pk || $f->is_foreign_key;
            }

            if ( $is_pk ) {
                $name .= ' *';
                $legend{'Primary key'} = '*';
            }
            elsif ( $f->is_unique ) {
                $name .= ' [U]';
                $legend{'Unique constraint'} = '[U]';
            }

            my $desc = $f->data_type;
            $desc   .= '('.$f->size.')' if $f->size;
            
            my $nlen  = length $name;
            $max_name = $nlen if $nlen > $max_name;
            push @fld_desc, [ $name, $desc, $f->{'name'}, $is_pk ];
        }

        $max_name += 4;
        for my $fld_desc ( @fld_desc ) {
            my ( $name, $desc, $orig_name, $is_pk ) = @$fld_desc;
            my $diff = $max_name - length $name;
            $name   .= ' ' x $diff;
            $desc    = $name . $desc;

            push @shapes, [ 'string', $font, $this_col_x, $y, $desc, 'black' ];
            $y         += $font->height + 2;
            my $length  = $this_col_x + ( $font->width * length( $desc ) );
            $this_max_x = $length if $length > $this_max_x;

            my $constraints = $table->{'fields'}{ $orig_name }{'constraints'};

            if ( $natural_join && !$skip{ $orig_name } ) {
                push @{ $nj_registry{ $orig_name } }, $table_name;
            }

            my $y_link = $y - $font->height/2;
            $coords{ $table_name }{ $orig_name }{'coords'} = {
                left     => [ $this_col_x - 6, $y_link ],
                right    => [ $length + 2    , $y_link ],
                table    => $table_name,
                field_no => ++$field_no,
                is_pk    => $is_pk,
                fld_name => $orig_name,
            };
        }

        unless ( $natural_join ) {
            for my $c ( $table->get_constraints ) {
                next unless $c->type eq FOREIGN_KEY;
                my $fk_table = $c->reference_table or next;

                for my $field_name ( $c->fields ) {
                    for my $fk_field ( $c->reference_fields ) {
                        next unless defined $schema->get_table( $fk_table );
                        push @fk_registry, [
                            [ $table_name, $field_name ],
                            [ $fk_table  , $fk_field  ],
                        ];
                    }
                }
            }
        }

        $this_max_x += 5;
        $table_x{ $table_name } = $this_max_x + 5;
        push @shapes, [ 'line', $this_col_x - 5, $below_table_name, 
            $this_max_x, $below_table_name, 'black' ];
        my @bounds = ( $this_col_x - 5, $top - 5, $this_max_x, $y + 5 );
        if ( $add_color ) {
            unshift @shapes, [ 
                'filledRectangle', 
                $bounds[0], $bounds[1],
                $this_max_x, $below_table_name,
                'khaki' 
            ];
            unshift @shapes, [ 'filledRectangle', @bounds, 'white' ];
        }
        push @shapes, [ 'rectangle', @bounds, 'black' ];
        $max_x = $this_max_x if $this_max_x > $max_x;
        $y    += 25;
        
        if ( ++$no_this_col == $no_per_col ) {# if we've filled up this column
            $cur_col++;                       # up the column number
            $no_this_col = 0;                 # reset the number of tables
            $max_x      += $gutter;           # push the x over for next column
            $this_col_x  = $max_x;            # remember the max x for this col
            $max_y       = $y if $y > $max_y; # note the max y
            $y           = $orig_y;           # reset the y for next column
        }
    }

    #
    # Connect the lines.
    #
    my %horz_taken;
    my %done;
    unless ( $no_lines ) {
        my @position_bunches;

        if ( $natural_join ) {
            for my $field_name ( keys %nj_registry ) {
                my @positions;
                my @table_names = 
                    @{ $nj_registry{ $field_name } || [] } or next;
                next if scalar @table_names == 1;

                for my $table_name ( @table_names ) {
                    push @positions,
                        $coords{ $table_name }{ $field_name }{'coords'};
                }

                push @position_bunches, [ @positions ];
            }
        }
        else {
            for my $pair ( @fk_registry ) {
                push @position_bunches, [ 
                    $coords{$pair->[0][0]}{ $pair->[0][1] }{'coords'},
                    $coords{$pair->[1][0]}{ $pair->[1][1] }{'coords'},
                ];
            }
        }

        my $is_directed = $natural_join ? 0 : 1;

        for my $bunch ( @position_bunches ) {
            my @positions = @$bunch;

            for my $i ( 0 .. $#positions ) {
                my $pos1        = $positions[ $i ];
                my ( $ax, $ay ) = @{ $pos1->{'left'}  || [] } or next;
                my ( $bx, $by ) = @{ $pos1->{'right'} || [] } or next;
                my $table1      = $pos1->{'table'};
                my $fno1        = $pos1->{'field_no'};
                my $is_pk       = $pos1->{'is_pk'};
                next if $join_pk_only and !$is_pk;

                for my $j ( 0 .. $#positions ) {
                    my $pos2        = $positions[ $j ];
                    my ( $cx, $cy ) = @{ $pos2->{'left'}  || [] } or next;
                    my ( $dx, $dy ) = @{ $pos2->{'right'} || [] } or next;
                    my $table2      = $pos2->{'table'};
                    my $fno2        = $pos2->{'field_no'};
                    next if $table1 eq $table2;
                    next if $done{ $fno1 }{ $fno2 };
                    next if $fno1 == $fno2;

                    my @distances = ();
                    push @distances, [
                        abs ( $ax - $cx ) + abs ( $ay - $cy ),
                        [ $ax, $ay, $cx, $cy ],
                        [ 'left', 'left' ]
                    ];
                    push @distances, [
                        abs ( $ax - $dx ) + abs ( $ay - $dy ),
                        [ $ax, $ay, $dx, $dy ],
                        [ 'left', 'right' ],
                    ];
                    push @distances, [
                        abs ( $bx - $cx ) + abs ( $by - $cy ),
                        [ $bx, $by, $cx, $cy ],
                        [ 'right', 'left' ],
                    ];
                    push @distances, [
                        abs ( $bx - $dx ) + abs ( $by - $dy ),
                        [ $bx, $by, $dx, $dy ],
                        [ 'right', 'right' ],
                    ];
                    @distances   = sort { $a->[0] <=> $b->[0] } @distances;
                    my $shortest = $distances[0];
                    my ( $x1, $y1, $x2, $y2 ) = @{ $shortest->[1] };
                    my ( $side1, $side2     ) = @{ $shortest->[2] };
                    my ( $start, $end );
                    my $offset     = 9;
                    my $col1_right = $table_x{ $table1 };
                    my $col2_right = $table_x{ $table2 };

                    my $diff = 0;
                    if ( $x1 == $x2 ) {
                        while ( $horz_taken{ $x1 + $diff } ) {
                            $diff = $side1 eq 'left' ? $diff - 2 : $diff + 2; 
                        }
                        $horz_taken{ $x1 + $diff } = 1;
                    }

                    if ( $side1 eq 'left' ) {
                        $start = $x1 - $offset + $diff;
                    }
                    else {
                        $start = $col1_right + $diff;
                    }

                    if ( $side2 eq 'left' ) {
                        $end = $x2 - $offset + $diff;
                    } 
                    else {
                        $end = $col2_right + $diff;
                    } 

                    push @shapes, 
                        [ 'line', $x1,    $y1, $start, $y1, 'cadetblue' ];
                    push @shapes, 
                        [ 'line', $start, $y1, $end,   $y2, 'cadetblue' ];
                    push @shapes, 
                        [ 'line', $end,   $y2, $x2,    $y2, 'cadetblue' ];

                    if ( $is_directed ) {
                        if (
                            $side1 eq 'right' && $side2 eq 'left'
                            ||
                            $side1 eq 'left' && $side2 eq 'left'
                        ) {
                            push @shapes, [ 
                                'line', $x2 - 3, $y2 - 3, $x2, $y2, 'cadetblue' 
                            ];
                            push @shapes, [ 
                                'line', $x2 - 3, $y2 + 3, $x2, $y2, 'cadetblue' 
                            ];
                            push @shapes, [ 
                                'line', $x2 - 3, $y2 - 3, $x2 - 3, $y2 +3, 
                                'cadetblue' 
                            ];
                        }
                        else {
                            push @shapes, [ 
                                'line', $x2 + 3, $y2 - 3, $x2, $y2, 'cadetblue' 
                            ];
                            push @shapes, [ 
                                'line', $x2 + 3, $y2 + 3, $x2, $y2, 'cadetblue' 
                            ];
                            push @shapes, [ 
                                'line', $x2 + 3, $y2 - 3, $x2 + 3, $y2 +3, 
                                'cadetblue' 
                            ];
                        }
                    }

                    $done{ $fno1 }{ $fno2 } = 1;
                    $done{ $fno2 }{ $fno1 } = 1;
                }
            }
        }
    }

    #
    # Add the title, legend and signature.
    #
    my $large_font = gdLargeFont;
    my $title_len  = $large_font->width * length $title;
    push @shapes, [ 
        'string', $large_font, $max_x/2 - $title_len/2, 10, $title, 'black' 
    ];

    if ( %legend ) {
        $max_y += 5;
        push @shapes, [ 
            'string', $font, $x, $max_y - $font->height - 4, 'Legend', 'black'
        ];
        $max_y += $font->height + 4;

        my $longest;
        for my $len ( map { length $_ } values %legend ) {
            $longest = $len if $len > $longest; 
        }
        $longest += 2;

        while ( my ( $key, $shape ) = each %legend ) {
            my $space = $longest - length $shape;
            push @shapes, [ 
                'string', $font, $x, $max_y - $font->height - 4, 
                join( '', $shape, ' ' x $space, $key ), 'black'
            ];

            $max_y += $font->height + 4;
        }
    }

    my $sig     = __PACKAGE__." $VERSION";
    my $sig_len = $font->width * length $sig;
    push @shapes, [ 
        'string', $font, $max_x - $sig_len, $max_y - $font->height - 4, 
        $sig, 'black'
    ];

    #
    # Render the image.
    #
    my $gd = GD::Image->new( $max_x + 30, $max_y );
    unless ( $gd->can( $image_type ) ) {
        die "GD can't create images of type '$image_type'\n";
    }
    my %colors = map { $_->[0], $gd->colorAllocate( @{$_->[1]} ) } (
        [ white                => [ 255, 255, 255 ] ],
        [ beige                => [ 245, 245, 220 ] ],
        [ black                => [   0,   0,   0 ] ],
        [ lightblue            => [ 173, 216, 230 ] ],
        [ cadetblue            => [  95, 158, 160 ] ],
        [ lightgoldenrodyellow => [ 250, 250, 210 ] ],
        [ khaki                => [ 240, 230, 140 ] ],
        [ red                  => [ 255,   0,   0 ] ],
    );
    $gd->interlaced( 'true' );
    my $background_color = $add_color ? 'lightgoldenrodyellow' : 'white';
    $gd->fill( 0, 0, $colors{ $background_color } );
    for my $shape ( @shapes ) {
        my $method = shift @$shape;
        my $color  = pop   @$shape;
        $gd->$method( @$shape, $colors{ $color } );
    }

    #
    # Print the image.
    #
    if ( $out_file ) {
        open my $fh, ">$out_file" or die "Can't write '$out_file': $!\n";
        print $fh $gd->$image_type;
        close $fh;
    }
    else {
        return $gd->$image_type;
    }
}

1;

=pod

=head1 NAME

SQL::Translator::Producer::Diagram - ER diagram producer for SQL::Translator

=head1 AUTHOR

Ken Y. Clark E<lt>kclark@cpan.orgE<gt>

=cut