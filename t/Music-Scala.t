#!perl

use strict;
use warnings;

use Test::More;    # plan is down at bottom
use Test::Exception;

eval 'use Test::Differences';    # display convenience
my $deeply = $@ ? \&is_deeply : \&eq_or_diff;

BEGIN { use_ok('Music::Scala') }

my $scala = Music::Scala->new;
isa_ok( $scala, 'Music::Scala' );

is( $scala->get_concertfreq, 440, 'default concert frequency' );
is( $scala->get_description, '',  'default description' );
dies_ok( sub { $scala->get_notes }, 'get_notes before read_scala' );

# for MIDI/equal temperament reference operations
is( $scala->freq2pitch(440), 69,  'frequency to pitch, MIDI ref freq' );
is( $scala->pitch2freq(69),  440, 'pitch to frequency, MIDI ref pitch' );

dies_ok( sub { $scala->read_scala( file => 'Makefile.PL' ) },
  'invalid scala file' );

# Notable for having both ratios and cents, as well as funny ISO 8859-1
# characters in the description.
isa_ok(
  $scala->read_scala(
    file    => 'groenewald_bach.scl',
    binmode => ':encoding(iso-8859-1):crlf'
  ),
  'Music::Scala'
);
is(
  $scala->get_description,
  "J\x{fc}rgen Gr\x{f6}newald, simplified Bach temperament, Ars Organi vol.57 no.1, March 2009, p.39",
  'Latin 1 infested'
);
$deeply->(
  [ $scala->get_notes ],
  [ qw{256/243 189.25008 32/27 386.60605 4/3 1024/729 693.17509 128/81 887.27506 16/9 1086.80812 2/1}
  ],
  'Bach temperament'
);

$deeply->(
  [ map { my $s = sprintf "%.2f", $_; $s }
      $scala->interval2freq( 0, 12, 24, -12, -24, 1, 2, 3 )
  ],
  [ map { my $s = sprintf "%.2f", $_; $s } 440,
    880, 1760, 220, 110, 463.54, 490.83, 521.48
  ],
  'frequency conversion'
);

# These were copied & pasted from scala site, plus blank desc and number
# of subsequent notes to create a minimally valid file.
$scala->read_scala( file => 'valid-pitch-lines.scl' );
is( $scala->get_description, 'this is a test', 'desc' );
$deeply->(
  [ $scala->get_notes ],
  [qw{81/64 408.0 408. 5/1 -5.0 10/20 100.0 100.0 5/4}],
  'valid pitch lines'
);

$scala = Music::Scala->new( MAX_LINES => 1 );
dies_ok( sub { $scala->read_scala( file => 'groenewald_bach.scl' ) },
  'absurd MAX_LINES to cause exception' );

# Global binmode specifier, plus no crlf handling
$scala = Music::Scala->new( binmode => ':encoding(iso-8859-1)' );
$scala->read_scala( file => 'groenewald_bach.scl' );
is(
  $scala->get_description,
  "J\x{fc}rgen Gr\x{f6}newald, simplified Bach temperament, Ars Organi vol.57 no.1, March 2009, p.39\r",
  'Latin 1 infested II'
);
# but crlf handling should not affect the note parsing...
$deeply->(
  [ $scala->get_notes ],
  [ qw{256/243 189.25008 32/27 386.60605 4/3 1024/729 693.17509 128/81 887.27506 16/9 1086.80812 2/1}
  ],
  'Bach temperament'
);

isa_ok( $scala->set_description('test'), 'Music::Scala' );
isa_ok( $scala->set_notes( [qw{256/243 9/8}] ), 'Music::Scala' );

my $output = '';
open my $ofh, '>', \$output or die 'could not open in-memory fh ' . $!;
isa_ok( $scala->write_scala( fh => $ofh ), 'Music::Scala' );
close $ofh;
is( $output, "test\n 2\n!\n 256/243\n 9/8\n", 'output to fh' );

isa_ok( $scala->set_concertfreq(123.4), 'Music::Scala' );
is( $scala->get_concertfreq, 123.4, 'custom concert frequency' );

# more cents testing - via slendro_ky2.scl
$scala = Music::Scala->new( concertfreq => 295 );
is( $scala->get_concertfreq, 295, 'check cf via new' );

# NOTE Perl will map things like a bare 1200.000 to '1200' which then
# becomes the ratio 1200/1 which is wrong.
$scala->set_notes( 250.868, 483.311, 715.595, 951.130, '1200.000' );
$deeply->(
  [ map { my $s = sprintf "%.2f", $_; $s }
      $scala->interval2freq( 0, 5, 10, -5, -10 )
  ],
  [ map { my $s = sprintf "%.2f", $_; $s } 295, 590, 1180, 147.5, 73.75 ],
  'frequency conversion'
);

# file => via new() to save on then typing read_scala out
$scala = Music::Scala->new( file => 'valid-pitch-lines.scl' );
is( $scala->get_description, 'this is a test', 'desc' );

is( $scala->get_binmode, undef, 'default binmode' );
isa_ok( $scala->set_binmode(':crlf'), 'Music::Scala' );
is( $scala->get_binmode, ':crlf', 'custom binmode' );

$scala->set_notes( '2/1', '1200.0', '5/4' );
$deeply->(
  [ $scala->notes2ratios( $scala->get_notes ) ],
  [ 2, 2, 5 / 4 ],
  'notes2ratios'
);
# get_ratios uses notes2ratios internally, though ratios only calculated
# and saved in object when necessary
$deeply->( [ $scala->get_ratios ], [ 2, 2, 5 / 4 ], 'notes2ratios' );

plan tests => 31;
