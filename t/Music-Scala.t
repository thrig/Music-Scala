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

dies_ok( sub { $scala->get_description },
  'get_description before read_scala' );
dies_ok( sub { $scala->get_notes }, 'get_notes before read_scala' );

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

# These were copied & pasted from scala site, plus blank desc and number
# of subsequent notes to create a minimally valid file.
$scala->read_scala( file => 'valid-pitch-lines.scl' );
is( $scala->get_description, '', 'blank desc' );
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

plan tests => 17;
