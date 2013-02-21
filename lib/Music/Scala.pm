# -*- Perl -*-
#
# Scala scale (musical tuning and temperament) support for Perl, based
# on specification at: http://www.huygens-fokker.org/scala/

package Music::Scala;

use 5.010000;
use strict;
use warnings;

use Carp qw/croak/;
use Scalar::Util qw/looks_like_number reftype/;

our $VERSION = '0.30';

# To avoid file reader from wasting too much time on bum input (longest
# scala file 'fortune.scl' in archive as of 2013-02-19 has 617 lines).
my $MAX_LINES = 3000;

########################################################################
#
# SUBROUTINES

sub get_concertpitch {
  my ($self) = @_;
  return $self->{_concertpitch} // 440;
}

sub get_description {
  my ($self) = @_;
  return $self->{_description} // '';
}

# No method for getting the spec file note count value as that's a) not
# saved in the object and b) can be obtained by calling what this
# returns as array in scalar context.
sub get_notes {
  my ($self) = @_;
  croak 'no scala loaded' if !exists $self->{_notes};
  return @{ $self->{_notes} };
}

sub interval2freq {
  my $self = shift;
  croak 'no scala loaded' if !exists $self->{_notes};

  if ( !defined $self->{_ratios} ) {
    my @ratios;
    for my $n ( @{ $self->{_notes} } ) {
      if ( $n =~ m{(\d+)/(\d+)} ) {
        push @ratios, $1 / $2;    # ratio, as marked with /
      } else {
        push @ratios, "TODO";
      }
    }
    $self->{_ratios} = \@ratios;
  }

  my @freqs;
  for my $i ( ref $_[0] eq 'ARRAY' ? @{ $_[0] } : @_ ) {
    if ( $i == 0 ) {              # special case for unison (ratio 1/1)
      push @freqs, $self->{_concertpitch};
    } else {
      my $is_dsc = $i < 0 ? 1 : 0;

      # "Octave" portion (might be zero) - how many times the interval
      # passes through the complete scale
      my $octave_count  = abs int $i / @{ $self->{_ratios} };
      my $octave_offset = $self->{_ratios}->[-1] * $octave_count;
      $octave_offset = 1 / $octave_offset if $is_dsc and $octave_offset != 0;

      my $remainder = 0;
      my $offset    = $i % @{ $self->{_ratios} };
      if ( $offset != 0 ) {
        # non-octave portion of remainder, if any
        $offset--;
        $remainder = $self->{_ratios}->[$offset];
        $remainder = 1 / $remainder if $is_dsc and $remainder != 0;
      }

      push @freqs, $octave_offset * $self->{_concertpitch} +
        $remainder * $self->{_concertpitch};
    }
  }

  return @freqs > 1 ? @freqs : $freqs[0];
}

sub new {
  my ( $class, %param ) = @_;
  my $self = {};

  $self->{_binmode} = $param{binmode} if exists $param{binmode};

  $self->{_concertpitch} = 440;
  if ( exists $param{concertpitch} ) {
    croak 'concert pitch must be a positive number (Hz)'
      if !defined $param{concertpitch}
      or !looks_like_number $param{concertpitch}
      or $param{concertpitch} < 0;
    $self->{_concertpitch} = $self->{_concertpitch};
  }

  $self->{_MAX_LINES} =
    exists $param{MAX_LINES} ? $param{MAX_LINES} : $MAX_LINES;

  $self->{_ratios} = undef;

  bless $self, $class;
  return $self;
}

sub read_scala {
  my $self = shift;
  my %param;
  if ( @_ == 1 ) {
    $param{file} = $_[0];
  } else {
    %param = @_;
  }

  my $fh;
  if ( exists $param{file} ) {
    open( $fh, '<', $param{file} ) or croak 'open failed: ' . $!;
  } elsif ( exists $param{fh} ) {
    $fh = $param{fh};
  } else {
    croak 'must specify file or fh parameter to read_scala';
  }
  if ( exists $param{binmode} ) {
    binmode $fh, $param{binmode} or croak 'binmode failed: ' . $!;
  } elsif ( exists $self->{_binmode} ) {
    binmode $fh, $self->{_binmode} or croak 'binmode failed: ' . $!;
  }

  my ( @scala, $line_count );
  while ( !eof($fh) ) {
    my $line = readline $fh;
    croak 'readline failed: ' . $! unless defined $line;
    croak 'input exceeds MAX_LINES' if ++$line_count >= $self->{_MAX_LINES};
    next if $line =~ m/^[!]/;    # skip comments

    chomp $line;
    push @scala, $line;

    last if @scala == 2;
  }
  # but as might hit the MAX_LINES or eof() instead check again...
  if ( @scala != 2 ) {
    croak 'missing description or note count lines';
  }

  $self->{_description} = shift @scala;
  my $NOTECOUNT;
  if ( $scala[-1] =~ m/^\s*([0-9]+)/ ) {
    $NOTECOUNT = $1;
  } else {
    croak 'could not parse note count';
  }

  my @notes;
  my $cur_note = 1;
  while ( !eof($fh) ) {
    my $line = readline $fh;
    croak 'readline failed: ' . $! unless defined $line;
    croak 'input exceeds MAX_LINES' if ++$line_count >= $self->{_MAX_LINES};
    next if $line =~ m/^[!]/;    # skip comments

    # All the scales.zip *.scl files as of 2013-02-19 have digits on
    # both sides of the dot (so there are no ".42" cent values, but the
    # "these are all valid pitch lines" does include a "408." as
    # allowed). Some scale files have negative cents, though that is
    # illegal for ratios. All the ratios are plain numbers (no period),
    # or if they have a slash, it is followed by another number (so no
    # "42/" cases). Checked via various greps on the file contents.
    if ( $line =~ m/^\s* ( -?[0-9]+\. [0-9]* ) /x ) {
      push @notes, $1;    # cents
    } elsif ( $line =~ m{^\s* -[0-9] }x ) {
      # specification says these "should give a read error"
      croak 'invalid negative ratio in note list';
    } elsif ( $line =~ m{^\s* ( [1-9][0-9]* (?:/[0-9]+)? ) }x ) {
      my $ratio = $1;
      $ratio .= '/1' if $ratio !~ m{/};    # implicit qualify of ratios
      push @notes, $ratio;
    } else {
      # Nothing in the spec about non-matching lines, so blow up.
      # However, there are six files in scales.zip that have trailing
      # blank lines, though these blank lines occur only after an
      # appropriate number of note entries. So must exit loop before
      # reading those invalid? lines.
      croak 'invalid note specification on line ' . $.;
    }

    last if $cur_note++ >= $NOTECOUNT;
  }
  if ( @notes != $NOTECOUNT ) {
    croak 'expected '
      . $NOTECOUNT
      . ' notes but got '
      . scalar(@notes)
      . " notes";
  }
  $self->{_notes}  = \@notes;
  $self->{_ratios} = undef;

  return $self;
}

sub set_concertpitch {
  my ( $self, $cp ) = @_;
  croak 'concert pitch must be a positive number (Hz)'
    if !defined $cp
    or !looks_like_number $cp
    or $cp < 0;
  $self->{_concertpitch} = $cp;
  return $self;
}

sub set_description {
  my ( $self, $desc ) = @_;
  croak 'description must be string value'
    if !defined $desc
    or defined reftype $desc;
  $self->{_description} = $desc;
  return $self;
}

sub set_notes {
  my $self = shift;
  my @notes;
  for my $n ( ref $_[0] eq 'ARRAY' ? @{ $_[0] } : @_ ) {
    if ( $n =~ m{^ -?[0-9]+\. (?:[0-9]+)? $}x ) {
      push @notes, $n;
    } elsif ( $n =~ m{^ [1-9][0-9]* (?:/[0-9]+)? $}x ) {
      my $ratio = $n;
      $ratio .= '/1' if $ratio !~ m{/};    # implicit qualify of ratios
      push @notes, $ratio;
    } else {
      croak 'notes must be integer ratios or real numbers';
    }
  }
  $self->{_notes}  = \@notes;
  $self->{_ratios} = undef;
  return $self;
}

sub write_scala {
  my $self = shift;
  croak 'no scala loaded' if !exists $self->{_notes};

  my %param;
  if ( @_ == 1 ) {
    $param{file} = $_[0];
  } else {
    %param = @_;
  }

  my $fh;
  if ( exists $param{file} ) {
    open( $fh, '>', $param{file} ) or croak 'open failed: ' . $!;
  } elsif ( exists $param{fh} ) {
    $fh = $param{fh};
  } else {
    croak 'must specify file or fh parameter to write_scala';
  }
  if ( exists $param{binmode} ) {
    binmode $fh, $param{binmode} or croak 'binmode failed: ' . $!;
  } elsif ( exists $self->{_binmode} ) {
    binmode $fh, $self->{_binmode} or croak 'binmode failed: ' . $!;
  }

  say $fh ( exists $self->{_description} and defined $self->{_description} )
    ? $self->{_description}
    : '';
  say $fh ' ', scalar @{ $self->{_notes} };
  say $fh '!';    # conventional comment between note count and notes
  for my $note ( @{ $self->{_notes} } ) {
    say $fh ' ', $note;
  }

  return $self;
}

1;
__END__

=head1 NAME

Music::Scala - Scala scale support for Perl

=head1 SYNOPSIS

  use Music::Scala ();
  my $scala = Music::Scala->new;

  $scala->read_scala('groenewald_bach.scl');
  $scala->get_description; # "Jurgen Gronewald, si..."
  $scala->get_notes;       # (256/243, 189.25008, ...)

  $scala->set_concertpitch(422.5);
  $scala->interval2freq(0, 1); # (422.5, 445.1)

  $scala->set_description('Heavenly Chimes');
  $scala->set_notes(qw{ 32/29 1/2 16/29 });
  $scala->write_scala('chimes.scl');

=head1 DESCRIPTION

Scala scale support for Perl: reading, writing, setting, and interval to
frequency conversion methods are provided. The L</"SEE ALSO"> section
links to the developer pages for the specification, along with an
archive of scala files that define various tunings and temperaments.

Warning! This is a new module. Features or handling in particular of
cents versus ratios may change as I figure out the code.

=head1 METHODS

Methods will B<die> or B<croak> under various conditions, mostly related
to bad input. B<new> would be a good one to start with.

=over 4

=item B<get_concertpitch>

Returns the concert pitch presently set in the object. 440 (Hz) is
the default.

=item B<get_description>

Returns the description of the scala data. This will be the empty string
if no description was read or set prior.

=item B<get_notes>

Returns, as a list, the "notes" of the scala, but throws an exception if
this field has not been set by some previous method. The notes are
either real numbers (representing values in cents, or 1/1200 of an
octave (these may be negative)) or otherwise integer ratios (e.g.
C<3/2> or C<2>).

  $scala->read_scala(file => $some_file);
  my @notes = $scala->get_notes;
  if (@notes == 12) { ...

The implicit C<1/1> for unison is not contained in the list of notes;
the first element is for the 2nd degree of the scale (e.g. the minor
second of a 12-tone scale).

=item B<new> I<optional_params>, ...

Constructor. Returns object. Accepts various optional parameters.

=over 4

=item *

I<binmode> - sets a default C<binmode> layer that will be used when
reading scala files (unless that B<read_scala> call has a different
I<binmode> passed to it). Given that the scala file specification
demands "ISO 8859-1 'Latin-1' or the ASCII subset", and uses Internet
linefeeds, a reasonable default would be:

  Music::Scala->new( binmode => ':encoding(iso-8859-1):crlf' );

Output encoding may also need to be set, if in particular the
I<description> field of the scala definition will be printed or saved
elsewhere. See L<perluniintro> for details. Note that both B<read_scala>
and B<write_scala> will use this same global I<binmode> value if no
I<binmode> is passed to those methods.

=item *

I<concertpitch> - sets the reference value (in Hertz) for conversions
using the B<interval2freq> method. By default this is 440Hz.

=item *

I<MAX_LINES> - sets the maximum number of lines to read while parsing
data. Sanity check high water mark in the event bad input is passed.

=back

=item B<read_scala> I<filename>

Parses a scala file. Will throw some kind of exception if anything at
all is wrong with the input. Use the C<get_*> methods to obtain the
scala data thus parsed. Comments in the input file are ignored, so
anything subsequently written using B<write_scala> will lack those. All
ratios are made implicit by this method; that is, a C<2> would be
qualified as C<2/1>.

As an alternative, accepts also I<file> or I<fh> hash keys, along with
I<binmode> as in the B<new> method:

  $scala->read_scala('somefile');
  $scala->read_scala( file => 'file.scl', binmode => ':crlf' );
  $scala->read_scala( fh   => $input_fh );

Returns the Music::Scala object, so can be chained with other calls.

=item B<set_concertpitch> I<frequency>

Sets the concert pitch to the specified positive value. Will throw an
exception if the input does not look like a positive number.

=item B<set_description> I<description>

Sets the description. Should be a string. Returns the Music::Scala
object, so can be chained with other calls.

=item B<set_notes> I<array_or_array_ref>

Sets the notes. Can be either an array, or an array reference, ideally
containing values in ratios or cents as per the Scala scale file
specification, and the method will throw an exception if these ideals
are not met. Returns the Music::Scala object, so can be chained with
other calls.

=item B<write_scala> I<filename>

Writes a scala file. Will throw some kind of exception if anything at
all is wrong, such as not having scala data loaded in the object. Like
B<read_scala> alternatively accepts I<file> or I<fh> hash keys, along
with a I<binmode> option to set the output encoding.

  $scala->write_scala('out.scl');
  $scala->write_scala( file => 'out.scl', binmode => ':crlf' );
  $scala->write_scala( fh => $output_fh );

Data will likely not be written until the I<fh> passed is closed. If
this seems surprising, see L<http://perl.plover.com/FAQs/Buffering.html>
to learn why it is not.

Returns the Music::Scala object, so can be chained with other calls.

=back

=head1 EXAMPLES

Print names of any scala files whose note count is 12 (only about 29% of
the C<scales.zip> as of 2013-02-20).

  #!/usr/bin/env perl
  use strict;
  use warnings;
  use feature qw/say/;
  
  use Music::Scala ();
  my $s = Music::Scala->new;
  
  for my $file ( glob('*.scl') ) {
    eval { say $file if $s->read_scala($file)->get_notes == 12 };
    warn "could not parse '$file': $@" if $@;
  }

=head1 SEE ALSO

L<http://www.huygens-fokker.org/scala/> by Manuel Op de Coul, and the
scala archive L<http://www.huygens-fokker.org/docs/scales.zip>.

Scales, tunings, and temperament would be good music theory topics to
read up on, e.g. chapters in "Musicmathics, volume 1" by Gareth Loy
(among many other more in-depth treatments stemming from the more than
one centuries of development behind these topics).

L<http://github.com/thrig/Music-Scala> for the perhaps more current
version of this code, or to report bugs, etc.

=head1 AUTHOR

Jeremy Mates, E<lt>jmates@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2013 by Jeremy Mates

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself, either Perl version 5.16 or, at
your option, any later version of Perl 5 you may have available.

=cut
