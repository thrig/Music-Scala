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

our $VERSION = '0.50';

# To avoid file reader from wasting too much time on bum input (longest
# scala file 'fortune.scl' in archive as of 2013-02-19 has 617 lines).
my $MAX_LINES = 3000;

########################################################################
#
# SUBROUTINES

# MIDI calculation, for easy comparison to scala results
sub freq2pitch {
  my ( $self, $freq ) = @_;
  croak "frequency must be a positive number"
    if !looks_like_number $freq
    or $freq < 0;

  return sprintf "%.0f",
    69 + 12 * ( log( $freq / $self->{_concertpitch} ) / log(2) );
}

sub get_binmode {
  my ($self) = @_;
  return $self->{_binmode};
}

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

sub get_ratios {
  my ($self) = @_;
  croak 'no scala loaded' if !exists $self->{_notes};
  if ( !defined $self->{_ratios} ) {
    $self->{_ratios} = [ $self->notes2ratios( $self->{_notes} ) ];
  }
  return @{ $self->{_ratios} };
}

sub interval2freq {
  my $self = shift;
  croak 'no scala loaded' if !exists $self->{_notes};

  if ( !defined $self->{_ratios} ) {
    $self->{_ratios} = [ $self->notes2ratios( $self->{_notes} ) ];
  }

  my @freqs;
  for my $i ( ref $_[0] eq 'ARRAY' ? @{ $_[0] } : @_ ) {
    if ( $i == 0 ) {    # special case for unison (ratio 1/1)
      push @freqs, $self->{_concertpitch};
    } else {
      my $is_dsc = $i < 0 ? 1 : 0;

      # "Octave" portion (might be zero) - how many times the interval
      # passes through the complete scale
      my $octave_count  = abs int $i / @{ $self->{_ratios} };
      my $octave_offset = $self->{_ratios}->[-1] * $octave_count;
      $octave_offset = 1 / $octave_offset
        if $is_dsc and $octave_offset != 0;

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
    $self->{_concertpitch} = $param{concertpitch};
  }

  $self->{_MAX_LINES} =
    exists $param{MAX_LINES} ? $param{MAX_LINES} : $MAX_LINES;

  $self->{_ratios} = undef;

  bless $self, $class;

  if ( exists $param{file} ) {
    $self->read_scala( file => $param{file} );
  } elsif ( exists $param{fh} ) {
    $self->read_scala( fh => $param{fh} );
  }

  return $self;
}

sub notes2ratios {
  my $self = shift;

  my @ratios;
  for my $n ( ref $_[0] eq 'ARRAY' ? @{ $_[0] } : @_ ) {
    if ( $n =~ m{(\d+)/(\d+)} ) {
      push @ratios, $1 / $2;    # ratio, as marked with /
    } else {
      # Inverse cent (cent to ratio) equation. Lifted from
      # "Musimathics, volume 1", p.46. Magic number is
      # (1200/log10(2))
      push @ratios, 10**( $n / 3986.31371386484 );
    }
  }

  return @ratios > 1 ? @ratios : $ratios[0];
}

# MIDI for comparison, the other way
sub pitch2freq {
  my ( $self, $pitch ) = @_;
  croak "pitch must be MIDI number"
    if !looks_like_number $pitch
    or $pitch < 0;

  return $self->{_concertpitch} * ( 2**( ( $pitch - 69 ) / 12 ) );
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

sub set_binmode {
  my ( $self, $binmode ) = @_;
  $self->{_binmode} = $binmode;
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
  $scala->get_ratios;

  $scala->set_concertpitch(422.5);
  $scala->interval2freq(0, 1); # (422.5, 445.1)

  $scala->set_description('Heavenly Chimes');
  $scala->set_notes(qw{ 32/29 1/2 16/29 });
  $scala->write_scala('chimes.scl');

  # or cents, note the quoting on .0 value
  $scala->set_notes(250.9, 483.3, 715.6, 951.1, '1200.0');

  # MIDI equal temperament algos for comparison
  $scala->pitch2freq(69);
  $scala->freq2pitch(440);

=head1 DESCRIPTION

Scala scale support for Perl: reading, writing, setting, and interval to
frequency conversion methods are provided. The L</"SEE ALSO"> section
links to the developer pages for the specification, along with an
archive of scala files that define various tunings and temperaments.

=head1 METHODS

Methods will B<die> or B<croak> under various conditions, mostly related
to bad input. B<new> would be a good one to start with.

=over 4

=item B<freq2pitch> I<frequency>

Converts the passed frequency (Hz) to the corresponding MIDI pitch
number using the MIDI algorithm (equal temperament), as influenced by
the I<concertpitch> setting. Unrelated to scala, but perhaps handy for
comparison with results from B<interval2freq>.

=item B<get_binmode>

Returns the current C<binmode> layer setting, C<undef> by default.

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

=item B<get_ratios>

Returns, as a list, the "notes" of the scala, except all converted to
ratios (notes may either be interval numbers or values in cents). Throws
an exception if the notes have not been set by some previous method.

=item B<interval2freq> I<intervals ...>

Converts a list of passed interval numbers (list or single array
reference) to frequencies (in Hz) that are returned as a list. Interval
numbers are integers, C<0> for unison, C<1> for the first interval
(which would be a minor 2nd for a 12-note scale, but something different
for scales of other sizes), and so on up to an octave or moral
equivalent thereof, depending on the scale. Negative intervals take the
frequency in the other direction, e.g. C<-1> for what in a 12-note
system would be a minor 2nd downwards.

Conversions are based on the I<concertpitch> setting, which is 440Hz by
default. Use B<set_concertpitch> to adjust this, for example to base the
conversion around the frequency of MIDI pitch 60:

  $scala->set_concertpitch(261.63);

Some scala files note what this value should be in the comments or
description, or it may vary based on the specific software or
instruments involved.

The output frequencies may need to be rounded because of floating
point math:

  # octave, plus default concert pitch of 440, so expect 880
  my $scala = Music::Scala->new->set_notes('1200.000');

  $scala->interval2freq(1);   # 879.999999999999

  sprintf "%.2f", $scala->interval2freq(1); # 880.00

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

I<file> - filename, if specified, that will be passed to B<read_scala>.

=item *

I<fh> - file handle, if specified, that will be passed to B<read_scala>,
but only if I<file> is not specified.

=item *

I<MAX_LINES> - sets the maximum number of lines to read while parsing
data. Sanity check high water mark in the event bad input is passed.

=back

=item B<notes2ratios> I<notes ...>

Given a list of notes, returns a list of corresponding ratios. Used
internally by the B<get_ratios> and B<interval2freq> methods.

=item B<pitch2freq> I<MIDI_pitch_number>

Converts the given MIDI pitch number to a frequency using the MIDI
conversion algorithm (equal temperament), as influenced by the
I<concertpitch> setting.

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

=item B<set_binmode> I<binmode_layer>

Sets the default C<binmode> layer used in B<read_scala> and
B<write_scala> methods (unless a custom I<binmode> argument is passed to
those calls). Returns the Music::Scala object, so can be chained with
other calls.

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

NOTE cents with no value past the decimal must be quoted in code, as
otherwise Perl converts the value to C<1200> which the code then turns
into the integer ratio C<1200/1> instead of what should be C<2/1>.
B<read_scala> does not suffer this problem, as it is looking for the
literal dot (that nothing is removing automatically) and that is a
different code path than what happens for ratios.

  $scala->set_notes(250.9, 483.3, 715.6, 951.1, '1200.0');

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

Another interesting question is which scales contain the octave (and
whether that octave is also the ultimate element of the note list). This
requires converting the "notes" into actual ratios, as otherwise an
octave might be represented as C<2/1> or C<1200.0> or C<4/2> or so
forth. Roughly 87% of the scales are bounded at the octave:

  for my $file ( glob('*.scl') ) {
    my @ratios = $s->read_scala($file)->get_ratios;
    if ( $ratios[-1] == 2 ) { say $file }
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
