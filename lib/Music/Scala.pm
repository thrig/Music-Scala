# -*- Perl -*-
#
# Scala scale (musical tuning and temperament) support for Perl, based
# on specification at: http://www.huygens-fokker.org/scala/

package Music::Scala;

use 5.010000;
use strict;
use warnings;

use Carp qw/croak/;

our $VERSION = '0.10';

# To avoid file reader from wasting too much time on bum input (longest
# scala file 'fortune.scl' in archive as of 2013-02-19 has 617 lines).
my $MAX_LINES = 3000;

########################################################################
#
# SUBROUTINES

sub new {
  my ( $class, %param ) = @_;
  my $self = {};

  $self->{_binmode} = $param{binmode} if exists $param{binmode};
  $self->{_MAX_LINES} =
    exists $param{MAX_LINES} ? $param{MAX_LINES} : $MAX_LINES;

  bless $self, $class;
  return $self;
}

sub get_description {
  my ($self) = @_;
  croak 'no scala file loaded' if !exists $self->{_scala}->{description};
  return $self->{_scala}->{description} // '';
}

# No method for getting the spec file note count value as that's a) not
# saved in the object and b) can be obtained by calling what this
# returns as array in scalar context.
sub get_notes {
  my ($self) = @_;
  croak 'no scala file loaded' if !exists $self->{_scala}->{notes};
  return $self->{_scala}->{notes};
}

sub read_scala {
  my ( $self, %param ) = @_;

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

  $self->{_scala}->{description} = shift @scala;
  my $NOTECOUNT;
  if ( $scala[-1] =~ m/^\s*([0-9]+)/ ) {
    $NOTECOUNT = $1;
  } else {
    croak 'could not parse note count';
  }

  my @notes;
  my $notes_to_read = $NOTECOUNT;
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
    if ( $line =~ m/^\s*(-?[0-9]+\.[0-9]*)/ ) {
      push @notes, $1;    # cents
    } elsif ( $line =~ m{^\s*-[0-9]+} ) {
      # specification says these "should give a read error"
      croak 'invalid negative ratio in note list';
    } elsif ( $line =~ m{^\s*([0-9]+(?:/[0-9]+)?)} ) {
      my $ratio = $1;
      $ratio .= '/1' if $ratio !~ m{/};
      push @notes, $ratio;
    } else {
      # nothing in the spec about non-matching lines, so blow up
      croak 'invalid note specification at line ' . $.;
    }

    last if $notes_to_read-- < 1;
  }
  if ( @notes != $NOTECOUNT ) {
    croak 'expected '
      . $NOTECOUNT
      . ' notes but got '
      . scalar(@notes)
      . " notes";
  }

  unshift @notes, '1/1';    # "degree 0 of 1/1 is implicit"
  $self->{_scala}->{notes} = \@notes;

  return $self;
}

1;
__END__

=head1 NAME

Music::Scala - Scala scale support for Perl

=head1 SYNOPSIS

  use Music::Scala;
  my $scala = Music::Scala->new;

  $scala->read_scala( file => ' bossart-muri . scl ' );

  $scala->get_description;  # "Victor Ferdinand Bossart's..."
  $scala->get_notes;        # [" 1 / 1 ", 80.4499, 195.11250, ...]

=head1 DESCRIPTION

Scala scale (C<*.scl> file) support for Perl. The L</" SEE ALSO ">
section links to the developer pages for the specification, along with
an archive of scala files for various tunings and temperaments.

Warning! This is a new module. Features or handling in particular of
cents versus ratios may change as I figure out the code. Additional
methods will likely be added to assist with the task of calculating
frequencies given particular notes or pitches or whatnot, but for now
there is reasonable C<*.scl> parsing support.

=head1 METHODS

Methods may B<die> or B<croak> under various conditions. B<new> would be
a good one to start with.

=over 4

=item B<get_description>

Returns the description (a string, possibly the empty one) of the scala
data, but throws an exception if B<read_scala> has not yet parsed a
scala definition into the object.

=item B<get_notes>

Returns the notes of the scala data (as an array reference), but throws
an exception if B<read_scala> has not yet parsed a scala definition into
the object. The notes may be either real numbers (values in cents,
possibly even negative, noted by a C<.> somewhere in them) or otherwise
integer ratios (e.g. C<3/2>) that denote ratios.

The first element is for unison, hence the ratio of C<1/1>.

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
elsewhere. See L<perluniintro> for details.

=item *

I<MAX_LINES> - sets the maximum number of lines to read while parsing
data. Sanity check high water mark in the event bad input is passed.

=back

=item B<read_scala> I<file => 'filename'> | I<fh => $fh>

Parses a scala I<file> (or instead filehandle via the I<fh> option),
perhaps also with a I<binmode> specification (same as documented under
the B<new> method). Will throw some kind of exception if anything at all
is wrong with the input. Use the C<get_*> methods to obtain the scala
data thus parsed.

Returns the Music::Scala object, so can be chained with other calls.

=back

=head1 SEE ALSO

L<http://www.huygens-fokker.org/scala/> by Manuel Op de Coul, and the
scala archive L<http://www.huygens-fokker.org/docs/scales.zip>.

Scales, tunings, and temperament would be good music theory topics to
read up on, e.g. chapters in "Musicmathics, volume 1" by Gareth Loy
(among many other more in-depth treatments stemming from the more than
one centuries of development behind these topics).

=head1 AUTHOR

Jeremy Mates, E<lt>jmates@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2013 by Jeremy Mates

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself, either Perl version 5.16 or, at
your option, any later version of Perl 5 you may have available.

=cut
