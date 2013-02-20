# -*- Perl -*-
#
# Scala scale support for Perl - http://www.huygens-fokker.org/scala/

package Music::Scala;

use 5.010000;
use strict;
use warnings;

use Carp qw/croak/;

our $VERSION = '0.01';

########################################################################
#
# SUBROUTINES

sub new {
  my ( $class, %param ) = @_;
  my $self = {};
  bless $self, $class;
  return $self;
}

1;
__END__

=head1 NAME

Music::Scala - Scala scale support for Perl

=head1 SYNOPSIS

  use Music::Scala;
  TODO

=head1 DESCRIPTION

Scala scale support for Perl. The L</"SEE ALSO"> section links to
the developer pages and tuning archive available in the Scala scale
file format.

=head1 METHODS

TODO

=head1 SEE ALSO

http://www.huygens-fokker.org/scala/ by Manuel Op de Coul.

http://www.huygens-fokker.org/docs/scales.zip

=head1 AUTHOR

Jeremy Mates, E<lt>jmates@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2013 by Jeremy Mates

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself, either Perl version 5.16 or, at
your option, any later version of Perl 5 you may have available.

=cut
