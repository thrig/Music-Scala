use 5.010000;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'Music::Scala',
    VERSION_FROM      => 'lib/Music/Scala.pm', # finds $VERSION
    PREREQ_PM         => {}, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/Music/Scala.pm', # retrieve abstract from module
       AUTHOR         => 'Jeremy Mates <jmates@cpan.org>') : ()),
);
