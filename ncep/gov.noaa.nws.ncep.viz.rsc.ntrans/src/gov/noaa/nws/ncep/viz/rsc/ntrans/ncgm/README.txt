ncgm -- Computer Graphics Metafile interpreter and renderer

See also the other README.TXT, in sibling "jcgm" package.

This package contains classes which extend a limited subset of the classes
in the "jcgm" package.

(For more information about "jcgm", see:  http://jcgm.sourceforge.net)

The key classes here are...
  NcCGM (extends jcgm's CGM):
      Builds (via NcCGM.read(DataInput)) and holds (as an ordered list
      of "commands") a Java representation of a single CGM image.
  NcCommand (extends jcgm's Command):
      Builds (via *static* NcCommand.read(DataInput)) and returns
      a single CGM command.  The readCommand() method has been modified
      from the jcgm original to "know" which commands have extensions
      in ncgm, and to create them instead of the base jcgm versions.
  INcCommand:
      Interface whose implementation enables these ncgm extended
      commands to know how to contribute themselves to an AWIPS II
      image.

An AWIPS II image is constructed in an ImageBuilder object, by sequential
"execution" of these CGM commands.

We only extend the "jcgm" commands that are of interest to NTRANS.  Any others
(if somehow encountered in an input metafile) will simply be ignored.