jcgm -- Computer Graphics Metafile interpreter and renderer

This package contains source code from the "jcgm" open source Java
implementation to interpret and render Computer Graphics Metafile (CGM)
graphics files.

For more information about "jcgm", see:  http://jcgm.sourceforge.net

See also LICENSE.txt, which states (in part):
  "Redistribution and use in source and binary forms, with or without
   modification, are permitted...
   [subject to conditions met by LICENSE.txt contents]"

Some of these files have been modified slightly from their original form,
mainly to make originally 'private' fields 'protected' in order to allow
access from classes over in the "ncgm" package which extend them.

These changes are usually marked by comments showing the //ORIGINAL// form
of the code.

Note that only the "core" code from jcgm appears here; the "image" code
(from a separate branch of the jcgm source tree) is not included.  That's
because we are interested only in the "interpreter" part of jcgm, and not
the "rendering" part; that is, we use jcgm to build a Java representation
of CGM commands, but we must do our own rendering via AWIPS II graphics.

Classes in the "ncgm" package generally extend classes from "jcgm", in order
to implement the INcCommand interface, and so perform this rendering.

Note that all of the jcgm core classes are present, even though we do not
currently use most of them.  This is for completeness, and to allow
possible future expansion.