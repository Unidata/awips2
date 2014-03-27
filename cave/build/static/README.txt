-----README-----

The static directory contains common scripts and other files that need to
be installed with CAVE. These files generally are not used by a specific
component unless the component is "common" and they do not need to be compiled.

The following common directories exist within the static directory:
	common - these files should be installed on every Operating System for every architecture.
	linux - these files should be installed on every LINUX Operating System for every 
			architecture.
			
We also have the following os / architecture specific directories within the static
directory. The name of the directories is based on the eclipse (3.6.1) os.arch
designation.
	linux.x86 - these files will only be installed on a 32-bit Linux Operating System.
	linux.x86_64 - these files will only be installed on a 64-bit Linux Operating System.
	win32.x86 - these files will only be installed on a 32-bit MS Windows Operating System.
	win32.amd64 - these files will only be installed on a 64-bit Windows Operating System.
	macosx.x86 - these files will only be installed on a 32-bit Apple OS X Operating System.