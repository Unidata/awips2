-----README-----

The caveEnvironment directory contains scripts, libraries and excutable files that are
installed by awips2-cave-wrapper to run the TMCP application. The scripts and executable files reside in the
bin directory with the source for the executable files are in the AWIPS2-nativelib repository. Two of the library
files libutil.so and libwhfs.so are also generated from the AWIPS2-nativelib build. 

The following directories exist within the caveEnvironment directory:
	bin - the executable files should copied here after being built as part of the AWIPS2_nativelib project build.
	lib - libutil.so and libwhfs.so can be copied here after being created as part of the AWIPS2_nativelib project build.
			
To create these files run the build script AWIPS2_nativelib/build.native/build.sh.

       From AWIPS2_nativelib run:

            ./build.native/build.sh -c Default /awips2/eclipse

       The executable files will be created in 
            
            AWIPS2_nativelib/build.native/i386-pc-linux-gnu/bin/ 
 
       and should be copied to 

           the AWIPS2_baseline/cave/build/static/linux/cave/caveEnvironment/bin 

       The library files library.ohd.util.so and library.ohd.whfs.so created in 

           AWIPS2_nativelib/build.native/i386-pc-linux-gnu/lib/

       and can be copied to 

           AWIPS2_baseline/cave/build/static/linux/cave/caveEnvironment/lib/
       
       as libutil.so and libwhfs.so if needed.
 
