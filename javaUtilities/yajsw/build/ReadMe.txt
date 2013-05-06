
The manifest in this folder should be added to wrapper.jar and wrapperApp.jar
It assumes that the relative folder structure of the jar files is not changed.
If you would like to change the folder structure you should adapt the manifest accordingly.

A build script is included in the subfolder gradle.

Building SNAPSHOT jars is not included in this build. Please use the following subversion 
revisions to build them:

commons-vfs-2.0-SNAPSHOT.jar						784083
commons-cli-2-SNAPSHOT.jar							647073
commons-configuration-1.7-SNAPSHOT.jar	784085

NOTE: the above projects have not had new releases for years, but have been commiting new features which are used here.