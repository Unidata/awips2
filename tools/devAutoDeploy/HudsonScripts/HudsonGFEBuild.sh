#!/bin/sh
# $1 = output directory to put installer jar files (/share1/installers)
#	     This is designed so that different builds can use the same script but generate
#            output to different directories so they don't overwrite each other 
#            For example buid on the main trun2 and build on a tag/branch.
# $2 = base svn path (home/keystone/repo/products/awips/trunk2)
#            the base svn path should contain directories such as edexOsgi, cave, cots, etc
#            This is designed this way so the same script can be used to build the main 
#            trunk2 and a tag/branch.
# $3 = use update (true or false), if true the workspace will be kept so that the next time
# 	     the build is run only the recent changes will need to be checked out.  
#            Otherwise all files in the workspace will be deleted at the end of the build
#            to save space.
# $4 = type of build to run (edex, cave, nightly).  
#            If edex - only EDEX will be built-for the svn polling build to build EDEX on check-ins
#            If cave - only CAVE will be built-for the svn polling build to build CAVE on check-ins
#            If nightly - edex, cave and all installers will be built-for nightly builds
outputdir="$1"
svnpath="$2"
useupdate="$3"
type="$4"
command=''

echo "***SVN Info***"
/usr/local/bin/svn info file:///$svnpath


if [ "$useupdate" = "true" ]
then
	command="checkout"
	echo "****Using checkout****"
else
	command="export"
	echo "****Using export****"
fi

echo "------------------------------------------------------------"
echo "Copy the Hudson skeleton directory before building anything"
echo "-------------------------------------------------------------"
mkdir -p all/

WORKING_DIR=`pwd`
projects=''
if [ "$type" = "nightly" -o "$type" = "cave" ]
then
	projects='cave cots edexOsgi RadarServer'
else
	projects='cots edexOsgi'
fi


echo "------------------------------------------------------------"
echo "Exporting ${projects}"
echo "------------------------------------------------------------"

cd all

for p in $projects
do
   echo "exporting project ${p}"
   for a in `/usr/local/bin/svn list file:///${svnpath}/${p}`
   do
      if [ "${a}" != ".project" ]
      then
         /usr/local/bin/svn $command --force file:///${svnpath}/${p}/${a}
      fi
   done
done

if [ "$type" = "nightly" ]
then
	echo "Checking out ade/Installer.edex"
	/usr/local/bin/svn $command --force file:///$svnpath/ade/Installer.edex Installer.edex
	echo "Checking out ade/Installer.cli"
	/usr/local/bin/svn $command --force file:///$svnpath/ade/Installer.cli Installer.cli
	echo "Checking out ade/Installer.cave"
	/usr/local/bin/svn $command --force file:///$svnpath/ade/Installer.cave Installer.cave
	echo "Checking out ade/installer.radarserver"
	/usr/local/bin/svn $command --force file:///$svnpath/ade/installer.radarserver installer.radarserver
	echo "Checking out ade/Installer.gfe"
	/usr/local/bin/svn $command --force file:///$svnpath/ade/Installer.gfe Installer.gfe
	#echo "Checking out ade/Installer.python"
	#/usr/local/bin/svn $command --force file:///$svnpath/ade/Installer.python Installer.python
	echo "Checking out nativeLib/build.native.i386-pc-linux-gnu"
	/usr/local/bin/svn $command --force file:///$svnpath/nativeLib/build.native.i386-pc-linux-gnu build.native.i386-pc-linux-gnu
	echo "Checking out nativeLib/dist.native"
	/usr/local/bin/svn $command --force file:///$svnpath/nativeLib/dist.native dist.native
fi

cd ..
echo "Getting skeleton code"
cp -pRuf /usr/share/tomcat5/AWIPS_skeleton/all .

echo "current directory: $WORKING_DIR"

#if [ "$type" = "nightly" ]
#then
#	cd $WORKING_DIR/all/build.rcm
#	echo "-------------------------------------------------------------"
#	echo "Building RadarServer"
#	echo "-------------------------------------------------------------"
#	./build.sh -eclipse=$WORKING_DIR/all/uframe-eclipse
#	cd ../installer.radarserver
#	$ANT_HOME/bin/ant build.all
#	cp temp/build/jar/radarserver-installer.jar $outputdir
#fi
#if [ "$type" = "edex" -o "$type" = "nightly" ]
#then
#	echo "--------------------------------------------------------------"
#	echo " Building Edex"
#	echo "--------------------------------------------------------------"
#	cd $WORKING_DIR/all/build.edex
#	./build.sh -eclipse=$WORKING_DIR/all/uframe-eclipse
#fi
#if [ "$type" = "nightly" ]
#then
#	cd $WORKING_DIR/all/Installer.edex
#	$ANT_HOME/bin/ant buildAll
#	cp temp/build/jar/edex-installer.jar $outputdir
#	echo "--------------------------------------------------------------"
#	echo " Building CLI Installer"
#	echo "--------------------------------------------------------------"
#	cd $WORKING_DIR/all/Installer.cli
#	$ANT_HOME/bin/ant buildAll
#	cp temp/build/jar/cli-installer.jar $outputdir
#	#echo "--------------------------------------------------------------"
#	#echo " Building Python Installer"
#	#echo "--------------------------------------------------------------"
#	#cd $WORKING_DIR/all/Installer.python
#	#$ANT_HOME/bin/ant buildAll
#	#cp temp/build/jar/python-installer.jar /share1/installers
#fi
if [ "$type" = "cave" -o "$type" = "nightly" ]
then
	echo "--------------------------------------------------------------"
	echo "Building CAVE"
	echo "--------------------------------------------------------------"
	cd $WORKING_DIR/all/build
	./build.sh -eclipse=$WORKING_DIR/all/uframe-eclipse
fi
if [ "$type" = "nightly" ]
then
#	cp alertviz/tmp/I.AlertViz/AlertViz-linux.gtk.x86.zip $outputdir
#	cd $WORKING_DIR/all/Installer.cave
#	$ANT_HOME/bin/ant buildAll
#	cp temp/build/jar/cave-installer.jar $outputdir
	echo "--------------------------------------------------------------"
	echo "Building GFE CLient Installer"
	echo "--------------------------------------------------------------"
	cd $WORKING_DIR/all/Installer.gfe
	$ANT_HOME/bin/ant buildAll
	cp release~/gfe-client-installer.jar $outputdir
	# echo " ----------------------------------------------------------------"
	# echo " Building Javadocs"
	# echo " ----------------------------------------------------------------"
	# echo " move back to trunk/build.edex for the javadoc to build from there"
	# cd $WORKING_DIR/all/build.edex
	# build the javadocs
	# $ANT_HOME/bin/ant -f build-docs.xml
	# echo " ----------------------------------------------------------------"
	# echo " Finished Building Javadocs"
	# echo " ----------------------------------------------------------------"
fi

if [ "$useupdate" != "true" ]
then
	# If not using update clean up the working dir when done
	echo " "
	echo " ----------------------------------------------------------------"
	echo " Deleting working files"
	echo " ----------------------------------------------------------------"
	cd $WORKING_DIR
	rm -rf all
fi

echo "***SVN Info***"
/usr/local/bin/svn info file:///$svnpath
