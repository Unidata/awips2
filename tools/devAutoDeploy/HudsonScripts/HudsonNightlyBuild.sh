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

echo "------------------------------------------------------------"
echo "Copy the Hudson skeleton directory before building anything"
echo "-------------------------------------------------------------"
mkdir -p all/

echo "------------------------------------------------------------"
echo "Checking out code"
echo "------------------------------------------------------------"

echo "Checking out cave"
/usr/local/bin/svn checkout file:///$2/cave cave
echo "Checking out cots"
/usr/local/bin/svn checkout file:///$2/cots cots
echo "Checking out edexOsgi"
/usr/local/bin/svn checkout file:///$2/edexOsgi edexOsgi
echo "Checking out RadarServer"
/usr/local/bin/svn checkout file:///$2/RadarServer RadarServer
echo "Checking out ade/Installer.edex"
/usr/local/bin/svn checkout file:///$2/ade/Installer.edex Installer.edex
echo "Checking out ade/Installer.cli"
/usr/local/bin/svn checkout file:///$2/ade/Installer.cli Installer.cli
echo "Checking out ade/Installer.cave"
/usr/local/bin/svn checkout file:///$2/ade/Installer.cave Installer.cave
echo "Checking out ade/installer.radarserver"
/usr/local/bin/svn checkout file:///$2/ade/installer.radarserver installer.radarserver
echo "Checking out ade/Installer.gfe"
/usr/local/bin/svn checkout file:///$2/ade/Installer.gfe Installer.gfe
#echo "Checking out ade/Installer.python"
#/usr/local/bin/svn checkout file:///$2/ade/Installer.python Installer.python
echo "Checking out nativeLib/build.native.i386-pc-linux-gnu"
/usr/local/bin/svn checkout file:///$2/nativeLib/build.native.i386-pc-linux-gnu build.native.i386-pc-linux-gnu
echo "Checking out nativeLib/dist.native"
/usr/local/bin/svn checkout file:///$2/nativeLib/dist.native dist.native

echo "Moving all files to all directory"
rsync -ruq --delete cave/* all
rsync -ruq --delete cots/* all
rsync -ruq --delete edexOsgi/* all
rsync -ruq --delete RadarServer/* all
rsync -ruq --delete Installer.edex all
rsync -ruq --delete Installer.cave all
rsync -ruq --delete Installer.cli all
rsync -ruq --delete Installer.gfe all
#rsync -ruq --delete Installer.python all
rsync -ruq --delete installer.radarserver all
rsync -ruq --delete build.native.i386-pc-linux-gnu all
rsync -ruq --delete dist.native all

echo "Getting skeleton code"
cp -pRuf /usr/share/tomcat5/AWIPS_skeleton/all .

WORKING_DIR=`pwd`

echo "current directory: $WORKING_DIR"

echo "-------------------------------------------------------------"
echo "Building RadarServer"
echo "-------------------------------------------------------------"
cd all/build.rcm
./build.sh -eclipse=$WORKING_DIR/all/uframe-eclipse
cd ../installer.radarserver
$ANT_HOME/bin/ant build.all
cp temp/build/jar/radarserver-installer.jar $1
echo "--------------------------------------------------------------"
echo " Building Edex"
echo "--------------------------------------------------------------"
cd ../build.edex
./build.sh -eclipse=$WORKING_DIR/all/uframe-eclipse
cd ../Installer.edex
$ANT_HOME/bin/ant buildAll
cp temp/build/jar/edex-installer.jar $1
echo "--------------------------------------------------------------"
echo " Building CLI Installer"
echo "--------------------------------------------------------------"
cd ../Installer.cli
$ANT_HOME/bin/ant buildAll
cp temp/build/jar/cli-installer.jar $1
#echo "--------------------------------------------------------------"
#echo " Building Python Installer"
#echo "--------------------------------------------------------------"
#cd ../Installer.python
#$ANT_HOME/bin/ant buildAll
#cp temp/build/jar/python-installer.jar /share1/installers
echo "--------------------------------------------------------------"
echo "Building CAVE"
echo "--------------------------------------------------------------"
cd ../build
./build.sh -eclipse=$WORKING_DIR/all/uframe-eclipse
cp alertviz/tmp/I.AlertViz/AlertViz-linux.gtk.x86.zip $1
cd ../Installer.cave
$ANT_HOME/bin/ant buildAll
cp temp/build/jar/cave-installer.jar $1
echo "--------------------------------------------------------------"
echo "Building GFE CLient Installer"
echo "--------------------------------------------------------------"
cd ../Installer.gfe
$ANT_HOME/bin/ant buildAll
cp release~/gfe-client-installer.jar $1
# echo " ----------------------------------------------------------------"
# echo " Building Javadocs"
# echo " ----------------------------------------------------------------"
# echo " move back to trunk/build.edex for the javadoc to build from there"
# cd ../build.edex
# build the javadocs
# $ANT_HOME/bin/ant -f build-docs.xml
# echo " ----------------------------------------------------------------"
# echo " Finished Building Javadocs"
# echo " ----------------------------------------------------------------"

if[ "$3" != "true"]
then
	# If not using update clean up the working dir when done
	echo " "
	echo " ----------------------------------------------------------------"
	echo " Deleting working files"
	echo " ----------------------------------------------------------------"
	cd ../..
	rm -rf *
fi
