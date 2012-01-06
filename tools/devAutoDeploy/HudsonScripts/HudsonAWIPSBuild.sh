#!/bin/bash -x
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
# $5 = output directory to put javadoc files (/share1/installers/javadocs)
# $6 = useFullEmail, if set to true uses the full email list, if set to false uses the test email list.
outputdir="$1"
svnpath="$2"
useupdate="$3"
type="$4"
javadocdir="$5"
useFullEmail="$6"
command=''
EMAILLIST=''

if [ "$useFullEmail" = "true" ]
then
	EMAILLIST="Kace_Chrisman@raytheon.com,mark_w_fegan@raytheon.com,Bryan_J_Rockwood@raytheon.com,Scott_W_Nicholson@raytheon.com,James_G_Berry@raytheon.com,Scott_Risch@raytheon.com,chammack@raytheon.com,Ron.Anderson@raytheon.com,Nathan.Jensen@raytheon.com,Nathan.Jensen@raytheon.com"
else
	EMAILLIST="Kace_Chrisman@raytheon.com" 
fi

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
	echo "Checking out ade/docs.edex"
	/usr/local/bin/svn $command --force file:///$svnpath/ade/docs.edex docs.edex
	echo "Checking out nativeLib/build.native"
	/usr/local/bin/svn $command --force file:///$svnpath/nativeLib/build.native
	echo "Checking out nativeLib/dist.native"
	/usr/local/bin/svn $command --force file:///$svnpath/nativeLib/dist.native dist.native
fi

cd ..
echo "Getting skeleton code"
cp -pRuf /usr/share/tomcat5/AWIPS_skeleton/all .

echo "current directory: $WORKING_DIR"

if [ "$type" = "nightly" ]
then
	cd $WORKING_DIR/all/build.rcm
	echo "-------------------------------------------------------------"
	echo "Building RadarServer"
	echo "-------------------------------------------------------------"
	./build.sh -eclipse=$WORKING_DIR/all/uframe-eclipse
	if [ $? -ne 0 ]
	then
		echo "----build.rcm failed----"	
		echo "----Sending Warning Email----"
		echo "`uname -n`: At `date`, build.rcm failed in Nightly build, continuing with rest of nightly build. Working directory: $WORKING_DIR." > email.txt
		mail -s "Warning build.rcm failed in Nightly build" ${EMAILLIST} < email.txt
	else
		cd ../installer.radarserver
		$ANT_HOME/bin/ant build.all
		if [ $? -ne 0 -o ! -e temp/build/jar/radarserver-installer.jar ]
		then
			echo "----installer.radarserver build failed----"	
			echo "----Sending Warning Email----"
			echo "`uname -n`: At `date`, installer.radarserver failed in Nightly build, continuing with rest of nightly build. Working directory: $WORKING_DIR." > email.txt
			mail -s "Warning installer.radarserver failed in Nightly build" ${EMAILLIST} < email.txt
		else
			cp temp/build/jar/radarserver-installer.jar $outputdir
		fi
	fi
fi
if [ "$type" = "edex" -o "$type" = "nightly" ]
then
	echo "--------------------------------------------------------------"
	echo " Building Edex"
	echo "--------------------------------------------------------------"
	cd $WORKING_DIR/all/build.edex
	./build.sh -eclipse=$WORKING_DIR/all/uframe-eclipse
	if [ $? -ne 0 ]
	then
		echo "----build.edex failed----"	
		echo "----Sending Error Email----"
		echo "`uname -n`: At `date`, build.edex failed in Nightly build, quitting build. Working directory: $WORKING_DIR." > email.txt
		mail -s "Error build.edex failed in Nightly build" ${EMAILLIST} < email.txt
		exit 99
	fi
fi
if [ "$type" = "nightly" ]
then
	cd $WORKING_DIR/all/Installer.edex
	$ANT_HOME/bin/ant buildAll
	if [ $? -ne 0 -o ! -e temp/build/jar/edex-installer.jar ]
	then
		echo "----Installer.edex failed----"	
		echo "----Sending Error Email----"
		echo "`uname -n`: At `date`, Installer.edex failed in Nightly build, quitting build. Working directory: $WORKING_DIR." > email.txt
		mail -s "Error Installer.edex failed in Nightly build" ${EMAILLIST} < email.txt
		exit 99
	else
		cp temp/build/jar/edex-installer.jar $outputdir
	fi
	echo "--------------------------------------------------------------"
	echo " Building CLI Installer"
	echo "--------------------------------------------------------------"
	cd $WORKING_DIR/all/Installer.cli
	$ANT_HOME/bin/ant buildAll
	if [ $? -ne 0 -o ! -e temp/build/jar/cli-installer.jar ]
	then
		echo "----Installer.cli build failed----"	
		echo "----Sending Warning Email----"
		echo "`uname -n`: At `date`, Installer.cli failed in Nightly build, continuing with rest of nightly build. Working directory: $WORKING_DIR." > email.txt
		mail -s "Warning Installer.cli failed in Nightly build" ${EMAILLIST} < email.txt
	else
		cp temp/build/jar/cli-installer.jar $outputdir
	fi
	#echo "--------------------------------------------------------------"
	#echo " Building Python Installer"
	#echo "--------------------------------------------------------------"
	#cd $WORKING_DIR/all/Installer.python
	#$ANT_HOME/bin/ant buildAll
	#cp temp/build/jar/python-installer.jar /share1/installers
fi
if [ "$type" = "cave" -o "$type" = "nightly" ]
then
	echo "--------------------------------------------------------------"
	echo "Building CAVE"
	echo "--------------------------------------------------------------"
	cd $WORKING_DIR/all/build
	./build.sh -eclipse=$WORKING_DIR/all/uframe-eclipse
	CAVE_RETURN_CODE=$?
	if [ $CAVE_RETURN_CODE -ne 0 ]
	then
		echo "----cave build.sh failed----"	
		echo "----Sending Warning Email----"
		echo "`uname -n`: At `date`, cave build.sh failed in Nightly build, continuing with rest of nightly build (skipping cave and gfe-client installers). Working directory: $WORKING_DIR." > email.txt
		mail -s "Warning cave build.sh failed in Nightly build" ${EMAILLIST} < email.txt
	fi
fi
if [ "$type" = "nightly" -a $CAVE_RETURN_CODE -eq 0 ]
then
	cp alertviz/tmp/I.AlertViz/AlertViz-linux.gtk.x86.zip $outputdir
	cd $WORKING_DIR/all/Installer.cave
	$ANT_HOME/bin/ant buildAll
	if [ $? -ne 0 -o ! -e temp/build/jar/cave-installer.jar ]
	then
		echo "----Installer.cave build failed----"	
		echo "----Sending Warning Email----"
		echo "`uname -n`: At `date`, Installer.cave failed in Nightly build, continuing with rest of nightly build. Working directory: $WORKING_DIR." > email.txt
		mail -s "Warning Installer.cave failed in Nightly build" ${EMAILLIST} < email.txt
	else
		cp temp/build/jar/cave-installer.jar $outputdir
	fi
	echo "--------------------------------------------------------------"
	echo "Building GFE CLient Installer"
	echo "--------------------------------------------------------------"
	cd $WORKING_DIR/all/Installer.gfe
	$ANT_HOME/bin/ant buildAll
	if [ $? -ne 0 -o ! -e release~/gfe-client-installer.jar ]
	then
		echo "----Installer.gfe build failed----"	
		echo "----Sending Warning Email----"
		echo "`uname -n`: At `date`, Installer.gfe failed in Nightly build, continuing with rest of nightly build. Working directory: $WORKING_DIR." > email.txt
		mail -s "Warning Installer.gfe failed in Nightly build" ${EMAILLIST} < email.txt
	else
		cp release~/gfe-client-installer.jar $outputdir
	fi
	echo " ----------------------------------------------------------------"
	echo " Building Javadocs"
	echo " ----------------------------------------------------------------"
	echo " move back to trunk/build.edex for the javadoc to build from there"
	cd $WORKING_DIR/all/build.edex
	# build the javadocs
	$ANT_HOME/bin/ant -f build-docs.xml
	if [ $? -ne 0 ]
	then
		echo "----Javadocs build failed----"	
		echo "----Sending Warning Email----"
		echo "`uname -n`: At `date`, Javadocs build failed in Nightly build. Working directory: $WORKING_DIR." > email.txt
		mail -s "Warning Javadocs build failed in Nightly build" ${EMAILLIST} < email.txt
	else
		echo "cleaning out old javadocs"
		rm -rf $javadocdir/*
		echo " copying javadocs to $javadocdir"
		cp -R $WORKING_DIR/all/docs.edex/* $javadocdir
	fi
	echo " ----------------------------------------------------------------"
	echo " Finished Building Javadocs"
	echo " ----------------------------------------------------------------"
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
