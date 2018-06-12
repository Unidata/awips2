#!/bin/sh -xe
#
# Build Unidata AWIPS RPMs from source 
# Author: mjames@ucar.edu
#

#
# Require el6 or el7 be specified
# RPM name is optional (see below)
#
os_version=$1
rpm=$2

if [ -z "$os_version" ]; then
  echo "supply os_version (el6, el7)"
  exit
fi

#
# Set up AWIPS environment
#
. /awips2/repo/awips2-builds/build/buildEnvironment.sh
buildsh=$REPO/awips2-builds/build/build.sh
pushd $REPO

#
# If local source directories, exist, mount them to the 
# container, otherwise clone the repo from github
#
if [ ! -d awips2-ncep ]; then		git clone https://github.com/Unidata/awips2-ncep.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-goesr ]; then		git clone https://github.com/Unidata/awips2-goesr.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-unidata ]; then	git clone https://github.com/Unidata/awips2-unidata.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-core ]; then		git clone https://github.com/Unidata/awips2-core.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-core-foss ]; then	git clone https://github.com/Unidata/awips2-core-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-foss ]; then		git clone https://github.com/Unidata/awips2-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-nws ]; then		git clone https://github.com/Unidata/awips2-nws.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-rpm ]; then		git clone https://github.com/Unidata/awips2-rpm.git --branch unidata_${AWIPSII_VERSION} --single-branch		;fi

#
# AWIPS Static files are too large to host on github
#
if [ ! -d awips2-static ]; then
   mkdir awips2-static
   cd awips2-static
   wget https://www.unidata.ucar.edu/downloads/awips2/static.tar
   tar -xvf static.tar
   rm -rf static.tar
fi

# 
# If RPM name is given
#
if [ ! -z "$rpm" ]; then

  frst="$(echo $rpm | head -c 1)"
  if [[ "$frst" = "-" ]]; then
    # If first character is a dash, then a build group alias was given
    su - awips -c "/bin/bash $buildsh $rpm"
  else
    su - awips -c "/bin/bash $buildsh -b $rpm"
  fi

else
  
  # If RPM name is not given
  # Build all groups (in this order)
  su - awips -c "/bin/bash $buildsh -ade"
  su - awips -c "/bin/bash $buildsh -python"
  su - awips -c "/bin/bash $buildsh -qpid"
  su - awips -c "/bin/bash $buildsh -server"
  su - awips -c "/bin/bash $buildsh -database"
  su - awips -c "/bin/bash $buildsh -edex"
  su - awips -c "/bin/bash $buildsh -httpd"
  su - awips -c "/bin/bash $buildsh -cave"

fi

# Manage RPMs
if [ "$(ls -A ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/x86_64/)" ]; then
   mv ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/x86_64/* /awips2/repo/awips2-builds/dist/${os_version}-dev/x86_64/
fi
if [ "$(ls -A ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/noarch/)" ]; then
   mv ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/noarch/* /awips2/repo/awips2-builds/dist/${os_version}-dev/noarch/
fi
