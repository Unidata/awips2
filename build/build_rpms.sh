#!/bin/sh -xe
#
# Build Unidata AWIPS RPMs from source
# author: Michael James
# maintainer: <tiffanym@ucar.edu>
#

#
# Require el6 or el7 be specified
# RPM name is optional (see below)
#
os_version=$1
rpmname=$2

if [ -z "$os_version" ]; then
  echo "supply os_version (el7)"
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
if [ ! -d awips2-core-foss ]; then      git clone https://github.com/Unidata/awips2-core-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch   ;fi
if [ ! -d awips2-core ]; then           git clone https://github.com/Unidata/awips2-core.git --branch unidata_${AWIPSII_VERSION} --single-branch        ;fi
if [ ! -d awips2-foss ]; then           git clone https://github.com/Unidata/awips2-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch        ;fi
if [ ! -d awips2-goesr ]; then          git clone https://github.com/Unidata/awips2-goesr.git --branch unidata_${AWIPSII_VERSION} --single-branch       ;fi
if [ ! -d awips2-ncep ]; then           git clone https://github.com/Unidata/awips2-ncep.git --branch unidata_${AWIPSII_VERSION} --single-branch        ;fi
if [ ! -d awips2-nws ]; then           git clone https://github.com/Unidata/awips2-nws.git --branch unidata_${AWIPSII_VERSION} --single-branch        ;fi
#if [ ! -d awips2-unidata ]; then        git clone https://github.com/Unidata/awips2-unidata.git --branch unidata_${AWIPSII_VERSION} --single-branch     ;fi

#
# AWIPS Static files are too large to host on github
#
if [ ! -d awips2-static && ! $rpmname = "buildCAVE" ]; then
   mkdir awips2-static
   cd awips2-static
   wget https://www.unidata.ucar.edu/downloads/awips2/static.tar
   tar -xvf static.tar
   rm -rf static.tar
fi
#
# If RPM name is given
#
if [ ! -z "$rpmname" ]; then

  frst="$(echo $rpmname | head -c 1)"
  if [[ "$frst" = "-" ]]; then
    # If first character is a dash, then a build group alias was given
    su - awips -c "/bin/bash $buildsh $rpmname"
  else
    su - awips -c "/bin/bash $buildsh -b $rpmname"
  fi

else

  # If RPM name is not given build all groups in this order
#  yum localinstall /awips2/repo/awips2-builds/dist/18.2.1-ade/x86_64/awips2-hdf5* -y
#  yum localinstall /awips2/repo/awips2-builds/dist/18.2.1-ade/x86_64/awips2-netcdf* -y
  su - awips -c "/bin/bash $buildsh -ade"
  su - awips -c "/bin/bash $buildsh -python"
  su - awips -c "/bin/bash $buildsh -qpid"
  su - awips -c "/bin/bash $buildsh -server"
  su - awips -c "/bin/bash $buildsh -database"
  su - awips -c "/bin/bash $buildsh -edex"
  su - awips -c "/bin/bash $buildsh -httpd"
  su - awips -c "/bin/bash $buildsh -cave"

fi

# Move RPMs to awips2-builds/dist
if [ "$(ls -A ${JENKINS_HOME}/build/rpms/awips2_latest/x86_64/)" ]; then
   mkdir -p /awips2/repo/awips2-builds/dist/${os_version}-dev/x86_64/
   mv ${JENKINS_HOME}/build/rpms/awips2_latest/x86_64/* /awips2/repo/awips2-builds/dist/${os_version}-dev/x86_64/
fi
if [ "$(ls -A ${JENKINS_HOME}/build/rpms/awips2_latest/noarch/)" ]; then
   mkdir -p /awips2/repo/awips2-builds/dist/${os_version}-dev/noarch/
   mv ${JENKINS_HOME}/build/rpms/awips2_latest/noarch/* /awips2/repo/awips2-builds/dist/${os_version}-dev/noarch/
fi

