#!/bin/sh -xe
# Run rpmbuild scripts for awips
# type=$(rpm -qa awips2 | rev | cut -d "." -f3|rev)
type=$1
if [ -z "$type" ]; then
  echo "supply type (el6, el7)"
  exit
fi

. /awips2/repo/awips2-builds/rpms/unidata/buildEnvironment.sh
buildsh=$REPO/awips2-builds/rpms/unidata/cibuild.sh
pushd $REPO

# if not mounted to docker container, clone from github
if [ ! -d awips2-ncep ]; then		git clone https://github.com/Unidata/awips2-ncep.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-core ]; then		git clone https://github.com/Unidata/awips2-core.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-core-foss ]; then	git clone https://github.com/Unidata/awips2-core-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-foss ]; then		git clone https://github.com/Unidata/awips2-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-nws ]; then		git clone https://github.com/Unidata/awips2-nws.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-rpm ]; then		git clone https://github.com/Unidata/awips2-rpmbuild.git awips2-rpm --branch el7_rpm --single-branch		;fi
if [ ! -d awips2-static ]; then
   mkdir awips2-static
   cd awips2-static
   wget http://www.unidata.ucar.edu/downloads/awips2/static.tar
   tar -xvf static.tar
   rm -rf static.tar
fi

# Test build procedure on a quick rpm
su - awips -c "/bin/bash $buildsh -b awips2-ldm"

# Build all groups (in this order)
#su - awips -c "/bin/bash $buildsh -ade"
#su - awips -c "/bin/bash $buildsh -python"
#su - awips -c "/bin/bash $buildsh -qpid"
#su - awips -c "/bin/bash $buildsh -server"
#su - awips -c "/bin/bash $buildsh -edex"
#su - awips -c "/bin/bash $buildsh -database"
#su - awips -c "/bin/bash $buildsh -cave"

# Manage RPMs
if [ "$(ls -A ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/x86_64/)" ]; then
   mv ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/x86_64/* /awips2/repo/awips2-builds/dist/$type/x86_64/
fi
if [ "$(ls -A ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/noarch/)" ]; then
   mv ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/noarch/* /awips2/repo/awips2-builds/dist/$type/noarch/
fi
