#!/bin/sh -xe
# Run rpmbuild scripts for awips
# type=$(rpm -qa awips2 | rev | cut -d "." -f3|rev)
type=$1
if [ -z "$type" ]; then
  echo "supply type (el6, el7)"
  exit
fi

. /awips2/repo/awips2-builds/build/buildEnvironment.sh
buildsh=$REPO/awips2-builds/build/build.sh
pushd $REPO

# if not mounted to docker container, clone from github
if [ ! -d awips2-ncep ]; then		git clone https://github.com/Unidata/awips2-ncep.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-goesr ]; then		git clone https://github.com/Unidata/awips2-goesr.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-core ]; then		git clone https://github.com/Unidata/awips2-core.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-core-foss ]; then	git clone https://github.com/Unidata/awips2-core-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-foss ]; then		git clone https://github.com/Unidata/awips2-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-nws ]; then		git clone https://github.com/Unidata/awips2-nws.git --branch unidata_${AWIPSII_VERSION} --single-branch 	;fi
if [ ! -d awips2-rpm ]; then		git clone https://github.com/Unidata/awips2-rpm.git --branch unidata_${AWIPSII_VERSION} --single-branch		;fi
if [ ! -d awips2-static ]; then
   mkdir awips2-static
   cd awips2-static
   wget https://www.unidata.ucar.edu/downloads/awips2/static.tar
   tar -xvf static.tar
   rm -rf static.tar
fi

# to test the dockerized build procedure on a quick rpm
#su - awips -c "/bin/bash $buildsh -b awips2-ldm"

# provides rpm name as arg (ex: ./build/setup.sh el7 awips2-python-jep)
prog=$2
if [ ! -z "$prog" ]; then
  su - awips -c "/bin/bash $buildsh -b $prog"
else
  # Build all groups (in this order)
  #su - awips -c "/bin/bash $buildsh -ade"
  #su - awips -c "/bin/bash $buildsh -python"
  #su - awips -c "/bin/bash $buildsh -qpid"
  #su - awips -c "/bin/bash $buildsh -server"
  #su - awips -c "/bin/bash $buildsh -database"
  #su - awips -c "/bin/bash $buildsh -b awips2-ldm"
  su - awips -c "/bin/bash $buildsh -edex"
  #su - awips -c "/bin/bash $buildsh -httpd"
  #su - awips -c "/bin/bash $buildsh -cave"
  ls -al
fi

# Manage RPMs
if [ "$(ls -A ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/x86_64/)" ]; then
   mv ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/x86_64/* /awips2/repo/awips2-builds/dist/${type}-dev/x86_64/
fi
if [ "$(ls -A ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/noarch/)" ]; then
   mv ${JENKINS_HOME}/build/rpms/awips2_${AWIPSII_VERSION}/noarch/* /awips2/repo/awips2-builds/dist/${type}-dev/noarch/
fi
