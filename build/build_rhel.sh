#!/bin/sh -xe
OS_TYPE=$1
OS_VERSION=$2

yum -y clean all
yum -y clean expire-cache
yum groupinstall "Development tools" -y >& /dev/null
yum install wget cmake ruby boost-devel libuuid-devel -y
wget -O /etc/yum.repos.d/awips2.repo http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo
yum -y clean all
yum groupinstall awips2-ade -y >& /dev/null
yum install git -y >& /dev/null

. /awips2/repo/awips2-builds/rpms/unidata/buildEnvironment.sh

mkdir -p /awips2/jenkins/buildspace/workspace/AWIPS2-UPC_build/baseline
mkdir -p /awips2/jenkins/buildspace/workspace/tmp
mkdir -p /awips2/jenkins/build/rpms/awips2_${AWIPSII_VERSION}/x86_64/
mkdir -p /awips2/jenkins/build/rpms/awips2_${AWIPSII_VERSION}/noarch/

pushd /awips2/repo

git clone https://github.com/Unidata/awips2-ncep.git --branch unidata_${AWIPSII_VERSION} --single-branch 
git clone https://github.com/Unidata/awips2-core.git --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-core-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-rpmbuild.git awips2-rpm --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-nws.git --branch unidata_${AWIPSII_VERSION} --single-branch

cd /awips2/repo/awips2-builds/rpms/unidata/

/bin/bash cibuild.sh -qpid
/bin/bash cibuild.sh -b buildEDEX

find /awips2/jenkins/build/rpms/awips2_${AWIPSII_VERSION}/
