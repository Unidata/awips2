#!/bin/sh -xe
OS_TYPE=$1
OS_VERSION=$2

groupadd fxalpha && useradd -G fxalpha awips
wget -O /etc/yum.repos.d/awips2.repo http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo

yum -y clean all
yum -y clean expire-cache
yum groupinstall "Development tools" -y >& /dev/null
yum groupinstall awips2-ade -y >& /dev/null
yum install git -y >& /dev/null

mkdir -p /awips2/jenkins/buildspace/workspace/AWIPS2-UPC_build/baseline

. /etc/profile.d/awips.sh

# get ${AWIPSII_VERSION} abd ${AWIPSII_RELEASE}
. /awips2/repo/awips2-builds/rpms/unidata/buildEnvironment.sh

pushd /awips2/repo

git clone https://github.com/Unidata/awips2-ncep.git --branch unidata_${AWIPSII_VERSION} --single-branch 
git clone https://github.com/Unidata/awips2-core.git --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-core-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-rpmbuild.git --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-nws.git --branch unidata_${AWIPSII_VERSION} --single-branch

cd /awips2/repo/awips2-builds/rpm/unidata/
/bin/bash build.sh -b buildEDEX

find /awips2/jenkins/build/rpms/awips2_${AWIPSII_VERSION}/
