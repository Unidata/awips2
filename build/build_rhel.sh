#!/bin/sh -xe
OS_TYPE=$1
OS_VERSION=$2
if [ $OS_VERSION == "centos7" ]; then
    REPO_EXT="-el7" 
    repo_name="el7.repo"
else
    repo_name="awips-dev.repo"
fi

groupadd fxalpha && useradd -G fxalpha awips
yum -y clean all
yum -y clean expire-cache
yum groupinstall "Development tools" -y >& /dev/null
yum install wget qt-devel cmake gcc gcc-c++ ruby boost-devel libuuid-devel tk-devel tcl-devel rsync git net-tools rpm-build netcdf-devel -y
wget -O /etc/yum.repos.d/awips2.repo http://www.unidata.ucar.edu/software/awips2/doc/${repo_name}
yum -y clean all
yum groupinstall awips2-ade -y

su - awips
. /awips2/repo/awips2-src/rpms/unidata/buildEnvironment.sh

mkdir -p /awips2/jenkins/buildspace/workspace/AWIPS2-UPC_build/baseline
mkdir -p /awips2/jenkins/buildspace/workspace/tmp
mkdir -p /awips2/jenkins/build/rpms/awips2_${AWIPSII_VERSION}/{x86_64,noarch}/

pushd /awips2/repo

git clone https://github.com/Unidata/awips2.git --branch unidata_${AWIPSII_VERSION}${REPO_EXT} --single-branch awips2-builds
git clone https://github.com/Unidata/awips2-ncep.git --branch unidata_${AWIPSII_VERSION} --single-branch 
git clone https://github.com/Unidata/awips2-core.git --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-core-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-foss.git --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-rpmbuild.git awips2-rpm --branch unidata_${AWIPSII_VERSION} --single-branch
git clone https://github.com/Unidata/awips2-nws.git --branch unidata_${AWIPSII_VERSION} --single-branch

cd /awips2/repo/awips2-builds/rpms/unidata/

# build all RPMs
/bin/bash cibuild.sh -qpid
/bin/bash cibuild.sh -python
/bin/bash cibuild.sh -server
/bin/bash cibuild.sh -database
/bin/bash cibuild.sh -dev
/bin/bash cibuild.sh -b buildEDEX >& /dev/null
/bin/bash cibuild.sh -b buildCAVE >& /dev/null

export RPMDIR=/awips2/jenkins/build/rpms/awips2_${AWIPSII_VERSION}/
export LD_LIBRARY_PATH=/usr/lib:/lib:/usr/lib64:/lib64
cp comps.xml ${RPMDIR}
cd ${RPMDIR}
repomanage -k1 --old . | xargs rm -f
createrepo -g ./comps.xml .
unset LD_LIBRARY_PATH
. /etc/profile.d/awips2.sh

tar -cf /awips2/jenkins/build/rpms/awips2_${AWIPSII_VERSION}/ /awips2/repo/awips2-src/dist/awips2_${AWIPSII_VERSION}_${OS_TYPE}-${OS_VERSION}.tar
