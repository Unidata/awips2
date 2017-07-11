#!/bin/bash
source buildEnvironment.sh
export LD_LIBRARY_PATH=/usr/lib:/lib:/usr/lib64:/lib64
export RPMDIR=/awips2/jenkins/build/rpms/awips2_${AWIPSII_VERSION}/
cd ${RPMDIR}
pwd
repomanage -k1 --old . | xargs rm -f
createrepo -g ./comps.xml .
unset LD_LIBRARY_PATH
#. /etc/profile.d/awips2.sh
rsync --archive --delete $RPMDIR js-17-218.jetstream-cloud.org:/awips2/repo/awips2_17.1.1

cd ..
#rm -rf awips2_${AWIPSII_VERSION}.tar
#tar -cf awips2_${AWIPSII_VERSION}.tar awips2_${AWIPSII_VERSION}
#scp awips2_${AWIPSII_VERSION}.tar mjames@129.114.17.218:/awips2/repo/
