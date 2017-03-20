#!/bin/bash
source buildEnvironment.sh
export LD_LIBRARY_PATH=/usr/lib:/lib:/usr/lib64:/lib64
export RPMDIR=/awips2/jenkins/build/rpms/awips2_${AWIPSII_VERSION}/
cd ${RPMDIR}
pwd
repomanage -k1 --old . | xargs rm -f
createrepo -g ./comps.xml .
unset LD_LIBRARY_PATH
. /etc/profile.d/awips2.sh
rsync --archive --delete $RPMDIR /awips2/repo/release
#cp -R $RPMDIR/x86_64/* /awips2/repo/release/x86_64/
#cp -R $RPMDIR/noarch/* /awips2/repo/release/noarch/
#cp -R $RPMDIR/repodata/* /awips2/repo/release/repodata/
#cp -R $RPMDIR/comps.xml /awips2/repo/release/
