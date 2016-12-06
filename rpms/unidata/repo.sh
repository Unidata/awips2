#!/bin/bash
export LD_LIBRARY_PATH=/usr/lib:/lib:/usr/lib64:/lib64
#if [ -z "$1" ]
#  then
#  exit 1
#fi
#unset LD_LIBRARY_PATH
pushd /awips2/jenkins/build/awips2-el7
repomanage -k2 --old . | xargs rm -f
createrepo -g ./comps.xml .
rsync -ruql --delete . /awips2/repo/awips2_16.2.2
