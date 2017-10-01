#!/bin/sh -xe
os_type=$1
os_version=$2
sudo docker run --rm=true -v `pwd`:/awips2/repo/awips2-builds:rw ${os_type}:${os_version} /bin/bash -c "bash -xe /awips2/repo/awips2-builds/build/build_rhel.sh ${os_type} ${os_version}"
