#!/bin/bash -v
dir="$( cd "$(dirname "$0")" ; pwd -P )"
pushd $dir
. ../buildEnvironment.sh
existing=$(sudo docker images |grep awips-ade-server | grep $1 | awk '{ print $3 }')
if [ ! -z "$existing" ]; then
   sudo docker rmi $existing
fi
pushd /awips2/repo/awips2-builds/build/awips-ade-server
sudo docker build -t unidata/awips-ade-server -f Dockerfile .
sudo docker push unidata/awips-ade-server
