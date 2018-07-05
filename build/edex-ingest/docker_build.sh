#!/bin/bash -v
dir="$( cd "$(dirname "$0")" ; pwd -P )"
pushd $dir
. ../buildEnvironment.sh

existing=$(sudo docker images |grep edex-ingest | grep $1 | awk '{ print $3 }')
if [ ! -z "$existing" ]; then
   sudo docker rmi $existing
fi
pushd /awips2/repo/awips2-builds/build/edex-ingest
sudo docker build -t unidata/edex-ingest -f Dockerfile.edex .
dockerID=$(sudo docker images | grep edex-ingest | grep latest | awk '{print $3}' | head -1 )
sudo docker tag $dockerID unidata/edex-ingest:${AWIPSII_VERSION}
sudo docker rmi unidata/edex-ingest:latest
sudo docker push unidata/edex-ingest
