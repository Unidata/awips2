#!/bin/bash -v
rebuild=false

# current directory
dir="$( cd "$(dirname "$0")" ; pwd -P )"
pushd $dir

# source AWIPS build env vars
. ../buildEnvironment.sh

# remove existing image (optional)
if $rebuild; then
   img=$(sudo docker images | grep edex-ingest | awk '{ print $3 }')
   if [ ! -z "$img" ]; then
      echo "removing edex-ingest:latest"
      sudo docker rmi $img
   fi
fi

# build image
pushd /awips2/repo/awips2-builds/build/edex-ingest
sudo docker build -t unidata/edex-ingest -f Dockerfile.edex .

# push to dockerhub
sudo docker push unidata/edex-ingest
