#!/bin/bash
dir="$( cd "$(dirname "$0")" ; pwd -P )"
pushd $dir 
. ../buildEnvironment.sh


if [ -z "$1" ]; then
  echo "supply type (el7)"
  exit
fi
os_version=$1

existing=$(docker images |grep awips-ade | grep $1 | awk '{ print $3 }')
if [ ! -z "$existing" ]; then
   docker rmi $existing
fi
img="20.3.2-2"

pushd /awips2/repo/awips2-builds/build/awips-ade
docker build -t tiffanym13/awips-ade-${img} -f Dockerfile.awips-ade-${img}.${os_version} .
dockerID=$(docker images | grep awips-ade | awk '{print $3}' | head -1 )
#docker tag $dockerID unidata/awips-ade:${AWIPSII_VERSION}-${os_version} 
docker tag $dockerID tiffanym13/awips-ade-${img}:${AWIPSII_VERSION}-${os_version} 
docker rmi tiffanym13/awips-ade-${img}:latest
#docker rmi tiffanym13/awips-ade-${img}:${AWIPSII_VERSION}-${os_version}
docker push tiffanym13/awips-ade-${img}:${AWIPSII_VERSION}-${os_version}
