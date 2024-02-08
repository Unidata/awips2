#!/bin/bash
dir="$( cd "$(dirname "$0")" ; pwd -P )"
pushd $dir 
. ../buildEnvironment.sh
img="awips-devel-20.3.2-2"


if [ -z "$1" ]; then
  echo "supply type (el7)"
  exit
fi
os_version=$1

existing=$(sudo docker images |grep ${img} | grep $1 | awk '{ print $3 }')
if [ ! -z "$existing" ]; then
   docker rmi $existing
fi
pushd /awips2/repo/awips2-builds/build/awips-ade
docker build -t tiffanym13/${img} -f Dockerfile.${img}.${os_version} .
dockerID=$(docker images | grep ${img} | grep latest | awk '{print $3}' | head -1 )
docker tag $dockerID tiffanym13/${img}:${os_version} 
docker rmi tiffanym13/${img}:latest
docker push tiffanym13/${img}:${os_version}
