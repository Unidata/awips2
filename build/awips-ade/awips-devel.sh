#!/bin/bash
dir="$( cd "$(dirname "$0")" ; pwd -P )"
pushd $dir 
. ../buildEnvironment.sh
img="awips-devel-20.3.2-1"


if [ -z "$1" ]; then
  echo "supply type (el7)"
  exit
fi
os_version=$1

existing=$(sudo docker images |grep ${img} | grep $1 | awk '{ print $3 }')
if [ ! -z "$existing" ]; then
   sudo docker rmi $existing
fi
pushd /awips2/repo/awips2-builds/build/awips-ade
sudo docker build -t tiffanym13/${img} -f Dockerfile.${img}.${os_version} .
dockerID=$(sudo docker images | grep ${img} | grep latest | awk '{print $3}' | head -1 )
sudo docker tag $dockerID tiffanym13/${img}:${os_version} 
sudo docker rmi tiffanym13/${img}:latest
sudo docker push tiffanym13/${img}:${os_version}
