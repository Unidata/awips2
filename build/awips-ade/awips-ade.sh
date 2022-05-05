#!/bin/bash
dir="$( cd "$(dirname "$0")" ; pwd -P )"
pushd $dir 
. ../buildEnvironment.sh


if [ -z "$1" ]; then
  echo "supply type (el7)"
  exit
fi
os_version=$1

existing=$(sudo docker images |grep awips-ade | grep $1 | awk '{ print $3 }')
if [ ! -z "$existing" ]; then
   sudo docker rmi $existing
fi
img="20.3.2-1"

pushd /awips2/repo/awips2-builds/build/awips-ade
sudo docker build -t tiffanym13/awips-ade-${img} -f Dockerfile.awips-ade-${img}.${os_version} .
dockerID=$(sudo docker images | grep awips-ade | grep latest | awk '{print $3}' | head -1 )
#sudo docker tag $dockerID unidata/awips-ade:${AWIPSII_VERSION}-${os_version} 
sudo docker tag $dockerID tiffanym13/awips-ade-${img}:${AWIPSII_VERSION}-${os_version} 
sudo docker rmi tiffanym13/awips-ade-${img}:latest
sudo docker push tiffanym13/awips-ade-${img}:${AWIPSII_VERSION}-${os_version}
