#!/bin/bash
dir="$( cd "$(dirname "$0")" ; pwd -P )"
pushd $dir 
. ../buildEnvironment.sh
img="awips-devel-23.4.3-1"


if [ -z "$1" ]; then
  echo "supply type (el8)"
  exit
fi
os_version=$1

existing=$(podman images |grep ${img} | grep $1 | awk '{ print $3 }')
if [ ! -z "$existing" ]; then
   podman rmi $existing
fi

echo "/home/awips/docker_login" | podman login registry-1.docker.io --username tiffanym13 --password-stdin
pushd /awips2/repo/awips2/build/awips-ade
podman build -t tiffanym13/${img} -f Dockerfile.${img}.${os_version} .
podmanID=$(podman images | grep ${img} | grep latest | awk '{print $3}' | head -1 )
podman tag $podmanID tiffanym13/${img}:${os_version} 
podman rmi tiffanym13/${img}:latest
podman push tiffanym13/${img}:${os_version}
