#!/bin/bash
dir="$( cd "$(dirname "$0")" ; pwd -P )"
pushd $dir 
. ../buildEnvironment.sh


if [ -z "$1" ]; then
  echo "supply type (el8)"
  exit
fi
os_version=$1

existing=$(podman images |grep awips-ade | grep $1 | awk '{ print $3 }')
if [ ! -z "$existing" ]; then
   podman rmi $existing
fi
img="23.4.3-1"

echo "/home/awips/docker_login" | podman login registry-1.docker.io --username tiffanym13 --password-stdin
pushd /awips2/repo/awips2/build/awips-ade
podman build -t tiffanym13/awips-ade-${img} -f Dockerfile.awips-ade-${img}.${os_version} .
podmanID=$(podman images | grep awips-ade | awk '{print $3}' | head -1 )
#podman tag $podmanID unidata/awips-ade:${AWIPSII_VERSION}-${os_version} 
podman tag $podmanID tiffanym13/awips-ade-${img}:${AWIPSII_VERSION}-${os_version} 
podman rmi tiffanym13/awips-ade-${img}:latest
podman push tiffanym13/awips-ade-${img}:${AWIPSII_VERSION}-${os_version}
