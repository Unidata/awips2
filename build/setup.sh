#!/bin/sh -xe
#
# Unidata AWIPS Build Setup Script
# Author: mjames@ucar.edu
#

#
# Require el6 or el7 be specified
#
if [ -z "$1" ]; then
  echo "supply type (el6, el7)"
  exit
fi
os_version=$1
rpmname=$2
dirs=" -v `pwd`:/awips2/repo/awips2-builds:rw "
. /awips2/repo/awips2-builds/build/buildEnvironment.sh

#
# If local source directories, exist, mount them to the container
for dn in `cat build/repos`
do
  echo $dn
  if [ -d /awips2/repo/$dn ]; then		dirs+=" -v /awips2/repo/${dn}:/awips2/repo/${dn} ";fi
done

#
# Run Docker AWIPS ADE Image
#
imgname=unidata/awips-ade
imgvers=${AWIPSII_VERSION}
sudo docker run --entrypoint=/bin/bash --privileged -d -ti -e "container=docker" $dirs $imgname:$imgvers-$os_version
dockerID=$(sudo docker ps | grep awips-ade | awk '{print $1}' | head -1 )
sudo docker logs $dockerID
sudo docker exec -ti $dockerID /bin/bash -xec "/awips2/repo/awips2-builds/build/build_rpms.sh $os_version $rpmname";
sudo docker stop $dockerID
sudo docker rm -v $dockerID

#
# Update/Recreate YUM Repository
#
if [[ $(whoami) == "mjames" ]]; then # local build
  sudo chown -R mjames:ustaff dist/${os_version}-dev
  repomanage -k1 --old dist/${os_version}-dev | xargs rm -f
  createrepo -g ../comps.xml dist/${os_version}-dev
  #
  # Push to web server
  #
  rsync --archive --delete dist/${os_version}-dev tomcat@www:/web/content/repos/yum/
fi
