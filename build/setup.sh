#!/bin/sh -xe
#
# Unidata AWIPS Build Setup Script
# author: Michael James
# maintainer: <tiffanym@ucar.edu>
#

#
# Require el6 or el7 be specified
#
if [ -z "$1" ]; then
  echo "supply type (el7)"
  exit
fi
os_version=$1
rpmname=$2
dirs=" -v `pwd`:/awips2/repo/awips2-builds:rw "
. /awips2/repo/awips2-builds/build/buildEnvironment.sh

# If local source directories, exist, mount them to the container
if [ $rpmname = "buildCAVE" ]; then
  for dn in `cat build/repos| grep -v static| grep -v nativelib |grep -v awips2-rpm`
  do
    echo $dn
    if [ -d /awips2/repo/$dn ]; then
      dirs+=" -v /awips2/repo/${dn}:/awips2/repo/${dn} "
    fi
  done
else
  for dn in `cat build/repos`
  do
    echo $dn
    if [ -d /awips2/repo/$dn ]; then
       dirs+=" -v /awips2/repo/${dn}:/awips2/repo/${dn} "
    fi
  done
fi

#
# Run Docker AWIPS ADE Image
#
imgname=unidata/awips-ade
imgvers=18.2.1
sudo docker run --entrypoint=/bin/bash --privileged -d -ti -e "container=docker" $dirs $imgname:$imgvers-$os_version
dockerID=$(sudo docker ps | grep awips-ade | awk '{print $1}' | head -1 )
sudo docker logs $dockerID
sudo docker exec -ti $dockerID /bin/bash -xec "/awips2/repo/awips2-builds/build/build_rpms.sh $os_version $rpmname";
#sudo docker stop $dockerID
#sudo docker rm -v $dockerID

#
# Update/Recreate YUM Repository
#

date=$(date +%Y%m%d)

if [[ $(whoami) == "awips" ]]; then # local build
  mv dist/${os_version}-dev dist/${os_version}-dev-${date}
  sudo su - -c "createrepo -g /awips2/repo/awips2/dist/comps.xml /awips2/repo/awips2/dist/${os_version}-dev-${date}/"
  sudo chown -R awips:fxalpha dist/${os_version}-dev-${date}
  rsync -aP dist/${os_version}-dev-${date} awips@hardy:~/
  rsync -aP dist/${os_version}-dev-${date} tomcat@robin:/web/content/repos/yum/
  #repomanage -k1 --old dist/${os_version}-dev | xargs rm -f
  #
  # Push to web server
  #
  #rsync --archive --delete dist/${os_version}-dev tomcat@www:/web/content/repos/yum/
fi
