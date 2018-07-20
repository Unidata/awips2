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
if [ -d /awips2/repo/awips2-cimss ]; then		dirs+=" -v /awips2/repo/awips2-cimss:/awips2/repo/awips2-cimss ";fi
if [ -d /awips2/repo/awips2-core-foss ]; then		dirs+=" -v /awips2/repo/awips2-core-foss:/awips2/repo/awips2-core-foss ";fi
if [ -d /awips2/repo/awips2-core ]; then		dirs+=" -v /awips2/repo/awips2-core:/awips2/repo/awips2-core ";fi
if [ -d /awips2/repo/awips2-data-delivery ]; then	dirs+=" -v /awips2/repo/awips2-data-delivery:/awips2/repo/awips2-data-delivery ";fi
if [ -d /awips2/repo/awips2-drawing ]; then		dirs+=" -v /awips2/repo/awips2-drawing:/awips2/repo/awips2-drawing ";fi
if [ -d /awips2/repo/awips2-foss ]; then		dirs+=" -v /awips2/repo/awips2-foss:/awips2/repo/awips2-foss ";fi
if [ -d /awips2/repo/awips2-goesr ]; then		dirs+=" -v /awips2/repo/awips2-goesr:/awips2/repo/awips2-goesr ";fi
if [ -d /awips2/repo/awips2-gsd ]; then			dirs+=" -v /awips2/repo/awips2-gsd:/awips2/repo/awips2-gsd ";fi
if [ -d /awips2/repo/awips2-hazards ]; then		dirs+=" -v /awips2/repo/awips2-hazards:/awips2/repo/awips2-hazards ";fi
if [ -d /awips2/repo/awips2-nasa ]; then		dirs+=" -v /awips2/repo/awips2-nasa:/awips2/repo/awips2-nasa ";fi
if [ -d /awips2/repo/awips2-ncep ]; then		dirs+=" -v /awips2/repo/awips2-ncep:/awips2/repo/awips2-ncep ";fi
if [ -d /awips2/repo/awips2-nws ]; then			dirs+=" -v /awips2/repo/awips2-nws:/awips2/repo/awips2-nws ";fi
if [ -d /awips2/repo/awips2-ogc ]; then			dirs+=" -v /awips2/repo/awips2-ogc:/awips2/repo/awips2-ogc ";fi
if [ -d /awips2/repo/awips2-ohd ]; then			dirs+=" -v /awips2/repo/awips2-ohd:/awips2/repo/awips2-ohd ";fi
if [ -d /awips2/repo/awips2-rpm ]; then			dirs+=" -v /awips2/repo/awips2-rpm:/awips2/repo/awips2-rpm ";fi
if [ -d /awips2/repo/awips2-static ]; then 		dirs+=" -v /awips2/repo/awips2-static:/awips2/repo/awips2-static ";fi
if [ -d /awips2/repo/awips2-swpc ]; then		dirs+=" -v /awips2/repo/awips2-swpc:/awips2/repo/awips2-swpc ";fi
if [ -d /awips2/repo/awips2-unidata ]; then		dirs+=" -v /awips2/repo/awips2-unidata:/awips2/repo/awips2-unidata ";fi
if [ -d /awips2/repo/python-awips ]; then		dirs+=" -v /awips2/repo/python-awips:/awips2/repo/python-awips ";fi
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
