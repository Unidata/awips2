#!/bin/sh -xe
if [ -z "$1" ]; then
  echo "supply type (el6, el7)"
  exit
fi
os_version=$1
builds_dir=" -v `pwd`:/awips2/repo/awips2-builds:rw "

# if repos exist locally, mount rather than clone (see build_rpms.sh)
if [ -d /awips2/repo/awips2-static ]; then 	static_dir="   -v /awips2/repo/awips2-static:/awips2/repo/awips2-static "	;fi
if [ -d /awips2/repo/awips2-rpm ]; then		rpm_dir="      -v /awips2/repo/awips2-rpm:/awips2/repo/awips2-rpm "		;fi
if [ -d /awips2/repo/awips2-core ]; then	core_dir="     -v /awips2/repo/awips2-core:/awips2/repo/awips2-core "		;fi
if [ -d /awips2/repo/awips2-core-foss ]; then	corefoss_dir=" -v /awips2/repo/awips2-core-foss:/awips2/repo/awips2-core-foss "	;fi
if [ -d /awips2/repo/awips2-foss ]; then	foss_dir="     -v /awips2/repo/awips2-foss:/awips2/repo/awips2-foss "		;fi
if [ -d /awips2/repo/awips2-nws ]; then		nws_dir="      -v /awips2/repo/awips2-nws:/awips2/repo/awips2-nws "		;fi
if [ -d /awips2/repo/awips2-ncep ]; then	ncep_dir="     -v /awips2/repo/awips2-ncep:/awips2/repo/awips2-ncep "		;fi
if [ -d /awips2/repo/awips2-goesr ]; then	goesr_dir="    -v /awips2/repo/awips2-goesr:/awips2/repo/awips2-goesr "		;fi
if [ -d /awips2/repo/awips2-unidata ]; then	upc_dir="      -v /awips2/repo/awips2-unidata:/awips2/repo/awips2-unidata "	;fi
if [ -d /awips2/repo/python-awips ]; then	python_dir="   -v /awips2/repo/python-awips:/awips2/repo/python-awips "		;fi
dirs=$builds_dir$static_dir$rpm_dir$core_dir$corefoss_dir$foss_dir$nws_dir$ncep_dir$goesr_dir$upc_dir$python_dir

# run
sudo docker run --entrypoint=/bin/bash --privileged -d -ti -e "container=docker" $dirs unidata/awips-ade:17.1.1-$os_version
dockerID=$(sudo docker ps | grep awips-ade | awk '{print $1}' | head -1 )
sudo docker logs $dockerID
sudo docker exec -ti $dockerID /bin/bash -xec "/awips2/repo/awips2-builds/build/build_rpms.sh $os_version $2";
sudo docker stop $dockerID
sudo docker rm -v $dockerID

if [[ $(whoami) == "mjames" ]]; then # local build
  sudo chown -R mjames:ustaff dist/${os_version}-dev
  repomanage -k1 --old dist/${os_version}-dev | xargs rm -f
  createrepo -g ../../build/comps.xml dist/${os_version}-dev
  rsync --archive --delete dist/${os_version}-dev tomcat@www:/web/content/repos/yum/
fi
