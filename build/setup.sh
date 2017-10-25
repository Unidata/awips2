#!/bin/sh -xe
os_type=centos
os_version=centos7

if [ "$os_version" = "centos6" ]; then
    sudo docker run --rm=true -v `pwd`:/awips2/repo/awips2-builds:rw ${os_type}:${os_version} /bin/bash -c "bash -xe /awips2/repo/awips2-builds/build/build_rhel.sh ${os_type} ${os_version}"
elif [ "$os_version" = "centos7" ]; then
    sudo docker run --privileged -d -ti -e "container=docker" -v `pwd`:/awips2/repo/awips2-builds:rw  ${os_type}:${os_version}   /usr/sbin/init
    DOCKER_CONTAINER_ID=$(sudo docker ps | grep ${os_version} | awk '{print $1}' | head -1 )
    sudo docker logs $DOCKER_CONTAINER_ID
    sudo docker exec -ti $DOCKER_CONTAINER_ID /bin/bash -xec "bash -xe /awips2/repo/awips2-builds/build/build_rhel.sh ${os_type} ${os_version}";
    sudo docker ps -a
    sudo docker stop $DOCKER_CONTAINER_ID
    sudo docker rm -v $DOCKER_CONTAINER_ID
fi
