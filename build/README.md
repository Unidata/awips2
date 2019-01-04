# Unidata AWIPS Build Notes

## Build the AWIPS Development Environment Docker Container

* https://hub.docker.com/r/unidata/awips-ade/tags/

We create and use the Docker image unidata/awips-ade to build the two RHEL binary distributions of AWIPS (RPMS). The directory **awips2-builds/build/awips-ade/** contains Dockerfiles for 64-bit EL6 and EL7 CentOS.

    ./build/awips-ade/awips-ade.sh ${os_version}

where **${os_version}** is either *el6* or *el7*.

ADE Docker images will be named with the latest major release and OS version

    docker images
    REPOSITORY              TAG           IMAGE ID        CREATED       SIZE
    unidata/awips-ade       18.1.1-el6    77ea90df5919    16 min ago    4.13GB
    unidata/awips-ade       18.1.1-el7    f030be21eda9    23 min ago    3.95GB

## Build AWIPS RPMs

Build individual AWIPS RPMs with the command

    ./build/setup.sh el7 ${rpm_name}

for example:

    ./build/setup.sh el7 awips2-java
    ./build/setup.sh el7 awips2-python
    ./build/setup.sh el7 awips2-postgresql

You can also build group aliases:

    ./build/setup.sh el7 buildEDEX
    ./build/setup.sh el7 buildCAVE
    ./build/setup.sh el7 buildLocalization
    ./build/setup.sh el7 buildShapefiles


Finally, if no program name is given (e.g. `./build/setup.sh el7`), then ALL AWIPS RPMs and groups will be built, in order:

    su - awips -c "/bin/bash $buildsh -ade"
    su - awips -c "/bin/bash $buildsh -python"
    su - awips -c "/bin/bash $buildsh -qpid"
    su - awips -c "/bin/bash $buildsh -server"
    su - awips -c "/bin/bash $buildsh -database"
    su - awips -c "/bin/bash $buildsh -edex"
    su - awips -c "/bin/bash $buildsh -httpd"
    su - awips -c "/bin/bash $buildsh -cave"

(See `./build/build_rpms.sh` and `./rpms/build/x86_64/rpms.sh` for more insight.)

## Yum Repository

AWIPS RPMs are written to the directories `./dist/el6-dev/` and `./dist/el7-dev/`, and are packaged as a YUM repository with the commands

    repomanage -k1 --old dist/${os_version}-dev | xargs rm -e
    createrepo -g ../comps.xml dist/${os_version}-dev

Optionally, you can push the repo to your webserver with the command

    rsync --archive --delete dist/${os_version}-dev ${USER}@{WEBSERVER}:{$REMOTE_DIR}

 

