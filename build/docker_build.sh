#!/bin/bash
existing=$(sudo docker images |grep awips-ade | awk '{ print $3 }'|uniq)
if [ ! -z "$existing" ]; then
   sudo docker rmi $existing
fi
pushd /awips2/repo/awips2-builds/build/awips-ade
sudo docker build -t unidata/awips-ade -f Dockerfile.awips-ade.el7 .
dockerID=$(sudo docker ps | grep awips-ade | awk '{print $1}' | head -1 )
sudo docker tag $dockerID unidata/awips-ade:17.1.1-el7 
sudo docker push unidata/awips-ade
