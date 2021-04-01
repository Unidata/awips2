#!/bin/bash 
if [[ ! $(grep awips /etc/security/limits.conf) ]]; then
  echo "Checking /etc/security/limits.conf for awips: Not found. Adding..."
  printf "awips soft nproc 65536\nawips soft nofile 65536\n" >> /etc/security/limits.conf
fi
for dir in /awips2/tmp /awips2/data_store ; do
  if [ ! -d $dir ]; then
    echo "creating $dir"
    mkdir -p $dir
    chown awips:fxalpha $dir
  fi
done
yum groupinstall awips2-ade-server -y
/usr/bin/edex setup
