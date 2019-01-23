#!/bin/bash
dir="$( cd "$(dirname "$0")" ; pwd -P )"
. /etc/profile.d/awips2.sh

# Retrieves ASCAT grib2 for EDEX
# New file is posted approx. once per hour at 5-8 min past the hour
# best use is to run once per hour at 10 or 15 minutes past


HOST='ftp.opc.ncep.noaa.gov'
USER='anonymous'
PASSWD='mjames@ucar.edu'

ftp -n -v $HOST << EOT > ftp.filelist
ascii
user $USER $PASSWD
prompt
cd data/ascat_ab
ls
bye
EOT

for file in $(cat ftp.filelist |grep grb2|grep -v latest| cut -c 57-); do
  if [ ! -f $file ]; then
    wget ftp://$HOST:/data/ascat_ab/$file
    /awips2/edex/bin/qpidNotify.py $dir/$file grib
  fi
done
