#!/bin/bash
# This adds the filterSbnDuplicates tags to the Radar Server
# config.xml file.  Must be run on dx1/dv1.
#
# The template file should be updated when upgraded to 22.1.1.

hostId=`hostname | cut -c1-3`
if [ "${hostId}" != "dx1" ] && [ "${hostId}" != "dv1" ]; then
    echo "This script must be run on dx1/dv1.  Exiting."
    exit 1
else
    echo "Running on $hostId"
fi

RADAR_CONF=/awips2/rcm/data/config/persist

if [[ -d "$RADAR_CONF" ]] ; then
  if [[ -f "$RADAR_CONF/config.xml" ]] ; then
    rm -f $RADAR_CONF/*.tmp
    sed '/filterSbnDuplicates/d' $RADAR_CONF/config.xml > $RADAR_CONF/config.tmp
    cmp -s $RADAR_CONF/config.xml $RADAR_CONF/config.tmp
    if [[ $? != 0 ]] ; then # need to add tags
      rm -f $RADAR_CONF/config.bak
      mv $RADAR_CONF/config.xml $RADAR_CONF/config.bak
      sed -i '/collectionEnabled>/a \ \ \ \ <filterSbnDuplicates>true</filterSbnDuplicates>' $RADAR_CONF/config.tmp
      mv $RADAR_CONF/config.tmp $RADAR_CONF/config.xml
      chmod 660 $RADAR_CONF/config.xml
      chown awips:fxalpha $RADAR_CONF/config.xml
      echo "converted $RADAR_CONF/config.xml"
    else
      echo "No conversion needed for $RADAR_CONF/config.xml"
      rm -f $RADAR_CONF/config.tmp
    fi
  fi
fi

