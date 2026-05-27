#!/bin/bash
# This removes the tdwrCollectionLimited tags from Radar Server
# config.xml file.  Must be run on dx1.
#
# The template file should be updated when upgraded to 20.2.1.

hostId=`hostname | cut -c1-3`
if [ "${hostId}" != "dx1" ]; then
    echo "This script must be run on dx1.  Exiting."
    exit 1
fi

RADAR_CONF=/awips2/rcm/data/config/persist

if [[ -d "$RADAR_CONF" ]] ; then
  if [[ -f "$RADAR_CONF/config.xml" ]] ; then
    rm -f $RARAR_CONF/*.tmp
    sed '/tdwrCollectionLimited/d' $RADAR_CONF/config.xml > $RADAR_CONF/config.tmp
    cmp -s $RADAR_CONF/config.xml $RADAR_CONF/config.tmp
    if [[ $? != 0 ]] ; then
      rm -f $RADAR_CONF/config.bak
      mv $RADAR_CONF/config.xml $RADAR_CONF/config.bak
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

