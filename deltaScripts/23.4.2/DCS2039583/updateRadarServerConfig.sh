#!/bin/bash
# This replaces the maxListRpsSize 300 with 800 for the Radar Server
# config.xml file.  Must be run on dv1.
#
# The radar config file should be updated when upgraded to 23.4.2.

hostId=`hostname | cut -c1-3`
if [ "${hostId}" != "dv1" ]; then
    echo "This script must be run on dv1.  Exiting."
    exit 1
fi

RADAR_CONF=/awips2/rcm/data/config/persist

if [[ -d "$RADAR_CONF" ]] ; then
  if [[ -f "$RADAR_CONF/config.xml" ]] ; then
    rm -f $RARAR_CONF/*.tmp
    sed 's/<maxRpsListSize>300/<maxRpsListSize>800/g' $RADAR_CONF/config.xml > $RADAR_CONF/config.tmp
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

