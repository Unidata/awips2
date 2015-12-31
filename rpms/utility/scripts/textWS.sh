#!/bin/sh
#
# Launcher script for textws.
# Added for #10906 for sites that have local apps to be launched before
# or with textws
#

#wait for alertviz, exit at time limit to avoid continuous loop
#wait at least 5s for Alertviz.sh to load binary
found=false
counter=0
until $found
do
        sleep 5
        if [ "" !=  "`ps h -o pid -C "alertviz"`" ]; then
                found=true
        else
                let counter=$counter+5
                if [ $counter -gt 10 ];
                        then exit
                fi
        fi
done

CAVE_DIR=/awips2/cave
$CAVE_DIR/run.sh -component textws &

FXA_HOME=/awips/fxa
. /awips/fxa/readenv.sh
#If WFO, launch legacy A1 hmMonitor
if [ "$SITE_TYPE" = "wfo" ]; then
   $FXA_HOME/bin/hmMonitor.tcl --forTextWS &
fi
