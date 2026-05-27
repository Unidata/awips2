#!/bin/sh
# This script should be run from dv1, it will remove the lsr software from all devices
#
for host in $DX_SERVERS pv1 pv2 $LX_WORKSTATIONS
do
   echo Removing lsr files from $host
   ssh -q $host "rm -f /awips/fxa/bin/*lsr* /awips/fxa/bin/*LSR* /awips/fxa/data/*lsr* /awips/fxa/data/*LSR* /awips/fxa/bin/backupConfigFiles.sh /awips/fxa/bin/deployConfigFiles.sh /awips/fxa/data/CitiesInfo.txt* /awips/fxa/data/CitiesInfoMixedCase.txt* /awips/fxa/data/hail.magnitude* /awips/fxa/data/hailMixedCase.magnitude*"
done
