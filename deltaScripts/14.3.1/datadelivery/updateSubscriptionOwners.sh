#!/bin/bash
#
# DO NOT RUN THIS SCRIPT UNLESS YOU READ THIS!!!!!!!!!!!!!!!
# This update needs to be performed with build 14.3.1.
# This update is only for edex servers which have run DD and have subscriptions loaded.
# FIRST!  Backup your subscriptions from the Registry Web Interface, 
# http://$REGISTRY_HOST:8082/registry/RegistryInterface.html. <Backup All Subscriptions>
# Then run this update script, after script runs, <Restore Subscriptions>
# That will update the subscriptions to reflect the correct ownership at the registry level.

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

files=`find /awips2/edex/data/registrySubscriptionBackup -iname \*-RECURRING`

if [[ -z "$files" ]]; then
echo "FATAL: Update Subscriptions has Failed, No subscription backup files found!"
exit 1
fi

for f in $files; do
    echo Updating $f
    bf=$f.bak.`date +%m%d%y`
    cp $f $bf
    perl updateSubscriptionOwners.pl $f;
    done
  