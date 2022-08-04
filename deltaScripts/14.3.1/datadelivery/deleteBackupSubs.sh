#!/bin/bash
#
# Deletes the backuped up subscriptions *.bak files

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

rm -rf /awips2/edex/data/registrySubscriptionBackup/*.bak
