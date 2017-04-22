#!/bin/sh

# Delta script for DR 19698
#
# This script will modify /awips2/fxa/bin/setup.env for sites that use a remote
# CP and have Qpid on px1f.

. "${FXA_DATA:-/data/fxa}/INSTALL/awips2/scripts/.global" || exit 1

do_update_cli_setup_env()
{
    for i in $DX_SERVERS $PX_SERVER $LX_WORKSTATIONS; do
        ssh "$i" sed -i -e \
            \''$a\# The following line is a special case for the remote CP'\' \
            -e \''$a\export BROKER_HOST=px1f'\' /awips2/fxa/bin/setup.env
    done
}

case ${SITE_IDENTIFIER} in
    ${remCPCaseArray} ) do_update_cli_setup_env ;;
esac

