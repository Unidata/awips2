#!/bin/bash

# Following the updated "V1" operation, test scripts should be self-repairing.
#   Therefore, when watchdog runs '/path/to/watchdog_script.sh test', a return code
#   will be passed back to the test script as follows:
#       '/path/to/watchdog_script.sh repair RETURN_CODE /path/to/watchdog_script.sh'

source /etc/watchdog.d/utilities/watchdogutils.sh

err=0

case "$1" in
    # what watchdog calls when testing the script
    test)
        service_action "status" "edex_postgres" 60
        err=$?
    ;;

    # what watchdog calls when a tested script returns non-zero
    repair)
        # edex_postgres was reported as down; restart and check status
        service_action "restart" "edex_postgres" 60
        service_action "status" "edex_postgres" 60
        err=$?
    ;;

    # defaulting to 245 which watchdog recognizes as "state unknown" and doesn't
    #   treat the result as an error; ultimately, watchdog just ignores it
    # doing this to prevent any unwanted reboots
    *)
        err=245
esac

exit $err

