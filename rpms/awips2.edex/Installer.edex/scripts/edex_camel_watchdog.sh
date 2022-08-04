#!/bin/bash

# Following the updated "V1" operation, test scripts should be self-repairing.
#   Therefore, when watchdog runs '/path/to/watchdog_script.sh test', a return code
#   will be passed back to the test script as follows:
#       '/path/to/watchdog_script.sh repair RETURN_CODE /path/to/watchdog_script.sh'

source /etc/init.d/edexServiceList
source /etc/watchdog.d/utilities/watchdogutils.sh

run_test ()
{
    let return_code=100
    for service in ${SERVICES[*]};
    do
        service_action "status" $service 60 "edex_camel"
        if [ $? -ne 0 ]; then
            if [ ! -f /awips2/edex/etc/${service}.sh ]; then
                echo "ERROR `date_cmd` unrecognized Edex service $service..."
                return 244
            fi
            return $return_code
        fi
        let return_code+=1
    done

    return 0
}

err=0

case "$1" in
    # what watchdog calls when testing the script
    test)
        run_test
        err=$?
    ;;

    # what watchdog calls when a tested script returns non-zero
    repair)
        if [ $2 -ne 244 ]; then
            local service_to_repair
            let idx=$2-100
            service_to_repair=${SERVICES[$idx]}
            service_action "restart" $service_to_repair 60 "edex_camel"
            service_action "status" $service_to_repair 60 "edex_camel"
            err=$?
            if [ $err -ne 0 ]; then
                echo "ERROR `date_cmd` Edex $service_to_repair failed to restart; rebooting system..."
            fi
        else
            echo "ERROR `date_cmd` unrecognized service found; check test-bin.stdout for tested service name..."
            err=244
        fi
    ;;

    # defaulting to 245 which watchdog recognizes as "state unknown" and doesn't
    #   treat the result as an error; ultimately, watchdog just ignores it
    # doing this to prevent any unwanted reboots
    *)
        err=245
esac

exit $err
