#!/bin/bash

# Following the updated "V1" operation, test scripts should be self-repairing.
#   Therefore, when watchdog runs '/path/to/watchdog_script.sh test', a return code
#   will be passed back to the test script as follows:
#       '/path/to/watchdog_script.sh repair RETURN_CODE /path/to/watchdog_script.sh'

source /etc/watchdog.d/utilities/watchdogutils.sh

# time in ms
last_rcvd_product_threshold=60000

# ldm feeds
#   note: these are placeholder config names and do not represent the final product
ldm_line_1="eth1"
ldm_line_2="eth2"

check_interface ()
{
    ifconfig $1 2> /dev/null | grep -q "<UP,"
    # maybe use this instead?
    # cat /sys/class/net/$1/operstate 2> /dev/null | grep -q up
    return $?
}

run_test ()
{
    if [ -f "$watchdog_status/ldmcp" ]; then
        # ldmcp was manually stopped; skip
        echo "INFO `date_cmd` LDM is manually shutdown; skipping tests"
        return 0
    fi

    # if ldmcp is currently (re)starting, then skip the rest of the tests
    starting_=`ps aux | grep -i "/bin/bash /etc/init.d/ldmcp [re]*start" | awk '{print $13}'`
    for idx in ${starting_[@]}; do
        if [[ $idx =~ .*"start".* ]]; then
            return 0
        fi
    done

    # check that ldmcp is running
    return_code=101
    noaaportingesters=`ps aux | grep -i "noaaportingester -b" | awk '{print $11}'`
    for idx in ${noaaportingesters[@]}; do
        if [ $idx == "noaaportIngester" ]; then
            return_code=0
            break
        fi
    done

    if [ $return_code -eq "101" ]; then
        return 101
    fi

    # check that we have a line up
    check_interface $ldm_line_1
    ldm_line_1_status=$?

    check_interface $ldm_line_2
    ldm_line_2_status=$?

    if [ $ldm_line_1_status -gt 0 ] && [ $ldm_line_2_status -gt 0 ]
    then
        # neither ldm lines are up
        return 102
    fi

    if [ $ldm_line_1_status -eq 0 ] && [ $ldm_line_2_status -eq 0 ]
    then
        # both ldm lines are up
        return 105
    fi

    # check that ldm recently received data
    # ldmcp currently does not support a status function; instead we will check if
    #   any new products have been received within the last n milliseconds
    su - ldm -c "wasReceived -o $last_rcvd_product_threshold" 2> /dev/null
    if [ $? -ne 0 ]; then
        # ldm isn't receiving any new products
        return 103
    fi

    return 0
}

run_repair ()
{
    # the code returned from running the test
    local return_code="$1"
    # the object is the script used to test; in our self-repairing test scripts case,
    #   this means the "object" can be ignored as it will just be this script's name
    local object="$2"
    local test_result=0
    local startup_status=0
    
    # there are predefined codes watchdog looks for, but we could throw our own from
    #   the tests to indicate more specific service status (e.g. 101 for a service
    #   not being started, 102 for a service being in an error state, etc.) which we
    #   could then check against and would then allow us to perform different tailored
    #   tasks other than just trying to start up a particular service.

    # 101 indicates ldmcp was not running; start it up
    if [ $return_code -eq "101" ]; then
        echo "INFO `date_cmd` ldmcp not running, attempting to start"
        service_action "start" "ldmcp" 120
        run_test
        test_result=$?
    fi

    # 102 indicates neither ldm lines were up
    if [ $return_code -eq "102" ]; then
        # start by trying to bring up ldm_line_1
        echo "INFO `date_cmd` attempting to bring up $ldm_line_1"
        ip link set $ldm_line_1 up
        ifup $ldm_line_1
        startup_status=$?
        if [ $startup_status -gt 0 ]; then
            # ifup error'd; cleanup ldm_line_1 just in case
            echo "ERROR `date_cmd` failed to bring up $ldm_line_1; attempting to bring up $ldm_line_2"
            ifdown $ldm_line_1
            ip link set $ldm_line_1 down
            # alternatively, try to bring up ldm_line_2
            ip link set $ldm_line_2 up
            ifup $ldm_line_2
            startup_status=$?
            if [ $startup_status -gt 0 ]; then
                # ifup error'd; cleanup ldm_line_2 just in case
                ifdown $ldm_line_2
                ip link set $ldm_line_2 down
                echo "ERROR `date_cmd` Both ldm lines are down; nothing to be done..."
            fi
        fi
        
        if [ $startup_status -eq 0 ]; then
            # ldm needs to be restarted if a line was successfully brought up
            /usr/sbin/service ldmcp restart
        fi
    fi

    # 103 indicates there were no products received within the last however many seconds
    if [ $return_code -eq "103" ]; then
        # try switching the data_feeds to attempt to fix the problem
        check_interface $ldm_line_1
        if [ $? -eq 0 ]; then
            echo "WARN `date_cmd` no incoming products, attempting to switch from $ldm_line_1 to $ldm_line_2"
            ifdown $ldm_line_1
            ip link set $ldm_line_1 down
            ip link set $ldm_line_2 up
            ifup $ldm_line_2
        else
            echo "WARN `date_cmd` no incoming products, attempting to switch from $ldm_line_2 to $ldm_line_1"
            ifdown $ldm_line_2
            ip link set $ldm_line_2 down
            ip link set $ldm_line_1 up
            ifup $ldm_line_1
        fi
        # ldm needs to be restarted if the receiving line was swapped
        echo "INFO `date_cmd` restarting ldmcp"
        /usr/sbin/service ldmcp restart
    fi

    # 105 indicates both lines are up; default to using ldm_line_1 and shutdown ldm_line_2
    if [ $return_code -eq "105" ]; then
        echo "INFO `date_cmd` both ldm lines are up; shutting down $ldm_line_2 to ensure stability"
        ifdown $ldm_line_2
        ip link set $ldm_line_2 down
        test_result=$?
    fi

    return $test_result
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
        run_repair "$2" "$3"
        err=$?
    ;;

    # defaulting to 245 which watchdog recognizes as "state unknown" and doesn't
    #   treat the result as an error; ultimately, watchdog just ignores it
    # doing this to prevent any unwanted reboots
    *)
        err=245
esac

exit $err
