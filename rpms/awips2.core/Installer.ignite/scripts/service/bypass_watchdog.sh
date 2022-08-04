#!/bin/bash

# where to indicate to watchdog that this service is manually shutdown
watchdog_status=/tmp/watchdog_status

if [ ! -d "$watchdog_status" ]; then
	mkdir --parents "$watchdog_status"
fi
touch "$watchdog_status/ignite"