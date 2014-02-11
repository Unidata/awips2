#!/bin/bash

(cd $(dirname "$0")
PIDFILE=collabserver.pid
if [[ -e $PIDFILE ]]
then
	kill `cat $PIDFILE`
	rm $PIDFILE
fi
)
