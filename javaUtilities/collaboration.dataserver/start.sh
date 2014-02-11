#!/bin/bash

(cd $(dirname "$0")
PIDFILE=collabserver.pid
if [[ -e $PIDFILE ]]
then
	echo "PID file already exists at $PIDFILE, exiting"
	exit 1
fi
nohup java -server -jar collabserver.jar &
echo $! > $PIDFILE
)
