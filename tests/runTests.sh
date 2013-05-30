#!/bin/sh

if [ "${INITIAL_PERMGEN_SIZE}" = "" ]
then
   export INITIAL_PERMGEN_SIZE="64m"
fi

if [ "${MAX_PERMGEN_SIZE}" = "" ]
then
   export MAX_PERMGEN_SIZE="192m"
fi

export ANT_OPTS="-XX:PermSize=${INITIAL_PERMGEN_SIZE} -XX:MaxPermSize=${MAX_PERMGEN_SIZE}"
ant
