#!/bin/sh

TRUE=1

while [ $TRUE -eq 1 ]
do
  COUNT=`find /tmp/sbn -mmin +10 |wc -l`
  if [ $COUNT -gt 0 ]
  then
    find /tmp/sbn -mmin +10 | xargs rm -rf
    echo "Deleted $COUNT files from /tmp/sbn."
  fi
  sleep 5
done

