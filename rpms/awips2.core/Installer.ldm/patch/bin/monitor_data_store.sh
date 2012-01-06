#!/bin/bash
myPID=$$
echo -e "`date +%Y%m%d`\t`date +%H:%M:%S`\tStarting Script (pid = $myPID, parent = $PPID)" >> ~/logs/`basename $0 .sh`
if ps -wef|grep `basename $0` | grep -v grep | grep -v $myPID | grep -v $PPID
then
	exit 0
fi

cd /data_store
while true
do
	for _dir in `ls`
	do
		echo -e "`date +%Y%m%d`\t`date +%H:%M:%S`\t\tfind ${_dir} -mtime +0 -type f -exec rm -f {} \;" >> ~/logs/`basename $0 .sh`
		find ${_dir} -mtime +0 -type f -exec rm -f {} \;
	done
done
