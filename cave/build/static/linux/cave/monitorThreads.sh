#!/bin/sh
# runs a jstack every X seconds until killed

basePath="/data/fxa/cave"
hostName=`hostname -s`
hostPath="${basePath}/${hostName}"

pid="$1"
sleepTime="$2"

sleep 10 # put in to allow exec of cave jvm 

# days worth of jstacks to keep
purge_retention="7"

function purgeJstacks()
{
  find $basePath -type f -name "*pid*jstacks.tgz" -mtime +${purge_retention} -exec rm -f {} \; >> /dev/null 2>&1
}

for i in $( ps -p ${pid} -f  | grep -v UID ) ; do  
    if [[ ! "${perspective}" ]] ; then 
        if [[ "$j" == "-perspective" || "$j" == "-component" ]] ; then 
            perspective=$i 
        fi 
    j=$i 
    fi
done

if [[ ! "${perspective}" ]] ; then perspective="cave" ; fi 

if [ "$pid" == "" ]; then
	echo "Usage: continualJstack.sh <PID> <SLEEP_INTERVAL>"
	echo "    SLEEP_INTERVAL defaults to 2 if not specified"
else
  # purge old jstacks
  purgeJstacks &

  if [ "$sleepTime" == "" ]; then
    sleepTime=2
  fi

  if [[ ! -d ${hostPath} ]] ; then mkdir -p ${hostPath} ; fi
  cd ${hostPath}
  t1=$( date +"%Y%m%d_%H%M" )
  pidPath="${t1}_pid${pid}_${perspective}_jstacks"
  mkdir -p ${pidPath}
  cd ${pidPath}

  prevDatePortion=""

  while ps -p ${pid} > /dev/null ; do
    t1=$( date +"%Y%m%d_%H%M" )

    # strip off the last minute, allowing for tars to be created in 10 minute segments
    newDatePortion="${t1%?}"

    if [[ $newDatePortion != $prevDatePortion ]]; then
      # verify this isn't the first one
      if [[ "$prevDatePortion" != "" ]]; then
        tar -czf jstack_${prevDatePortion}X_${pid}.log.tgz --remove-files jstack_${prevDatePortion}*${pid}.log &
      fi

      prevDatePortion="$newDatePortion"
    fi

    log="jstack_${t1}_${pid}.log"
    jstack -l $pid >> $log
    echo "" >> $log
    echo "" >> $log

    sleep $sleepTime
  done

  tar -czf jstack_${newDatePortion}X_${pid}.log.tgz --remove-files jstack_${newDatePortion}*${pid}.log
  # ensure any background threads have finished taring also
  wait
  cd ..
  tar -czf ${pidPath}.tgz --remove-files ${pidPath}/
  chmod g+w ${pidPath}.tgz
  rm -rf ${pidPath}
  echo -e "$pid not detected ... exiting"
fi

