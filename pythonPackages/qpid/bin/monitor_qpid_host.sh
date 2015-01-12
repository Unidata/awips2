#!/bin/bash

# script to gather information on a qpid host
# 20 July 2012 - Initial script (kpj)


function setupEnv() {
  
  runTimeDate=$( date +"%Y%m%d %H:%M:%S" )
  nowTimeDate=$( date +%A )
  logDirectory=/data/fxa/qpid
  logName=$( basename $0 .sh ).${nowTimeDate}.log 
  nasHost=nas1
  nasVolName=dataFXA  # This is so we can change it for new nas! 
  lsofCommand="lsof -Pns -p"
  platformName=$( hostname | cut -f2 -d'-')

  if [[ ${logDirectory}/${logName} -ot ${logDirectory}/$( basename $0 .sh ).$(date --date='1 day ago' +%A).log ]]
  then
    for myFile in ${logName} ${nowTimeDate}-lsof_qpid.out ${nowTimeDate}-qpid-stat.out ${nowTimeDate}-netstat.out ${nowTimeDate}-ipvsadm.out ${nowTimeDate}-captureQpidHeapInfo.out
    do
      echo > ${logDirectory}/${myFile}
    done
    gzip -f ${logDirectory}/*$(date --date='1 day ago' +%A)*
  fi
    

}

function color_echo() {

        # To echo something to stdout with color
        #
        # Usage:  color_echo $COLOR $BOLD $STRING 
        #
        #       COLOR:          Color string from: black, green, blue, red, yellow, white
        #       BOLD:           1 = true, 0 = false
        #       STRING:         String to echo

        sColor=$1
        sBold=$2
        sString=$3

        if [[ -z "${sColor}" || -z "${sBold}" || -z "${sString}" ]] || [[ ${sBold} -ne 1 && ${sBold} -ne 0 ]]
        then
                echo -e "ERROR IN $FUNCNAME:\t Usage:\t$FUNCNAME $COLOR $BOLD $STRING"
        else
                case ${sColor} in
                        "green" )       colorInt=32 ;;
                        "blue"  )       colorInt=34 ;;
                        "red"   )       colorInt=31 ;;
                        "yellow" )      colorInt=33 ;;
                        "white" )       colorInt=37 ;;
                        "black" )       colorInt=30 ;;
                        *       )       colorInt="" ;;
                esac

                if [[ ${sBold} -eq 0 ]]; then sBold="" ; fi
                echo -e "\033[${sBold};${colorInt}m${sString}\033[0m"
        fi

        return 0
}

function echoDate() {

      echo -ne "|-- $( date +"%Y%m%d %H:%M:%S" )"

}

function echoFail() {

    echoDate && color_echo red 1 "\t$1"

}

function cleanup() {

  if [[ "${hadToMount}" ]]
  then
      umount /data/fxa 
  fi

}

function runlsof() {

  echo -ne "\n| START " >>  ${logDirectory}/${nowTimeDate}-lsof_qpid.out
  echoDate >>  ${logDirectory}/${nowTimeDate}-lsof_qpid.out
  echo -e "----------------------------------------------------------------|\n" >>  ${logDirectory}/${nowTimeDate}-lsof_qpid.out

  if ${lsofCommand} ${qpidPid} >> ${logDirectory}/${nowTimeDate}-lsof_qpid.out 2>&1 
  then
    return 0
  else
    return 1
  fi

}

function captureQpidStat() {

  local returnCode=0
  local qpidConnLimit=500
  local qpidConnMedAlarm=75
  local qpidConnHighAlarm=40
  local qpidConnCritAlarm=15

  case "${platformName}" in
    [a-z][a-z][a-z]n    )   qpidConnLimit=1000 ; echo -e "\tNOTE:  Setting Max qpidd connection to 1000 due to NCEP site" >>  ${logDirectory}/${nowTimeDate}-qpid-stat.out ;; 
  esac

  echo -ne "\n| START " >> ${logDirectory}/${nowTimeDate}-qpid-stat.out
  echoDate >> ${logDirectory}/${nowTimeDate}-qpid-stat.out
  echo -e "----------------------------------------------------------------|\n" >> ${logDirectory}/${nowTimeDate}-qpid-stat.out

  # Send ITO alarm to NCF - Thank you Sean Bowser for your guidance here.  You are wise.
  numQpidConnections=$( qpid-stat -c | wc -l )
  (( numQpidConnections-=3 )) 
  echo -e "Total Number of QPID Connections: ${numQpidConnections}" >> ${logDirectory}/${nowTimeDate}-qpid-stat.out
	  
  echo >> ${logDirectory}/${nowTimeDate}-qpid-stat.out

  for cmdArg in "-b" "-c" "-s" "-e" "-q -Smsg" 
  do
      if ! qpid-stat ${cmdArg} >> ${logDirectory}/${nowTimeDate}-qpid-stat.out 2>&1
      then
	(( returnCode+=1 ))
	echoFail "\tqpid-stat ${cmdArg} returned non-zero exit code" 
      fi
      echo >> ${logDirectory}/${nowTimeDate}-qpid-stat.out
  done

  return ${returnCode}
}

function captureNetstat() {

  echo -ne "\n| START " >> ${logDirectory}/${nowTimeDate}-netstat.out
  echoDate >> ${logDirectory}/${nowTimeDate}-netstat.out
  echo -e "----------------------------------------------------------------|\n" >> ${logDirectory}/${nowTimeDate}-netstat.out
  
  if netstat -tunape | grep :5672 >> ${logDirectory}/${nowTimeDate}-netstat.out 2>&1
  then
    return 0
  else
    return 1
  fi
}

function captureIPVS() {

  local returnCode=0

  echo -ne "\n| START " >> ${logDirectory}/${nowTimeDate}-ipvsadm.out 2>&1
  echoDate >> ${logDirectory}/${nowTimeDate}-ipvsadm.out 2>&1
  echo -e "----------------------------------------------------------------|\n" >> ${logDirectory}/${nowTimeDate}-ipvsadm.out 2>&1

  if ! ipvsadm --list >> ${logDirectory}/${nowTimeDate}-ipvsadm.out 2>&1
  then
    (( returnCode+=1 ))
  fi
  echo >> ${logDirectory}/${nowTimeDate}-ipvsadm.out

  if ! ipvsadm --list --stats >> ${logDirectory}/${nowTimeDate}-ipvsadm.out 2>&1
  then
    (( returnCode+=1 ))
  fi
  echo >> ${logDirectory}/${nowTimeDate}-ipvsadm.out

  if ! ipvsadm --list --connection --sort >> ${logDirectory}/${nowTimeDate}-ipvsadm.out 2>&1
  then
    (( returnCode+=1 ))
  fi
  echo >> ${logDirectory}/${nowTimeDate}-ipvsadm.out

  return ${returnCode}
}

function captureQpidHeapInfo() {


	local returnCode=0
	local logFile=${logDirectory}/${nowTimeDate}-$FUNCNAME.out

	echo -ne "\n| START "  >> ${logFile} 2>&1
	echoDate >> ${logFile} 2>&1
	echo -e "----------------------------------------------------------------|\n" >> ${logFile} 2>&1 
	
	#qpidPid=$( jps -v | grep QPBRKR | awk '{print $1}' ) 

	if ! ps -p ${qpidPid} > /dev/null ; then
		echoFail "\tCan not find returned qpidd pid (${qpidPid}): $( jps -v | grep QPBRKR | awk '{print $1}' )" 
		return 1 
	fi

	echo -e "\tFound qpidd on PID ${qpidPid}" >> ${logFile}

	echo -e "\tGetting HEAP usage...........................\n\n" >> ${logFile}
	jmap -heap ${qpidPid} | while read line ; do
		echo -e "\t\t${line}" >> ${logFile}
	done

	echo -e "\n\n\tGetting Garbage Collection Information ..................\n\n" >> ${logFile}
	jstat -gcutil ${qpidPid} 1000 30 | while read line ; do 
		echo -e "\t\t${line}" >> ${logFile}
	done

	return 0 

}



## main() 
setupEnv

{

echo -ne "\n| START " && echoDate && echo -e "----------------------------------------------------------------|\n"

if ! grep /data/fxa /proc/mounts | grep nfs 2>&1 > /dev/null
then
  # /data/fxa isn't an nfs mount  
  if mount ${nasHost}:${nasVolName} /data/fxa
  then
      hadToMount=true
  else
      echoFail "ERROR:\t Couldn't mount /data/fxa and that is where the log goes!"
      exit 1
  fi
fi

# now check write permission 
if [[ ! -d ${logDirectory} ]]
then
  if ! mkdir -p ${logDirectory} > /dev/null 2>&1 
  then
      echoFail "ERROR:\t Couldn't create ${logDirectory}" 
      exit 1
  fi
fi

if ! touch ${logDirectory}/testfile > /dev/null 2>&1
then
  echoFail "ERROR:\tNo write permissions to ${logDirectory}"
  exit 1
else
  rm ${logDirectory}/testfile
fi

if ! qpidPid=$( jps -v | grep QPBRKR | awk '{print $1}' )
then
  echoFail "ERROR:\tCan't find qpidd on this host (run: jps -v | grep QPBRKR failed)."
  exit 1
fi

runlsof &
functionPID=$!
_cnt=0 
while ps -p ${functionPID} > /dev/null
do
	sleep 1
	_cnt=$(($_cnt+1))
	if [[ ${_cnt} -ge 10 ]] 
	then
	  kill -9 ${functionPID} 
	  echoFail "ERROR: lsof running for more than 10 seconds, killing" 
	fi
done	

if ! wait ${functionPID}
then
  echoFail "ERROR: Grabbing of lsof on qpidd failed" 
fi


captureQpidStat &
functionPID=$!
_cnt=0 
while ps -p ${functionPID} > /dev/null
do
	sleep 1
	_cnt=$(($_cnt+1))
	if [[ ${_cnt} -ge 30 ]] 
	then
	  kill -9 ${functionPID} 
	  echoFail "ERROR: qpid-stat running for more than 30 seconds, killing" 
	fi
done	

if ! wait ${functionPID}
then
  echoFail "ERROR: Grabbing of qpid-stat failed" 
fi

captureQpidHeapInfo &
functionPID=$!
_cnt=0 
while ps -p ${functionPID} > /dev/null
do
	sleep 1
	_cnt=$(($_cnt+1))
	if [[ ${_cnt} -ge 60 ]] 
	then
	  kill -9 ${functionPID} 
	  echoFail "ERROR: Getting heap infomation running for more than 60 seconds, killing" 
	fi
done	

if ! wait ${functionPID}
then
  echoFail "ERROR: Grabbing of heap utilization failed" 
fi

captureNetstat &
functionPID=$!
_cnt=0 
while ps -p ${functionPID} > /dev/null
do
	sleep 1
	_cnt=$(($_cnt+1))
	if [[ ${_cnt} -ge 10 ]] 
	then
	  kill -9 ${functionPID} 
	  echoFail "ERROR: netstat running for more than 10 seconds, killing" 
	fi
done	

if ! wait ${functionPID}
then
  echoFail "ERROR: Grabbing of netstat failed" 
fi

if ! pidof pulse > /dev/null  2>&1
then
  echoFail "ERROR: IPVS doesn't appear to be running on this host ($( hostname ))" 
else
  captureIPVS &
  functionPID=$!
  _cnt=0 
  while ps -p ${functionPID} > /dev/null
  do
	  sleep 1
	  _cnt=$(($_cnt+1))
	  if [[ ${_cnt} -ge 20 ]] 
	  then
	    kill -9 ${functionPID} 
	    echoFail "ERROR: ipvs capture running for more than 20 seconds, killing" 
	  fi
  done	
fi

} >> ${logDirectory}/${logName} 2>&1 
exit 0
