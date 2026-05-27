#!/bin/bash

# check-localized-files.sh

# goal is to alert the user to which files changed in baseline for which they may have overrides

### Supporting / Reoccuring functions in my scripts

function isUp() {

# sub-script for checking availability of a host via ping
# USAGE:        isUp $host
# EXAMPLE:      isUp lx1

ping -q -c2 $1 > /dev/null 2>&1
return $?

}


function print_bar {

        _int=$((${1} % 8))
        case "${_int}" in
                [04]    )       echo -ne "\b\b\ " ;;
                [15]    )       echo -ne "\b\b| " ;;
                [26]    )       echo -ne "\b\b/ " ;;
                [37]    )       echo -ne "\b\b- " ;;
        esac

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


      echo -ne "|-- $( date +"%Y%m%d %H:%M:%S" ) "

}

function logIt() {

  #built for easier logging.  

  # Usage:      logIt $LEVEL $SCRIPT_LOCATION $TEXT
  # Example:    logIt ERROR $FUNCNAME "Error in doing something" 

  local logLevel=$1
  local scriptLoc=$2
  local logText=$3

  case "${logLevel}" in
        "INFO"          )       echoDate && echo -e "\t[${scriptLoc}]:\tINFO: ${logText}" ;;
        "NOTE"          )       echoDate && echo -ne "\t[${scriptLoc}]:\t" && color_echo yellow 0 "NOTE: ${logText}" ;;
        "ERROR"         )       echoDate && echo -ne "\t[${scriptLoc}]:\t" && color_echo red 1 "ERROR: ${logText}" ;;
        "DEBUG"         )       if [[ "${DEBUG}" ]] ; then echoDate && echo -ne "\t[${scriptLoc}]:\tDEBUG: ${logText}\n"; fi ;;
        "HEADER"        )       echoDate && echo -ne "\t[${scriptLoc}]:\t" && color_echo blue 1 "${logText}" ;;
        *               )       echoDate && echo -e "\t[${scriptLoc}]:\t${logText}" ;;
  esac

  return 0
}


## Script Specific

function setupEnv() {
	
	scriptRoot=$( dirname $0 )	
	if [[ "${scriptRoot}" == "." ]]
	then
		fullScriptPath=$( pwd )
	else
		fullScriptPath=${scriptRoot}
	fi
	
	inputFileName="changed_xml_1355_filtered.txt"
	
	if [[ $# -gt 0 ]] ; then
		inputFileName=$1
		logIt INFO main "User passed an argument which we're using for the input file name : $1"
	fi

	if echo ${inputFileName} | grep '/' > /dev/null ; then
		inputFile=${inputFileName}
	else
		inputFile=${fullScriptPath}/${inputFileName}
	fi
	
	runTimeStamp=$( date +%Y%m%d_%H%M%S )

}

# Main

# Catch the passed arguments
while getopts ":d" Option
do
	case $Option in
		d	)	DEBUG=1 ;;
#		y	)	debug_echo "Option y -- Accept Defaults specified." && acceptDefaults=1 && color_echo yellow 0 "*** Accept Defaults Specified ***" ;;
		*	)	logIt ERROR main "Unimplemented $Option.  OPTIND=$OPTIND -- IGNORING" ;;
	esac
done

shift $(($OPTIND - 1))

setupEnv $1

	



{
logIt HEADER main "Starting Main Program"
if [[ ! -f ${inputFile} ]] ; then
	color_echo red 1 "ERROR:  You are missing ${inputFileName}"
	exit 1
fi

logIt INFO main "\tAbout to check for new files....."
while read file ; do 

	fileName=${file##*/}
	fileLocation=${file%%/*}
	#echo -e "NEW FILE:  ${fileName}\t${fileLocation}\t${file}"
	case ${fileLocation} in 
		"cave"	)	paredPath=${file#cave/*/localization/}
					paredPath=${paredPath%/*}
					searchPath="cave_static/(site|user)/.*/${paredPath}" ;;
		"edexOsgi"	)	paredPath=${file#edexOsgi/*/utility/}
						paredPath=${paredPath%/*}
						#searchPath=${paredPath#*/} 
						#searchPath=${paredPath%/*}
						searchPath=$( echo ${paredPath} | sed -e "s/base/\(site|user\)\/\.\*/g" );; 
	esac
	logIt DEBUG main "Search Path:  ${searchPath}"
	logIt DEBUG main "File Name: ${fileName}" 
	if ! cd /awips2/edex/data/utility ; then
		logIt ERROR main "Can not change directory (cd) to /awips2/edex/data/utility.  Please check you are on a device with this path mounted"
		exit 1
	fi
	#echo -e "------------------------------------------"
	if [[ ! -n ${searchPath} || ${searchPath} != "" ]] ; then
		logIt DEBUG main "FIND COMMAND:  find . -type f -regextype posix-egrep -regex \"./${searchPath}/${fileName}\" -ls"
		find . -type f -regextype posix-egrep -regex "./${searchPath}/${fileName}" -ls | while read line ; do
			logIt NOTE main "Found file which may match a new baselined file: " 
			logIt "" main "\tBaseline File:\t ${paredPath}/${fileName}"
			logIt "" main "\tYour File:\t${line}"
		done
		#echo && echo 
		#find . -type f -name ${fileName} -ls
	fi
	#echo -e "------------------------------------------"
	

done< ${inputFile}

logIt HEADER main "Completing Main Program"
} | tee -a /tmp/$(basename $0 .sh)_${runTimeStamp}.log
exit 0
