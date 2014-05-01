#!/bin/bash

##Script to query the RPM database and return Name,Version,Release ID,Build Machine and Install Date for AWIPS-II RPMs

#set the report spacing
nameSz=35
verSz=15
relSz=10
bhSz=15 
dateSz=30
tmpFile="START"
tmp=""

#create the empty arrays
EDEX=()
CAVE=()
PYTHON=()
POSTGRESQL=()
JAVA=()
Dependencies=()

#define the components
components=(CAVE EDEX JAVA POSTGRESQL PYTHON Dependencies)
setTemp=false

#start the report
printf '\nAWIPS-II RPMS installed on %s\n\n' `hostname`
printf '%-'$nameSz's%-'$verSz's%-'$relSz's%-'$bhSz's%-'$dateSz's\n' Name Version Release 'Build Host' 'Install Date' 
printf -v f "%"$nameSz"s%"$verSz"s%"$relSz"s%"$bhSz"s%"$dateSz"s" ; printf "%s\n" "${f// /+}"

#iterate through each RPM in the AWIPSII group
#assign to an array
for file in `rpm -qg "AWIPSII"|sort`
do
	case `echo $file | awk -F '-' '{print $2}'` in 

		"edex")	EDEX=("${EDEX[@]}" "$file")
			;;
		"cave"|"alertviz") CAVE=("${CAVE[@]}" "$file")
			;;
		"psql"|"postgresql") POSTGRESQL=("${POSTGRESQL[@]}" "$file")
			;;
		"java") JAVA=("${JAVA[@]}" "$file")
			;;
		"python") PYTHON=("${PYTHON[@]}" "$file")
			;;
		*) Dependencies=("${Dependencies[@]}" "$file")
			;;
	esac
done

#iterate through the components array
for comp in ${components[@]}; do
	eval temp=\${$comp[@]}
	
	#if the component array is not empty, print out a header and then values
	if [ ${#temp} -gt 0 ]; then
		printf '%s\n' $comp 
		for rpm in ${temp[@]};do 
			rpm -q --queryformat '\-%-'` expr $nameSz - 1`'{NAME}%-'$verSz'{VERSION}%-'$relSz'{RELEASE}%-'$bhSz'{BUILDHOST}%-'$dateSz'{INSTALLTIME:date}\n' $rpm
		done
	fi
done

