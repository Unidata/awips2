#!/bin/bash

# Last  Modified: 10/15/2015
# By: Pablo Santos and Joseph Maloney
# Version: AWIPS 2 Version 16.2.1

NWPSLOCAL="/awips2/GFESuite/nwps"
umask 002

#
PATH="/awips2/GFESuite/bin:/bin:/usr/bin:/usr/local/bin"
siteid=$(hostname -s|cut -c 5-)
source ${NWPSLOCAL}/etc/sitevars.${siteid} ${siteid}
#
SSHARGS="-x -o stricthostkeychecking=no"
SCPARGS="-o stricthostkeychecking=no"
#
Program="/awips2/GFESuite/bin/ifpnetCDF"
GFESERVER="ec"
RUNSERVER="px"

# FUNCTIONS

function logit () {
        echo "$@" | tee -a $logfile
}

GFEDomainname=${1} 
logit "Processing $GFEDomainname"
gfedomainname=$(echo ${GFEDomainname} | tr [:upper:] [:lower:])

cd ${NWPSLOCAL}

if [ ! -e ${NWPSLOCAL}/${GFEDomainname} ]
then
    mkdir ${NWPSLOCAL}/${GFEDomainname}
    chmod 777 ${NWPSLOCAL}/${GFEDomainname}
fi

if [ ! -e ${NWPSLOCAL}/input/${GFEDomainname} ]
then
    mkdir -p ${NWPSLOCAL}/input/${GFEDomainname}
    chmod -R 777 ${NWPSLOCAL}/input
fi

if [ ! -e ${NWPSLOCAL}/wcoss/${GFEDomainname} ]
then
    mkdir -p ${NWPSLOCAL}/wcoss/${GFEDomainname}
    chmod -R 777 ${NWPSLOCAL}/wcoss
fi

if [ ! -e ${NWPSLOCAL}/logs ]
then
    mkdir ${NWPSLOCAL}/logs
    chmod 777 ${NWPSLOCAL}/logs
fi

logfile=${NWPSLOCAL}/logs/${GFEDomainname}_nwps_runManual_Outside_AWIPS.log
##################################################################
### START A CLEAN LOG FILE:
#
rm -f $logfile
echo " " > $logfile
STARTED=$(date)
logit "STARTED FOR ${GFEDomainname}: $STARTED"

DB="${GFEDomainname}_GRID__Fcst_00000000_0000"

if [ ${MULTISITE} == "Yes" ]
then
Output_Dir="${NWPSLOCAL}/input/${GFEDomainname}"
else
Output_Dir="${NWPSLOCAL}/input"
fi

WRKSWN="${NWPSLOCAL}/${GFEDomainname}/SUAWRKNWP.dat"
date=$(date "+%D  %H:%M:%S")
Output_File="${Output_Dir}/Wind_File"
textfile="${Output_Dir}/$(date +%Y%m%d%H%M)_WIND.txt"
wcoss_textfile="${gfedomainname}_$(date +%Y%m%d%H%M)_WIND.txt"
flagfile="${Output_Dir}/SWANflag"

### LOCK FILE STUFF:

source ${NWPSLOCAL}/bin/process_lock.sh
PROGRAMname="$0"
LOCKfile="${NWPSLOCAL}/logs/runManual_Outside_AWIPS_${GFEDomainname}.lck"
MINold="300"
LockFileCheck $MINold
CreateLockFile

### CHECK THAT THIS IS THE px2 (or px1 if failed over) HOST MACHINE:

HOST=$(hostname|cut -c1-2)
if [[ $HOST != $RUNSERVER ]]
then
        logit "YOU ARE RUNNING FROM $HOST. THIS SCRIPT SHOULD ONLY BE RAN FROM $RUNSERVER."
        logit "Exiting ... "
        RemoveLockFile
        exit 1
fi

### RUN OPTIONS:

if [ -e ${NWPSLOCAL}/${GFEDomainname}_var/inp_args ]

then

    inp_args=`cat ${NWPSLOCAL}/${GFEDomainname}_var/inp_args`
    IFS=':' read -a inp <<< "${inp_args}"

    RUNLEN=${inp[0]}
    WNA=${inp[1]}
    NEST=${inp[2]}
    GS=${inp[3]}
    WINDS=${inp[4]}
    WEB=${inp[5]}
    PLOT=${inp[6]}
    DELTAC=${inp[7]}
    HOTSTART=${inp[8]}
    WATERLEVELS=${inp[9]}
    CORE=${inp[10]}
    EXCD=${inp[11]}
    WHERETORUN=${inp[12]}

    logit " "
    logit "Arguments are: $RUNLEN $WNA $NEST $GS $WINDS $WEB $PLOT $DELTAC $HOTSTART $WATERLEVELS $CORE $EXCD $WHERETORUN"
    logit " "

    rm -f ${NWPSLOCAL}/${GFEDomainname}_var/inp_args
    rm -f ${NWPSLOCAL}/wcoss/${GFEDomainname}/* | tee -a $logfile
    rm -f ${Output_Dir}/* | tee -a $logfile
    cp ${NWPSLOCAL}/domains/${GFEDomainname} ${NWPSLOCAL}/wcoss/${GFEDomainname}/${gfedomainname}_domain_setup.cfg
    chmod 666 ${NWPSLOCAL}/wcoss/${GFEDomainname}/${gfedomainname}_domain_setup.cfg

else

    logit "No arguments or arguments file provided. No run to process. Exiting ${GFEDomainname}."
    RemoveLockFile
    continue
fi

logit " "
##################################################################
logit "### Setting Up SWAN Input Model Forcing Time Range"
logit " "
##################################################################


echo "" > $WRKSWN
echo "____________________NWPS RUN REQUEST DETAILS__________" >> $WRKSWN
echo "" >> $WRKSWN
echo "Run for ${GFEDomainname} initiated at: ${date}" >> $WRKSWN
echo "" >> $WRKSWN
echo "Runlength: ${RUNLEN}" >> $WRKSWN
echo "Boundary Conditions: ${WNA}" >> $WRKSWN
echo "Nest: ${NEST}" >> $WRKSWN
echo "Current: ${GS}" >> $WRKSWN
echo "Winds: ${WINDS}" >> $WRKSWN
echo "Timestep: ${DELTAC}" >> $WRKSWN
echo "Hotstart: ${HOTSTART}" >> $WRKSWN
echo "WATERLEVELS: ${WATERLEVELS}" >> $WRKSWN
echo "Model Core: ${CORE}" >> $WRKSWN
echo "Psurge % Exceedance: ${EXCD}" >> $WRKSWN
echo "Running model in: ${WHERETORUN}" >> $WRKSWN
echo "" >> $WRKSWN
echo "______________________________________________________" >> $WRKSWN

##################################################################
logit "### CREATE THE WIND NETCDF FILE AND SEND OVER TO SWAN BOX FOR PROCESSING:"

logit "$Program -o $Output_File -d $DB -h $GFESERVER -g -p NWPSwind"
$Program -o $Output_File -d $DB -h $GFESERVER -g -p NWPSwind | tee -a $logfile

/usr/local/netcdf/bin/ncdump $Output_File > $textfile
sed -i "s/NWPSwind/Wind/g" $textfile
cp $textfile ${NWPSLOCAL}/wcoss/${GFEDomainname}/${wcoss_textfile}
chmod 666 ${NWPSLOCAL}/wcoss/${GFEDomainname}/${wcoss_textfile}

gzip $textfile
touch $flagfile

chmod 666 $textfile.gz
chmod 666 $flagfile


echo "$RUNLEN:$WNA:$NEST:$GS:$WINDS:$WEB:$PLOT:$DELTAC:$HOTSTART:$WATERLEVELS:$CORE:$EXCD" > ${NWPSLOCAL}/wcoss/${GFEDomainname}/${gfedomainname}_inp_args.ctl
chmod 666 ${NWPSLOCAL}/wcoss/${GFEDomainname}/${gfedomainname}_inp_args.ctl
cd ${NWPSLOCAL}/wcoss/${GFEDomainname}
NWPSWINDGRID="NWPSWINDGRID_${gfedomainname}_$(date +%Y%m%d%H%M)_$$.tar.gz"
tar cvfz ${NWPSWINDGRID} ${gfedomainname}_inp_args.ctl ${gfedomainname}_domain_setup.cfg ${wcoss_textfile}

logit " "
##################################################################
logit " "
RemoveLockFile

##################################################################
logit " "
date
logit "FINISHED ${GFEDomainname}: $(date)"
logit " "

exit 0
