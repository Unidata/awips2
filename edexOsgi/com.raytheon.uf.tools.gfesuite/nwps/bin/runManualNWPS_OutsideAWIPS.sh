#!/bin/bash

NWPSLOCAL="/awips2/GFESuite/nwps"
if [ ! -e ${NWPSLOCAL}/input ]
then
    mkdir ${NWPSLOCAL}/input
    chmod 777 ${NWPSLOCAL}/input
fi
if [ ! -e ${NWPSLOCAL}/logs ]
then
    mkdir ${NWPSLOCAL}/logs
    chmod 777 ${NWPSLOCAL}/logs
fi
if [ ! -e ${NWPSLOCAL}/var ]
then
    mkdir ${NWPSLOCAL}/var
    chmod 777 ${NWPSLOCAL}/var
fi
source /awips2/GFESuite/bin/run/site.sh
logfile=${NWPSLOCAL}/logs/nwps_runManual_Outside_AWIPS.log
rm -f ${NWPSLOCAL}/logs/*
rm -f ${NWPSLOCAL}/wcoss/*

PATH="/awips2/GFESuite/bin:/bin:/usr/bin:/usr/local/bin"
SITEID=${GFESUITE_SITEID}
siteid=$(echo ${SITEID} | tr [:upper:] [:lower:])

SSHARGS="-x -o stricthostkeychecking=no"
SCPARGS="-o stricthostkeychecking=no"
HOST="ldad@ls1-${siteid}"
DIR="/data/ldad/nwps/input"

Program="/awips2/GFESuite/bin/ifpnetCDF"
DB="${SITEID}_GRID__Fcst_00000000_0000"
GFESERVER="ec"
RUNSERVER="px"
WRKSWN="${NWPSLOCAL}/SUAWRKNWP.dat"

Output_Dir="${NWPSLOCAL}/input"

date=$(date "+%D  %H:%M:%S")
Output_File="${Output_Dir}/Wind_File"
textfile="${Output_Dir}/$(date +%Y%m%d%H%M)_WIND.txt"
wcoss_textfile="${siteid}_$(date +%Y%m%d%H%M)_WIND.txt"
flagfile="${Output_Dir}/SWANflag"
cp ${NWPSLOCAL}/domains/${SITEID} ${NWPSLOCAL}/wcoss/${siteid}_domain_setup.cfg
chmod 666 ${NWPSLOCAL}/wcoss/${siteid}_domain_setup.cfg

### LOCK FILE STUFF:

source ${NWPSLOCAL}/bin/process_lock.sh
PROGRAMname="$0"
LOCKfile="${NWPSLOCAL}/logs/runManual_Outside_AWIPS.lck"
MINold="0"
LockFileCheck $MINold
CreateLockFile

### FUNCTIONS:

function logit () {
        echo "$@" | tee -a $logfile
}

### RUN OPTIONS:

if [ -e ${NWPSLOCAL}/var/inp_args ]
then

    inp_args=`cat ${NWPSLOCAL}/var/inp_args`
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

else

    logit "No arguments or arguments file provided. No run to process. Exiting."
    RemoveLockFile
    exit 1
fi

### CHECK FOR GFECRON USER RUNNING THIS SCRIPT:

ID=$(id -u)
if [[ $ID -ne 104 ]]
then
        logit "ONLY GFECRON USER IS ALLOWED TO RUN THIS SCRIPT."
        logit "Exiting ..."
        RemoveLockFile
        exit 1
fi


### CHECK THAT THIS IS THE px2 (or px1 if failed over) HOST MACHINE:

HOST=$(hostname|cut -c1-2)
if [[ $HOST != $RUNSERVER ]]
then
        logit "YOU ARE RUNNING FROM $HOST. THIS SCRIPT SHOULD ONLY BE RAN FROM $RUNSERVER."
        logit "Exiting ... "
        RemoveLockFile
        exit 1
fi


##################################################################
### MAKE CRONTAB FRIENDLY:

cd ${NWPSLOCAL}

##################################################################
### START A CLEAN LOG FILE:

echo " " > $logfile
STARTED=$(date)
logit "STARTED: $STARTED"

logit " "
# logit "Program called with: $0 $@"

logit " "
##################################################################
logit "### Setting Up SWAN Input Model Forcing Time Range"
logit " "
##################################################################

logit " "
logit "scp ${SCPARGS} $WRKSWN ldad@ls1:/data/Incoming/"
logit " "

echo "" > $WRKSWN
echo "____________________NWPS RUN REQUEST DETAILS__________" >> $WRKSWN
echo "" >> $WRKSWN
echo "Run initiated at: ${date}" >> $WRKSWN
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

scp ${SCPARGS} $WRKSWN ldad@ls1:/data/Incoming/

##################################################################
logit "### CREATE THE WIND NETCDF FILE AND SEND OVER TO SWAN BOX FOR PROCESSING:"

rm -vf $Output_Dir/*  | tee -a $logfile

logit "$Program -o $Output_File -d $DB -h $GFESERVER -g -p NWPSwind"
$Program -o $Output_File -d $DB -h $GFESERVER -g -p NWPSwind | tee -a $logfile

/usr/local/netcdf/bin/ncdump $Output_File > $textfile
sed -i "s/NWPSwind/Wind/g" $textfile
cp $textfile ${NWPSLOCAL}/wcoss/${wcoss_textfile}
chmod 666 ${NWPSLOCAL}/wcoss/${wcoss_textfile}

gzip $textfile
touch $flagfile

chmod 666 $textfile.gz
chmod 666 $flagfile

if [ $WHERETORUN == "Local" ]
then

    logit " "
    logit "RUNNING IN WORKSTATION"   
    logit " "
    logit "### ROUTING 3 WIND FILES NEEDED BY SWAN THROUGH LDAD TO LOCAL WORKSTATION:"

    ssh ldad@ls1 mkdir -p ${DIR} 
    scp ${SCPARGS} $Output_File ldad@ls1:${DIR}  | tee -a $logfile
    scp ${SCPARGS} $textfile.gz ldad@ls1:${DIR}  | tee -a $logfile
    scp ${SCPARGS} $flagfile ldad@ls1:${DIR}     | tee -a $logfile

    logit " "
    #########################################################################
    logit "##################################################################"
    logit "### SENDING WIND FILES TO NWPS SYSTEM VIA LDAD TO TRIGGER NEW RUN"
    logit "### Start Time is: $(date)"
    logit "##################################################################"
    logit " "

    logit "Runtime Parameters are: $RUNLEN:$WNA:$NEST:$GS:$WINDS:$WEB:$PLOT:$DELTAC:$HOTSTART:$WATERLEVELS:$CORE:$EXCD"

    ssh ${SSHARGS} ldad@ls1 echo "$RUNLEN:$WNA:$NEST:$GS:$WINDS:$WEB:$PLOT:$DELTAC:$HOTSTART:$WATERLEVELS:$CORE:$EXCD > /data/ldad/nwps/input/inp_args" 2>&1 | tee -a $logfile

else

    logit " "
    logit "RUNNING IN NCEP"   
    logit " "
    echo "$RUNLEN:$WNA:$NEST:$GS:$WINDS:$WEB:$PLOT:$DELTAC:$HOTSTART:$WATERLEVELS:$CORE:$EXCD" > ${NWPSLOCAL}/wcoss/${siteid}_inp_args.ctl

    chmod 666 ${NWPSLOCAL}/wcoss/${siteid}_inp_args.ctl

    cd ${NWPSLOCAL}/wcoss/
    NWPSWINDGRID="NWPSWINDGRID_${siteid}_$(date +%Y%m%d%H%M)_$$.tar.gz"
    tar cvfz ${NWPSWINDGRID} ${siteid}_inp_args.ctl ${siteid}_domain_setup.cfg ${wcoss_textfile}
    scp ${NWPSWINDGRID} ldad@ls1:/tmp/ 
    ssh ldad@ls1 "cd /tmp; /usr/local/ldm/bin/ldmsend -v -h srh-ls-cpnrs1.srh.noaa.gov -f EXP ${NWPSWINDGRID}" 2>&1 | tee -a $logfile
    ssh ldad@ls1 "cd /tmp; /usr/local/ldm/bin/ldmsend -v -h srh-ls-cpnrs2.srh.noaa.gov -f EXP ${NWPSWINDGRID}" 2>&1 | tee -a $logfile
    ssh ldad@ls1 rm -fv /tmp/${NWPSWINDGRID}

fi

logit " "
##################################################################
logit " "
RemoveLockFile

##################################################################
logit " "
logit "STARTED: $STARTED"
logit "FINISHED: $(date)"
logit " "
exit 0
