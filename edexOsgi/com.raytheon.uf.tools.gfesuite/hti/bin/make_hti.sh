#!/bin/bash -l
# UNIX Shell Script
# Tested Operating System(s): RHEL 3, 4, 5, 6
# Tested Run Level(s): 3, 5
# Shell Used: BASH shell
# Original Author(s): Douglas.Gaer@noaa.gov
# File Creation Date: 01/27/2009
# Date Last Modified: 10/20/2014
#
# Contributors:
# Joe Maloney (MFL), Pablo Santos (MFL)
# ----------------------------------------------------------- 
# ------------- Program Description and Details ------------- 
# ----------------------------------------------------------- 
#
# This script uses ifpImage to create PNG graphics of tropical threat
# grids, then runProcedure to create KML of the same grids.  Finally,
# the output files are rsync'd to LDAD and finally the webfarm.
#
# To execute:
#    ./make_hti.sh wfo
# (where wfo is the three letter wfo id)
#
# The script can be launched from the Scripts... menu in GFE, though
# it should also be run via trigger.
#
# Directory program runs from: /awips2/GFESuite/hti/bin
#
# Needed configuration on ls2/3:  /data/ldad/hti needs to exist.  The
#                                 script will create this directory if
#                                 it is missing.
#
# History:
# 20 OCT 2014 - jcm - created from make_ghls.sh, broke out of webapps
#                     package.  Renamed make_hti.sh.
#
########################################################################
#  CHECK TO SEE IF SITE ID WAS PASSED AS ARGUMENT
#  IF NOT THEN EXIT FROM THE SCRIPT
########################################################################
if [ $# -lt 1 ] ;then
   echo Invalid number of arguments.
   echo Script stopped.
   echo ./rsyncGridsToCWF.sh wfo
   exit
else
   SITE=$(echo ${1} | tr '[a-z]' '[A-Z]')
   site=$(echo ${1} | tr '[A-Z]' '[a-z]')
fi

########################################################################
#  CONFIGURATION SECTION BELOW
########################################################################
GFESUITE_HOME="/awips2/GFESuite"
HTI_HOME="${GFESUITE_HOME}/hti"

if [ ! -f ${HTI_HOME}/etc/sitevars.${site} ]; then
   cp ${HTI_HOME}/etc/sitevars.ccc ${HTI_HOME}/etc/sitevars.${site}
fi

# SITES CAN CUSTOMIZE THE SITEVARS AS NEEDED
. ${HTI_HOME}/etc/sitevars.${site}

########################################################################
#  BEGIN MAIN SCRIPT
########################################################################

# set current data and log file name
currdate=$(date -u +%Y%m%d)
export LOG_FILE="${HTI_HOME}/logs/${currdate}/make_hti.log"

# check to see if log directory structure exists.
if [ ! -d  ${HTI_HOME}/logs ] ;then
   mkdir -p ${HTI_HOME}/logs
   chmod 777 ${HTI_HOME}/logs
   chown awips:fxalpha ${HTI_HOME}/logs
fi
if [ ! -d  ${HTI_HOME}/logs/${currdate} ] ;then
   mkdir -p ${HTI_HOME}/logs/${currdate}
   chmod 777 ${HTI_HOME}/logs/${currdate}
   chown awips:fxalpha ${HTI_HOME}/logs/${currdate}
fi

# Log file header
echo " " >> $LOG_FILE
echo "####################################################################################" >> $LOG_FILE
echo "# Starting Make_HTI Script....                                                    #" >> $LOG_FILE
echo "####################################################################################" >> $LOG_FILE
chmod 666 $LOG_FILE

# Check to see if ${PRODUCTdir} exists.  If not, create.
echo "Making sure that ${PRODUCTdir} exists at $(date)" >> $LOG_FILE
if [ ! -d ${PRODUCTdir} ]; then
   echo " **** ${PRODUCTdir} directory not found." >> $LOG_FILE
   echo " **** Creating ${PRODUCTdir} directory..." >> $LOG_FILE
   mkdir -p $PRODUCTdir
   echo " **** Changing permissions of ${PRODUCTdir} directory..." >> $LOG_FILE
   chmod 777 $PRODUCTdir
   echo " **** Changing ownership of ${PRODUCTdir} directory..." >> $LOG_FILE
   chown awips:fxalpha $PRODUCTdir
else
   echo "  ${PRODUCTdir} exists." >> $LOG_FILE
fi
echo "...finished." >> $LOG_FILE
echo " " >> $LOG_FILE

# THESE SHOULD MATCH THREAT GRIDS IN GFE
PARMS="WindThreat FloodingRainThreat StormSurgeThreat TornadoThreat"
    
echo "Starting ifpIMAGE loop." >> $LOG_FILE
for PARM in $PARMS
do
     # NOTE: cannot run ifpIMAGE on dx3/dx4 - must ssh to a px
     echo "Creating ${PARM} image..." >> $LOG_FILE
     ssh px1 "unset DISPLAY; ${GFEBINdir}/ifpIMAGE -site ${SITE} -c ${SITE}${PARM} -o ${PRODUCTdir}"
     convert ${PRODUCTdir}/${SITE}${PARM}.png -resize 104x148 ${PRODUCTdir}/${SITE}${PARM}_sm.png
done
    
rm -f ${PRODUCTdir}/*.info
    
# Generate KML automatically via runProcedure
echo "Running KML procedure." >> $LOG_FILE
#ssh px1 "unset DISPLAY; ${GFEBINdir}/runProcedure -site ${SITE} -n TCImpactGraphics_KML -c gfeConfig"
ssh px1 "unset DISPLAY; ${GFEBINdir}/runProcedure -site ${SITE} -n TCImpactGraphics_KML2015 -c gfeConfig"

# Create legends for KML
${HTI_HOME}/bin/kml_legend.sh

echo "Copying image and kml files to LDAD for WEB processing" >> $LOG_FILE
/usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} mkdir -p ${LDAD_DATA} &> /dev/null
/usr/bin/rsync -av --force --progress --stats -e "/usr/bin/ssh -o stricthostkeychecking=no -x" ${PRODUCTdir}/*.txt ${PRODUCTdir}/*.png ${LDADuser}@${LDADserver}:/${LDAD_DATA}/.
echo "Done copying image and text files to LDAD" >> $LOG_FILE

# NOTE: The lines below are for SR and ER sites to upload images to the National Website
# NOTE: This can be ran as any AWIPS user so we must SSH as ldad@ls1 and run the commands via an SSH tunnel
echo "Copying graphics to NWSHQ Web farm ${NWSHQ_RSYNCSERVER}" >> $LOG_FILE
CMD="/usr/bin/rsync -av --force --progress --stats ${LDAD_DATA}/*.png ${NWSHQ_RSYNCSERVER}::ghls_images/${site}"
echo "/usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} ${CMD}" >> $LOG_FILE
/usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} ${CMD}

echo "Copying KML.TXT files to NWSHQ Web farm ${NWSHQ_RSYNCSERVER}" >> $LOG_FILE
CMD="/usr/bin/rsync -av --force --progress --stats ${LDAD_DATA}/*.txt ${NWSHQ_RSYNCSERVER}::ghls_includes/${siteid}"
echo "/usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} ${CMD}" >> $LOG_FILE
/usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} "${CMD}"
echo "Copy to ${NWSHQ_RSYNCSERVER} complete" >> $LOG_FILE

# NOTE: Local processing is site specific and not required. 
# NOTE: If you site is doing any local processing you must set local processing to TRUE and make sure
# NOTE: all you local lw processing variables are setup in your /awips2/GFESuite/hti/etc/sitevars.ccc file
# NOTE: If you want local lw processing on for all scripts add the following line to the 
# NOTE: /awips2/GFESuite/hti/etc/sitevars.ccc (where ccc is your siteid).
# NOTE: LOCALlw_PROCESSING="TRUE"
if [ "${LOCALlw_PROCESSING}" == "" ]; then LOCALlw_PROCESSING="FALSE"; fi

if [ "${LOCALlw_PROCESSING}" == "TRUE" ] 
    then
    # NOTE: This can be ran as any AWIPS user so we must SSH as ldad@ls1 and run the commands via and SSH tunnel
    echo "Copying image and text files to ${LOCALlwserver} Linux system for local WEB processing and archiving" >> $LOG_FILE
    CMD="/usr/bin/rsync -av --force --progress --stats -e ssh ${LDAD_DATA}/*.txt ${LDAD_DATA}/*.png ${LOCALlwuser}@${LOCALlwserver}:${LOCAL_LWDATA}/."
    echo "/usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} ${CMD}" >> $LOG_FILE
    /usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} "${CMD}"
    echo "Done copying image and text files to ${LOCALlwserver}" >> $LOG_FILE
fi
 
# Archive *Threat.png and *kml.txt files if desired
if [ "${ARCHIVE}" == "YES" ] 
then
    echo "Begin archiving process..." >> $LOG_FILE
    echo "Making sure ${PRODUCTdir}/archive exists at $(date)" >> $LOG_FILE
    if [ ! -d ${PRODUCTdir}/archive ]; then 
       echo "  ${PRODUCTdir}/archive directory not found!" >> $LOG_FILE
       echo "  Creating archive directory..." >> $LOG_FILE
       mkdir -p ${PRODUCTdir}/archive 
       echo "  Changing permissions on ${PRODUCTdir}/archive..." >> $LOG_FILE
       chmod 777 ${PRODUCTdir}/archive
       echo "  Changing ownership on ${PRODUCTdir}/archive..." >> $LOG_FILE
       chown awips:fxalpha ${PRODUCTdir}/archive
    else
       echo "  ${PRODUCTdir}/archive directory exists!" >> $LOG_FILE
    fi
    DATESTAMP=`date +%Y%m%d_%H%M`
    if [ ! -d ${PRODUCTdir}/archive/${DATESTAMP} ]; then mkdir -p ${PRODUCTdir}/archive/${DATESTAMP} ; fi

    ARCHFILES=`cd ${PRODUCTdir}; ls *Threat.png *kml.txt` 
    echo "Copying files to ${PRODUCTdir}/archive/${DATESTAMP} " >> $LOG_FILE
    for ARCHFILE in $ARCHFILES
    do
       cp ${PRODUCTdir}/${ARCHFILE} ${PRODUCTdir}/archive/${DATESTAMP}/
    done
    echo "Archiving complete!" >> $LOG_FILE
else
    echo "**** NO HTI PRODUCTS ARCHIVED!  If you wish to archive HTI products, check " >> $LOG_FILE
    echo "**** ${HTI_HOME}/etc/sitevars.${site}" >> $LOG_FILE
fi

echo "Script complete at $(date)"  >> ${LOGfile}
echo " "  >> ${LOGfile}
exit 0
########################################################################
