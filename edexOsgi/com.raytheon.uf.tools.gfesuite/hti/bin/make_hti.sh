#!/bin/bash -l
# UNIX Shell Script
# Tested Operating System(s): RHEL 3, 4, 5, 6
# Tested Run Level(s): 3, 5
# Shell Used: BASH shell
# Original Author(s): Joseph.Maloney@noaa.gov
# File Creation Date: 01/27/2009
# Date Last Modified: 09/23/2016 - For 17.1.1
#
# Contributors:
# Joe Maloney (MFL), Pablo Santos (MFL)
# ----------------------------------------------------------- 
# ------------- Program Description and Details ------------- 
# ----------------------------------------------------------- 
#
# This script uses ifpImage to create PNG graphics of tropical threat
# grids, then runProcedure to create KML of the same grids.
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
# History:
# 20 OCT 2014 - jcm - created from make_ghls.sh, broke out of webapps
#                     package.  Renamed make_hti.sh.
# 20 FEB 2015 - jcm - modified ifpIMAGE line to use ssh -x px2f; fixed
#                     LOG_FILE at end of script; corrected $site variable
#                     for kml rsync.
# 10 JUN 2016 - jcm - brought back ghls_active.txt because apparently
#                     NIDS is using this file in their KML mosiacking
#                     code instead of just looking at the timestamps
#                     of the KML files themselves.
# 22 SEP 2016 - jcm - clean out $PRODUCTdir every time you run
# 29 JAN 2019 - mj@ucar  - Clean up: no ssh, rsync
#
########################################################################
SITE=$(grep AW_SITE_IDENTIFIER /awips2/edex/setup.env | head -1 | cut -d = -f 2 )
site=$(echo $SITE | tr '[:upper:]' '[:lower:]' )

GFESUITE_HOME="/awips2/GFESuite"
HTI_HOME="${GFESUITE_HOME}/hti"
AWIPS_USER="awips"
AWIPS_GRP="fxalpha"
. ${HTI_HOME}/etc/sitevars.ccc

# set current data and log file name
currdate=$(date -u +%Y%m%d)
export LOG_FILE="${HTI_HOME}/logs/${currdate}/make_hti.log"

# check to see if log directory structure exists.
if [ ! -d  ${HTI_HOME}/logs ] ;then
   mkdir -p ${HTI_HOME}/logs
   chmod 777 ${HTI_HOME}/logs
   chown ${AWIPS_USER}:${AWIPS_GRP} ${HTI_HOME}/logs
fi
if [ ! -d  ${HTI_HOME}/logs/${currdate} ] ;then
   mkdir -p ${HTI_HOME}/logs/${currdate}
   chmod 777 ${HTI_HOME}/logs/${currdate}
   chown ${AWIPS_USER}:${AWIPS_GRP} ${HTI_HOME}/logs/${currdate}
fi

# Log file header
echo " " >> $LOG_FILE
echo "####################################################################################" >> $LOG_FILE
echo "# Starting Make_HTI Script....                                                     #" >> $LOG_FILE
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
   chown ${AWIPS_USER}:${AWIPS_GRP} $PRODUCTdir
else
   echo "  ${PRODUCTdir} exists." >> $LOG_FILE
   # clean old png and kml.txt files from $PRODUCTdir
   echo "Removing old png and kml.txt files from ${PRODUCTdir}." >> $LOG_FILE
   rm -f ${PRODUCTdir}/*png ${PRODUCTdir}/*kml.txt
fi
echo "...finished." >> $LOG_FILE
echo " " >> $LOG_FILE

# THESE SHOULD MATCH THREAT GRIDS IN GFE
PARMS="WindThreat FloodingRainThreat StormSurgeThreat TornadoThreat"
    
echo "Starting ifpIMAGE loop." >> $LOG_FILE
for PARM in $PARMS
do
     echo "Creating ${PARM} image..." >> $LOG_FILE
     ${GFEBINdir}/ifpIMAGE -site ${SITE} -c ${PARM} -o ${PRODUCTdir}
     convert ${PRODUCTdir}/${SITE}${PARM}.png -resize 104x148 ${PRODUCTdir}/${SITE}${PARM}_sm.png
done
    
rm -f ${PRODUCTdir}/*.info
    
# Generate KML automatically via runProcedure
echo "Running KML procedure." >> $LOG_FILE
${GFEBINdir}/runProcedure -site ${SITE} -n TCImpactGraphics_KML -c gfeConfig

# Create legends for KML
${HTI_HOME}/bin/kml_legend.sh

########################################################################
# 2016-06-10  Because it is too challenging for NIDS / IDG to modify 
# their script to stop using a file that was discontinued after the 2014 
# season, and instead check the age of the kml files directly, and NCO 
# will not allow them to divert ONE person to do this the right way,  
# every coastal WFO will now have to send up the old ghls_active file  
# (even though the ghls is LONG DEAD now)
########################################################################

# get storm number from VTEC in TCV
stormnum=`grep "/O..........................T....Z-......T....Z/" /awips2/edex/data/fxa/trigger/*TCV${SITE} | head -1|cut -c 20-21`
# get storm name from header in TCV
stormname=`grep LOCAL /awips2/edex/data/fxa/trigger/*TCV${SITE} | head -1 | sed -e "s/ LOCAL .*//"`
# get two-digit year
stormyr=`date +%y`

## TEST
#echo "STORM NAME IS:  $stormname"
#echo "STORM NUMBER AND YEAR:  ${stormnum}${stormyr}"
## TEST

# Trigger the Web side PHP script to display the ACTIVE GHLS logo
date +%s > ${PRODUCTdir}/ghls_active.txt
echo ${stormnum} >> ${PRODUCTdir}/ghls_active.txt
echo ${stormname} >> ${PRODUCTdir}/ghls_active.txt
echo ${stormyr} >> ${PRODUCTdir}/ghls_active.txt

########################################################################
# need to rename two kml's for mosaic code to work
cp ${PRODUCTdir}/StormSurgeThreat.kml.txt ${PRODUCTdir}/CoastalThreat.kml.txt
cp ${PRODUCTdir}/FloodingRainThreat.kml.txt ${PRODUCTdir}/InlandThreat.kml.txt
########################################################################

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
       chown ${AWIPS_USER}:${AWIPS_GRP} ${PRODUCTdir}/archive
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

echo "Script complete at $(date)"  >> ${LOG_FILE}
echo " "  >> ${LOG_FILE}
exit 0
########################################################################
