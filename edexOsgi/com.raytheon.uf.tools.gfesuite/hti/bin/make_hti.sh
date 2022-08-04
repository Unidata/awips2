#!/bin/bash -l
# UNIX Shell Script
# Tested Operating System(s): RHEL 3, 4, 5, 6, 7
# Tested Run Level(s): 3, 5
# Shell Used: BASH shell
# Original Author(s): Joseph.Maloney@noaa.gov
# File Creation Date: 01/27/2009
#
# Contributors:
# Joe Maloney (MRX), Pablo Santos (MFL), Jonathan Lamb (CHS)
# ----------------------------------------------------------- 
# ------------- Program Description and Details ------------- 
# ----------------------------------------------------------- 
#
# This script creates the KML legend, runs the TCImpactGraphics_KML procedure to
# create KML files for the HTI grids, and sends everything to the webfarm.
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
# 20 OCT 2014 - jcm      created from make_ghls.sh, broke out of webapps
#                        package.  Renamed make_hti.sh.
# 20 FEB 2015 - jcm      modified ifpIMAGE line to use ssh -x px2f; fixed
#                        LOG_FILE at end of script; corrected $site variable
#                        for kml rsync.
# 10 JUN 2016 - jcm      brought back ghls_active.txt because apparently
#                        NIDS is using this file in their KML mosiacking
#                        code instead of just looking at the timestamps
#                        of the KML files themselves.
# 22 SEP 2016 - jcm      clean out $PRODUCTdir every time you run
# 23 AUG 2018 - jcm      changed ghls_active.txt to always have stormname
#                        DUMMY and stormnumber 99
# 29 SEP 2020 - jcm      added error checking after KML procedure.  If a
#                        KML file is missing or only 0 bytes, a GFE banner
#                        will alert the forecasters and nothing will be 
#                        sync'd to the web.
# 4 NOV 2020 - ps/jcm    reversed order running kml check first for faster
#                        feedback.
# 7 JUL 2022  J. Lamb    Override sitevars to ensure PRODUCTdir is set correctly
#                        Merged kml_legend.sh content into make_hti.sh
#                        Added check to automatically determine PX/PV server name
#                        Disabled generation of legacy ifpImage files
#
########################################################################
#  CHECK TO SEE IF SITE ID WAS PASSED AS ARGUMENT
#  IF NOT THEN EXIT FROM THE SCRIPT
########################################################################
if [ $# -lt 1 ]
then
   echo Invalid number of arguments.
   echo Script stopped.
   echo ./make_hti.sh wfo
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

if [ ! -f ${HTI_HOME}/etc/sitevars.${site} ]
then
   cp ${HTI_HOME}/etc/sitevars.ccc ${HTI_HOME}/etc/sitevars.${site}
fi

# SITES CAN CUSTOMIZE THE SITEVARS AS NEEDED
. ${HTI_HOME}/etc/sitevars.${site}

########################################################################
#  BEGIN MAIN SCRIPT
########################################################################

# Force PRODUCTdir proper path, regardless of sitevars content
PRODUCTdir="${HTI_HOME}/data/${site}"

# Determine server name
if [[ $(grep -c pv <<< "$PX_SERVERS") -gt 0 ]]
then
  serverId="pv2"
else
  serverId="px2f"
fi

# set current data and log file name
currdate=$(date -u +%Y%m%d)
export LOG_FILE="${HTI_HOME}/logs/${currdate}/make_hti.log"

# check to see if log directory structure exists.
if [ ! -d  ${HTI_HOME}/logs ]
then
   mkdir -p ${HTI_HOME}/logs
   chmod 777 ${HTI_HOME}/logs
   chown awips:fxalpha ${HTI_HOME}/logs
fi
if [ ! -d  ${HTI_HOME}/logs/${currdate} ]
then
   mkdir -p ${HTI_HOME}/logs/${currdate}
   chmod 777 ${HTI_HOME}/logs/${currdate}
   chown awips:fxalpha ${HTI_HOME}/logs/${currdate}
fi

# Log file header
echo " " >> $LOG_FILE
echo "####################################################################################" >> $LOG_FILE
echo "# Starting Make_HTI Script at $(date)...." >> $LOG_FILE
echo "####################################################################################" >> $LOG_FILE
chmod 666 $LOG_FILE

# Check to see if ${PRODUCTdir} exists.  If not, create.
echo "Making sure that ${PRODUCTdir} exists." >> $LOG_FILE
if [ ! -d ${PRODUCTdir} ]
then
   echo " **** ${PRODUCTdir} directory not found." >> $LOG_FILE
   echo " **** Creating ${PRODUCTdir} directory..." >> $LOG_FILE
   mkdir -p $PRODUCTdir
   echo " **** Changing permissions of ${PRODUCTdir} directory..." >> $LOG_FILE
   chmod 777 $PRODUCTdir
   echo " **** Changing ownership of ${PRODUCTdir} directory..." >> $LOG_FILE
   chown awips:fxalpha $PRODUCTdir
else
   echo "  ${PRODUCTdir} exists." >> $LOG_FILE
   # clean old png and kml.txt files from $PRODUCTdir and on LDAD
   echo "Removing old png and kml.txt files from ${PRODUCTdir}." >> $LOG_FILE
   rm -f ${PRODUCTdir}/*png ${PRODUCTdir}/*kml.txt
   echo "Removing old png and kml.txt files from LDAD ${LDAD_DATA}." >> $LOG_FILE
   /usr/bin/ssh -o stricthostkeychecking=no -x -q ${LDADuser}@${LDADserver} "rm -f ${LDAD_DATA}/*png ${LDAD_DATA}/*kml.txt"
fi
echo "...finished." >> $LOG_FILE
echo " " >> $LOG_FILE

# THESE SHOULD MATCH THREAT GRIDS IN GFE
PARMS="StormSurgeThreat WindThreat FloodingRainThreat TornadoThreat"
    
# Generate KML automatically via runProcedure
echo "Running KML procedure." >> $LOG_FILE
ssh -x "${serverId}" "unset DISPLAY; ${GFEBINdir}/runProcedure -site ${SITE} -n TCImpactGraphics_KML -c gfeConfig"

# 2020-09-29 VERIFY KMLs were actually generated before proceeding
for PARM in $PARMS
do
   if [ "${PARM}" == "StormSurgeThreat" ]
   then
      # Only verify StormSurgeThreat if StormSurgeWW_EditArea_Local edit area 
      # exists in common_static/site/$SITE/gfe/editAreas
      [ ! -e /awips2/edex/data/utility/common_static/site/${SITE}/gfe/editAreas/StormSurgeWW_EditArea_Local.xml ] && continue
   fi

   echo "Checking $PARM.kml.txt" >> $LOG_FILE

   # Either file exists and has non-zero size...
   [ -s ${PRODUCTdir}/${PARM}.kml.txt ] && continue

   # Or we end up here...send a banner and quit
   echo "Error detected in generation of $PARM kml file." >> $LOG_FILE
   /awips2/GFESuite/bin/sendGfeMessage -u -m "Error detected in generation of $PARM kml file. Ensure HTI grids are saved/published and re-run the HTI script."
   exit

done


##########################################################################
# BEGIN kml_legend content
##########################################################################

echo "Generating KML legends..." >> $LOG_FILE

# Create legends for KML
#${HTI_HOME}/bin/kml_legend.sh ${site}

cd ${PRODUCTdir}

for element in wind surge flood tornado
do

# create canvases
if [ $element = "surge" ]
then
convert -size 500x500 xc:black temp.png
else # tornado, wind, flood
convert -size 400x500 xc:black temp.png
fi

# make the image transparent
convert temp.png null: -matte -compose Clear -composite -compose Over transparent.png

# insert the logos at the bottom
composite -gravity south -geometry +0+0 ${HTI_HOME}/bin/logos.png transparent.png trans2.png

# write the date onto the image
DATE=$(date +"Issued %F %H%MZ")
convert trans2.png -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +0+400 "${DATE}" trans2a.png

if [ $element = "wind" ]
then

text1="Wind < 39 mph"
text2="Potential for Wind 39-57 mph"
text3="Potential for Wind 58-73 mph"
text4="Potential for Wind 74-110 mph"
text5="Potential for Wind > 110 mph"

title="Wind Threat"
outputFn="windthreatlegend.png"

elif [ $element = "surge" ]
then

text1="Little to no storm surge flooding"
text2="Potential for storm surge flooding > 1 ft above ground"
text3="Potential for storm surge flooding > 3 ft above ground"
text4="Potential for storm surge flooding > 6 ft above ground"
text5="Potential for storm surge flooding > 9 ft above ground"

title="Storm Surge Threat"
outputFn="stormsurgethreatlegend.png"

elif [ $element = "flood" ]
then

text1="Little to no potential for flooding rain"
text2="Potential for localized flooding rain"
text3="Potential for moderate flooding rain"
text4="Potential for major flooding rain"
text5="Potential for extreme flooding rain"

title="Flooding Rain Threat"
outputFn="floodingrainthreatlegend.png"

# Tornado
else

text1="Tornadoes not expected"
text2="Potential for a few tornadoes"
text3="Potential for several tornadoes"
text4="Potential for many tornadoes"
text5="Potential for outbreak of tornadoes"

title="Tornado Threat"
outputFn="tornadothreatlegend.png"

fi

convert trans2a.png -fill black -draw 'rectangle 5,340 25,360' \
                    -fill "#E5E5E5" -draw 'rectangle 6,341 24,359' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+355 "${text1}" \
                    -fill black -draw 'rectangle 5,320 25,340' \
                    -fill "#FFFF00" -draw 'rectangle 6,321 24,339' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+335 "${text2}" \
                    -fill black -draw 'rectangle 5,300 25,320' \
                    -fill "#FFA70F" -draw 'rectangle 6,301 24,319' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+315 "${text3}" \
                    -fill black -draw 'rectangle 5,280 25,300' \
                    -fill "#FF0000" -draw 'rectangle 6,281 24,299' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+295 "${text4}" \
                    -fill black -draw 'rectangle 5,260 25,280' \
                    -fill "#CC00CC" -draw 'rectangle 6,261 24,279' \
                    -font Century-Schoolbook-Bold -pointsize 16 -fill black -annotate +30+275 "${text5}" \
                    trans2b.png

convert trans2b.png -font Century-Schoolbook-Bold -pointsize 20 -fill black -annotate +5+250 "${title}" $outputFn

# clean up scraps
rm temp.png transparent.png trans2.png trans2a.png trans2b.png

done

chmod 666 *legend.png
#mv *legend.png ${PRODUCTdir}/

##########################################################################
# END kml_legend content
##########################################################################

# Disabled ifpImage generation 15 June 2022
#echo "Starting ifpIMAGE loop." >> $LOG_FILE
#for PARM in $PARMS
#do
#     # NOTE: cannot run ifpIMAGE on dv3/dv4 - must ssh to a PX/PV
#     echo "Creating ${PARM} image..." >> $LOG_FILE
#     ssh -x "${serverId}" "unset DISPLAY; ${GFEBINdir}/ifpIMAGE -site ${SITE} -c ${PARM} -o ${PRODUCTdir}"
#     convert ${PRODUCTdir}/${SITE}${PARM}.png -resize 104x148 ${PRODUCTdir}/${SITE}${PARM}_sm.png
#done
    
#rm -f ${PRODUCTdir}/*.info

########################################################################
# 2016-06-10  Because it is too challenging for NIDS / IDG to modify 
# their script to stop using a file that was discontinued after the 2014 
# season, and instead check the age of the kml files directly, and NCO 
# will not allow them to divert ONE person to do this the right way,  
# every coastal WFO will now have to send up the old ghls_active file  
# (even though the ghls is LONG DEAD now)
# 2018-08-23  Update, fetching the storm name and number from the TCV
# is no longer needed.  We will hardcode dummy values into the file now.
########################################################################

# get storm number from VTEC in TCV
##stormnum=`grep "/O..........................T....Z-......T....Z/" /awips2/edex/data/fxa/trigger/*TCV${SITE} | head -1|cut -c 20-21`
# get storm name from header in TCV
##stormname=`grep LOCAL /awips2/edex/data/fxa/trigger/*TCV${SITE} | head -1 | sed -e "s/ LOCAL .*//"`
# get two-digit year
stormyr=$(date +%y)

## TEST
#echo "STORM NAME IS:  $stormname"
#echo "STORM NUMBER AND YEAR:  ${stormnum}${stormyr}"
## TEST

# Trigger the Web side PHP script to display the ACTIVE GHLS logo
date +%s > ${PRODUCTdir}/ghls_active.txt
#echo ${stormnum} >> ${PRODUCTdir}/ghls_active.txt
#echo ${stormname} >> ${PRODUCTdir}/ghls_active.txt
echo 99 >> ${PRODUCTdir}/ghls_active.txt
echo DUMMY >> ${PRODUCTdir}/ghls_active.txt
echo ${stormyr} >> ${PRODUCTdir}/ghls_active.txt

# Set all permission in product dir content to 666 per NIDS request
chmod 666 ${PRODUCTdir}/*

########################################################################
# need to rename two kml's for mosaic code to work
cp ${PRODUCTdir}/StormSurgeThreat.kml.txt ${PRODUCTdir}/CoastalThreat.kml.txt
cp ${PRODUCTdir}/FloodingRainThreat.kml.txt ${PRODUCTdir}/InlandThreat.kml.txt
########################################################################

echo "Copying image and kml files to LDAD for WEB processing" >> $LOG_FILE
/usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} mkdir -p ${LDAD_DATA} &> /dev/null
/usr/bin/rsync -av --force --progress --stats -e "/usr/bin/ssh -o stricthostkeychecking=no -x" ${PRODUCTdir}/*.txt ${PRODUCTdir}/*.png ${LDADuser}@${LDADserver}:/${LDAD_DATA}/.
echo "Done copying image and text files to LDAD" >> $LOG_FILE

# NOTE: The lines below are for SR and ER sites to upload images to the National Website
# NOTE: This can be run as any AWIPS user so we must SSH as ldad@ls1 and run the commands via an SSH tunnel
echo "Copying graphics to NWSHQ Web farm ${NWSHQ_RSYNCSERVER}" >> $LOG_FILE
CMD="/usr/bin/rsync -av --force --progress --stats ${LDAD_DATA}/*.png ${NWSHQ_RSYNCSERVER}::ghls_images/${site}"
echo "/usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} ${CMD}" >> $LOG_FILE
/usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} ${CMD}

echo "Copying KML.TXT files to NWSHQ Web farm ${NWSHQ_RSYNCSERVER}" >> $LOG_FILE
CMD="/usr/bin/rsync -av --force --progress --stats ${LDAD_DATA}/*.txt ${NWSHQ_RSYNCSERVER}::ghls_includes/${site}"
echo "/usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} ${CMD}" >> $LOG_FILE
/usr/bin/ssh -o stricthostkeychecking=no -x ${LDADuser}@${LDADserver} "${CMD}"
echo "Copy to ${NWSHQ_RSYNCSERVER} complete" >> $LOG_FILE

# NOTE: Local processing is site specific and not required. 
# NOTE: If you site is doing any local processing you must set local processing to TRUE and make sure
# NOTE: all you local lw processing variables are setup in your /awips2/GFESuite/hti/etc/sitevars.ccc file
# NOTE: If you want local lw processing on for all scripts add the following line to the 
# NOTE: /awips2/GFESuite/hti/etc/sitevars.ccc (where ccc is your siteid).
# NOTE: LOCALlw_PROCESSING="TRUE"
if [ "${LOCALlw_PROCESSING}" == "" ]
then
  LOCALlw_PROCESSING="FALSE"
fi

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
    if [ ! -d ${PRODUCTdir}/archive ]
    then 
       echo "  ${PRODUCTdir}/archive directory not found!" >> $LOG_FILE
       echo "  Creating archive directory..." >> $LOG_FILE
       mkdir -p ${PRODUCTdir}/archive 
       echo "  Changing permissions on ${PRODUCTdir}/archive..." >> $LOG_FILE
       chmod 770 ${PRODUCTdir}/archive
       echo "  Changing ownership on ${PRODUCTdir}/archive..." >> $LOG_FILE
       chown awips:fxalpha ${PRODUCTdir}/archive
    else
       chmod 770 ${PRODUCTdir}/archive 
       echo "  ${PRODUCTdir}/archive directory exists!" >> $LOG_FILE
    fi
    DATESTAMP=$(date +"%Y%m%d_%H%M")
    if [ ! -d ${PRODUCTdir}/archive/${DATESTAMP} ]
    then
      mkdir -p ${PRODUCTdir}/archive/${DATESTAMP}
    fi

    ARCHFILES=$(cd ${PRODUCTdir}; ls *Threat.png *kml.txt)
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