#!/bin/sh
#----------------------------------------------------------------------
# Auto umount script for the EDEX server. 
#----------------------------------------------------------------------

DATE=`date`
EDEXINSTALLFOLDER=/awips2
NASSERVER=dx2

echo "**************************************************************************************"
echo "Auto Pre Install Processing - $DATE"
echo "**************************************************************************************"

echo "Making hdf5 directory"
mkdir -p $EDEXINSTALLFOLDER/edex/data/hdf5
chown -R ${EDEXUSER}:${EDEXUSER} $EDEXINSTALLFOLDER
chmod -Rf 777  $EDEXINSTALLFOLDER

echo "unmounting to hdf5 on ${NASSERVER}"

umount ${NASSERVER}:/hdf5 $EDEXINSTALLFOLDER/edex/data

echo "mount -l (verify unmount of directory worked)"
mount
echo ""

DATE=`date`

echo "**************************************************************************************"
echo "EDEX Unmounting for Uninstall Processing Completed At $DATE"
echo "**************************************************************************************"
echo ""

exit
