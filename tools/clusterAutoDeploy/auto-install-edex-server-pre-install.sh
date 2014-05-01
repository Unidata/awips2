#!/bin/sh

DATE=`date`

echo "**************************************************************************************"
echo "Auto Int Server Pre Install Processing - $DATE"
echo "**************************************************************************************"

echo "Getting variables from env.txt"
. env.txt

echo "Making hdf5 directory"
mkdir -p $EDEXINSTALLFOLDER/edex/data/hdf5
chown -R ${EDEXUSER}:${EDEXUSER} $EDEXINSTALLFOLDER

echo "Mounting to hdf5 on ${NASSERVER}"
mount ${NASSERVER}:/hdf5 $EDEXINSTALLFOLDER/edex/data/hdf5 -o nfsvers=3

echo "mount -l (verify mount of hdf5 directory worked)"
mount
echo ""

DATE=`date`

echo "**************************************************************************************"
echo "EDEX Int Server Pre Install Processing Completed At $DATE"
echo "**************************************************************************************"
echo ""

exit

