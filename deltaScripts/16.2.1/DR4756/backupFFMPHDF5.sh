#!/bin/sh
# This script must be run on the dx box which contains the HDF5 directory and pypies mount
# DR #4756 - This script switches the HDF5 dir to use the pathKey determined by the ffmpPathKeys.xml file
#            and the FFMPSourceConfig.xml.
FFMP_HDF5_DIR="/awips2/edex/data/hdf5/ffmp"

# sources listed in current FFMPSourceConfig.xml, new pathKeys more or less.
source_array=(DHR DPR DHRMOSAIC BDHRMOSAIC QPFSCAN PRTM BPRTM FFG0124hr FFG0324hr FFG0124hr MRMS MRMSQPF MRMSVGB VGBHPE VGBBIASHPE VGBDHR VGBDPR)

cd $FFMP_HDF5_DIR
files=$(ls -trh *.h5)
count=0;
# add up the files
for file in ${files}
do
  count=$((count+1))
done

echo "Found ${count} files."
# decrement by 1 for processing
count=$((count-1))

for source_entry in ${source_array[@]}
do
  echo "Creating directory for source: ${source_entry}"
  mkdir ${source_entry}
  i=0;
  for file_entry in ${files}
    do
      echo ${file_entry}
      if [ ${i} -lt ${count} ]
      then
         echo "Creating linked file: ./${source_entry}/${file_entry}"
         ln ${file_entry} ./${source_entry}/${file_entry}
      else
         echo "Copying current file: ./${source_entry}/${file_entry}"
         cp ${file_entry} ./${source_entry}/${file_entry}
      fi
      i=$((i+1))
    done
done

