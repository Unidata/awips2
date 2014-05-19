#!/bin/bash
# DR 2864 - update the location of saved subset files

echo "Updating saved subset file locations"

startDir=/awips2/edex/data/utility/cave_static/user
cd $startDir
users=$(ls -1)

for i in $users
do
   cd $i
   if [ -e dataDelivery/subset ]
   then
       cd dataDelivery/subset
   
       if [ ! -e GRID ]
       then
          mkdir GRID
       fi
       
       if [ ! -e POINT ]
       then
          mkdir POINT
       fi
       
       gridFiles=$(grep providerName *.xml | grep NOMADS | cut -d: -f1)
       pointFiles=$(grep providerName *.xml | grep MADIS | cut -d: -f1)
       for j in $gridFiles
       do
          mv $j* GRID
       done
       for j in $pointFiles
       do
          mv $j* POINT
       done
   fi

   cd $startDir

done

echo "Update complete"
