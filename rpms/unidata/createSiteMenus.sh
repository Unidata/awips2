#!/bin/bash


#cd /home/mjames/awips2-builds/localization/localization/utility/edex_static/site
#cd /home/mjames/awips2-builds/localization/localization/utility/common_static/site
cd /home/mjames/awips2-builds/localization/localization/utility/cave_static/site


# need
#site = OAX, OKC, etc.
file=wfo.dat
for site in $(cat /home/mjames/awips2-builds/localization/localization/utility/cave_static/site/$file |cut -c -3)
do
   #cat $file |grep $site
   lat=$(cat $file |grep $site | cut -f9)
   lon=$(cat $file |grep $site | cut -f10)
   echo $site, $lat, $lon

   rm -rf $site
   cp -R XXX $site
   grep -rl 'XXX' $site | xargs sed -i 's/XXX/'$site'/g'
   grep -rl 'LATITUDE' $site | xargs sed -i 's/LATITUDE/'$lat'/g'
   grep -rl 'LONGITUDE' $site | xargs sed -i 's/LONGITUDE/'$lon'/g'

done

tar -cf site.tar --exclude='XXX' --exclude='createSiteMenus.sh' --exclude='wfo.dat' .
scp site.tar awips@edex:
