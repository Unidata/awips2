#This file sends an apache request daily to let us know if you are running EDEX

source /awips2/edex/conf/edexServiceList

version=`rpm -q awips2-version --qf %{VERSION}-%{RELEASE}`
ip=`curl ifconfig.me`
url="https://downloads.unidata.ucar.edu/awips2/edex_users"
date=$(date +%Y%m%d-%H:%M:%S)

if [ $# -gt 0 ]; then
   SERVICES=("$@")
fi
for service in ${SERVICES[*]}; do
   edex_ps=`ps aux | grep "edex.run.mode=$service "| grep -v grep | awk '{ print $2 }'`
   if [ ! -z $edex_ps ]; then
         output="EDEX $service version $version is running on $ip"
         curl -A "$output" $url
         echo "This server pinged Unidata that EDEX is running on ${date}"
   fi
done

