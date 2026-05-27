# Set PGHOST appropriately:
# - If we're already on "dv1", set it to dv1.
# - If we're on any other host with postgres running, set it 
#   to "localhost".
# - If we're on any other host without postgres running, set it
#   to "dv1".

export PGHOST='localhost'

host=$(hostname)

#extract first part of hostname of the form dv1-xxx.xxx
IFS="-"
hostTemp=''
for part in $host
do
    hostTemp=$part
    break
done
unset IFS

# if we're on dv1, just set "PGHOST" to dv1
if [ $hostTemp == 'dv1' ] ; then
    export PGHOST=$hostTemp
fi

#check if postgres is running on this host. Should be many 
#processes with "postgres" in the name.
numPostgres=$(ps -ef | grep postgres | wc -l)

#choosing 3 for safety, since the "grep" is atleast 1
if [ $numPostgres -lt 3 ] ; then
   export PGHOST='dv1'
fi
