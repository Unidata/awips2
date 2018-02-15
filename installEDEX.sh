#!/bin/bash -f
#
# installEDEX
# author: Michael James <mjames@ucar.edu>
#
# run as "sudo ./installEDEX.sh"

# Download yum repo file from Unidata
if [ ! -f /etc/yum.repos.d/awips2.repo ]; then
  echo
  echo 'Downloading awips2repo yum file to /etc/yum.repos.d/awips2.repo'
  echo
  if [[ $(grep "release 7" /etc/redhat-release) ]]; then
    wget -O /etc/yum.repos.d/awips2.repo http://www.unidata.ucar.edu/software/awips2/doc/el7.repo
  else
    wget -O /etc/yum.repos.d/awips2.repo http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo
  fi
fi

# Check for and add to limits.conf
if [[ $(grep awips /etc/security/limits.conf) ]]; then
  echo "/etc/security/limits.conf OK"
else
  echo "adding awips user entries to /etc/security/limits.conf ..."
  printf "awips soft nproc 65536\nawips soft nofile 65536\n" >> /etc/security/limits.conf
  echo "done with /etc/security/limits.conf"
fi

# Clean yum cache
echo
echo "Running 'yum clean all'"
echo
yum clean all

if [[ $(rpm -qa | grep awips2-edex) ]]; then
  echo "found EDEX RPMs installed. Updating..."
else
  echo "  EDEX RPMs not installled"
  echo
  echo "  cleaning up /awips2/data/"
  rm -rf /awips2/data/
fi

# stop AWIPS services
service edex_camel stop
service qpidd stop
service httpd-pypies stop
service edex_postgres stop
service edex_ldm stop
service qpidd stop

# check that /awips2/data_store exists, if not, create
for dir in /awips2/tmp /awips2/data_store ; do
  if [ ! -d $dir ]; then
    mkdir -p $dir
    chown awips:fxalpha $dir
  fi
done

if [[ $1 -eq "reinstall" ]]; then
  echo
  echo 'Removing and reinstalling EDEX...'
  echo
  echo "Running 'yum groupremove awips2-server'"
  echo
  yum groupremove awips2-server -y 2>&1 /dev/null
  rm -rf /awips2/data/ /awips2/database/ 
fi

echo
echo "Running 'yum groupinstall awips2-server'"
echo
yum groupinstall awips2-server -y 2>&1 | tee -a /tmp/edex-install.log

if getent passwd awips &>/dev/null; then
  echo -n ''
else
  echo
  echo "--- user awips does not exist"
  echo "--- you should set owner/group permissions for directories in /awips2/"
fi
echo
echo "Done..."
echo

PATH=$PATH:/awips2/edex/bin/
exit
