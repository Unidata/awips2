#!/bin/bash -f
#
# installEDEX.sh - a short script to manage the yum repo setup and install
#                  of AWIPS II EDEX
#
# 10/15         mjames@ucar.edu         Creation
#
if [ ! -f /etc/yum.repos.d/awips2.repo ]; then
  echo ''
  echo 'Downloading awips2repo yum file to /etc/yum.repos.d/awips2.repo'
  echo ''
  wget -O /etc/yum.repos.d/awips2.repo http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo
fi
echo ''
echo "Running 'yum clean all'"
echo ''
yum clean all


if [[ $(rpm -qa | grep awips2-edex) ]]; then
  echo "found EDEX RPMs installed"
else
  echo "  EDEX RPMs not installled"
  echo ""
  echo "  cleaning up /awips2/edex/, /awips2/data/"
  rm -rf /awips2/edex/ /awips2/data/
fi

service edex_camel stop
service qpidd stop
service httpd-pypies stop
service edex_postgres stop
service edex_ldm stop

# check that /awips2/data_store exists, if not, create it
if [ ! -d /awips2/data_store ]; then
  mkdir -p /awips2/data_store
fi
chown -R awips:fxalpha /awips2/data_store

echo ''
echo "Running 'yum groupinstall awips2-server'"
echo ''
yum groupinstall awips2-server -y 2>&1 | tee -a /tmp/edex-install.log

if getent passwd awips &>/dev/null; then
  echo -n ''
else
  echo ''
  echo "--- user awips does not exist"
  echo "--- you should set owner/group permissions for directories in /awips2/"
fi
echo ""
echo "Done..."
echo ""
exit
