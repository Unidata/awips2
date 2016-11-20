#!/bin/bash -f
#
# installCAVE-el7.sh 
#
#  Download awips2.repo from the Unidata web server
# 
if [ ! -f /etc/yum.repos.d/awips2.repo ]; then
  echo ''
  echo 'Downloading awips2repo yum file to /etc/yum.repos.d/awips2.repo'
  echo ''
  wget -O /etc/yum.repos.d/awips2.repo http://www.unidata.ucar.edu/software/awips2/doc/el7.repo
fi

echo "Running 'yum clean all'"
yum clean all
echo ''

echo ''
echo "Running 'yum groupinstall awips2-cave'"
echo ''
yum groupinstall awips2-cave -y 2>&1 | tee -a /tmp/cave-install.log

if getent passwd awips &>/dev/null; then
  echo ''
  echo "Setting permissions to user awips:awips"
  /bin/chown -R awips:awips /awips2/cave /awips2/alertviz
else
  echo ''
  echo "--- user awips does not exist"
  echo "--- you should set owner/group permissions for /awips2/cave and /awips2/alertviz:"
  echo "tried to run 'chown -R awips:awips /awips2/cave /awips2/alertviz'"
fi
echo ""
echo "Done."
