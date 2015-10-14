#!/bin/bash -f
#
# installCAVE.sh - a short script to manage the yum repo setup and install
#                  of AWIPS II CAVE
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
echo ''
echo "Running 'yum groupinstall awips2-cave'"
echo ''
yum groupinstall awips2-cave -y 2>&1 | tee -a /tmp/cave-install.log

if getent passwd awips &>/dev/null; then
  echo ''
  echo "Setting permissions to user awips:fxalpha"
  /bin/chown -R awips:fxalpha /awips2/cave /awips2/alertviz
else
  echo ''
  echo "--- user awips does not exist"
  echo "--- you should set owner/group permissions for /awips2/cave and /awips2/alertviz:"
  echo "tried to run 'chown -R awips:fxalpha /awips2/cave /awips2/alertviz'"
fi
echo ""
echo "Done..."
echo ""
echo "  to run cave:"
echo ""
echo "  /awips2/cave/cave.sh"
echo ""
exit
