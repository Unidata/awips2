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
echo "  to run EDEX:"
echo ""
echo "  edex start"
echo '  su ldm -c "ldmadmin mkqueue" && su ldm -c "ldmadmin start"'
exit
