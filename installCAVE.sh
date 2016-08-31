#!/bin/bash -f
#
# installCAVE.sh - a short script to manage the yum repo setup and install
#                  of AWIPS II CAVE
#
# 10/15         mjames@ucar.edu         Creation
# 05/16         mjames@ucar.edu         Will now remove and install
#

#
#  Download awips2.repo from the Unidata web server
# 

if [ ! -f /etc/yum.repos.d/awips2.repo ]; then
  echo ''
  echo 'Downloading awips2repo yum file to /etc/yum.repos.d/awips2.repo'
  echo ''
  if [[ $(grep "release 7" /etc/redhat-release) ]]; then
    wget -O /etc/yum.repos.d/awips2.repo http://www.unidata.ucar.edu/software/awips2/doc/el7.repo
  else
    wget -O /etc/yum.repos.d/awips2.repo http://www.unidata.ucar.edu/software/awips2/doc/awips2.repo
  fi
fi

echo "Running 'yum clean all'"
yum clean all
echo ''

#
# If CAVE is not installed them make sure /awips2/cave/
# and /awips2/alertviz/ are removed before installing.
#
if [[ $(rpm -qa | grep awips2-cave) ]]; then
  echo "found CAVE installed. Checking version..."
  caveVersion=$(rpm -qa | grep awips2-cave-1| cut -d"-" -f3 )
  versionMajor=$(rpm -qa | grep awips2-cave-1| cut -d"-" -f3 | cut -c -2)
  if [[ ${versionMajor} -lt 16 ]]; then
    echo "CAVE ${caveVersion} installed. Removing to update..."
    yum groupremove awips2-cave -y
    if [ -d /awips2/cave ]; then
      rm -rf /awips2/cave 
    fi
  else
    echo "CAVE ${caveVersion} installed. Continuing..."
  fi
fi

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
exit
