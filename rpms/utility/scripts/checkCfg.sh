#!/bin/bash
#
# Check the workstation configuration.
# Autostart script for Text Workstation application consumes the exit code
# from this script and launches (exit code  0) or does not launch
# (exit code  1) the application.
#
# This is a temporary solution - when all the sites are off XTs this
# script needs to be removed and the autostart simplified.
#
# Exit Code:
#
#  0 - if the workstation is an XT
#    - if the workstation is a new LX without a coupled XT
#
#  1 - if the workstation is an old LX
#    - if the workstation is a new LX with a coupled XT
#    - if the workstation is neither an LX nor a XT
#
if [ $(hostname | cut -c 1-2) = "xt" ]; then
   exit 0
fi
if [ $(hostname | cut -c 1-2) = "lx" ]; then
   lxtype=$(dmesg | grep "^DMI:" | cut -d " " -f "4")
   ping -c 1 xt`hostname | cut -c 3-3` > /dev/null 2>&1 && hasxt=0 || hasxt=2
   if [ $lxtype = "Z600" ]; then
      exit 1
   elif [ $lxtype = "Z620" ] || [ $lxtype = "Z640" ]; then
      if [ $hasxt == 0 ]; then
         exit 1
      else
         exit 0
      fi
   fi
else
   exit 1
fi
