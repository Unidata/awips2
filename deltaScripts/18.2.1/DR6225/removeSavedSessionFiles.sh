#! /bin/bash
#
# This script removes all text editor saved session files older than 1 day
#
find /awips2/edex/data/utility/cave_static/user/*/com.raytheon.viz.texteditor/savedSession/ -type f -ctime +1 -delete
