#!/bin/sh
# the next line restarts using wish \
exec /usr/local/tcltk8.3.2/bin/wish "$0" "$@"

#exec textWish "$0" "$@"

# ---------------------------------------------------------------------------
#
# textwsNotification.tcl
#
# Author: Dan Martin
#
# Modification History:
# ---------------------------------------------------------------------------

# --- module -----------------------------------------------------------------
#
#
# --- implementation ---------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
# Collaborative scripts
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
# Module-global vars
#
# ---------------------------------------------------------------------------

set TITLE "Climactic Record Notification" 

# ---------------------------------------------------------------------------
#
# Global functions (called from outside this file)
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
# Procedure: main
#
# Purpose: create a notification box... this is temporary 
#          should use Announcer
#
# ---------------------------------------------------------------------------

#main

# don't show the parent
wm withdraw .

# display a message box
tk_messageBox -icon "info" -message "$argv" -title "$TITLE" -type "ok"

#destroy parent and return
destroy . 
return 0
