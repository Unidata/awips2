#!/bin/sh
# the next line restarts using wish \
exec wish "$0"
#################################################################################
#
#       wait.tcl
#
#       Purpose
#         This script notifies the user with a tcl button that climate is
#         running.  When the processes have completed, the button will disappear
#         automatically. 
# 
#       BER	January 2000
#
#################################################################################

proc blink {w option value1 value2 interval} {
	$w config $option $value1
	after $interval [list blink $w $option\
		$value2 $value1 $interval]
	}
wm title . "Running..."
      wm geometry . +1080+400
      wm overrideredirect . 1
      button .clim -bd 5\
        -image [image create photo -file $env(CLIMATE_DIR)/data/hour_clim.gif]\
        -command {
           destroy .
           }
        blink .clim -bg brown black 500
      pack .clim 
        
