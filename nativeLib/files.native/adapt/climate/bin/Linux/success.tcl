#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"
#################################################################################
#
#       success.tcl
#
#       Purpose
#         This script notifies the user with a tcl button that climate has
# 	  successfully Finished
# 
#       BER	February 2000
#
#################################################################################

wm title . "Climate is Done"
      wm geometry . +1030+400
      wm overrideredirect . 1
       button .done -bd 4 -bg black -highlightbackground black \
        -activebackground black\
        -image [image create photo -file $env(CLIMATE_DIR)/data/climate_map.gif]\
         -command {
           destroy .
           }
      pack .done 

        
