#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"
#################################################################################
#
#       error.tcl
#
#       Purpose
#         This script notifies the user with a tcl button that climate has 
#	  ended with an error.
# 
#       BER	January 2000
#
#################################################################################
  proc read_file {filename} {
  set data ""

  if { [file readable $filename] } {
  
     set fileid [open $filename "r"]
     set data [read $fileid]
     close $fileid
  }
 
  return $data
}  
###########################################
proc read_into_text {textwidget filename} {
   set data [read_file $filename]
   $textwidget delete 1.0 end
   $textwidget insert end $data
   
}   

############################################
proc file_write { filename $data } {

       return [catch {
       set fileid [open $filename "w"]                 
               puts -nonewline $fileid $data  
	       close $fileid
       }]	       
}
##############################################
proc error_log_window {w} {  

global env

   wm withdraw $w
   toplevel .w
   wm overrideredirect .w 0

   wm title .w "Climate"
   wm geometry .w +200+300
   frame .w.mbar -relief raised -bd 2
   pack .w.mbar -side top -fill x
   menubutton .w.mbar.file -text File -underline 0 \
	      -menu .w.mbar.file.menu
#   menubutton .mbar.help -text Help -underline 0 \
#	      -menu .mbar.help.menu
   pack .w.mbar.file -side left
#   pack .mbar.help -side right
   menu .w.mbar.file.menu
#        .mbar.file.menu add command -label "Save New Product" -command exit
        .w.mbar.file.menu add command -label "Close" -command exit
  
   text .w.text -relief raised -bg white -bd 2 \
	      -yscrollcommand ".w.scroll set" -fg black
   scrollbar .w.scroll -command ".w.text yview"
   pack .w.scroll -side right -fill y
   pack .w.text -side left
  
#   cd /staging/bld50/adapt/adapt_apps/src/climate/work/tmp      
    cd $env(CLIMATE_DIR)/tmp 
#CAN"T SEEM TO GET THIS LINE TO WORK!!!
   
#   set hold 

#exec tail -10 temp.txt < hold.txt
#more hold.txt
#   set hold [read_file tmp.txt]
   
#   puts $hold
   
   read_file temp.txt 
   read_into_text .w.text tmp.txt   
   
return

}

################################################
proc blink {w option value1 value2 interval} {
	$w config $option $value1
	after $interval [list blink $w $option\
		$value2 $value1 $interval]
	}
################################################	
   wm withdraw .
   toplevel .error
   wm title .error "Error"
   wm geometry .error +1000+400
   wm overrideredirect .error 1
   frame .error.nothing -bg black -bd 5 -relief groove
   pack .error.nothing
      button .error.nothing.clim -bd 5 -bg red\
        -image [image create photo -file $env(CLIMATE_DIR)/data/error_blast.gif]   
#	blink .error.nothing -bg red black 500
      pack .error.nothing.clim 
      button .error.nothing.ok -width 11 -text "View Log File" -bg white -fg black \
       -highlightcolor white -highlightbackground white -activebackground white\
    -activeforeground black -command {error_log_window {.error} }
      button .error.nothing.close -width 4 -text "Close" -bg white -fg black \
       -highlightcolor white -highlightbackground white -activebackground white\
       -activeforeground black -command exit
      pack .error.nothing.ok .error.nothing.close -side left -padx 1m -pady 1m 


      
 
      
