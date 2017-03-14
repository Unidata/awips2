#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"
#PROCEDURES
#***************************************************************************
# Procedure multi_scroll
#
# Purpose
#   To enable the scrollbar to be attached to several listboxes at the same
#   time
#
# Variables (I=Input) (G=Global) (O=output)
#   scroll_list (I)  List of scroll paths
#   args        (I)  List specifying scroll design options
#
# Returns:
#   NULL    ##!/usr/local/tk/bin/wish -f
#
# History:
#     7/97 Mike Dutter: modified from Welch, Practical Programming in Tcl
#
# Notes:
#   None
#
#***************************************************************************
proc multi_scroll {scroll_list args} {

   foreach i $scroll_list {
      eval {$i yview} $args
   }
   return
}
#***************************************************************************
# Procedure multi_scroll2
#
# Purpose
#   This allows a scrollbar that is attached to several listboxes to remain
#   in alignment no matter how the user scrolls.
#
# Variables (I=Input) (G=Global) (O=output)
#   scroll_path (I)  Tk scroll paths
#   box_list    (I)  List of tk-listbox paths
#   args        (I)  List specifying scroll design options
#
# Returns:
#   NULL
#
# History:
#     5/98 Howard Berger: created
#
# Notes:
#   None
#
#***************************************************************************
proc multi_scroll2 {scroll_path box_list args} {

   eval {$scroll_path set} $args
   foreach box $box_list {
      eval {$box yview moveto} [lindex $args 0]
   }
   return
}

#******************************************************************************
#  Procedure clear_Window
#
#  Purpose:
#    This procedure deletes all of the information in the text window.
#    
#  Variables: (I=input) (O=output) (G=global)
#  
#  Returns:
#    NULL
#
#  History:
#    4/98   Teresa Peachey	Created
#    12/99  Dan Zipper          Modified
#  Notes:
#    Modified from Teresa's clearWindow routine.
#******************************************************************************
proc clear_Window {} {
   
   set t3 .f3.text
   
set result [tk_messageBox -parent . -title "Clear?" -type yesno -icon warning \
                            -message "Clear the product?\
			    Clear will not save changes."]
			    
    if {$result == "yes"} then {
         
       # Delete the Text file information
       $t3 delete 1.0 end 
    }
    
   return
}

#******************************************************************************
#  Procedure clean_Window
#
#  Purpose:
#    This procedure deletes all of the information in the text window.
#    
#  Variables: (I=input) (O=output) (G=global)
#  
#  Returns:
#    NULL
#
#  History:
#    4/98   Teresa Peachey	Created
#    12/99  Dan Zipper          Modified
#  Notes:
#    Modified from Teresa's clearWindow routine.
#******************************************************************************
proc clean_Window {} {
   
   set t3 .f3.text   
         
       # Delete the Text file information
       $t3 delete 1.0 end 
    
   return
}

#******************************************************************************
#  Procedure deleteEntry
#
#  Purpose:
#    Delete an entry in the selection box.
#    
#  Variables: (I=input) (O=output) (G=global)
#    selection    (I)  The index number of the list entry to be
#                      deleted
#    llist	  (G)  List of all listbox paths
#  
#  Returns:
#    NULL
#
#  History:
#    4/98   Teresa Peachey	Created
#   
#  Notes:
#
#******************************************************************************
proc deleteEntry {selection} {

    global llist
    
    foreach i $llist {
       if { $selection == ""} {
          toplevel .error
          tk_dialog .error "No product selected" \
             "Select an entry to delete from the list, and try again." \
              error 0 "OK"
       } else {
            $i delete [lindex $selection 0]
       }
    }

    return
}

#****************************************************************************  
#  Procedure delete_line
#
#  Purpose:
#    Delete a file from the selection list.
#    
#  Variables: (I=input) (O=output) (G=global)
#    None
#  
#  Returns:
#    NULL
#
#  History:
#    4/98   Teresa Peachey	Created
#   12/99   Dan Zipper          Modified
#  Notes:
#    Modified from Teresa's delete routine
#****************************************************************************
proc delete_line {} {

   global filename1
   
   set stnList .f2.frame2.list2
     
# Determine which entry has been selected by the user for deletion and
#   which station that entry represents

  set selection [$stnList curselection]
  
  if { $selection == ""} then {
       toplevel .error
       tk_dialog .error "No product selected" \
          "Select an entry to delete from the list, and try again." \
           error 0 "OK"
        return
  } else {
    set result [tk_messageBox -parent . -title "Delete?" -type yesno -icon warning \
                            -message "Delete $filename1 from list?"]
			    
    if {$result == "yes"} then {
    
      set File [$stnList get $selection]
 
      deleteEntry $selection
            
      exec rm $filename1
      
      clean_Window
      
    } 
  }
     
  return

}

#****************************************************************************  
#  Procedure destory_message
#
#  Purpose:
#    Delete a file from the selection list.
#    
#  Variables: (I=input) (O=output) (G=global)
#    None
#  
#  Returns:
#    NULL
#
#  History:
#   12/99   Dan Zipper         Created
#  Notes:
#    Modified from Teresa's delete routine
#****************************************************************************
proc destroy_message {text} {

  if {$text == "continue"} then {
  
#    set result [tk_messageBox -parent . -title "Accept Climate Run?" \
#                -type yesno -icon warning ] 
		 
#    if {$result == "yes"} then { 
        destroy .tbar .f2 .f3 .buttons . "Review Climate Product"

    } 		 
    				 
    
  } else {			    

#    set result [tk_messageBox -parent . -title "Abort Climate Run?" \
#                -type yesno -icon warning ]
		
#		\
#                -message "Choosing 'Yes' will stop the current\
#                 climate session without saving any added data."]  
  
#    if {$result == "yes"} then { 
       	
        set return_value 1
#	destroy .tbar .f2 .f3 .buttons . "Review Climate Product"
 
        exit 1
       
    }
    
    } 
   
   
#  return $return_value
 
   return
  }

#******************************************************************************
#  Procedure list_Press
#
#  Purpose:
#    This procedure takes several actions when a user selects an entry in the
#    product list in the Free_text GUI.  First it highlights all 
#    items in the same row as that entry.  It then places all related
#    data for the entry into the text box in the GUI.  
#    
#  Variables: (I=input) (O=output) (G=global)
#    listbox  (I) The name of the listbox that the user has selected
#    others   (I) The names of all the listboxes containing notification
#                 information 
#  
#  Returns:
#    NULL
#
#  History:
#    4/98   Teresa Peachey	Created
#   12/99   Dan Zipper          Modified
#  Notes:
#    Modified from teresa's listPress routine
#******************************************************************************
proc list_Press {listbox others} {
   
   set newlist .f2.frame1.list1
   set index [$listbox curselection]
   
# Highlight the corresponding entry in every listbox  
   foreach i $others {
      $i selection clear 0 end
      $i selection set $index
   }
   
   clean_Window
# Write all the related information into the GUI's text window
   putCliText $listbox

   return
}

#****************************************************************************  
# Procedure read_file
#
#  Purpose:
#    This procedure will read a selected output file.
#
#  Variables: (I=input) (O=output) (G=global)
#    filename       (I) The name of the listbox that the user has selected.
#  
#  Returns:
#    NULL
#
#  History:
#    12/99   Dan Zipper     created
#****************************************************************************  
proc read_file {filename} {

  global filename1
  set data ""

  if { [file readable $filename] } {
  
     set fileid [open $filename "r"]
     set data [read $fileid]
     close $fileid
     
     set filename1 $filename
  
  }     
   
  return $data
}  

#****************************************************************************  
# Procedure read_into_text
#
#  Purpose:
#    This procedure will read a selected output file into the window.
#
#  Variables: (I=input) (O=output) (G=global)
#    textwidget   (I) name of text_widget where file is being read into.
#    filename     (I) The name of the listbox that the user has selected
#  
#  Returns:
#    NULL
#
#  History:
#    12/99   Dan Zipper    created
#****************************************************************************
proc read_into_text {textwidget filename} {
   set data [read_file $filename]
   $textwidget insert end $data
} 

#****************************************************************************  
# Procedure file_write
#
#  Purpose:
#    This procedure will save the new output file into the window.
#
#  Variables: (I=input) (O=output) (G=global)
#    listbox        (I) The name of the listbox that the user has selected
#    notifArray     (G) An array of notification information for each station
#    w              (G) top level for TAF Monitoring Window
#  
#  Returns:
#    NULL
#
#  History:
#    12/99   Dan Zipper PRC/TDL
#****************************************************************************
proc file_write { filename data } {

       set fileid [open $filename "w"]
               puts -nonewline $fileid $data  
	       close $fileid
            
}

#****************************************************************************  
# Procedure file_save
#
#  Purpose:
#    This procedure will save the new output file into the window.
#
#  History:
#    12/99   Dan Zipper PRC/TDL
#****************************************************************************
proc file_save {textwidget filename} {

    set result [tk_messageBox -parent . -title "Save?" -type yesno -icon warning \
                              -message "Save $filename?"]
			    
    if {$result == "yes"} then {
         
       set data [$textwidget get 1.0 end ]
       file_write $filename $data    
    }

  
  return   
}
#****************************************************************************  
# Procedure wish_to_send
#
#  Purpose:
#    This procedure will produce a warning message before saving the product.
#
#  History:
#    12/99   Dan Zipper PRC/TDL
#****************************************************************************
proc wish_to_send {} {

    set result [tk_messageBox -parent . -title "Send?" -type yesno -icon warning \
                              -message "Send the products?\
			      This will send ALL products listed in the window."]
			    
    if {$result == "yes"} then {
         
      exit  
    }

  
  return   
}



#****************************************************************************  
#  Procedure putCliText
#
#  Purpose:
#    This procedure places detailed notification info into the text window
#    of the TAF Monitoring GUI when the user selects a particular notification
#    entry.  The information includes the METAR and TAF values for all weather
#    elements with Amend/Alert notices, the entire TAF for the station ID, and
#    all the METARs from the past 3 hours.
#    
#  Variables: (I=input) (O=output) (G=global)
#    listbox        (I) The name of the listbox that the user has selected
#    notifArray     (G) An array of notification information for each station
#    w              (G) top level for TAF Monitoring Window
#  
#  Returns:
#    NULL
#
#  History:
#    4/98   Teresa Peachey	Created
#   12/99   Dan Zipper          Modified
#  Notes:
#    Modified from Teresa's putNotifText routine
#******************************************************************************
proc putCliText {listbox} {

    global textwidget
    global file_name
    set t3 .f3.text

# Determine the station ID for the notification the user has selected
    set selection [$listbox curselection]
    set index [lindex $selection 0]
    set File [.f2.frame1.list1 get $index]

# Allow modification of text window  
    $t3 config -state normal

read_into_text .f3.text $File
tk_messageBox -icon info -message "This product is now ready to be edited." \
              -parent .f3.text -title "Edit" -type ok


set textwidget $t3 
set file_name $File

return
}
#*****************************************************************************
#
#Proc web_page
#
# This procedure will call up the free_text GUI help page.
#
#proc web_page { program help_file } {
proc web_page {} {

   set err ""
   
   catch {
#    eval exec $program $help_file &
     cd $env(CLIMATE_DIR)/data
     eval exec netscape help_climo.html 0 &
   } err
   
   return $err
}
#******************************************************************************
# MAIN BODY FOR NOW.........................
#

   set return_value 1
   
   wm title . "Review Climate Product"
      
# Set up the menu bar with the "File", "Edit" & "Help" submenus
    frame .tbar -relief raised -bd 2
    pack .tbar -side top -fill x 
    menubutton .tbar.file -text File -fg black -font -adobe-times-medium-r-normal--19-130-72-72-p-88-hp-roman8 -underline 0\
                            -menu .tbar.file.menu 
    menubutton .tbar.edit -text Edit -fg black -font -adobe-times-medium-r-normal--19-130-72-72-p-88-hp-roman8 -underline 0\
                            -menu .tbar.edit.menu
#    menubutton .tbar.help -text Help -fg black -font -adobe-times-medium-r-normal--19-130-72-72-p-88-hp-roman8 -underline 0\
#                            -menu .tbar.help.menu
    pack .tbar.file .tbar.edit -side left
#    pack .tbar.help -side right
    
# Allow the user close the window or stop execution of climate

   set exit "exit"
   set continue "continue"

    menu .tbar.file.menu -tearoff 0
#    .tbar.file.menu add separator
    .tbar.file.menu add command -label "Accept Climate Run" -font -adobe-times-medium-r-normal--17-130-72-72-p-88-hp-roman8 \
                -command {destroy_message $continue}
    .tbar.file.menu add command -label "Abort Climate Run" -font -adobe-times-medium-r-normal--17-130-72-72-p-88-hp-roman8 \
                -command {destroy_message $exit} 
 		        		
# Allow the user to load the TAF associated with a notification into one of 
#   the editors, or to delete the notification from the GUI window
    menu .tbar.edit.menu -tearoff 0
    .tbar.edit.menu add command -label "Save" -font -adobe-times-medium-r-normal--17-130-72-72-p-88-hp-roman8 \
            -command {file_save $t3 $file_name}
    .tbar.edit.menu add command -label "Delete" -font -adobe-times-medium-r-normal--17-130-72-72-p-88-hp-roman8 \
             -command {delete_line}
#    .tbar.edit.menu add separator
#    .tbar.edit.menu add command -label "Import New product" \
#            -font Hel12 \
#            -command {delete_line}

# Allow the user to start the help screen in an html-like browser.
#      menu .tbar.help.menu -tearoff 0
#    .tbar.help.menu add command -label "Review product help" -font -adobe-times-medium-r-normal--17-130-72-72-p-88-hp-roman8 \
#                        -command {web_page}
	     
#	     netscape $env(CLIMATE_DIR)/data/help_climo.html}
	     
#            -command "load_html $env(CLIMATE_DIR)/data/help_climo.html 0"	     
   frame .f2
# Set up sub-frames to hold each of 2 list boxes and their associated labels
#  The listboxes contain a "new" indicator, the station id, the time of the
#  notification, the level of notification, and the types of present conditions
#  that triggered the notification.

   for {set j 1} {$j <= 2} {incr j} {
      set f2fr($j) .f2.frame$j
      set f2list($j) $f2fr($j).list$j
      set f2t($j) $f2fr($j).title$j
      lappend llist $f2list($j)
   }


# THIS SECTION IS CREATING THE SCROLLBAR AND LISTBOXES
# Create scrollbar to control all list boxes
   scrollbar .f2.scroll -command [list multi_scroll $llist]
   pack .f2.scroll -side right -fill y

   for {set j 1} {$j <= 2} {incr j} {

# Set the width of the "new" indicator listbox to be narrower than the others
      if {($j == 1)} {
         set listwidth 35
      } else {
         set listwidth 10
      }

      frame $f2fr($j)
      label $f2t($j) -width $listwidth
      listbox $f2list($j) -height 6 -exportselection off -fg black -bg white\
                -yscrollcommand [list multi_scroll2 .f2.scroll $llist] \
                -font Hel13 -width $listwidth -selectmode single -bd 0

      pack $f2list($j) -side bottom -expand true -fill both
      pack $f2t($j) -side top

# Do not allow the "new" indicator listbox to expand
      if {($j == 1)} {
         pack $f2fr($j) -side left -fill both
      } else {
         pack $f2fr($j) -side left -expand true -fill both
      }

   }
   
# Set labels for listboxes
   set k 1
   foreach title "{NWWS Climate Reports}  {Date} " {
      $f2t($k) config -text $title -fg black -font -adobe-times-medium-r-normal--19-130-72-72-p-88-hp-roman8
      incr k
   }
   
# Bind commands to all the listboxes.  For the "new" indicator do not bind
# the Double-Click.
  for {set j 1} {$j <= 2} {incr j} {
     bind $f2list($j) <ButtonRelease-1> [list list_Press $f2list($j) $llist]
     if { $j != 1} {
       bind $f2list($j) <Double-Button-1> {openEditor}
     }
   }

pack .f2 -fill x -side top

frame .f3
   
# Set up text window to hold more detailed notification information
      text .f3.text -state normal -relief raised -fg black -bg white -bd 2 \
    	              -width 69 -yscrollcommand ".f3.scroll set"
		      
      set t3 .f3.text
      
# Set up table to hold specific METAR and TAF values that are in discrepancy
      

      scrollbar .f3.scroll -command ".f3.text yview"

      pack .f3.scroll -side right -fill y 
      pack $t3 -side left -fill both -expand true      


      pack .f3.scroll -side right -fill y 
     
 cd $env(CLIMATE_DIR)/tmp

set files [glob -nocomplain *.nwws]  
 if {$files != ""} {
  foreach filename $files {
      $f2list(1) insert end "$filename"
      set info [file atime $filename]
      set date1 [clock format $info -format "%c"]
      $f2list(2) insert end "$date1"
  }
}
 
# -state disabled DISABLES BUTTONS
#THIS FRAME CREATES THE BUTTONS#

frame .buttons  
		    
button  .buttons.b1 -text "CLEAR" -font -adobe-times-bold-r-normal--17-120-100-100-p-91-iso8859-1 \
	           -fg black -command {clear_Window} 
button	.buttons.b2 -text "DELETE" -font -adobe-times-bold-r-normal--17-120-100-100-p-91-iso8859-1 \
	           -fg black -command {delete_line} 
button  .buttons.b3 -text "SAVE" -font -adobe-times-bold-r-normal--17-120-100-100-p-91-iso8859-1 \
	           -fg black -command {file_save $t3 $file_name}
button  .buttons.b4 -text "SEND" -font -adobe-times-bold-r-normal--17-120-100-100-p-91-iso8859-1 \
	           -fg black -command wish_to_send
pack  .buttons.b1  .buttons.b2  .buttons.b3 .buttons.b4\
                   -fill x -side left -padx 40 -expand true
     
pack .buttons  -side bottom -pady 12

pack .f3 -side bottom -pady 12 -expand true -fill both
