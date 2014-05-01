#!/usr/bin/tclsh
#

#
#  Open the log freezing level processing log file.  This will
#  be written into the MPE Editor log directory.
#  This script is called from the run_freezing_level script which
#  sources set_hydro_env and defines several environmental variables
#  used in this script. 
#
#  October 17, 2007 - This script was modified to take an optional
#  command line argument specifying the number of days to process
#
#  March 11, 2008 - Updated to log the names of the files in
#                   the RUC80. Fixed variable reference typos. 
#
#  Dec 2009 - Changed date on .E line to be one day earlier
#
#  Usage:  ruc.tcl [number of days to process]  
#
set run_from_dir [lindex $argv 0]
package require Tclx

#
# Check for a command line argument specifying the number of days to process.
# If no argument is present, then assume that only one day is being processed.
if { $argc == 3 } {

   set days_to_process [lindex $argv 1 ]
   set siteid [lindex $argv 2 ]

} else {

   set days_to_process 1
   set siteid $env(MPE_SITE_ID)

}

#
# Set the freezing level data input and output directories
set rucdir $env(RUC_MODEL_DATA_DIR)
set mmdir $env(MPE_POINT_FREEZING_DIR)
#set siteid $env(MPE_SITE_ID)

# get env variable DQC_PREPROCESSOR_BASETIME, if it is 18Z, load
#freezing level data at12~18Z, 18~00Z, 00~06Z, 06~12Z. IF it is
#12Z, load freezing level data at 6~12Z, 12~18Z, 18~00Z and
#00~06Z. If it is 00Z, load freezing level data at 18~00Z,
#00~06Z, 06~12Z, 12~18Z. If it is 06Z, load the freezing level
#data from 00~06Z, 06~12Z, 12~18Z, 18~00Z

set dqc_preprocessor_basetime $env(DQC_PREPROCESSOR_BASETIME)
puts "The dqc_preprocessor_basetime is $dqc_preprocessor_basetime"
puts "The number of days to process is $days_to_process"

# test purpose 

set rucscript $run_from_dir/ruc.pl

proc mformat {x} {
	if {$x == ""} {
		return M
	} elseif {$x == 0} {
		return 0.00S
	} else {
		set x [format %.1f $x]
		return [set x]0S
	}
}

# Force the dqc_preprocessor_basetime string to be lower case.
set dqc_preprocessor_basetime [string tolower $dqc_preprocessor_basetime]

#setup dates
set csecs [clock seconds]
set tsecs $csecs

set hour [clock format $csecs -format "%H" -gmt 1]
puts "current hour is $hour"

# set cdates and tdates.  Nothing needs to be done when the base hour is 00z.
if { $dqc_preprocessor_basetime == "12z" && $hour < 12 } {

   set csecs [expr $csecs-24*60*60]

} elseif { $dqc_preprocessor_basetime == "12z" && $hour >= 12 } {

   set tsecs [ expr $csecs+24*60*60] 

} elseif { $dqc_preprocessor_basetime == "18z" && $hour < 18 } {

   set csecs [expr $csecs-24*60*60]

} elseif { $dqc_preprocessor_basetime == "18z" && $hour >= 18 } {

   set tsecs [expr $csecs+24*60*60]

} elseif { $dqc_preprocessor_basetime == "06z" && $hour < 6 } {

   set csecs [ expr $csecs-24*60*60 ]

} elseif { $dqc_preprocess_basetime == "06z" && $hour >= 6 } {

   set tsecs [expr $csecs+24*60*60]
}

#
# List the available RUC80 files.

puts "Contents of $rucdir:"

set ruc_files [glob "$rucdir/*"]

foreach f $ruc_files {
      if {![file isdirectory $f]} {
         puts $f
      }
}

# Loop over the number of days to process freezing level
# data for.  Take the user specified number of days minus 1. 

set days_to_process [ expr $days_to_process-1 ]

for { set k $days_to_process } {$k >= 0 } { incr k -1 } {
# Simultaneously subtract from csecs and tsecs the number of days
#

set begin_secs [expr $csecs-24*60*60*$k]
set end_secs [expr $tsecs-24*60*60*$k]

#set k1 [expr $k +1]
#puts "k1 is $k1"
#set end_secs1 [expr $tsecs-24*60*60*$k1]
#set otdate1 [clock format $end_secs1 -format "%Y%m%d" -gmt 1]
#puts "otdate1 is $otdate1"
set otdate1 [clock format $begin_secs -format "%Y%m%d" -gmt 1]

set cdate [clock format $begin_secs -format "%Y%m%d" -gmt 1]
set tdate [clock format $end_secs -format "%Y%m%d" -gmt 1]

puts "current days being processed are $cdate and  $tdate"

#set file
set otdate [clock format $end_secs -format "%Y%m%d" -gmt 1]
set ofile $mmdir/freezing_1_${siteid}_point_$otdate
puts "output file is $ofile"

#get values for hours

if { $dqc_preprocessor_basetime == "18z" } {

   set dates [list $cdate $tdate $tdate $tdate]   
   set hrs [list 1800 0000 0600 1200]

} elseif { $dqc_preprocessor_basetime == "00z" } {

   set dates [list $tdate $tdate $tdate $tdate]   
   set hrs [list 0000 0600 1200 1800]

} elseif { $dqc_preprocessor_basetime == "06z" } {

   set dates [list $cdate $cdate $cdate $tdate]   
   set hrs [list 0600 1200 1800 0000]

} else {
   
   # dqc_preprocessor_basetime must be 12z
   set dates [list $cdate $cdate $tdate $tdate]
   set hrs [list 1200 1800 0000 0600]
}   

set i 0
foreach hr $hrs dt $dates {
	puts "hour $hr"
	set file $rucdir/[set dt]_$hr
	puts "rucfile is $file"
	if {[file exists $file]} {
		puts "$rucscript $file $siteid"
		set vals$i [exec $rucscript $file $siteid]
	} else {
                puts "File $file not found. Continuing."
		set vals$i ""
	}
	puts [set vals$i]
	incr i
}

#
# If there was no data, do not overwrite the existing freezing level file.
set len0 [ string length $vals0 ]
set len1 [ string length $vals1 ]
set len2 [ string length $vals2 ]
set len3 [ string length $vals3 ]

if { $len0 == 0 && $len1 == 0 && $len2 == 0 && $len3 == 0 } {
#  Probably want to continue here.
   puts "No data found for $ofile."
   continue;
}

#
# Check if the output file already exists.  If it does, then open it.
# Process each record.  For each blank freezing level value computed
# above, see if there was already one in the file from a previous
# run of this script.  If there is, use it.  This needs to be 
# done because RUC80 data files are only kept around for 8 hours or
# so.

if [catch {open $ofile r} fileId] {
#
#  The file does not exist or could not be opened.
   set i 0
   set out ""
   puts "file does not exist"
   
   foreach v0 $vals0 v1 $vals1 v2 $vals2 v3 $vals3 {
   	   set stn [format %05d $i]
   	   set v0 [mformat $v0]
	   set v1 [mformat $v1]
	   set v2 [mformat $v2]
	   set v3 [mformat $v3]	   	  
	   
	   if { $dqc_preprocessor_basetime == "18Z" ||
                $dqc_preprocessor_basetime == "18z"} {
	      append out ".E Z$stn $otdate1 DH18/HZIRZ/DIH+6/ $v0/ $v1/ $v2/ $v3\n"
	   
	   } elseif { $dqc_preprocessor_basetime == "00Z" ||
	              $dqc_preprocessor_basetime == "00z" } {
       	      append out ".E Z$stn $otdate1 DH00/HZIRZ/DIH+6/ $v0/ $v1/ $v2/ $v3\n"      
	      
	   } elseif { $dqc_preprocessor_basetime == "06Z" ||
	              $dqc_preprocessor_basetime == "06z" } {
       	      append out ".E Z$stn $otdate1 DH06/HZIRZ/DIH+6/ $v0/ $v1/ $v2/ $v3\n"       
	   
	   } else {
	      append out ".E Z$stn $otdate1 DH12/HZIRZ/DIH+6/ $v0/ $v1/ $v2/ $v3\n"   
	   }   
	      
	   incr i
   }

   puts -nonewline $out
} else {
#
#  The file does exist.
   set i 0
   set out ""
   
   puts "file exist"
   foreach v0 $vals0 v1 $vals1 v2 $vals2 v3 $vals3 {

   	   set stn [format %06d $i]
   	   set v0 [mformat $v0]
	   set v1 [mformat $v1]
	   set v2 [mformat $v2]
	   set v3 [mformat $v3]
	              
           # Read the record.
           gets $fileId line

           set len [ string length $line ]

           if {$len > 0} {
           
	      if { $dqc_preprocessor_basetime == "18Z" ||
                   $dqc_preprocessor_basetime == "18z"} { 

	         # Parse the line for the four freezing level values.
        	 scan $line {%s %s %s %s %s %s %s %s} sheftype hb5 date rest v18 v00 v06 v12

        	 set v18 [ string trimright $v18 / ]
        	 set v00 [ string trimright $v00 / ]
        	 set v06 [ string trimright $v06 / ]
        	 set v12 [ string trimright $v12 / ]
        	 set v18 [ string trimleft $v18 ]
        	 set v00 [ string trimleft $v00 ]
        	 set v06 [ string trimleft $v06 ]
        	 set v12 [ string trimleft $v12 ]

        	 if { $v0 == "M" && $v18 != "M" } {
                   set v0 $v18
        	 }
        	 if { $v1 == "M" && $v00 != "M" } {
                   set v1 $v00
        	 }
        	 if { $v2 == "M" && $v06 != "M" } {
                   set v2 $v06
        	 }
        	 if { $v3 == "M" && $v12 != "M" } {
                   set v3 $v12
        	 }
              	      
	      }	 elseif { $dqc_preprocessor_basetime == "00Z" ||
                          $dqc_preprocessor_basetime == "00z"} { 
	      
	         scan $line {%s %s %s %s %s %s %s %s} sheftype hb5 date rest v00 v06 v12 v18

        	 set v18 [ string trimright $v00 / ]
        	 set v00 [ string trimright $v06 / ]
        	 set v06 [ string trimright $v12 / ]
        	 set v12 [ string trimright $v18 / ]
        	 set v18 [ string trimleft $v00 ]
        	 set v00 [ string trimleft $v06 ]
        	 set v06 [ string trimleft $v12 ]
        	 set v12 [ string trimleft $v18 ]

        	 if { $v0 == "M" && $v00 != "M" } {
                   set v0 $v00
        	 }
        	 if { $v1 == "M" && $v06 != "M" } {
                   set v1 $v06
        	 }
        	 if { $v2 == "M" && $v12 != "M" } {
                   set v2 $v12
        	 }
        	 if { $v3 == "M" && $v18 != "M" } {
                   set v3 $v18
        	 }
		 
              } elseif { $dqc_preprocessor_basetime == "06Z" ||
                         $dqc_preprocessor_basetime == "06z"} { 
	      
	         scan $line {%s %s %s %s %s %s %s %s} sheftype hb5 date rest v06 v12 v18 v00

        	 set v18 [ string trimright $v06 / ]
        	 set v00 [ string trimright $v12 / ]
        	 set v06 [ string trimright $v18 / ]
        	 set v12 [ string trimright $v00 / ]
        	 set v18 [ string trimleft $v06 ]
        	 set v00 [ string trimleft $v12 ]
        	 set v06 [ string trimleft $v18 ]
        	 set v12 [ string trimleft $v00 ]

        	 if { $v0 == "M" && $v06 != "M" } {
                   set v0 $v06
        	 }
        	 if { $v1 == "M" && $v12 != "M" } {
                   set v1 $v12
        	 }
        	 if { $v2 == "M" && $v18 != "M" } {
                   set v2 $v18
        	 }
        	 if { $v3 == "M" && $v00 != "M" } {
                   set v3 $v00
        	 }
		 
	      } else {
		 
		 # Parse the line for the four freezing level values.
        	 scan $line {%s %s %s %s %s %s %s %s} sheftype hb5 date rest v12 v18 v00 v06

        	 set v12 [ string trimright $v12 / ]
        	 set v18 [ string trimright $v18 / ]
        	 set v00 [ string trimright $v00 / ]
        	 set v06 [ string trimright $v06 / ]
        	 set v12 [ string trimleft $v12 ]
        	 set v18 [ string trimleft $v18 ]
        	 set v00 [ string trimleft $v00 ]
        	 set v06 [ string trimleft $v06 ]

        	 if { $v0 == "M" && $v12 != "M" } {
                   set v0 $v12
        	 }
        	 if { $v1 == "M" && $v18 != "M" } {
                   set v1 $v18
        	 }
        	 if { $v2 == "M" && $v00 != "M" } {
                   set v2 $v00
        	 }
        	 if { $v3 == "M" && $v06 != "M" } {
                   set v3 $v06
        	 }
              }
           }
	   
	   if { $dqc_preprocessor_basetime == "18Z" ||
                $dqc_preprocessor_basetime == "18z"} {
             
	      append out ".E Z$stn $otdate1 DH18/HZIRZ/DIH+6/ $v0/ $v1/ $v2/ $v3\n"
	   
	   } elseif { $dqc_preprocessor_basetime == "00Z" ||
                      $dqc_preprocessor_basetime == "00z"} {
             
	      append out ".E Z$stn $otdate1 DH00/HZIRZ/DIH+6/ $v0/ $v1/ $v2/ $v3\n"
	   
	   } elseif { $dqc_preprocessor_basetime == "06Z" ||
                      $dqc_preprocessor_basetime == "06z"} {
             
	      append out ".E Z$stn $otdate1 DH06/HZIRZ/DIH+6/ $v0/ $v1/ $v2/ $v3\n"
	      
	   } else {
	   
	      append out ".E Z$stn $otdate1 DH12/HZIRZ/DIH+6/ $v0/ $v1/ $v2/ $v3\n"
	   }
	   
	   incr i
   }

   #
   # Close the freezing level data file.
   close $fileId

   puts -nonewline $out

}

# Write the Freezing Level File.
write_file $ofile $out

#Continue with the next day to process.
}

