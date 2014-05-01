#!/usr/local/python/bin/python
import sys, os, glob, cmd, string, getpass

###############################################################################
#
# toggle_cli_vip.py             Bob Morris, SAIC/MDL         February 2002
#
# DESCRIPTION
# -----------
#
# Modifies AWIPS Climate control files for NWR product creation.  Sets the
# Active/Inactive Switch, which controls the CRS voice selection, to the user's
# choice of "voice".  The AIS character is in position 4 in line 4 of the
# control files:  /awips/adapt/climate/data/header_*
#
# Modification History:
# ---------------------
# 12/05/2002  OB2  Bob Morris         Changed path /adapt_apps/ to /climate/
#
###############################################################################

me = getpass.getuser()
if me != "fxa" and me != "awipsusr":
   print "\nSorry, you must be fxa or awipsusr to run this script.  Exiting.\n"
   sys.exit()

print "\nPlease select a CRS Voice Type to be used for Climate NWR Products."
ais = "X"
while ais != "C" and ais != "A" and ais != "Q":
   ais = raw_input(" - Enter C for Concatenation, A for Synthesis, or Q to Quit: ")
   ais = string.capitalize(ais)

if ais == "C":
   print "Voice Concatenation Selected."
elif ais == "A":
   print "Voice Synthesis Selected."

if ais != "Q":
   os.chdir('/awips/adapt/climate/data')
   filenames = glob.glob("header_*")
   for file in filenames:
      print "Modifying file:  " + file
      fp=open(file)
      lines=fp.readlines()
      newlist = []
      n = 1
      
      for line in lines:
         if n in [ 4 ]:
            nline = line[:3] + ais[0:1] + line[4:]
            newlist.append(nline)
         else:
            newlist.append(line)
         n = n + 1
      
      fp.close()
      fp=open(file, 'w')
      fp.writelines(newlist)
      fp.close()
else:
   print "Exiting, no changes."
