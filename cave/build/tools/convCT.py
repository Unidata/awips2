#!/usr/bin/env python
##
##
# Converts netcdf style colormaps to AWIPS II XML colormaps
#
# Usage: ./convCT.py colormap1 colormap2 colormap3
#
# Requires scipy and numpy
#
# Deposits files in /tmp
#
# SOFTWARE HISTORY
# Date		Ticket#		Engineer	Description
# ------------	----------	-----------	--------------------------
# Jun 23, 2008		        chammack	Initial creation
#
 
import pupynere as netcdf
import numpy
import sys
import os

def convert(i):
    return str((i & 0xFF) / 255.0)

ct = sys.argv
numct = len(ct)

for k in range(1, numct):
   print 'Converting: ' + ct[k]
   nc = netcdf.netcdf_file(ct[k], "r")
   colors = nc.variables['tableColors'][:][0]
   f = open('/tmp/' + os.path.basename(ct[k]).replace('.COLORTABLE', '.cmap'), 'w')
   f.write('<colorMap>\n')

   aVal = 1.0
   for i in range(numpy.shape(colors)[1]):
       f.write("    <color ")
       f.write('r = "' + convert(colors[0,i]) + '" ')
       f.write('g = "' + convert(colors[1,i]) + '" ')
       f.write('b = "' + convert(colors[2,i]) + '" ')
       f.write('a = "' + str(aVal) + '" ')
       f.write('/>\n')

   f.write('</colorMap>\n')
   f.close()
