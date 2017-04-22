#!/usr/bin/env python
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
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
