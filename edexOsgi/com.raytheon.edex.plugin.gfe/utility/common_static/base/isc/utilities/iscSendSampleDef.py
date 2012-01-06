# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# iscSendSampleDef
#
# Author: mathewson
# ----------------------------------------------------------------------------

# The configuration interval file is an optional capability of ifpnetCDF.   
# It can be used to select certain grids to be placed in the ifpnetCDF 
# output file, rather than all grids in the inventory.  For example, you can 
# choose to only include 3-hrly temperature grids out to 24 hours, then 
# 6-hrly temperature grids out to 72 hours, and then no temperature grids 
# past 72 hours.   You can control this capability on a per weather element 
# basis.  The definition determines a set of explicit times.  If there 
# is a grid that contains that explicit time, then the grid is included in 
# the output.

# The configuration interval file is a python file and must reside in the 
# ifpServer's TEXT/Utility directory.  You can create the file through the 
# use of the GFE, with the GFE->Define Text Products menu, or by using 
# a conventional text editor and the ifpServerText utility.

# This is the default for ifpnetCDF with regard to its use with ISC traffic.
# The data is sent from -24h to the future in 1 hour intervals.

HR=3600

SampleDef = {}
SampleDef['default'] = (
   [0*HR],              #first tuple is basetimes
    [                   #2nd tuple is list of offset from basetime, interval
     (-24*HR,  1*HR),   #start at basetime, take every hour
    ])

