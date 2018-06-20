# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# ----------------------------------------------------------------------------
##
# HighSeas_AT2_Overrides
#
# Author: Thomas LeFebvre
#
# SOFTWARE HISTORY
#  Date        Ticket#    Engineer    Description
#  ----------- ---------- ----------- --------------------------
#  12/20/2017  DCS17686   tlefebvre   Initial version.
#
##
# ----------------------------------------------------------------------------

import HighSeas_AT2
import string, time, re, os, types, copy
import TextRules, SampleAnalysis
import ForecastNarrative
import UserInfo

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum contents of this file are the following class definition
# and the __init__ method with only "pass" line in it.

class HighSeas_AT2_Overrides:
    def __init__(self):
       pass
   
# End MAKE NO CHANGES HERE
#**********************************************************************

#**********************************************************************
# REQUIRED OVERRIDES
# _Text2 is set to the basin description string
# 
#     def _Text2(self):
#         return "ATLANTIC FROM 07N TO 31N W OF 35W INCLUDING CARIBBEAN SEA AND\n" + \
#                "GULF OF MEXICO\n\n"

# End REQUIRED OVERRIDES
#**********************************************************************
   
    def __init__(self):
       TextRules.TextRules.__init__(self)
       SampleAnalysis.SampleAnalysis.__init__(self)
