##
##

##
# This is a base file that is not intended to be overridden.
##

# ---------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical  support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
#
# <product>_<region>_Overrides
#
#  This file provides any product specific regional overrides for the
#  <product> product.  This file is under configuration control by
#  the region and should not be edited by the site.
#
# Definition Section:
#   Overrides: 
#   Additions: 
#
# Methods:
#   Overrides:
#   Additions:
#
# ---------------------------------------------------------------------

import string, time, re, os, types, copy
import TextRules, SampleAnalysis


# Define Regional overrides of Product Definition settings and
# default values of additional Regional Definition settings
#  ( This Definition section must be before the Class definition)

#***** THIS NEXT LINE IS REQUIRED *****
Definition = {}

#####################################################
# Override VariableList if desired
#
#VariableList = []
#
# <region> Definitions:
# Definition statements must start in column 1

### Regional settings of baseline options: ###

#Definition["displayName"] = "<product>_<region>"

### New Regional Definitions not in the baseline ###

# END <region> definitions
############################################################

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum contents of this file are the above Definition = {} line
# plus following class definition and the __init__ method with only
# the "pass" line in it.

class <product>_<region>_Overrides:
    """Class NNN_FILETYPE - Version: IFPS"""

    def __init__(self):
        pass

# End MAKE NO CHANGES HERE
#**********************************************************************
    # Add methods here making sure to indent inside the class statement
    # <region> <product> Overrides ------------------------

    # It is helpful to put a debug statement at the beginning of each
    # method to help with trouble-shooting.
    #def _method(self):
        #self.debug_print("Debug: _method in <product>_<region>_Overrides")

