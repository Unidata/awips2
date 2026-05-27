# ---------------------------------------------------------------------
# Experimental TAF
#
# This software is in the public domain, furnished "as is", without
# technical  support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
#
# TAF_<region>_Overrides.TextUtility
#
#  This file provides any product specific regional overrides for the
#  Experimental TAF product.  This file is under configuration control
#  by the region and should not be edited by the site.
#
# Version: 2017.07.25-0  (07/25/2017)
#
# Authors:  GSD Digital Aviation Services Group
#
# Support Email: nws.digital.aviation.services@noaa.gov
#
# Definition Section:
#   Overrides: 
#     None
#
#   Additions: 
#     None
#
# Methods:
#   Overrides:
#     None
#
#   Additions:
#     None
#        
#-------------------------------------------------------------------------------

import string, time, re, os, types, copy
import TextRules, SampleAnalysis

# Define Regional overrides of Product Definition settings and
# default values of additional Regional Definition settings
#  ( This Definition section must be before the Class definition)

#***** THIS NEXT LINE IS REQUIRED *****
Definition = {}

#-------------------------------------------------------------------------------
# <region> Definitions:
# Definition statements must start in column 1

### Regional settings of baseline options: ###

#Definition["displayName"] = "TAF_<region>"
#
# CR weatherRules definition
#
if "CR" == "<region>":
    Definition["weatherRules"] = {

            #  Handle Thunderstorms specifically
            "T": {
                # Number of hours since Issuance time
                (0, 3): {
                    # Probability/Coverage (Intensity optional)
                    'SChc':     "", # This isn't probable enough to show
                    'Iso':      "",
                    'Chc':      "VCTS",
                    'Sct':      "VCTS",
                    'default':  "PREVAIL"},
                (3, 9): {
                    'SChc':     "",
                    'Iso':      "",
                    'Chc':      "VCTS",
                    'Sct':      "VCTS",
                    'default':  "TEMPO"},
                (9, 18): {
                    'SChc':     "", # This is too far out and not probable enough to show
                    'Iso':      "",
                    'Chc':      "VCTS",
                    'Sct':      "VCTS",
#                    'Chc':      "PROB30", #Change to these lines if PROB30s desired
#                    'Sct':      "PROB30", #Change to these lines if PROB30s desired
                    'default':  "VCTS"},                  
                'default': {
                    'SChc':     "", # This is too far out and not probable enough to show
                    'Iso':      "",
                    'Chc':      "",
                    'Sct':      "",
                    'default':  "VCTS"},
            },

            # Handle Fog specifically
            "F": {
                # Number of hours since Issuance time
                'default': {
                    # Probability/Coverage (Intensity optional)
                    'default':  "PREVAIL"},
            },

            # Handle all other precipitation types
            "default" : {
                # Number of hours since Issuance time
                (0, 3): {
                    # Probability/Coverage (Intensity optional)
                    'SChc':     "", # This isn't probable enough to show
                    'Iso':      "",
                    'Chc':      "VCSH",
                    'Chc--':    "",
                    'Sct':      "VCSH",
                    'Sct--':    "",
                    'default':  "PREVAIL"},
                (3, 9): {
                    'SChc':     "",
                    'Iso':      "",
                    'Chc':      "VCSH",
                    'Chc--':    "",
                    'Sct':      "VCSH",
                    'Sct--':    "",                    
                    'default':  "PREVAIL"},
                (9, 18): {
                    'SChc':     "", # This is too far out and not probable enough to show
                    'Iso':      "",
                    'Chc':      "VCSH",
#                    'Chc':      "PROB30", # Change to this line if PROB30s desired
                    'Chc--':    "",
                    'Sct':      "VCSH",
#                    'Sct':      "PROB30", # Change to this line if PROB30s desired
                    'Sct--':    "",                    
                    'default':  "PREVAIL"},                         
                'default': {
                    'SChc':     "", # This is too far out and not probable enough to show
                    'Iso':      "",
                    'Chc':      "",
                    'Chc--':    "",
                    'Sct':      "",
                    'Sct--':    "",                    
                    'default':  "PREVAIL"},
            },
        }

# END <region> definitions
############################################################

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum contents of this file are the following class definition
# and the __init__ method with only "pass" line in it.

class TAF_<region>_Overrides:
    def __init__(self):
        pass

# End MAKE NO CHANGES HERE
#**********************************************************************
    # Make sure to indent methods inside the class statement.
    #----- <region> TAF Overrides -----

    # It is helpful to put a debug statement at the beginning of each
    # method to help with trouble-shooting.
    #def _method(self):
        #self.debug_print("Debug: _method in TAF_<region>_Overrides")

    # Example of Overriding a dictionary from TextRules
    #def phrase_descriptor_dict(self, tree, node):
        #dict = TextRules.TextRules.phrase_descriptor_dict(self, tree, node)
        #dict["PoP"] = "chance of"
        #return dict

