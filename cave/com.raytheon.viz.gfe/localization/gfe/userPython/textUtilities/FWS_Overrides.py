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

##
# This is a base file that is not intended to be overridden.
##

# ---------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical  support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
#
# FWS_Overrides.TextUtility
#
#  This file is used for National specific overrides of the FWS
#  formatter.
#
#
# Methods:
#   Overrides:
#
#   Additions:
#
# ---------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer     Description
# ------------- -------- ---------    ---------------------------------
# 03/11/2020    DCS20845 vmiddendorf  Upgrade GFE FWS formatter
#
# ---------------------------------------------------------------------
import TextRules

Definition = {}

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum contents of this file are the following class definition
# and the __init__ method with only "pass" line in it.

class FWS_Overrides:
    def __init__(self):
        pass

# End MAKE NO CHANGES HERE
#**********************************************************************
    # Make sure to indent methods inside the class statement.
    #----- FWS National Overrides -----

    # It is helpful to put a debug statement at the beginning of each
    # method to help with trouble-shooting.
    #def _method(self):
        #self.debug_print("Debug: _method in FWS_Overrides")

    # Example of Overriding a dictionary from TextRules
    #def phrase_descriptor_dict(self, tree, node):
        #dict = TextRules.TextRules.phrase_descriptor_dict(self, tree, node)
        #dict["PoP"] = "chance of"
        #return dict
