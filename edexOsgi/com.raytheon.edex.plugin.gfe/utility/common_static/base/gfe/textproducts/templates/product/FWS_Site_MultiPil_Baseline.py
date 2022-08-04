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
# technical support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
# ---------------------------------------------------------------------
#
# FWS_<site>_<MultiPil>_Baseline
#
#  This file should not be edited by the site.
#  Site changes should go in FWS_<site>_Overrides for methods and
#       FWS_<site>_<MultiPil> Definition to set up Product Definition Settings
#
#
# ---------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer     Description
# ------------- -------- ------------ -----------------------------------------
# 03/11/2020    DCS20845 vmiddendorf  Upgrade GFE FWS formatter
#
# -----------------------------------------------------------------------------

import FWS
import Patch_Overrides
import copy
import importlib


# Construct the names of the definition and override TextUtilities 
siteDefinition = "FWS_<site>_<MultiPil>_Definition"
nationalOverrides = "FWS_Overrides"

# These statements get the class object for the national and regional
# overrides class. The class and the module name (the file name) must
# be the same. Also need to get the local Definition object.
nationalOverrides_modObject = importlib.import_module(nationalOverrides)
siteDefinition_modObject = importlib.import_module(siteDefinition)

nationalOverrides_classObject = getattr(nationalOverrides_modObject,nationalOverrides,None)

# Get the region and site definitions into a known variable name
localDefinition = getattr(siteDefinition_modObject,"Definition",{})
nationalDefinition = getattr(nationalOverrides_modObject,"Definition",{})

class TextProduct(
                  nationalOverrides_classObject,
                  Patch_Overrides.Patch_Overrides,
                  FWS.TextProduct
                 ):
    Definition = copy.deepcopy(FWS.TextProduct.Definition)

    # Get National Definition settings
    Definition.update(nationalDefinition)

    # Get Site FWS Definition Settings
    Definition.update(localDefinition)

    # Get the VariableList if overridden in National FWS Overrides
    nationalVariableList = getattr(nationalOverrides_modObject,"VariableList",{})
    if nationalVariableList:
        VariableList = nationalVariableList

    # To turn on this product for testing,
    # set the display name.
    Definition["displayName"] = "Baseline_<MultiPil>_FWS"

    def __init__(self):
        FWS.TextProduct.__init__(self)
