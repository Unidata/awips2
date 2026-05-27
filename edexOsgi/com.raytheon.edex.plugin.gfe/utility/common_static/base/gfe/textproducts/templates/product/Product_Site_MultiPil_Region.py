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
# <product>_<site>_<MultiPil>_<region>
#
#  This file should not be edited by the site.
#  This file can act as a backup for trouble-shooting.
#  It incorporates only the Regional Overrides
#
# ---------------------------------------------------------------------

import <standard>
import sys, copy, types
import importlib

# Construct the names of the definition and override TextUtilities 
siteDefinition = "<product>_<site>_<MultiPil>_Definition"
#siteOverrides = "<product>_<site>_Overrides"
regionOverrides = "<product>_<region>_Overrides"

# Import the local site's Product Definition specifications
siteDefModule = importlib.import_module(siteDefinition)

# Import the local site's Overrides
# siteOverridesModule = importlib.import_module(siteOverrides)

# Import Regional Overrides 
regionOverridesModule = importlib.import_module(regionOverrides)

# Patches
import Patch_Overrides


# These statements get the class object for the region and site overrides class
# The class and the module name (the file name) must be the same!
regionOverrides_object=sys.modules[regionOverrides].__dict__[regionOverrides]
#siteOverrides_object=sys.modules[siteOverrides].__dict__[siteOverrides]

# Get the region and site definitions into a known variable name
localDefinition = siteDefModule.Definition
regionDefinition = regionOverridesModule.Definition

class TextProduct(
                  #siteOverrides_object,
                  regionOverrides_object,
                  Patch_Overrides.Patch_Overrides,
                  <standard>.TextProduct
                 ):
    Definition = copy.deepcopy(<standard>.TextProduct.Definition)

    # Get Regional Definition settings
    Definition.update(regionDefinition)

    # Get the Site Definition Settings
    Definition.update(localDefinition)

    # Get the VariableList if overridden in Region
    try:
        VariableList = regionOverridesModule.VariableList
    except:
        pass
        
    # Get the VariableList if overridden in Site
    #try:
    #    VariableList = siteDefModule.VariableList
    #except:
    #    pass

    # To turn on this product for testing,
    # set the display name.
    displayName = "<product>"
    displayName = displayName.replace("_", "")
    Definition["displayName"] = "Region_"+ displayName + "_<MultiPil>"
    
    def __init__(self):
        <standard>.TextProduct.__init__(self)
