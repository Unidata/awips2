# ---------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
# ---------------------------------------------------------------------
#
# TAF_XXX_RR
#
#  This file should not be edited by the site.
#  Site changes should go in TAF_XXX_Overrides for methods and
#       TAF_XXX_Definition to set up Product 
#       Definition Settings.
#
#
# ---------------------------------------------------------------------
# Version: 2017.07.25-0  (07/25/2017)
#-------------------------------------------------------------------------

import TAF
import sys, copy
import importlib

# Construct the names of the definition and override TextUtilities 
siteDefinition = "TAF_<site>_Definition"
#siteOverrides = "TAF_<site>_Overrides"
regionOverrides = "TAF_<region>_Overrides"

# Import the local site's Product Definition specifications
siteDefModule = importlib.import_module(siteDefinition)

# Import the local site's Overrides
# siteOverridesModule = importlib.import_module(siteOverrides)

# Import Regional Overrides 
regionModule = importlib.import_module(regionOverrides)

# Patches
import Patch_Overrides


# These statements get the class object for the region and site overrides class
# The class and the module name (the file name) must be the same!
regionOverrides_object=sys.modules[regionOverrides].__dict__[regionOverrides]
#siteOverrides_object=sys.modules[siteOverrides].__dict__[siteOverrides]

# Get the region and site definitions into a known variable name
localDefinition = siteDefModule.Definition
regionDefinition = regionModule.Definition

class TextProduct(
                  #siteOverrides_object,
                  regionOverrides_object,
                  Patch_Overrides.Patch_Overrides,
                  TAF.TextProduct
                 ):
    Definition = copy.deepcopy(TAF.TextProduct.Definition)

    # Get Regional Definition settings
    Definition.update(regionDefinition)

    # Get the Site Definition Settings
    Definition.update(localDefinition)

    # Get the VariableList if overridden in Region
    try:
        VariableList = regionModule.VariableList
    except:
        pass
        
    # Get the VariableList if overridden in Site
    #try:
    #    VariableList = siteDefModule.VariableList
    #except:
    #    pass

    # Definition overrides should go in TAF_XXX Definition
    # but may be put here for testing.
    # Most common would be the need to set a unique display name
    Definition["displayName"] = "Region_TAF"

    def __init__(self):
        TAF.TextProduct.__init__(self)
