##
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
import sys, copy, types, string

# Construct the names of the definition and override TextUtilities 
siteDefinition = "<product>_<site>_<MultiPil>_Definition"
#siteOverrides = "<product>_<site>_Overrides"
regionOverrides = "<product>_<region>_Overrides"

# Import the local site's Product Definition specifications
exec "import "+siteDefinition

# Import the local site's Overrides
#exec "import "+siteOverrides

# Import Regional Overrides 
exec "import "+regionOverrides

# Patches
import Patch_Overrides


# These statements get the class object for the region and site overrides class
# The class and the module name (the file name) must be the same!
regionOverrides_object=sys.modules[regionOverrides].__dict__[regionOverrides]
#siteOverrides_object=sys.modules[siteOverrides].__dict__[siteOverrides]

# Get the region and site definitions into a known variable name
exec "localDefinition = " + siteDefinition + ".Definition"
exec "regionDefinition = " + regionOverrides + ".Definition"

class TextProduct(
                  #siteOverrides_object,
                  regionOverrides_object,
                  Patch_Overrides.Patch_Overrides,
                  <standard>.TextProduct
                 ):
    Definition = copy.deepcopy(<standard>.TextProduct.Definition)

    # Get Regional Definition settings
    for key in regionDefinition.keys():
        Definition[key] = regionDefinition[key]

    # Get the Site Definition Settings
    for key in localDefinition.keys():
        Definition[key] = localDefinition[key]

    # Get the VariableList if overridden in Region
    try:
        exec "VariableList = "+regionOverrides+".VariableList"
    except:
        pass
        
    # Get the VariableList if overridden in Site
    #try:
    #    exec "VariableList = "+siteDefinition+".VariableList"
    #except:
    #    pass

    # To turn on this product for testing,
    # set the display name.
    displayName = "<product>"
    displayName = string.replace(displayName, "_", "")
    Definition["displayName"] = "Region_"+ displayName + "_<MultiPil>"
    
    def __init__(self):
        <standard>.TextProduct.__init__(self)
