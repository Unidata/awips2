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

import FWF
import sys, copy, types


# Construct the names of the definition and override TextUtilities 
siteDefinition = "FWS_<site>_<MultiPil>_Definition"
siteOverrides = "FWS_<site>_Overrides"
regionOverrides = "FWS_<region>_Overrides"
FWF_siteOverrides = "FWF_<site>_Overrides"
FWF_regionOverrides = "FWF_<region>_Overrides"

# Import the local site's Product Definition specifications
exec "import "+siteDefinition

# Import the local site's Overrides
exec "import "+siteOverrides
exec "import "+FWF_siteOverrides

# Import Regional Overrides 
exec "import "+regionOverrides
exec "import "+FWF_regionOverrides

# Patches
import Patch_Overrides
# Special FWS overrides
import FWS_Overrides

# These statements get the class object for the region and site overrides class
# The class and the module name (the file name) must be the same!
#regionOverrides_object=sys.modules[regionOverrides].__dict__[regionOverrides]
#siteOverrides_object=sys.modules[siteOverrides].__dict__[siteOverrides]
#FWF_regionOverrides_object=sys.modules[FWF_regionOverrides].__dict__[FWF_regionOverrides]
#FWF_siteOverrides_object=sys.modules[FWF_siteOverrides].__dict__[FWF_siteOverrides]

# Get the region and site definitions into a known variable name
exec "localDefinition = " + siteDefinition + ".Definition"
exec "regionDefinition = " + regionOverrides + ".Definition"
exec "FWF_regionDefinition = " + FWF_regionOverrides + ".Definition"
exec "FWS_Definition = FWS_Overrides.Definition"

class TextProduct(
                  #siteOverrides_object,
                  #regionOverrides_object,
                  FWS_Overrides.FWS_Overrides,
                  #FWF_siteOverrides_object,
                  #FWF_regionOverrides_object,
                  Patch_Overrides.Patch_Overrides,
                  FWF.TextProduct
                 ):
    Definition = copy.deepcopy(FWF.TextProduct.Definition)

    # Get FWF Regional Definition settings
    #for key in FWF_regionDefinition.keys():
    #    Definition[key] = FWF_regionDefinition[key]

    # Get FWS Definition settings
    for key in FWS_Definition.keys():
        Definition[key] = FWS_Definition[key]

    # Get Regional Definition settings
    #for key in regionDefinition.keys():
    #    Definition[key] = regionDefinition[key]

    # Get the Site Definition Settings
    for key in localDefinition.keys():
        Definition[key] = localDefinition[key]

    # Get the VariableList if overridden in FWF Region
    #try:
    #    exec "VariableList = "+FWF_regionOverrides+".VariableList"
    #except:
    #    pass

    # Get the VariableList if overridden in FWF Region
    try:
        exec "VariableList = FWS_Overrides.VariableList"
    except:
        pass

    # Get the VariableList if overridden in Region
    #try:
    #    exec "VariableList = "+regionOverrides+".VariableList"
    #except:
    #    pass
        
    # Get the VariableList if overridden in Site
    try:
        exec "VariableList = "+siteDefinition+".VariableList"
    except:
        pass

    # To turn on this product for testing,
    # set the display name.
    Definition["displayName"] = "Baseline_<MultiPil>_FWS"

    def __init__(self):
        FWF.TextProduct.__init__(self)
