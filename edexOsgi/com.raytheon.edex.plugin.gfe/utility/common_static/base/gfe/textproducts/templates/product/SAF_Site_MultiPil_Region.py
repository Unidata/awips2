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
# SAF_<site>_<MultiPil>_<region>
#
#  This file should not be edited by the site.
#  Site changes should go in SAF_<site>_Overrides for methods and
#       SAF_<site>_<MultiPil>_Definition to set up Product Definition Settings
#
#
# ---------------------------------------------------------------------

import <standard>
import sys, copy, types
import importlib

# Construct the names of the definition and override TextUtilities 
siteDefinition = "SAF_<site>_<MultiPil>_Definition"
siteOverrides = "SAF_<site>_Overrides"
regionOverrides = "SAF_<region>_Overrides"
ZFP_siteOverrides = "ZFP_<site>_Overrides"
ZFP_regionOverrides = "ZFP_<region>_Overrides"

# Import the local site's Product Definition specifications
siteDefModule = importlib.import_module(siteDefinition)

# Import the local site's Overrides
# siteOverridesModule = importlib.import_module(siteOverrides)
ZFPSiteModule = importlib.import_module(ZFP_siteOverrides)

# Import Regional Overrides 
regionModule = importlib.import_module(regionOverrides)
ZFPRegionModule = importlib.import_module(ZFP_regionOverrides)

# Patches
import Patch_Overrides
# Special SAF overrides
import SAF_Overrides

# These statements get the class object for the region and site overrides class
# The class and the module name (the file name) must be the same!
regionOverrides_object=sys.modules[regionOverrides].__dict__[regionOverrides]
#siteOverrides_object=sys.modules[siteOverrides].__dict__[siteOverrides]
ZFP_regionOverrides_object=sys.modules[ZFP_regionOverrides].__dict__[ZFP_regionOverrides]
#ZFP_siteOverrides_object=sys.modules[ZFP_siteOverrides].__dict__[ZFP_siteOverrides]

# Get the region and site definitions into a known variable name
localDefinition = siteDefModule.Definition
regionDefinition = regionModule.Definition
ZFP_regionDefinition = ZFPRegionModule.Definition
SAF_Definition = SAF_Overrides.Definition

class TextProduct(
                  #siteOverrides_object,
                  regionOverrides_object,
                  SAF_Overrides.SAF_Overrides,
                  #ZFP_siteOverrides_object,
                  ZFP_regionOverrides_object,
                  Patch_Overrides.Patch_Overrides,
                  AreaFcst.TextProduct
                 ):
    Definition = copy.deepcopy(AreaFcst.TextProduct.Definition)

    # Get ZFP Regional Definition settings
    Definition.update(ZFP_regionDefinition)

    # Get SAF Definition settings
    Definition.update(SAF_Definition)

    # Get Regional Definition settings
    Definition.update(regionDefinition)

    # Get the Site Definition Settings
    Definition.update(localDefinition)

    # Get the VariableList if overridden in ZFP Region
    try:
        VariableList = ZFPRegionModule.VariableList
    except:
        pass

    # Get the VariableList if overridden in ZFP Region
    try:
        VariableList = SAF_Overrides.VariableList
    except:
        pass

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

    # To turn on this product for testing,
    # set the display name.
    Definition["displayName"] = "Region_SAF_<MultiPil>"

    def __init__(self):
        AreaFcst.TextProduct.__init__(self)
