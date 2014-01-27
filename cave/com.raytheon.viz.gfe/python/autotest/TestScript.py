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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Generalized method for returning a TestScript to TextProductTest
#
# Author:
# ----------------------------------------------------------------------------

import SiteInfo

from com.raytheon.uf.common.dataplugin.gfe.python import GfePyIncludeUtil

    
def getStandardProductType(self, productType, level):
    siteID = self._dataMgr.getSiteID()
    baseline = "_Baseline"
    region = "_"+SiteInfo.SiteInfoDict[siteID]["region"]
    #print "site, region", siteID, region    
    tName = productType + "_"+ siteID
    #print "tName", tName, level
    ceProducts = self._textProductMgr.getModuleNames()
    for ceProduct in ceProducts:
        if ceProduct.find(tName) >= 0:
            #print "   found in ceProduct"
            if level == "Site":
                if ceProduct.find(baseline) >= 0 or ceProduct.find(region) >= 0:
                    continue
            if level == "Baseline":
                if ceProduct.find(baseline) < 0:
                    continue
            if level == "Region":
                if ceProduct.find(region) < 0:
                    continue
            #print "setting product", ceProduct
            productType = ceProduct
            break
    #print "returning", productType
    return productType

import cPickle

def generalTestScript(self, dataMgr, scripts, defaults={}, level="Site"):
    # "scripts" is a list test scripts each in dictionary form
    # Each script has the fields described in TextProductTest
    #
    # In addition, it may have the following fields to be processed
    # by this method to further set up the TestScript for TextProductTest
    #  "comboFlag": if 1 will set up a combinations file for the product
    #      The product must be a standard routine product.
    #  "combinations": if specified, use as the combinations.
    #      Otherwise, use simply "area3".
    #      Can be keyword "ZONE" to use the first zone found for the site.
    #  "level" is whether to use Site, Baseline, or Region for standard products
    #  "defaults" is an optional dictionary of default entries that apply
    #    to all scripts IF they do not have their own entries.  For example,
    #
    #    defaults = {"decodeVTEC": 1"}
    #
    #    Then any scripts that do not have a decodeVTEC entry will set it to 1
    #    (instead of the normal default of 0).
    #
    # Can't use "area3" for standard products since ProductWorkstation
    # examines the Combinations file and does not recognize "area3"
    # We'll have to deal with this if we want this test script to
    # run at any site.
    siteID = dataMgr.getSiteID()
    area = "area3"
    groupList = dataMgr.getRefManager().getGroupData("Zones_"+siteID)
    if groupList.size() > 0:
        zoneArea = str(groupList.get(0))
        
    #print "zoneArea", zoneArea
    cmdLineVars = "{('Product Issuance', 'productIssuance'): 'Morning', ('Issued By', 'issuedBy'): None}"
    # Make Script
    tScript = []
    for script in scripts:
        productType = getValue(script, "productType", defaults, "Phrase_Test_Local")
        if productType is not None:
            if productType != "Phrase_Test_Local":
                productType = getStandardProductType(self, productType, level)
        comboFlag = getValue(script, "comboFlag", defaults, 0)
        if comboFlag and productType is not None:
            productName = script["productType"]
            for s in ["_Local", "CivilEmerg_", "Hazard_"]:
                productName = productName.replace(s, "")
            combinationsFileName = "Combinations_"+productName+"_<site>"
            combinations = getValue(script, "combinations", defaults, None)
            if combinations is not None:
                if combinations == "ZONE":
                    combinations=  [([zoneArea], "")]
            else:
                # If not specified, use area3
                combinations = [([area], "")]
        else:
            combinationsFileName = None
            combinations = None
        newScript =  {
            "name": "test"+ script["name"],
            "productType": productType,
            "commentary": getValue(script, "commentary", defaults, None), 
            "cmdLineVars": getValue(script, "cmdLineVars", defaults, cmdLineVars),
            "database": getValue(script, "database", defaults, "<site>_GRID__Fcst_00000000_0000"),
            "checkMethod": getValue(script, "checkMethod", defaults, None),
            "checkStrings": getValue(script, "checkStrings", defaults, None),
            "notCheckStrings": getValue(script, "notCheckStrings", defaults, None),
            "orderStrings": getValue(script, "orderStrings", defaults, 0),
            "combinationsFileName": combinationsFileName,
            "combinations": combinations,
            "createGrids": getValue(script, "createGrids", defaults, []),
            "fileChanges": getValue(script, "fileChanges", defaults, []),
            "deleteGrids": getValue(script, "deleteGrids", defaults, []),
            "publishGrids": getValue(script, "publishGrids", defaults, None),
            "gridsStartTime": getValue(script, "gridsStartTime", defaults, None),
            "drtTime": getValue(script, "drtTime", defaults, None),
            "decodeVTEC": getValue(script, "decodeVTEC", defaults, 0),
            "internalStrip": getValue(script, "internalStrip", defaults, 1),
            "clearHazardsTable": getValue(script, "clearHazardsTable", defaults, 0),
            "vtecMode": getValue(script, "vtecMode", defaults, "O"),
            "writeableCopies": getValue(script, "writableCopies", defaults, None),
            "callVariables": getValue(script, "callVariables", defaults, cmdLineVars),
            }
        # Fix db is test mode
        #vtecMode = getValue(script, "vtecMode", defaults, "O")
        #if vtecMode == "T":
        #    newScript["database"] = "<site>_GRID_Test_Fcst_00000000_0000"
        #print "appending", newScript
        tScript.append(newScript)
        #break
    return tScript

def getValue(script, key, defaults, default):
    #print "\nkey", key, script.get(key), defaults.get(key), default
    #print "returning", script.get(key, defaults.get(key, default))
    return script.get(key, defaults.get(key, default))

general_createGrids = [
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin", "MaxTEnd", 70, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin", "MinTEnd", 43, "all"),
       ("Fcst", "T", "SCALAR", 6, 9, 70, "all"),
       ("Fcst", "Td", "SCALAR", 6, 9, 30, "all"),  
       ("Fcst", "Wind", "VECTOR", 6, 9, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 6, 9, 0, "all"),
       ("Fcst", "Wx", "WEATHER", 6, 9, "Iso:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 6, 9, 0, "all"),
       ("Fcst", "QPF", "SCALAR", 6, 9, .01, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 6, 9, .05, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 6, 9, 1, "all"),
       ("Fcst", "T", "SCALAR", 9, 12, 65, "all"),
       ("Fcst", "Td", "SCALAR", 9, 12, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 9, 12, (15, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 9, 12, 7, "all"),
       ("Fcst", "Wx", "WEATHER", 9, 12,
        "Sct:T:<NoInten>:<NoVis>:^Num:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 9, 12, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 9, 12, 0, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 9, 12, .01, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 9, 12, 0, "all"),
       ("Fcst", "T", "SCALAR", 12, 15, 60, "all"),
       ("Fcst", "Td", "SCALAR", 12, 15, 45, "all"),  
       ("Fcst", "Wind", "VECTOR", 12, 15, (15, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 12, 15, 6, "all"),
       ("Fcst", "Wx", "WEATHER", 12, 15,
        "Wide:T:<NoInten>:<NoVis>:^Ocnl:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 12, 15, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 12, 15, 0, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 12, 15, .01, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 12, 15, 2, "all"),
       ("Fcst", "T", "SCALAR", 15, 18, 55, "all"),
       ("Fcst", "Td", "SCALAR", 15, 18, 50, "all"),  
       ("Fcst", "Wind", "VECTOR", 15, 18, (25, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 15, 18, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 15, 18,
        "SChc:T:<NoInten>:<NoVis>:^Chc:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 15, 18, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 15, 18, .05, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 15, 18, .1, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 15, 18, 1, "all"),
       ("Fcst", "T", "SCALAR", 18, 21, 50, "all"),
       ("Fcst", "Td", "SCALAR", 18, 21, 50, "all"),  
       ("Fcst", "Wind", "VECTOR", 18, 21, (30, "NE"), "all"),
       ("Fcst", "Sky", "SCALAR", 18, 21, 15, "all"),
       ("Fcst", "Wx", "WEATHER", 18, 21,
        "Lkly:T:<NoInten>:<NoVis>:^Def:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 18, 21, 25, "all"),
       ("Fcst", "QPF", "SCALAR", 18, 21, .08, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 18, 21, .2, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 18, 21, 1.5, "all"),
       ("Fcst", "T", "SCALAR", 21, 24, 50, "all"),
       ("Fcst", "Td", "SCALAR", 21, 24, 48, "all"),  
       ("Fcst", "Wind", "VECTOR", 21, 24, (25, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 21, 24, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 21, 24,
        "Frq:T:<NoInten>:<NoVis>:^Brf:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 21, 24, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 21, 24, .1, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 21, 24, .25, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 21, 24, 1.2, "all"),
       ("Fcst", "T", "SCALAR", 24, 27, 48, "all"),
       ("Fcst", "Td", "SCALAR", 24, 27, 48, "all"),  
       ("Fcst", "Wind", "VECTOR", 24, 27, (22, "NW"), "all"),
       ("Fcst", "Sky", "SCALAR", 24, 27, 19, "all"),
       ("Fcst", "Wx", "WEATHER", 24, 27,
        "Pds:T:<NoInten>:<NoVis>:^Inter:T:<NoInten>:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 24, 27, 25, "all"),
       ("Fcst", "QPF", "SCALAR", 24, 27, 1, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 24, 27, 1, "all"),
       ("Fcst", "T", "SCALAR", 27, 30, 43, "all"),
       ("Fcst", "Td", "SCALAR", 27, 30, 43, "all"),  
       ("Fcst", "Wind", "VECTOR", 27, 30, (15, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 27, 30, 30, "all"),
       ("Fcst", "Wx", "WEATHER", 27, 30, "Wide:R:-:3SM:^Ocnl:R:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 27, 30, 50, "all"),
       ("Fcst", "QPF", "SCALAR", 27, 30, 3, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 27, 30, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 24", "MaxTEnd + 24", 50, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 24", "MinTEnd + 24", 33, "all"),
       ("Fcst", "T", "SCALAR", 30, 33, 40, "all"),
       ("Fcst", "Td", "SCALAR", 30, 33, 39, "all"),  
       ("Fcst", "Wind", "VECTOR", 30, 33, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 30, 33, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 30, 33, "SChc:R:--:0SM:^Chc:R:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 30, 33, 60, "all"),
       ("Fcst", "QPF", "SCALAR", 30, 33, 2.5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 30, 33, 0, "all"),
       ("Fcst", "T", "SCALAR", 33, 36, 41, "all"),
       ("Fcst", "Td", "SCALAR", 33, 36, 39, "all"),  
       ("Fcst", "Wind", "VECTOR", 33, 36, (10, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 33, 36, 90, "all"),
       ("Fcst", "Wx", "WEATHER", 33, 36, "Lkly:R:m:1/2SM:^Def:R:m:4SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 33, 36, 70, "all"),
       ("Fcst", "QPF", "SCALAR", 33, 36, 4, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 33, 36, 0, "all"),
       ("Fcst", "T", "SCALAR", 36, 39, 40, "all"),
       ("Fcst", "Td", "SCALAR", 36, 39, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 36, 39, (5, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 36, 39, 100, "all"),
       ("Fcst", "Wx", "WEATHER", 36, 39, "Frq:R:+:<NoVis>:^Brf:R:+:5SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 36, 39, 100, "all"),
       ("Fcst", "QPF", "SCALAR", 36, 39, 5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 36, 39, 0, "all"),
       ("Fcst", "T", "SCALAR", 39, 42, 33, "all"),
       ("Fcst", "Td", "SCALAR", 39, 42, 33, "all"),  
       ("Fcst", "Wind", "VECTOR", 39, 42, (5, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 39, 42, 100, "all"),
       ("Fcst", "Wx", "WEATHER", 39, 42, "Pds:R:m:<NoVis>:^Inter:R:m:2SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 39, 42, 100, "all"),
       ("Fcst", "QPF", "SCALAR", 39, 42, 5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 39, 42, 0, "all"),
       ("Fcst", "T", "SCALAR", 42, 45, 35, "all"),
       ("Fcst", "Td", "SCALAR", 42, 45, 33, "all"),  
       ("Fcst", "Wind", "VECTOR", 42, 45, (2.5, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 42, 45, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 42, 45, "Iso:RW:+:<NoVis>:^Sct:RW:+:3/4SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 42, 45, 95, "all"),
       ("Fcst", "QPF", "SCALAR", 42, 45, 5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 42, 45, 0, "all"),
       ("Fcst", "T", "SCALAR", 45, 48, 40, "all"),
       ("Fcst", "Td", "SCALAR", 45, 48, 35, "all"),  
       ("Fcst", "Wind", "VECTOR", 45, 48, (2, "NE"), "all"),
       ("Fcst", "Sky", "SCALAR", 45, 48, 85, "all"),
       ("Fcst", "Wx", "WEATHER", 45, 48, "Num:RW:m:<NoVis>:^Wide:RW:-:1/2SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 45, 48, 65, "all"),
       ("Fcst", "QPF", "SCALAR", 45, 48, 1, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 45, 48, 5, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 45, 48, 0, "all"),
       ("Fcst", "T", "SCALAR", 48, 51, 45, "all"),
       ("Fcst", "Td", "SCALAR", 48, 51, 38, "all"),  
       ("Fcst", "Wind", "VECTOR", 48, 51, (5, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 48, 51, 70, "all"),
       ("Fcst", "Wx", "WEATHER", 48, 51, "Ocnl:RW:-:<NoVis>:^SChc:RW:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 48, 51, 60, "all"),
       ("Fcst", "QPF", "SCALAR", 48, 51, .5, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 48, 51, 3, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 48, 51, 0, "all"),
       ("Fcst", "T", "SCALAR", 51, 54, 50, "all"),
       ("Fcst", "Td", "SCALAR", 51, 54, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 51, 54, (7, "NW"), "all"),
       ("Fcst", "Sky", "SCALAR", 51, 54, 35, "all"),
       ("Fcst", "Wx", "WEATHER", 51, 54, "Chc:RW:--:<NoVis>:^Lkly:RW:--:1SM:", "all"),
       ("Fcst", "PoP", "SCALAR", 51, 54, 50, "all"),
       ("Fcst", "QPF", "SCALAR", 51, 54, .5, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 51, 54, 2, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 51, 54, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 48", "MaxTEnd + 48", 70, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 48", "MinTEnd + 48", 50, "all"),
       ("Fcst", "T", "SCALAR", 54, 57, 50, "all"),
       ("Fcst", "Td", "SCALAR", 54, 57, 45, "all"),  
       ("Fcst", "Wind", "VECTOR", 54, 57, (10, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 54, 57, 30, "all"),
       ("Fcst", "Wx", "WEATHER", 54, 57, "Def:RW:--:<NoVis>:^Frq:RW:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 54, 57, 55, "all"),
       ("Fcst", "QPF", "SCALAR", 54, 57, .25, ["BelowElev"]),
       ("Fcst", "QPF", "SCALAR", 54, 57, 1, ["AboveElev"]),
       ("Fcst", "SnowAmt", "SCALAR", 54, 57, 0, "all"),
       ("Fcst", "T", "SCALAR", 57, 60, 55, "all"),
       ("Fcst", "Td", "SCALAR", 57, 60, 47, "all"),  
       ("Fcst", "Wind", "VECTOR", 57, 60, (12, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 57, 60, 40, "all"),
       ("Fcst", "Wx", "WEATHER", 57, 60, "Brf:RW:-:4SM:^Pds:RW:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 57, 60, 40, "all"),
       ("Fcst", "QPF", "SCALAR", 57, 60, .5, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 57, 60, 0, "all"),
       ("Fcst", "T", "SCALAR", 60, 63, 65, "all"),
       ("Fcst", "Td", "SCALAR", 60, 63, 43, "all"),  
       ("Fcst", "Wind", "VECTOR", 60, 63, (15, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 60, 63, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 60, 63, "Inter:RW:--:<NoVis>:^Wide:L:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 60, 63, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 60, 63, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 60, 63, 0, "all"),
       ("Fcst", "T", "SCALAR", 63, 66, 70, "all"),
       ("Fcst", "Td", "SCALAR", 63, 66, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 63, 66, (15, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 63, 66, 5, "all"),
       ("Fcst", "Wx", "WEATHER", 63, 66, "Ocnl:L:--:<NoVis>:^SChc:L:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 63, 66, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 63, 66, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 63, 66, 0, "all"),
       ("Fcst", "T", "SCALAR", 66, 69, 68, "all"),
       ("Fcst", "Td", "SCALAR", 66, 69, 55, "all"),  
       ("Fcst", "Wind", "VECTOR", 66, 69, (20, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 66, 69, 75, "all"),
       ("Fcst", "Wx", "WEATHER", 66, 69, "Chc:L:-:3SM:^Lkly:L:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 66, 69, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 66, 69, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 66, 69, 0, "all"),
       ("Fcst", "T", "SCALAR", 69, 75, 69, "all"),
       ("Fcst", "Td", "SCALAR", 69, 75, 56, "all"),  
       ("Fcst", "Wind", "VECTOR", 69, 75, (10, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 69, 75, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 69, 75, "Def:L:--:<NoVis>:^Areas:L:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 69, 75, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 69, 75, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 69, 75, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 72", "MaxTEnd + 72", 71, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 72", "MinTEnd + 72", 65, "all"),
       ("Fcst", "T", "SCALAR", 75, 81, 69, "all"),
       ("Fcst", "Td", "SCALAR", 75, 81, 60, "all"),  
       ("Fcst", "Wind", "VECTOR", 75, 81, (5, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 75, 81, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 75, 81, "Patchy:L:--:<NoVis>:^Frq:L:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 75, 81, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 75, 81, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 75, 81, 0, "all"),
       ("Fcst", "T", "SCALAR", 81, 87, 70, "all"),
       ("Fcst", "Td", "SCALAR", 81, 87, 61, "all"),  
       ("Fcst", "Wind", "VECTOR", 81, 87, (20, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 81, 87, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 81, 87, "Brf:L:--:<NoVis>:^Pds:L:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 81, 87, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 81, 87, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 81, 87, 0, "all"),
       ("Fcst", "T", "SCALAR", 87, 93, 71, "all"),
       ("Fcst", "Td", "SCALAR", 87, 93, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 87, 93, (15, "E"), "all"),
       ("Fcst", "Sky", "SCALAR", 87, 93, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 87, 93, "Inter:L:-:<NoVis>:^Wide:ZL:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 87, 93, 30, "all"),
       ("Fcst", "QPF", "SCALAR", 87, 93, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 87, 93, 0, "all"),
       ("Fcst", "T", "SCALAR", 93, 99, 65, "all"),
       ("Fcst", "Td", "SCALAR", 93, 99, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 93, 99, (23, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 93, 99, 50, "all"),
       ("Fcst", "Wx", "WEATHER", 93, 99, "Ocnl:ZL:-:<NoVis>:^SChc:ZL:-:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 93, 99, 50, "all"),
       ("Fcst", "QPF", "SCALAR", 93, 99, .01, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 93, 99, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 96", "MaxTEnd + 96", 75, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 96", "MinTEnd + 96", 68, "all"),
       ("Fcst", "T", "SCALAR", 99, 105, 68, "all"),
       ("Fcst", "Td", "SCALAR", 99, 105, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 99, 105, (31, "NE"), "all"),
       ("Fcst", "Sky", "SCALAR", 99, 105, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 99, 105, "Chc:ZL:--:<NoVis>:^Lkly:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 99, 105, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 99, 105, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 99, 105, 0, "all"),
       ("Fcst", "T", "SCALAR", 105, 111, 70, "all"),
       ("Fcst", "Td", "SCALAR", 105, 111, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 105, 111, (40, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 105, 111, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 105, 111, "Def:ZL:--:<NoVis>:^Areas:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 105, 111, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 105, 111, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 105, 111, 0, "all"),
       ("Fcst", "T", "SCALAR", 111, 117, 73, "all"),
       ("Fcst", "Td", "SCALAR", 111, 117, 65, "all"),  
       ("Fcst", "Wind", "VECTOR", 111, 117, (5, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 111, 117, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 111, 117, "Patchy:ZL:--:<NoVis>:^Frq:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 111, 117, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 111, 117, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 111, 117, 0, "all"),
       ("Fcst", "T", "SCALAR", 117, 123, 75, "all"),
       ("Fcst", "Td", "SCALAR", 117, 123, 60, "all"),  
       ("Fcst", "Wind", "VECTOR", 117, 123, (45, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 117, 123, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 117, 123, "Brf:ZL:--:<NoVis>:^Pds:ZL:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 117, 123, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 117, 123, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 117, 123, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 120", "MaxTEnd + 120", 83, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 120", "MinTEnd + 120", 78, "all"),
       ("Fcst", "T", "SCALAR", 123, 129, 78, "all"),
       ("Fcst", "Td", "SCALAR", 123, 129, 55, "all"),  
       ("Fcst", "Wind", "VECTOR", 123, 129, (17, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 123, 129, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 123, 129, "Inter:ZL:--:<NoVis>:^Wide:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 123, 129, 15, "all"),
       ("Fcst", "QPF", "SCALAR", 123, 129, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 123, 129, 0, "all"),
       ("Fcst", "T", "SCALAR", 129, 135, 80, "all"),
       ("Fcst", "Td", "SCALAR", 129, 135, 50, "all"),  
       ("Fcst", "Wind", "VECTOR", 129, 135, (12, "SE"), "all"),
       ("Fcst", "Sky", "SCALAR", 129, 135, 20, "all"),
       ("Fcst", "Wx", "WEATHER", 129, 135, "Ocnl:ZR:--:<NoVis>:^SChc:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 129, 135, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 129, 135, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 129, 135, 0, "all"),
       ("Fcst", "T", "SCALAR", 135, 141, 81, "all"),
       ("Fcst", "Td", "SCALAR", 135, 141, 45, "all"),  
       ("Fcst", "Wind", "VECTOR", 135, 141, (15, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 135, 141, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 135, 141, "Chc:ZR:--:<NoVis>:^Lkly:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 135, 141, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 135, 141, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 135, 141, 0, "all"),
       ("Fcst", "T", "SCALAR", 141, 147, 83, "all"),
       ("Fcst", "Td", "SCALAR", 141, 147, 43, "all"),  
       ("Fcst", "Wind", "VECTOR", 141, 147, (25, "NW"), "all"),
       ("Fcst", "Sky", "SCALAR", 141, 147, 20, "all"),
       ("Fcst", "Wx", "WEATHER", 141, 147, "Def:ZR:--:<NoVis>:^Frq:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 141, 147, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 141, 147, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 141, 147, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 144", "MaxTEnd + 144", 90, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 144", "MinTEnd + 144", 83, "all"),
       ("Fcst", "T", "SCALAR", 147, 153, 83, "all"),
       ("Fcst", "Td", "SCALAR", 147, 153, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 147, 153, (22, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 147, 153, 20, "all"),
       ("Fcst", "Wx", "WEATHER", 147, 153, "Brf:ZR:--:<NoVis>:^Pds:ZR:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 147, 153, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 147, 153, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 147, 153, 0, "all"),
       ("Fcst", "T", "SCALAR", 153, 159, 85, "all"),
       ("Fcst", "Td", "SCALAR", 153, 159, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 153, 159, (31, "N"), "all"),
       ("Fcst", "Sky", "SCALAR", 153, 159, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 153, 159, "Inter:ZR:--:<NoVis>:^Wide:S:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 153, 159, 20, "all"),
       ("Fcst", "QPF", "SCALAR", 153, 159, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 153, 159, 0, "all"),
       ("Fcst", "T", "SCALAR", 159, 165, 86, "all"),
       ("Fcst", "Td", "SCALAR", 159, 165, 39, "all"),  
       ("Fcst", "Wind", "VECTOR", 159, 165, (45, "S"), "all"),
       ("Fcst", "Sky", "SCALAR", 159, 165, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 159, 165, "Ocnl:S:--:<NoVis>:^SChc:S:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 159, 165, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 159, 165, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 159, 165, 0, "all"),
       ("Fcst", "T", "SCALAR", 165, 171, 90, "all"),
       ("Fcst", "Td", "SCALAR", 165, 171, 30, "all"),  
       ("Fcst", "Wind", "VECTOR", 165, 171, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 165, 171, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 165, 171, "Chc:S:--:<NoVis>:^Lkly:S:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 165, 171, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 165, 171, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 165, 171, 0, "all"),
       ("Fcst", "MaxT", "SCALAR", "MaxTBegin + 168", "MaxTEnd + 168", 93, "all"),
       ("Fcst", "MinT", "SCALAR", "MinTBegin + 168", "MinTEnd + 168", 90, "all"),
       ("Fcst", "T", "SCALAR", 171, 177, 93, "all"),
       ("Fcst", "Td", "SCALAR", 171, 177, 30, "all"),  
       ("Fcst", "Wind", "VECTOR", 171, 177, (10, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 171, 177, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 171, 177, "Def:S:--:<NoVis>:^Frq:S:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 171, 177, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 171, 177, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 171, 177, 0, "all"),
       ("Fcst", "T", "SCALAR", 177, 183, 91, "all"),
       ("Fcst", "Td", "SCALAR", 177, 183, 40, "all"),  
       ("Fcst", "Wind", "VECTOR", 177, 183, (15, "SW"), "all"),
       ("Fcst", "Sky", "SCALAR", 177, 183, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 177, 183, "Brf:S:--:<NoVis>:^Pds:S:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 177, 183, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 177, 183, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 177, 183, 0, "all"),
       ("Fcst", "T", "SCALAR", 183, 189, 90, "all"),
       ("Fcst", "Td", "SCALAR", 183, 189, 35, "all"),  
       ("Fcst", "Wind", "VECTOR", 183, 189, (20, "W"), "all"),
       ("Fcst", "Sky", "SCALAR", 183, 189, 10, "all"),
       ("Fcst", "Wx", "WEATHER", 183, 189, "Inter:S:--:<NoVis>:^Iso:SW:--:<NoVis>:", "all"),
       ("Fcst", "PoP", "SCALAR", 183, 189, 10, "all"),
       ("Fcst", "QPF", "SCALAR", 183, 189, 0, "all"),
       ("Fcst", "SnowAmt", "SCALAR", 183, 189, 0, "all"),
       ]

        
general_deleteGrids = [
        ("Fcst", "PoP", "SFC", -100,280),
        ("Fcst", "MaxT", "SFC", -100,280),
        ("Fcst", "MinT", "SFC", -100,280),
        ("Fcst", "T", "SFC", -100,280),
        ("Fcst", "Td", "SFC", -100,280),
        ("Fcst", "WindChill", "SFC", -100,280),
        ("Fcst", "HeatIndex", "SFC", -100,280),
        ("Fcst", "StormTotalSnow", "SFC", -100,280),
        ("Fcst", "SnowLevel", "SFC", -100,280),
        ("Fcst", "FzLevel", "SFC", -100,280),
        ("Fcst", "RH", "SFC", -100,280),
        ("Fcst", "Wind", "SFC", -100,280),
        ("Fcst", "Sky", "SFC", -100,280),
        ("Fcst", "WindGust", "SFC", -100,280),
        ("Fcst", "Wx", "SFC", -100,280),
        ("Fcst", "QPF", "SFC", -100,280),
        ("Fcst", "SnowAmt", "SFC", -100,280),
        ("Fcst", "Hazards", "SFC", -100,280),
        ("Fcst", "pwsD34", "SFC", -100,280),
        ("Fcst", "pwsN34", "SFC", -100,280),
        ("Fcst", "pwsD64", "SFC", -100,280),
        ("Fcst", "pwsN64", "SFC", -100,280),
        ]

