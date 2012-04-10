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

# AWIPS1 imports SmartScript to retrieve the site ID below, but
# since all we need is just that site ID, it makes more sense to
# import DataManager directly to get that rather than all of the
# SmartScript module
# import SmartScript
from com.raytheon.viz.gfe.core import DataManager

def sortHazardList(dict):
    #sorts the entries in the menus in alphabetical order, returns sorted
    #dictionary
    import VTECTable
    for ent in dict.keys():
        values = dict[ent]
        # get the descriptive word for this phen/sig
        items = []
        for v in values:
            desc = VTECTable.VTECTable.get(v,'')
            items.append((desc, v))
        items.sort()   #sorts by description
        #extract out the sorted phen/sig
        phensig = []
        for desc, v in items:
            phensig.append(v)
        dict[ent] = phensig
    return dict


###########################################################
##############                              ###############
##############    CONFIGURATION SECTION     ###############
##############                              ###############

# Lists of hazards organized by type in a dictionary
# Set these to value you use for your site.  To minimize scrolling,
# change the order so that the most common values your site uses are
# near the front of each list.  The key is the menu entry on the 
# Make Hazard dialog, the values are the key values for Hazards.
siteID = DataManager.getCurrentInstance().getSiteID()
if siteID == "GUM":
    hazardDict = {
        'Hydrology' : ["FF.A", "FA.A"],
        'Fire Weather' : ["FW.A", "FW.W"],
        'Coastal Flood' : ["CF.S", "LS.S", "CF.Y", "CF.W", "CF.A", 
              "SU.Y", "SU.W", "LS.Y", "LS.W", "LS.A", "RP.S"],
        'Non-Precipitation' : ["AF.W", "AF.Y", "AQ.Y", "AS.O", "AS.Y", "DU.Y", 
              "DS.W", "EH.W", "EH.A", "EC.W", "EC.A", "FG.Y", "FZ.W", "FZ.A", 
              "HZ.W", "HZ.A", "ZF.Y", "FR.Y", "HT.Y", "HW.W", "HW.A", 
              "LW.Y", "SM.Y", "WI.Y"],
        'Marine' : ["MA.S", "MH.W", "MH.Y", "BW.Y", "UP.Y", "MF.Y",
              "GL.A", "GL.W", "SE.A", "SE.W", "UP.A", "UP.W", "HF.A", "HF.W", "LO.Y", "SC.Y", "SW.Y", 
              "RB.Y", "SI.Y", "MS.Y", "SR.A", "SR.W"],
        'Typhoon' : ["TY.A", "TY.W", "TR.A", "TR.W", "HU.S"],
        'Tsunami' : ["TS.A", "TS.W"],
          
        #'Local' : ["TEST"],  #example of adding local hazards
        # you can define your own groups of hazards by adding new categories
        }
else:
    hazardDict = {
        'Winter Weather' : ["BZ.W", "BZ.A", "ZR.Y", 
              "IS.W", "LE.Y", "LE.W", "LE.A",
              "WC.Y", "WC.W", "WC.A", "WS.W", "WS.A", "WW.Y"],
        'Hydrology' : ["FF.A", "FA.A"],
        'Fire Weather' : ["FW.A", "FW.W"],
        'Convective Watches' : ["SV.A", "TO.A"],
        'Coastal Flood' : ["CF.S", "LS.S", "CF.Y", "CF.W", "CF.A", 
              "SU.Y", "SU.W", "LS.Y", "LS.W", "LS.A", "RP.S"],
        'Non-Precipitation' : ["AF.W", "AF.Y", "AQ.Y", "AS.O", "AS.Y", "DU.Y", 
              "DS.W", "EH.W", "EH.A", "EC.W", "EC.A", "FG.Y", "FZ.W", "FZ.A", 
              "HZ.W", "HZ.A", "ZF.Y", "FR.Y", "HT.Y", "HW.W", "HW.A", 
              "LW.Y", "SM.Y", "WI.Y"],
        'Marine' : ["MA.S", "MH.W", "MH.Y", "BW.Y", "UP.Y", "MF.Y",
              "GL.A", "GL.W", "SE.A", "SE.W", "UP.A", "UP.W", "HF.A", "HF.W", "LO.Y", "SC.Y", "SW.Y", 
              "RB.Y", "SI.Y", "MS.Y", "SR.A", "SR.W"],
        'Tropical Cyclone' : ["HU.W", "HU.A", "HU.S", "TR.W", "TR.A"],
        'Tsunami' : ["TS.A", "TS.W"],
          
        #'Local' : ["TEST"],  #example of adding local hazards
        # you can define your own groups of hazards by adding new categories
        }


# This function sorts the hazards in the hazardDict by description.
# Comment it out if this is not desired.
hazardDict = sortHazardList(hazardDict)

        # Dictionary of map categories and the map names.  The "<site>" is
        # substituted with your site name.  The names of the map must match
        # those defined in the ifpServer.   The keys in mapNames must match
        # the keys in hazardDict.

mapNames = {
  'Fire Weather' :            ["FireWxZones_<site>"],
  'Hydrology' :               ["Zones_<site>"],
  'Coastal Flood':            ["Zones_<site>"],
  'Convective Watches' :      ["Marine_Zones_<site>","FIPS_<site>"],
  'Non-Precipitation' :       ["Zones_<site>"],
  'Tropical Cyclone' :        ["Offshore_Marine_Zones_<site>",
                               "Marine_Zones_<site>","Zones_<site>"],
  'Typhoon' :                 ["Offshore_Marine_Zones_<site>",
                               "Marine_Zones_<site>","Zones_<site>"],
  'Tsunami' :                 ["Offshore_Marine_Zones_<site>",
                               "Marine_Zones_<site>","Zones_<site>"],
  'Winter Weather' :          ["Zones_<site>"],
  'Marine' :                  ["Offshore_Marine_Zones_<site>",
                               "Marine_Zones_<site>"],
  #'Local' :     ["Zones_<site>"],   #example of adding local class
  }

# The hazard type chosen when MakeHazard opens
defaultHazardType = "Non-Precipitation"

# this is the color for the selected areas in the map
mapColor = "red"

# initial map width
defaultMapWidth = 400;

# the percentage that an area must be covered to default to selected
areaThreshold = 0.10

# End time in hours of the time scales
timeScaleEndTime = 96

# Define the tropical product used to identify the particular storm
tcmList = []  # Comment out for HLS sites
        
# Uncomment line below for Atlantic basin sites 
#tcmList = ["TCMAT1", "TCMAT2", "TCMAT3", "TCMAT4", "TCMAT5"]

# Uncomment line below for EPac basin sites 
#tcmList = ["TCMEP1", "TCMEP2", "TCMEP3", "TCMEP4", "TCMEP5"]

# Uncomment line below for CPac basin sites 
#self.tcmList = ["TCMCP1", "TCMCP2", "TCMCP3", "TCMCP4", "TCMCP5"]

####################   END CONFIGURATION SECTION   #################