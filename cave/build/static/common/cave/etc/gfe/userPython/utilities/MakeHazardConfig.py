# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MakeHazard.py
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer     Description
# ------------ ---------- -----------  --------------------------
#  Jul 10,2012 436        randerso     Separated configuration data from the 
#                                      MakeHazard procedure
#
# Author: randerso
# ----------------------------------------------------------------------------

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

# Lists of hazards organized by type in a dictionary
# Set these to value you use for your site.  To minimize scrolling,
# change the order so that the most common values your site uses are
# near the front of each list.  The key is the menu entry on the 
# Make Hazard dialog, the values are the key values for Hazards.

# Using OrderedDict allows you to control the order in which the 
# Hazard Types are displayed in the dialog
#
from collections import OrderedDict
hazardDict = OrderedDict([
    ('Winter Weather', ["BZ.W", "BZ.A", "ZR.Y", 
          "IS.W", "LE.Y", "LE.W", "LE.A",
          "WC.Y", "WC.W", "WC.A", "WS.W", "WS.A", "WW.Y"]),
    ('Hydrology', ["FF.A", "FA.A"]),
    ('Fire Weather', ["FW.A", "FW.W"]),
    ('Convective Watches', ["SV.A", "TO.A"]),
    ('Coastal Flood', ["CF.S", "LS.S", "CF.Y", "CF.W", "CF.A", 
          "SU.Y", "SU.W", "LS.Y", "LS.W", "LS.A", "RP.S"]),
    ('Non-Precipitation', ["AF.W", "AF.Y", "AQ.Y", "AS.O", "AS.Y", "DU.Y", 
          "DS.W", "EH.W", "EH.A", "EC.W", "EC.A", "FG.Y", "FZ.W", "FZ.A", 
          "HZ.W", "HZ.A", "ZF.Y", "FR.Y", "HT.Y", "HW.W", "HW.A", 
          "LW.Y", "SM.Y", "WI.Y"]),
    ('Marine', ["MA.S", "MH.W", "MH.Y", "BW.Y", "UP.Y", "MF.Y",
          "GL.A", "GL.W", "SE.A", "SE.W", "UP.A", "UP.W", "HF.A", "HF.W", "LO.Y", "SC.Y", "SW.Y", 
          "RB.Y", "SI.Y", "MS.Y", "SR.A", "SR.W"]),
    ('Tropical Cyclone', ["HU.W", "HU.A", "HU.S", "TR.W", "TR.A"]),
    ('Tsunami', ["TS.A", "TS.W"]),
      
    # ('Local', ["TEST"]),  #example of adding local hazards
    # you can define your own groups of hazards by adding new categories
    ])

# for GUM use comment out the above definition and uncomment the one below

#hazardDict = OrderedDict([
#    ('Hydrology', ["FF.A", "FA.A"]),
#    ('Fire Weather', ["FW.A", "FW.W"]),
#    ('Coastal Flood', ["CF.S", "LS.S", "CF.Y", "CF.W", "CF.A", 
#          "SU.Y", "SU.W", "LS.Y", "LS.W", "LS.A", "RP.S"]),
#    ('Non-Precipitation', ["AF.W", "AF.Y", "AQ.Y", "AS.O", "AS.Y", "DU.Y", 
#          "DS.W", "EH.W", "EH.A", "EC.W", "EC.A", "FG.Y", "FZ.W", "FZ.A", 
#          "HZ.W", "HZ.A", "ZF.Y", "FR.Y", "HT.Y", "HW.W", "HW.A", 
#          "LW.Y", "SM.Y", "WI.Y"]),
#    ('Marine', ["MA.S", "MH.W", "MH.Y", "BW.Y", "UP.Y", "MF.Y",
#          "GL.A", "GL.W", "SE.A", "SE.W", "UP.A", "UP.W", "HF.A", "HF.W", "LO.Y", "SC.Y", "SW.Y", 
#          "RB.Y", "SI.Y", "MS.Y", "SR.A", "SR.W"]),
#    ('Typhoon', ["TY.A", "TY.W", "TR.A", "TR.W", "HU.S"]),
#    ('Tsunami', ["TS.A", "TS.W"]),
#      
#    # ('Local', ["TEST"]),  #example of adding local hazards
#    # you can define your own groups of hazards by adding new categories
#    ])


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

# The defaultHazardType - selected when the tool is first run.  This
# must be one of the categories (keys) in the mapNames and hazardDict.
defaultHazardType = "Non-Precipitation"

# this is the color for the selected areas in the map
mapColor = "red"  # color of selected areas

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
#tcmList = ["TCMCP1", "TCMCP2", "TCMCP3", "TCMCP4", "TCMCP5"]

# Dictionary mapping Hazard Types to applicable local effect areas 
#    that can be intersected with the zone edit areas. 
# You should not define localEffectAreas entries for Tropical Cyclone
# or Convective Watches.
localEffectAreas = {}

#localEffectAreas = {
#  'Winter Weather' : ["Below_1000","Below_1500","Below_2000","Below_2500","Below_3000","Below_3500","Below_4000",
#                      "Above_1000","Above_1500","Above_2000","Above_2500","Above_3000","Above_3500"],
#                   }

# Dictionary associating local Effect Area names with a corresponding 
#    segment number, display name, and list of zones to be auto-selected
# If you do not wish to auto-select zones you should supply an empty list
#
# The display name allows you to display a "pretty" string in the UI rather 
# than the edit area name. If the display name is empty ("") the edit area 
# name will be used.
localAreaData = {} 

#localAreaData = { 
#     "Below_1000" : ( 999, "", []),
#     "Below_1500" : (1499, "", []),
#     "Below_2000" : (1999, "", []),
#     "Below_2500" : (2499, "", []),
#     "Below_3000" : (2999, "", []),
#     "Below_3500" : (3499, "", []),
#     "Below_4000" : (3999, "", []),
#     "Above_1000" : (1000, "", []),
#     "Above_1500" : (1500, "", []),
#     "Above_2000" : (2000, "", []),
#     "Above_2500" : (2500, "", []),
#     "Above_3000" : (3000, "", []),
#     "Above_3500" : (3500, "", []),            
#     }

