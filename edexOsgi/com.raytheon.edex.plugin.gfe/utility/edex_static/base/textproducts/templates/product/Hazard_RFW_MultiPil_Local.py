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
########################################################################
# Hazard_RFW_Local.py
#
##
##########################################################################
import Hazard_RFW_<MultiPil>
import string, time, re, os, types, copy, sets

class TextProduct(Hazard_RFW_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_RFW_<MultiPil>.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "Hazard_RFW_<MultiPil> (FireWx Watch/Warning)"  

    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
    #Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    #Definition["includeOverview"] = 1   #If 1, the overview section is templated
    #Definition["accurateCities"] = 0  # If 1, cities are determined from grids
    
       ###################################################################################################
    ###
    ### Configuration Section:
    ###
    ###  Variable List:
    ###  Uncomment section and modify per guidelines below
    ###
    ###  rfwType = Red Flag Event Type.  Used to insert bullets and event phrases into the RFW headline
    ###            For Example: If forecaster selects "Wind and Humidity". WindRHPhrase is inserted into
    ###            the RFW segment headline and WindRHBullets are set as the default bullets.
    ###
    ###            Offices can add/remove rfwtype options from GUI as needed.
    ###            Offices can also edit "Extreme RFD/GFDI"  to fit their office. (i.e. - Extreme GFDI)
    ###            Be sure to change _phraseBulletDict as well.
    ###
    ###  elevationSource = Grids or Previous Text.  If Grids, then the LOCATIONS bullet is filled
    ###                    in based on the current Hazard grid. If Previous Text then the LOCATIONS
    ###                    bullet is left as is in the previous version of the product.
    ###  To add a new type insert...
    ###     1. Name to the RFW Type in the VariableList
    ###     2. Create a matching entry in the _bulletDict
    ###     3. Create a Definition["typePhrase"] and Definition["typeBullets"]
    ###
    ###  Uncomment section and modify per guidelines above. Any modifications need to be carried through
    ###  to the other sections. If nothing is modified, formatter will use Generic entry farther down.
    ###
##    VariableList = [
##        (("Select RFW Type", "rfwType"), [], "check", ["Wind and RH", "Thunderstorms", "Haines",\
##                 "Extreme RFD/GFDI"]), #remove any options not valid for your office
##        (("Source for Headline and \nAffected Area Bullet", "elevationSource"), "Grids", "radio", \
##                 ["Grids", "Previous Text"]),
##            ]
    ###
    ###  Dictionary to link Variable List selections above to the bullets and headline phrases
    ###  
    ###  The keys listed need to be the same as the check list in the VariableList
    ###  Ensure options you enable match Variable list entries above.
    ###
    ###  Uncomment section and modify per guidelines above

##    def _bulletDict(self):
##        return {
##            "Wind and RH"       : (self._WindRHPhrase,  self._WindRHBullets),
##            "Thunderstorms"     : (self._ThunderPhrase, self._ThunderBullets),
##            "Haines"            : (self._HainesPhrase,   self._HainesBullets),
##            "Extreme RFD/GFDI"   : (self._FireDangerPhrase,  self._FireDangerBullets)
##            }
    ###
    ### Hazard Headline Default Phrases - used in Hazard Hook and output into the RFW headlines.
    ### 
    ### Example:
    ### ...FIRE WEATHER WATCH IN EFFECT UNTIL 8 PM PDT THIS EVENING FOR
    ### GUSTY WINDS AND LOW HUMIDITY FOR FIRE WEATHER ZONES 271 AND 272...
    ### Again, match code below to previous entries
    ### If needed, edit any of the wording to fit local needs
    ###
    ###  Uncomment section and modify per guidelines above
 
##    Definition["WindRHPhrase"] = "Gusty Winds and Low Humidity "
##    Definition["ThunderPhrase"] = "Dry Thunderstorms and Gusty Outflow Winds "
##    Definition["HainesPhrase"] = "Very Hot and Dry Conditions "
##    Definition["FireDangerPhrase"] = "Extreme Grassland Fire Danger "

    ###
    ### RFW Bullet Lists - List appears based on VariableList selection
    ### Impact bullet added in baseline RFW, no need to configure
    ### First bullet is the locationsBullet and must match Definition["locationsBullet"] below
    ### Modify according to other sections above

##    Definition["WindRHBullets"] = ["Affected area", "Wind", "Humidity"]
##    Definition["ThunderBullets"] = ["Affected area", "Thunderstorms", "Outflow winds"]
##    Definition["HainesBullets"] = ["Affected area", "Haines", "Temperatures/humidity"]
##    Definition["FireDangerBullets"] = ["Affected area", "Timing", "Extreme grassland fire danger",\
##               "Wind", "Humidity"]
    ###
    ### Generic Bullets - Used if an RFW Type is not selected in the Initial GUI
    ### Product will put in these bullets as a generic guideline for the RFW
    Definition["GenericBullets"] = ["Affected area", "Wind", "Humidity", "Thunderstorms", "Highest threat", "Impacts"]
    
    ###
    ### Name for the Location Bullet - again, must match first bullet wording above
    Definition["locationsBullet"] = "Affected area"
    
    ### Geographical descriptor for headlines.
    ### Inserts zone numbers or a generic location into headlines
    ### output for 0 = |* LOCATION DESCRIPTION *|
    ### output for 1 = FIRE WEATHER ZONES 450...453 AND 459.

    Definition["numInHeadline"] = 1

    ###
    ### Include zone names in locationsBullet 
    ### output for 0 = FIRE WEATHER ZONE 450 SIERRA FRONT...FIRE WEATHER ZONE 273 MONO COUNTY
    ### output for 1 = FIRE WEATHER ZONE 450...273.
    Definition["noNameInBullet"] = 1
    
    ###
    ### Include State name in AFFECTED AREA ection and modify per guidelines below
    ### output for 0 = AFFECTED AREA: [List of Fire Zones]
    ### output for 1 = AFFECTED AREA: IN CALIFORNIA [List of Fire Zones]. IN NEVADA [List of Fire Zones].
    Definition["includeStateName"] = 0
    
    ###
    ### Text to insert below the last $$ of the product (WFO URL)
    ### Making no changes will leave default of no url text
    ### Single line example
##    Definition["urlText"] = "http://weather.gov/your_wfo"
    ### 
    ### multiple line example
##    Definition["urlText"] = "For more information from NOAA/s National Weather Service visit...\n" + \
##                            "http://weather.gov/your_wfo"
    ###
    ### End Configuration Section
    ######################################################################################################
    
    def __init__(self):
        Hazard_RFW_<MultiPil>.TextProduct.__init__(self)


