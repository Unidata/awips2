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
# CallToActions.py
# 
# This module contains all of the call to action statements based on the
# VTEC phen/sig code. Sites can override this file to make modifications
# to the list of call to actions.
#
# Author: Matt Davis (ARX), Mathewson
# Updated 9/24 Shannon for WW simplification
# Updated 11/25 Shannon for AS.O
# Updated 3/27/09 Shannon for tropical and AF.W
# Updated 3/29/10 Shannon for tropical
# Updated 1/12/11 Shannon to remove HI/TI hazards and fix typos
# Updated 7/28/15 yteng to change Red Flag Warning CTA language for DR 17777
# Updated 2/24/16 randerso for mixed case guidelines
# Updated 6/17/16 dgilling to fix a spelling error.
# ----------------------------------------------------------------------------

class CallToActions:

    def pydevDebug(self):
        import sys
        PYDEVD_PATH='/home/rtran/awipsdr4/Ade/eclipse/plugins/org.python.pydev.debug_1.5.4.2010011921/pysrc'
        if sys.path.count(PYDEVD_PATH) < 1:
            sys.path.append(PYDEVD_PATH)

        import pydevd
        pydevd.settrace() 

    # returns the default Call To Action 
    def defaultCTA(self, phensig):
        if self.ctaDict().has_key(phensig):
            func = self.ctaDict()[phensig]
            items = func()
            if len(items) > 0:
                return items[0]
        return ""   #No call to action

    def allCTAs(self, phensig):
        if self.ctaDict().has_key(phensig):
            func = self.ctaDict()[phensig]
            return func() 
        return [] #no Call to actions


    def pilCTAs(self, pil):
        if self.ctaPilDict().has_key(pil):
            func = self.ctaPilDict()[pil]
            return func() 
        return [] #no Call to actions


    # returns list of generic call to action statements
    def genericCTAs(self):
        return [
 """Monitor NOAA Weather Radio for the latest information...forecasts...and warnings.""",
 """Listen to NOAA Weather Radio or your local media for the latest updates on this situation.""",
        ]

        

##### PLEASE KEEP PHENSIG IN ALPHABETICAL ORDER #######

# CallToAction dictionary.  The key is the phen/sig, such as "BZ.W".  The
# value is a LIST of call to action statements.  The default formatter
# uses the first one in the list. Users can add additional entries which
# are accessible in the product editor.  The lists are actually function
# calls that the user can override if necessary.
# Updated in 9.3 to sync with VTECTable entries  
    def ctaDict(self):
        return {
         "AF.W": self.ctaAFW,
         "AF.Y": self.ctaAFY,
         "AS.O": self.ctaASO,
         "AS.Y": self.ctaASY,
         "BH.S": self.ctaBHS,
         "BW.Y": self.ctaBWY,
         "BZ.A": self.ctaBZA,
         "BZ.W": self.ctaBZW,
         "CF.A": self.ctaCFA,
         "CF.W": self.ctaCFW,
         "CF.Y": self.ctaCFY,
         "DS.W": self.ctaDSW,
         "DU.Y": self.ctaDUY,
         "EC.A": self.ctaECA,
         "EC.W": self.ctaECW,
         "EH.A": self.ctaEHA,
         "EH.W": self.ctaEHW,
         "FA.A": self.ctaFAA,
         "FF.A": self.ctaFFA,
         "FG.Y": self.ctaFGY,
         "FR.Y": self.ctaFRY,
         "FW.A": self.ctaFWA,
         "FW.W": self.ctaFWW,
         "FZ.A": self.ctaFZA,
         "FZ.W": self.ctaFZW,
         "GL.A": self.ctaGLA,
         "GL.W": self.ctaGLW,
         "HF.A": self.ctaHFA,
         "HF.W": self.ctaHFW,
         "HT.Y": self.ctaHTY,
         "HU.A": self.ctaHUA,
         "HU.W": self.ctaHUW,
         "HW.A": self.ctaHWA,
         "HW.W": self.ctaHWW,
         "HZ.A": self.ctaHZA,
         "HZ.W": self.ctaHZW,
         "IS.W": self.ctaISW,
         "LE.A": self.ctaLEA,
         "LE.W": self.ctaLEW,
         "LE.Y": self.ctaLEY,
         "LO.Y": self.ctaLOY,
         "LS.A": self.ctaLSA,
         "LS.W": self.ctaLSW,
         "LS.Y": self.ctaLSY,
         "LW.Y": self.ctaLWY,
         "MF.Y": self.ctaMFY,
         "MH.W": self.ctaMHW,
         "MH.Y": self.ctaMHY,
         "MS.Y": self.ctaMSY,
         "RB.Y": self.ctaRBY,
         "RP.S": self.ctaRPS,
         "SC.Y": self.ctaSCY,
         "SE.A": self.ctaSEA,
         "SE.W": self.ctaSEW,
         "SI.Y": self.ctaSIY,
         "SM.Y": self.ctaSMY,
         "SR.A": self.ctaSRA,
         "SR.W": self.ctaSRW,
         "SU.W": self.ctaSUW,
         "SU.Y": self.ctaSUY,
         "SW.Y": self.ctaSWY,
         "TR.A": self.ctaTRA,
         "TR.W": self.ctaTRW,
         "UP.A": self.ctaUPA,
         "UP.W": self.ctaUPW,
         "UP.Y": self.ctaUPY,
         "WC.A": self.ctaWCA,
         "WC.W": self.ctaWCW,
         "WC.Y": self.ctaWCY,
         "WI.Y": self.ctaWIY,
         "WS.A": self.ctaWSA,
         "WS.W": self.ctaWSW,
         "WW.Y": self.ctaWWY,
         "ZF.Y": self.ctaZFY,
         "ZR.Y": self.ctaZRY,
          }


##### PLEASE KEEP PILS IN ALPHABETICAL ORDER #######

# CallToAction PIL dictionary.  The key is the product pil, such as "HLS".
# The entries are available for a particular product.  None of these
# are entered automatically by the formatter, but are available through
# the product editor.
# Users can add additional entries which are accessible in the product 
# editor.  The lists are actually function calls that the user can 
# override if necessary.  
    def ctaPilDict(self):
        return {
          'ADR':  self.ctaPilADR,
          'AFD':  self.ctaPilAFD,
          'AFM':  self.ctaPilAFM,
          'AVA':  self.ctaPilAVA,
          'AVW':  self.ctaPilAVW,
          'CAE':  self.ctaPilCAE,
          'CCF':  self.ctaPilCCF,
          'CDW':  self.ctaPilCDW,
          'CEM':  self.ctaPilCEM,
          'CFW':  self.ctaPilCFW,
          'CWF':  self.ctaPilCWF,
          'EQR':  self.ctaPilEQR,
          'EQW':  self.ctaPilEQW,
          'ESF':  self.ctaPilESF,
          'EVI':  self.ctaPilEVI,
          'FFA':  self.ctaPilFFA,
          'FRW':  self.ctaPilFRW,
          'FWF':  self.ctaPilFWF,
          'FWM':  self.ctaPilFWM,
          'FWS':  self.ctaPilFWS,
          'GLF':  self.ctaPilGLF,
          'HLS':  self.ctaPilHLS,
          'HMW':  self.ctaPilHMW,
          'HWO':  self.ctaPilHWO,
          'LAE':  self.ctaPilLAE,
          'LEW':  self.ctaPilLEW,
          'MWS':  self.ctaPilMWS,
          'MVF':  self.ctaPilMVF,
          'MWW':  self.ctaPilMWW,
          'NOW':  self.ctaPilNOW,
          'NPW':  self.ctaPilNPW,
          'NSH':  self.ctaPilNSH,
          'NUW':  self.ctaPilNUW,
          'OFF':  self.ctaPilOFF,
          'PFM':  self.ctaPilPFM,
          'PNS':  self.ctaPilPNS,
          'RFD':  self.ctaPilRFD,
          'RFW':  self.ctaPilRFW,
          'RHW':  self.ctaPilRHW,
          'SAF':  self.ctaPilSAF,
          'SRF':  self.ctaPilSRF,
          'SFT':  self.ctaPilSFT,
          'SPS':  self.ctaPilSPS,
          'SPW':  self.ctaPilSPW,
          'TOE':  self.ctaPilTOE,
          'VOW':  self.ctaPilVOW,
          'WCN':  self.ctaPilWCN,
          'WSW':  self.ctaPilWSW,
          'ZFP':  self.ctaPilZFP,
          }


#------------------------------------------------------------------------
# CALL TO ACTIONS - winter events
# With the winter weather simplification, specfic winter hazard defs are not
# readily available. Forecaster can choose from the specific defs via the defs below.
# Since these statements are so long, we use the descriptive word format.
#------------------------------------------------------------------------
    def winterWScta(self):
        return [
      ("***HEAVY SNOW", """A Winter Storm Warning for heavy snow means severe winter weather conditions are expected or occurring.  Significant amounts of snow are forecast that will make travel dangerous. Only travel in an emergency. If you must travel...keep an extra flashlight...food...and water in your vehicle in case of an emergency."""),
      ("***SLEET", """A Winter Storm Warning for sleet means that a winter storm system is impacting the area with significant amounts of sleet. Travel is likely to be severely impacted."""),
      ("***MIXED PRECIP", """A Winter Storm Warning means significant amounts of snow...sleet...and ice are expected or occurring. Strong winds are also possible.  This will make travel very hazardous or impossible."""),
              ]

    def winterWWcta(self):
        return [
      ("***BLOWING SNOW", """A Winter Weather Advisory for blowing snow means that visibilities will be limited due to strong winds blowing snow around. Use caution when traveling...especially in open areas."""),      
      ("***SLEET", """A Winter Weather Advisory for sleet means periods of sleet are imminent or occurring. Sleet may cause driving to become extremely dangerous...so be prepared to use caution when traveling."""),
      ("***SNOW AND BLOWING SNOW", """A Winter Weather Advisory for |*lake effect*| snow and blowing snow means that visibilities will be limited due to a combination of falling and blowing snow. Use caution when traveling...especially in open areas."""),
      ("***SNOW", """A Winter Weather Advisory for snow means that periods of snow will cause primarily travel difficulties. Be prepared for snow covered roads and limited visibilities...and use caution while driving."""),
      ("***MIXED PRECIP", """A Winter Weather Advisory means that periods of snow...sleet...or freezing rain will cause travel difficulties. Be prepared for slippery roads and limited visibilities...and use caution while driving."""),
        ]
#------------------------------------------------------------------------
# CALL TO ACTIONS - individual functions for each phen/sig
#------------------------------------------------------------------------
# These are lists of strings.  The first one is used in the formatters,
# the others are available through the call to actions menu.

    def ctaAFW(self):
        return [ 
"""An Ashfall Warning means that significant accumulation of volcanic ash is expected or occurring due to a volcanic eruption or resuspension of previously deposited ash.
 
Seal windows and doors.  Protect electronics and cover air intakes and open water sources.  Avoid driving. Remain indoors unless absolutely necessary.  Use extreme caution clearing rooftops of ash.

Listen to NOAA Weather Radio or local media for further information.""",
        ]

    def ctaAFY(self):
        return [
 """An Ashfall Advisory means that large amounts of ash will be deposited in the advisory area. Persons with respiratory illnesses should remain indoors to avoid inhaling the ash particles...and all persons outside should cover their mouth and nose with a mask or cloth.""",
        ]

    def ctaASO(self):
        return [
 """An Air Stagnation Outlook is issued when an extended period of weather conditions are anticipated that could contribute to poor ventilation...and thus potentially poor air quality.  Be prepared for these conditions to develop in the next 2 to 3 days...and for the issuance of air stagnation advisories as the situation becomes imminent.""",
        ]

    def ctaASY(self):
        return [
 """An Air Stagnation Advisory indicates that due to limited movement of an air mass across the advisory area...pollution will increase to dangerous levels. Persons with respiratory illness should follow their physicians advice for dealing with high levels of air pollution.""",
        ]

    def ctaBHS(self):
        return [
"""A Beach Hazards Statement is issued when threats such as rip currents...longshore currents...sneaker waves and other hazards create life-threatening conditions in the surf zone. Caution should be used when in or near the water.""",
        ] 

    def ctaBWY(self):
        return [
 """A Brisk Wind Advisory means that winds will reach Small Craft Advisory criteria in areas that are primarily ice covered. Moving ice floes could damage small craft.""",
        ]

    def ctaBZA(self):
        return [
"""A Blizzard Watch means there is a potential for falling and/or blowing snow with strong winds and extremely poor visibilities. This can lead to whiteout conditions and make travel very dangerous.""",
        ]

    def ctaBZW(self):
        return [
 """A Blizzard Warning means severe winter weather conditions are expected or occurring. Falling and blowing snow with strong winds and poor visibilities are likely. This will lead to whiteout conditions...making travel extremely dangerous. Do not travel. If you must travel...have a winter survival kit with you. If you get stranded...stay with your vehicle.""",
        ]

    def ctaCFA(self):
        return [
 """A Coastal Flood Watch means that conditions favorable for flooding are expected to develop. Coastal residents should be alert for later statements or warnings...and take action to protect property.""",
        ]

    def ctaCFW(self):
        return [
 """A Coastal Flood Warning means that flooding is occurring or imminent. Coastal residents in the warned area should be alert for rising water...and take appropriate action to protect life and property.""",
        ]

    def ctaCFY(self):
        return [
 """A Coastal Flood Advisory indicates that onshore winds and tides will combine to generate flooding of low areas along the shore.""",
        ]

    def ctaDSW(self):
        return [
 """A Dust Storm Warning means severely limited visibilities are expected with blowing dust. Travel could become extremely dangerous. Persons with respiratory problems should make preparations to stay indoors until the storm passes.""",
        ]

    def ctaDUY(self):
        return [
 """A Blowing Dust Advisory means that blowing dust will restrict visibilities. Travelers are urged to use caution.""",
        ]

    def ctaECA(self):
        return [
 """An Extreme Cold Watch means that prolonged periods of very cold temperatures are expected. Ensure that outdoor animals have warm shelter...and that children wear a hat and gloves.""",
        ]

    def ctaECW(self):
        return [
 """An Extreme Cold Warning means that dangerously low temperatures are expected for a prolonged period of time. Frostbite and hypothermia are likely if exposed to these temperatures...so make sure a hat...facemask...and heavy gloves or mittens are available.""",
        ]

    def ctaEHA(self):
        return [
 """An Excessive Heat Watch means that a prolonged period of hot temperatures is expected. The combination of hot temperatures and high humidity will combine to create a DANGEROUS SITUATION in which heat illnesses are possible. Drink plenty of fluids...stay in an air-conditioned room...stay out of the sun...and check up on relatives and neighbors.""",
 """Young children and pets should never be left unattended in vehicles under any circumstances.  This is especially true during warm or hot weather when car interiors can reach lethal temperatures in a matter of minutes.""", 
        ]

    def ctaEHW(self):
        return [
 """Take extra precautions...if you work or spend time outside. When possible...reschedule strenuous activities to early morning or evening. Know the signs and symptoms of heat exhaustion and heat stroke. Wear light weight and loose fitting clothing when possible and drink plenty of water.\n\n
    To reduce risk during outdoor work...the occupational safety and health administration recommends scheduling frequent rest breaks in shaded or air conditioned environments. Anyone overcome by heat should be moved to a cool and shaded location.   Heat stroke is an emergency...call 9 1 1.\n\n
    An Excessive Heat Warning means that a prolonged period of dangerously hot temperatures will occur. The combination of hot temperatures and high humidity will combine to create a DANGEROUS SITUATION in which heat illnesses are likely. Drink plenty of fluids...stay in an air-conditioned room...stay out of the sun...and check up on relatives and neighbors.\n\n
    Young children and pets should never be left unattended in vehicles under any circumstances.  This is especially true during warm or hot weather when car interiors can reach lethal temperatures in a matter of minutes.""",
        ]

    def ctaFAA(self):
        return [
 """A Flood Watch means there is a potential for flooding based on current forecasts.\n\nYou should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.""",
        ]

    def ctaFFA(self):
        return [
 """A Flash Flood Watch means that conditions may develop that lead to flash flooding. Flash flooding is a VERY DANGEROUS SITUATION.\n\nYou should monitor later forecasts and be prepared to take action should Flash Flood Warnings be issued.""",
        ]

    def ctaFGY(self):
        return [
"""A Dense Fog Advisory means visibilities will frequently be reduced to less than one quarter mile. If driving...slow down...use your headlights...and leave plenty of distance ahead of you.""",
        ]

    def ctaFRY(self):
        return [
 """A Frost Advisory means that widespread frost is expected. Sensitive outdoor plants may be killed if left uncovered.""",
        ]

    def ctaFWA(self):
        return [
 """A Fire Weather Watch means that critical fire weather conditions are forecast to occur. Listen for later forecasts and possible Red Flag Warnings.""",
        ]

    def ctaFWW(self):
        return [
 """A Red Flag Warning means that critical fire weather conditions are either occurring now....or will shortly. A combination of strong winds...low relative humidity...and warm temperatures can contribute to extreme fire behavior.""",
        ]

    def ctaFZA(self):
        return [
 """A Freeze Watch means sub-freezing temperatures are possible. These conditions could kill crops and other sensitive vegetation.""",
        ]

    def ctaFZW(self):
        return [
 """A Freeze Warning means sub-freezing temperatures are imminent or highly likely. These conditions will kill crops and other sensitive vegetation.""",
        ]

    def ctaGLA(self):
        return [
 """A Gale Watch is issued when the risk of gale force winds of 34 to 47 knots has significantly increased...but the specific timing and/or location is still uncertain. It is intended to provide additional lead time for mariners who may wish to consider altering their plans.""",
        ]

    def ctaGLW(self):
        return [
 """A Gale Warning means winds of 34 to 47 knots are imminent or occuring. Operating a vessel in gale conditions requires experience and properly equipped vessels. It is highly recommended that mariners without the proper experience seek safe harbor prior to the onset of gale conditions.""",
        ]

    def ctaHFA(self):
        return [
 """A Hurricane Force Wind Watch is issued when the risk of hurricane force winds of 64 knots or greater has significantly increased...but the specific timing and/or location is still uncertain.  It is intended to provide additional lead time for mariners who may wish to consider altering their plans.""",
        ]

    def ctaHFW(self):
        return [
 """A Hurricane Force Wind Warning means winds of 64 knots or greater are imminent or occurring. All vessels should remain in port...or take shelter as soon as possible...until winds and waves subside.""",
        ]

    def ctaHTY(self):
        return [
 """A Heat Advisory means that a period of hot temperatures is expected. The combination of hot temperatures and high humidity will combine to create a situation in which heat illnesses are possible. Drink plenty of fluids...stay in an air-conditioned room...stay out of the sun...and check up on relatives and neighbors.\n\n
    Take extra precautions...if you work or spend time outside. When possible...reschedule strenuous activities to early morning or evening. Know the signs and symptoms of heat exhaustion and heat stroke. Wear light weight and loose fitting clothing when possible and drink plenty of water.\n\n
    To reduce risk during outdoor work...the occupational safety and health administration recommends scheduling frequent rest breaks in shaded or air conditioned environments. Anyone overcome by heat should be moved to a cool and shaded location.   Heat stroke is an emergency...call 9 1 1.""",
        ]   

    def ctaHUA(self):
        return [
 """A Hurricane Watch is issued when sustained winds of |* 64 kts or 74 mph *| or higher associated with a hurricane are possible within 48 hours.""",
        ] 

    def ctaHUW(self):
        return [
 """A Hurricane Warning means sustained winds of |* 64 kts or 74 mph *| or higher associated with a hurricane are expected within 36 hours. A Hurricane Warning can remain in effect when dangerously high water or a combination of dangerously high water and exceptionally high waves continue...even though winds may be less than hurricane force.""",
        ] 
    
    def ctaHWA(self):
        return [
 """A High Wind Watch means there is the potential for a hazardous high wind event. Sustained winds of at least 40 mph...or gusts of 58 mph or stronger may occur. Continue to monitor the latest forecasts.""",
        ]

    def ctaHWW(self):
        return [
 """A High Wind Warning means a hazardous high wind event is expected or occurring. Sustained wind speeds of at least 40 mph or gusts of 58 mph or more can lead to property damage.""",
        ]

    def ctaHZA(self):
        return [
 """A Hard Freeze Watch means sub-freezing temperatures are possible. These conditions could kill crops and other sensitive vegetation.""",
        ]

    def ctaHZW(self):
        return [
 """A Hard Freeze Warning means sub-freezing temperatures are imminent or highly likely. These conditions will kill crops and other sensitive vegetation.""",
        ]

    def ctaISW(self):
        return [
 """An Ice Storm Warning means severe winter weather conditions are expected or occurring. Significant amounts of ice accumulations will make travel dangerous or impossible. Travel is strongly discouraged. Commerce will likely be severely impacted. If you must travel...keep an extra flashlight...food...and water in your vehicle in case of an emergency. Ice accumulations and winds will likely lead to snapped power lines and falling tree branches that add to the danger.""",
        ]

    def ctaLEA(self):
         return [
 """A Lake Effect Snow Watch means there is a potential for a large amount of snow in only a few hours. Visibilities and depth of snow can vary greatly...impacting travel significantly. Continue to monitor the latest forecasts.""",
        ]

    def ctaLEW(self):
        return [
 """A Lake Effect Snow Warning means significant amounts of lake-effect snow are forecast that will make travel very hazardous or impossible. Lake-effect snow showers typically align themselves in bands and will likely be intense enough to drop 1 to several inches of snow per hour for several hours. Visibilities vary greatly and can drop to zero within minutes.  Travel is strongly discouraged. Commerce could be severely impacted. If you must travel...keep an extra flashlight...food...and water in your vehicle in case of an emergency.""",
        ]

    def ctaLEY(self):
        return [
 """A Lake Effect Snow Advisory means lake-effect snow is forecast that will make travel difficult in some areas. Lake-effect snow showers typically align themselves in bands and will likely be intense enough to drop several inches in localized areas. Use caution when traveling.""",
        ]

    def ctaLOY(self):
        return [
 """A Low Water Advisory means water levels are expected to be significantly below average. Mariners should use extreme caution and transit at the slowest safe navigable speed to minimize impact.""",
        ]

    def ctaLSA(self):
        return [
 """A Lakeshore Flood Watch means that conditions favorable for lakeshore flooding are expected to develop. Residents on or near the shore should take action to protect property...and listen for later statements or warnings.""",
        ]

    def ctaLSW(self):
        return [
 """A Lakeshore Flood Warning means that flooding is occurring or imminent along the lake. Residents on or near the shore in the warned area should be alert for rising water...and take appropriate action to protect life and property.""",
        ]

    def ctaLSY(self):
        return [
 """A Lakeshore Flood Advisory indicates that onshore winds will generate flooding of low areas along the lakeshore.""",
        ]

    def ctaLWY(self):
        return [
 """A Lake Wind Advisory indicates that winds will cause rough chop on area lakes. Small boats will be especially prone to capsizing.""",
        ]

    def ctaMHW(self):
        return [
 """An Ashfall Warning means that significant accumulation of ashfall is expected on vessels. It is recommended that vessels be prepared to take the necessary counter measures before putting to sea or entering the warning area.""",
        ]

    def ctaMFY(self):
        return [
"""A Dense Fog Advisory means visibilities will frequently be reduced to less than one mile. Inexperienced mariners...especially those operating smaller vessels should avoid navigating in these conditions. """,
        ]

    def ctaMHY(self):
        return [
"""An Ashfall Advisory means that a light accumulation of ashfall is expected on vessels. It is recommended that vessels be prepared to take appropriate counter measures before putting to sea or entering the advisory area.""",
        ]

    def ctaMSY(self):
        return [
 """A Dense Smoke Advisory means widespread fires will create smoke...limiting visibilities. Inexperienced mariners...especially those operating smaller vessels should avoid navigating in these conditions.""",
        ]

    def ctaRBY(self):
        return [
 """A Small Craft Advisory for rough bar means that wave conditions are expected to be hazardous to small craft in or near harbor entrances.""",
        ]

    def ctaRPS(self):
        return [
 """There is a high risk of rip currents.

Rip currents are powerful channels of water flowing quickly away from shore...which occur most often at low spots or breaks in the sandbar and in the vicinity of structures such as groins...jetties and piers. Heed the advice of lifeguards...beach patrol flags and signs.

If you become caught in a rip current...yell for help. Remain calm...do not exhaust yourself and stay afloat while waiting for help. If you have to swim out of a rip current...swim parallel to shore and back toward the beach when possible. Do not attempt to swim directly against a rip current as you will tire quickly. """,               
        ]

    def ctaSCY(self):
        return [
 """A Small Craft Advisory means that wind speeds of 21 to 33 knots are expected to produce hazardous wave conditions to small craft. Inexperienced mariners...especially those operating smaller vessels should avoid navigating in these conditions.""",
        ]

    def ctaSEA(self):
        return [
 """A Hazardous Seas Watch is issued when the risk of hazardous seas has significantly increased...but the specific timing and/or location is still uncertain.  It is intended to provide additional lead time for mariners who may wish to consider altering their plans.""",
        ]

    def ctaSEW(self):
        return [
 """A Hazardous Seas Warning means hazardous sea conditions are imminent or occurring. Recreational boaters should remain in port...or take shelter until waves subside.  Commercial vessels should prepare for rough seas and consider remaining in port or taking shelter in port until hazardous seas subside.""",
        ]

    def ctaSIY(self):
        return [
 """A Small Craft Advisory for wind means that wind speeds of 21 to 33 knots are expected. Inexperienced mariners...especially those operating smaller vessels should avoid navigating in these conditions.""",
        ]

    def ctaSMY(self):
        return [
 """A Dense Smoke Advisory means widespread fires will create smoke...limiting visibilities. If driving...slow down...use your headlights...and leave plenty of distance ahead of you in case a sudden stop is needed.""",
        ]

    def ctaSRA(self):
        return [
 """A Storm Watch is issued when the risk of storm force winds of 48 to 63 knots has significantly increased...but the specific timing and/or location is still uncertain.  It is intended to provide additional lead time for mariners who may wish to consider altering their plans.""",
        ]

    def ctaSRW(self):
        return [
 """A Storm Warning means winds of 48 to 63 knots are imminent or occuring. Recreational boaters should remain in port...or take shelter until winds and waves subside. Commercial vessels should prepare for very strong winds and dangerous sea conditions...and consider remaining in port or taking shelter in port until winds and waves subside.""",
        ]

    def ctaSUW(self):
        return [
 """A High Surf Warning indicates that dangerous...battering waves will pound the shoreline. This will result in life-threatening conditions.""",
        ]

    def ctaSUY(self):
        return [
 """A High Surf Advisory means that high surf will affect beaches in the advisory area...producing localized beach erosion and dangerous swimming conditions.""",
        ]

    def ctaSWY(self):
        return [
 """A Small Craft Advisory for hazardous seas means that waves are expected to be hazardous to small craft. Mariners should avoid shoaling areas. Long period swell can sharpen into large breaking waves in shoaling areas. It is not unusual for waves to break much farther from shoaling areas than is normally experienced. Remember...breaking waves can easily capsize even larger vessels.""",
        ]

    def ctaTRA(self):
        return [
 """A Tropical Storm Watch means sustained winds of |* 34 to 63 kt or 39 to 73 mph or 63 to 118 km per hr *| are possible due to a tropical storm within 48 hours.""",
        ] 

    def ctaTRW(self):
        return [
 """A Tropical Storm Warning means sustained winds of |* 34 to 63 kt or 39 to 73 mph or 63 to 118 km per hr *| are expected due to a tropical storm within 36 hours.""",
        ] 

    def ctaUPA(self):
        return [
 """A Heavy Freezing Spray Watch is issued when the risk of heavy freezing spray has significantly increased...but the specific timing and/or location is still uncertain.  It is intended to provide additional lead time for mariners who may wish to consider altering their plans.""",
        ]

    def ctaUPW(self):
        return [
 """A Heavy Freezing Spray Warning means heavy freezing spray is expected to rapidly accumulate on vessels. These conditions can be extremely hazardous to navigation. It is recommended that mariners not trained to operate in these conditions or vessels not properly equiped to do so...remain in port or avoid the waring area.""",
        ]

    def ctaUPY(self):
        return [
 """A Freezing Spray Advisory means that light to moderate accumulation of ice is expected on vessels. Operating a vessel in freezing spray can be hazardous. It is recommended that vessels be prepared to take appropriate counter measures before putting to sea or enter the advisory area.""",
        ]

    def ctaWCA(self):
        return [
 """A Wind Chill Watch means the there is the potential for a combination of very cold air and strong winds to create dangerously low wind chill values. Monitor the latest forecasts and warnings for updates on this situation.""",
        ]

    def ctaWCW(self):
        return [
 """A Wind Chill Warning means the combination of very cold air and strong winds will create dangerously low wind chill values. This will result in frost bite and lead to hypothermia or death if precautions are not taken.""",
        ]

    def ctaWCY(self):
        return [
 """A Wind Chill Advisory means that very cold air and strong winds will combine to generate low wind chills. This will result in frost bite and lead to hypothermia if precautions are not taken.  If you must venture outdoors...make sure you wear a hat and gloves.""",
        ]

    def ctaWIY(self):
        return [
 """A Wind Advisory means that winds of 35 mph are expected. Winds this strong can make driving difficult...especially for high profile vehicles. Use extra caution.""",
        ]

    def ctaWSA(self):
        return [
 """A Winter Storm Watch means there is a potential for significant snow...sleet...or ice accumulations that may impact travel. Continue to monitor the latest forecasts.""",
        ]

    def ctaWSW(self):
        return [
 """|*Choose the appropriate CTA below and delete the rest*|

A Winter Storm Warning for heavy snow means severe winter weather conditions are expected or occurring.  Significant amounts of snow are forecast that will make travel dangerous. Only travel in an emergency. If you must travel...keep an extra flashlight...food...and water in your vehicle in case of an emergency.

A Winter Storm Warning means significant amounts of snow...sleet...and ice are expected or occurring. Strong winds are also possible.  This will make travel very hazardous or impossible.

A Winter Storm Warning for sleet means that a winter storm system is impacting the area with significant amounts of sleet. Travel is likely to be severely impacted.""",
        ] 

    def ctaWWY(self):
        return [
 """|*Choose the appropriate CTA below and delete the rest*|

A Winter Weather Advisory means that periods of snow...sleet...or freezing rain will cause travel difficulties. Be prepared for slippery roads and limited visibilities...and use caution while driving.

A Winter Weather Advisory for blowing snow means that visibilities will be limited due to strong winds blowing snow around. Use caution when traveling...especially in open areas.
 
A Winter Weather Advisory for sleet means periods of sleet are imminent or occurring. Sleet may cause driving to become extremely dangerous...so be prepared to use caution when traveling.

A Winter Weather Advisory for |*lake effect*| snow and blowing snow means that visibilities will be limited due to a combination of falling and blowing snow. Use caution when traveling...especially in open areas.

A Winter Weather Advisory for snow means that periods of snow will cause primarily travel difficulties. Be prepared for snow covered roads and limited visibilities...and use caution while driving.""",

        ] 
        
    def ctaZFY(self):
        return [
 """A Freezing Fog Advisory means visibilities will frequently be reduced to less than one quarter mile. If driving...slow down...use your headlights...and leave plenty of distance ahead of you. Also...be alert for frost on bridge decks causing slippery roads.""",
        ]

    def ctaZRY(self):
        return [
 """A Freezing Rain Advisory means that periods of freezing rain or freezing drizzle will cause travel difficulties. Be prepared for slippery roads. Slow down and use caution while driving.""",
        ]

#------------------------------------------------------------------------
# CALL TO ACTIONS - individual functions for each product pil
#------------------------------------------------------------------------
# These are lists of strings.  These are available through the call to 
# actions menu.

    def ctaPilADR(self):
        return [
        ]

    def ctaPilAFD(self):
        return [
        ]

    def ctaPilAFM(self):
        return [
        ]

    def ctaPilAVA(self):
        return [
        ]

    def ctaPilAVW(self):
        return [
        ]

    def ctaPilCAE(self):
        return [
        ]

    def ctaPilCCF(self):
        return [
        ]

    def ctaPilCDW(self):
        return [
        ]

    def ctaPilCEM(self):
        return [
        ]

    def ctaPilCFW(self):
        return [("***RIP CURRENTS CTA", """Rip currents are powerful channels of water flowing quickly away from shore...which occur most often at low spots or breaks in the sandbar and in the vicinity of structures such as groins...jetties and piers. Heed the advice of lifeguards...beach patrol flags and signs.

If you become caught in a rip current...yell for help. Remain calm...do not exhaust yourself and stay afloat while waiting for help. If you have to swim out of a rip current...SWIM PARALLEL TO SHORE and back toward the beach when possible. Do not attempt to swim directly against a rip current as you will tire quickly."""),
 ("***LONGSHORE CURRENTS CTA", """Longshore currents commonly occur when waves approach the shoreline at an angle. They can be particularly dangerous near a jetty or pier."""),
 ("***SNEAKER WAVES CTA", """Add CTA here."""),
 ("***RED TIDE CTA", """Add CTA here"""),
 ("***SEA NETTLES CTA", """Add CTA here"""),
 ("***TSUNAMI DEBRIS CTA", """Add CTA here"""), 
 ("***OTHER BEACH HAZARDS CTA", """Add CTA here"""),
        ]

    def ctaPilCWF(self):
        return [
        ]

    def ctaPilEQR(self):
        return [
        ]

    def ctaPilESF(self):
        return [
        ]

    def ctaPilEQW(self):
        return [
        ]

    def ctaPilEVI(self):
        return [
        ]

    def ctaPilFFA(self):
        return [
        ]

    def ctaPilFRW(self):
        return [
        ]

    def ctaPilFWF(self):
        return [
        ]

    def ctaPilFWM(self):
        return [
        ]

    def ctaPilFWS(self):
        return [
        ]

    def ctaPilGLF(self):
        return [
        ]

    def ctaPilHLS(self):
        return [("***MINOR FLOODING", """Residents can expect minor flooding of roads...especially those with poor drainage. Known intersections with very poor drainage may have water levels up to 3 feet. Other poor drainage areas will have water rises of 1 foot."""),
 ("***WIDESPREAD FLOODING", """Residents can expect widespread flooding.  In poor drainage areas...minor to moderate property damage is expected...and several main thoroughfares may be closed.  Known intersections with very poor drainage may have water levels up to 5 feet.  Other poor drainage areas will have water rises up to 3 feet.  Levels will rise 1 foot elsewhere."""),
 """Small streams will surpass bank full...but only for one hour or less.""",
 ("***WIDESPREAD STREAM FLOODING", """Most small streams and creeks will surpass bank full...for up to 3 hours.  Larger rivers will rise...and those which respond quickly to very heavy rain may briefly exceed flood stage."""),
 ("***PRIOR NOTICE OF EXTENSIVE AREAL FLOODING", """Extensive flooding is expected |**today or tonight or next day**|. \n\n Persons living near or in poor drainage locations should prepare for possible evacuation later |**today or tonight or next day**|. In these areas...significant property damage will occur...and some power outages are likely.  Minor property damage is possible elsewhere. \n\nWater levels in very poor drainage areas will approach 7 feet.  Other poor drainage locations will have rises between 3 and 5 feet.  Elsewhere...expect water rises to near 2 feet.  Numerous main roads will be closed.  Driving is highly discouraged except for emergencies."""),
 ("***DANGEROUS FLOODING", """This is a dangerous flood situation!  \n\nPersons living in or near poor drainage areas should evacuate immediately.  Significant property damage will occur in these locations.  Minor property damage is possible in other areas.  Some power outages are expected. \n\n Water levels in very poor drainage areas will approach 7 feet.  Other poor drainage locations will have rises between 3 and 5 feet.  Elsewhere...expect water rises to near 2 feet.  Numerous main roads will be closed.  Driving is highly discouraged until well after flood waters recede. \n\n Move to safety immediately."""),
 ("***PRIOR NOTICE OF EXTENSIVE RIVER FLOODING", """Extensive flooding is expected |**today or tonight or next day**|. \n\n By |**time**|...all small streams and creeks will have surpassed bank full.  These conditions will last between 3 and 6 hours.  Some streams will exceed their banks by several feet and may flood nearby homes.  Evacuations are possible.\n\n Rivers in affected areas will rise...with some reaching or exceeding flood stage.  Normally quick-rising rivers will exceed flood stage by several feet...flooding homes along the riverside.  Pastures will also flood...but livestock losses should be minimal.  Several secondary roads and bridges will be washed out.  Driving is highly discouraged."""),
 ("***DANGEROUS RIVER FLOODING", """This is a DANGEROUS SITUATION!  \n\nAll streams...creeks..and some rivers will surpass bankfull...for between 3 and 6 hours.  Some streams will exceed their banks by several feet...flooding nearby homes.  Evacuations are possible. \n\n Rivers in affected areas will rise...with some reaching or exceeding flood stage.  Normally quick rising rivers will exceed flood stage by several feet...flooding homes along the riverside.  Pastures will also flood...but livestock losses should be minimal."""),
 ("***CATASTROPHIC FLOODING EXPECTED", """Catastrophic flooding is expected later |**edit day or night periods**|. \n\n A state of emergency has been issued |**by agency**| for |**edit area here**|. \n\n Residents in flood prone areas should rush to completion preparations to protect their property...then move to a place of safety...this |**edit time period**|. Mandatory evacuations are underway. \n\n |** opening paragraph describing antecedent rainfall and expected heavier rainfall **| \n\n life threatening flooding is likely!  In urban areas...extensive property damage will occur in all poor drainage areas...with moderate to major property damage elsewhere.  Widespread power outages are likely. \n\n In rural locations...all streams...creeks...and arroyos will surpass bank full for more than 6 hours.  Each will exceed their banks by several feet...flooding homes...even those up to one half mile away from the banks. \n\n In all areas...hundreds of roads will flood.  Dozens of secondary roads may become washed out in rural areas.  Numerous low water bridges will likely wash out as well. \n\n Water levels will exceed 5 feet in all poor drainage urban areas...and average at least 2 feet elsewhere.  All rivers in affected areas will rise...and most will exceed flood stage.  Quick rising rivers will exceed flood stage...and reach near record crests...causing inundation of nearby homes.  In rural locations...extensive pastureland flooding will occur as water levels rise to 2 feet or more.  Widespread livestock losses are likely."""),
 ("***CATASTROPHIC FLOODING OCCURRING", """Catastrophic flooding is occurring in |**edit area**|. \n\n States of emergency remain in effect for the following locations: \n\n |**edit counties and cities here**| \n\n residents remain prohibited from venturing out.  Law enforcement and |**military support group edit here**| evacuations are now underway. \n\n This remains a life threatening situation!  Extensive property damage is occurring in all poor drainage areas.  Elsewhere...moderate to major property damage is occurring.  Hundreds of roads are closed...and some are likely damaged.  Several area bridges are washed out.  Streams...creeks...and arroyos are several feet above bank full...and will remain so for hours.  Many rivers are nearing flood stage...and some have already surpassed it.  Homes near these rivers are likely flooded. Flood waters will continue for several more hours. \n\n Water levels are in excess of 5 feet in all poor drainage areas.  Elsewhere...average water levels are at least 2 feet. Power outages are widespread. \n\n Stay tuned to NOAA Weather Radio for further information on this dangerous flood.  Heed all evacuation orders from law enforcement or military personnel."""),
 ("***GENERATOR PRECAUTIONS", """If you plan on using a portable generator...be sure to observe all safety precautions to avoid carbon monoxide poisoning...electrocution...or fire.  Be sure to operate your generator in a dry outdoor area away from windows...doors and vents. Carbon monoxide poisoning deaths can occur due to improperly located portable generators!"""),
 ("***FLAMMABLES PRECAUTION", """Flammable liquids such as gasoline or kerosene should only be stored outside of the living areas in properly labeled...non glass safety containers.  Do not store in an attached garage as gas fumes can travel into the home and potentially ignite...especially if the home has natural or propane gas lines that could become damaged during the hurricane."""),
 ("***HURRICANE WARNING DEFINITION", """A Hurricane Warning means sustained winds of |* 64 kts or 74 mph *| or higher associated with a hurricane are expected within 36 hours. A Hurricane Warning can remain in effect when dangerously high water or a combination of dangerously high water and exceptionally high waves continue...even though winds may be less than hurricane force."""),
 ("***HURRICANE WATCH DEFINITION", """A Hurricane Watch is issued when sustained winds of |* 64 kts or 74 mph *| or higher associated with a hurricane are possible within 48 hours."""),
 ("***HURRICANE WIND WARNING DEFINITION", """A Hurricane Wind Warning is issued when a landfalling hurricane is expected to spread hurricane force winds well inland. Serious property damage...power outages...blowing debris...and fallen trees can be expected as winds reach or exceed 74 mph."""),
 ("***HURRICANE WIND WATCH DEFINITION", """A Hurricane Wind Watch is issued when a landfalling hurricane is expected to spread hurricane force winds well inland within the next 48 hours. Prepare for winds in excess of 74 mph."""),
 ("***TROPICAL STORM WARNING DEFINITION", """A Tropical Storm Warning means sustained winds of |* 34 to 63 kt or 39 to 73 mph or 63 to 118 km per hr *| are expected due to a tropical cyclone within 36 hours."""),
 ("***TROPICAL STORM WIND WARNING DEFINITION", """A Tropical Storm Wind Warning means winds of 39 to 73 mph are expected due to a landfalling hurricane or tropical storm. Winds of this magnitude are likely to cause sporadic power outages...fallen trees...minor property damage...and dangerous driving conditions for high profile vehicles."""),
 ("***TROPICAL STORM WATCH DEFINITION", """A Tropical Storm Watch means sustained winds of |* 34 to 63 kt or 39 to 73 mph or 63 to 118 km per hr *| are possible due to a tropical cyclone within 48 hours."""),
 ("***TROPICAL STORM WIND WATCH DEFINITION", """A Tropical Storm Wind Watch means winds of 39 to 73 mph are expected due to a landfalling hurricane or tropical storm within 48 hours."""),
        ]

    def ctaPilHMW(self):
        return [
        ]

    def ctaPilHWO(self):
        return [
        ]

    def ctaPilLAE(self):
        return [
        ]

    def ctaPilLEW(self):
        return [
        ]

    def ctaPilMWS(self):
        return [
        ]

    def ctaPilMWW(self):
        return [
 """Mariners should pay close attention to the marine forecast...and consider wind and sea conditions in planning.""",
        ]

    def ctaPilMVF(self):
        return [
        ]

    def ctaPilNOW(self):
        return [
        ]

    def ctaPilNPW(self):
        return [
        ]

    def ctaPilNSH(self):
        return [
        ]

    def ctaPilNUW(self):
        return [
        ]

    def ctaPilOFF(self):
        return [
        ]

    def ctaPilPFM(self):
        return [
        ]

    def ctaPilPNS(self):
        return [
        ]

    def ctaPilRFD(self):
        return [
        ]

    def ctaPilRFW(self):
        return [
        ]

    def ctaPilRHW(self):
        return [
        ]

    def ctaPilSAF(self):
        return [
        ]

    def ctaPilSRF(self):
        return [
        ]

    def ctaPilSFT(self):
        return [
        ]

    def ctaPilSPS(self):
        return [
        ]

    def ctaPilSPW(self):
        return [
        ]

    def ctaPilTOE(self):
        return [
        ]

    def ctaPilVOW(self):
        return [
        ]

    def ctaPilWCN(self):
        return [
        ]

    def ctaPilWSW(self):
        return [
        ]

    def ctaPilZFP(self):
        return [
        ]
