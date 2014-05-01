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
# -----------------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# BUF_WxTool - Version 2.1
#
# Author: John Rozbicki NWS BUF
# Date Last Modified: 08/25/08
#
# History of Changes:
#
# Version 2.1 (08/25/08) - Made the following changes:
#
#                          (1) Added code so that the tool just produces a single pop-up error message if
#                              thunderstorms (T) or severe thunderstorms (T+) are selected, along with
#                              multiple invalid thunderstorm attributes. Previously, the tool would generate
#                              an error message for each invalid attribute.
#
#                          (2) Added new checks to allow the tool to run to completion if the user selects:
#
#                              (a) Areal coverage qualifiers; and
#                              (b) A weather type of R, S, ZR, IP, L, or ZL; and
#                              (c) A valid alternate precipitation coverage.
#
#                              Previously, the tool would just ignore any alternate precipitation coverages,
#                              and return a pop-up error message if just conditions (a) and (b) were met. Now,
#                              the alternate precipitation coverage is also checked - an error message is returned
#                              only if conditions (a) and (b) are met and a valid alternate coverage is NOT
#                              selected.
#
#                          (3) Changed the error message returned when the user selects a coverage of "Iso",
#                              "Sct" or "Num" with a precip type of R, S, ZR, IP, L, or ZL. Previously, this
#                              message stated that all areal coverage qualifiers were not valid for these weather
#                              types; this really isn't true. The change makes the output less confusing for the
#                              user.
#
#                          (4) Made some minor miscellaneous changes to the tool documentation and other
#                              pop-up error messages.
#
# Version 2.0 (03/27/06) - Made the following changes:
#
#                          (1) Added the ability to override the default probability/coverage
#                              obtained from the corresponding PoP grid if desired. This can be done
#                              separately for each individual weather type. This allows the easy creation
#                              of weather types such as "Lkly RW:Chc SW". This approach also allows for
#                              the use of other weather types (F, L, and ZL) which aren't necessarily PoP-
#                              dependent.
#
#                          (2) In conjunction with (1), added 3 new weather types (F, L, and ZL). Also
#                              added coverage terms ("Patchy", "Areas") which are specific to these elements.
#
#                          (3) Added very light (--) to the list of intensities. This makes in possible
#                              to assign weather types such as RW-- (sprinkles) and SW-- (flurries).
#
#                          (4) In conjunction with the 3 changes listed above, added the following internal
#                              checks to ensure that the tool does not create invalid weather types:
#
#                              (a) A check for the use of "Patchy" and "Areas" coverages with weather
#                                  types other than L, ZL, and F. (prevents invalid types such
#                                  as "Patchy R").
#
#                              (b) A check for the use of Areal Coverage qualifiers other than "Patchy",
#                                  "Areas", and "Wide" with L, ZL, and F (prevents invalid types such as
#                                  "Iso ZL").
#
#                              (c) A check for the use of very light, light, or moderate intensities with
#                                  F (none of which are valid).
#
#                              If the conditions in (a) or (b) are met, the tool will display a
#                              message indicating that the proposed weather type is invalid,
#                              and exits without modifying the Wx grid.
#
#                              If the condition in (c) is met, the tool will display a message indicating
#                              that the proposed intensity cannot be assigned to fog. It will also
#                              automatically change the intensity to <NoInten> without any further action
#                              on the part of the forecaster.
#
#                          (5) Added the ability to select alternate prob/cov terms (i.e., other than
#                              "Def" or "Wide") if categorical (>=74.5%) PoPs are used. In such cases,
#                              the following additional options are now available: "Ocnl", "Frq", "Brf",
#                              "Pds", and "Inter". When used, this setting will apply to all categorical
#                              precip coverages that are derived from the PoP grid.
#
#                          (6) Added the ability to specify an alternate probability/coverage
#                              for T if desired. This setting will override the default probability/
#                              coverage obtained from the corresponding PoP grid, and makes possible
#                              to create precip types such as "Def RW:Chc T".
#
#                          (7) Added an Tornadoes to the list of thunderstorm attributes. A check was also
#                              added to prevent the use of the Tornadoes attribute with non-severe
#                              thunderstorms.
#
#                          (8) Modified the tool to prevent the use of the SmA (small hail) and
#                              GW (gusty winds) thunderstorm attributes with severe thunderstorms (T+).
#
#                          (9) Modified the tool to now display a pop-up message if the user tries to
#                              select invalid thunderstorm attributes. If this occurs, the tool will still
#                              automatically ignore such attributes and run to completion as before.
#
#                          (10) Made the minimum PoP threshold for SChc weather site-configurable. This
#                               allows sites to tailor this setting to meet their individual needs. The
#                               default setting remains 14.5% - the Eastern Region standard.
#
#                          IMPORTANT: The above enhancements resulted in numerous changes to the tool GUI and
#                          the tool code. If you have previously customized the tool for your site,
#                          it is strongly recommended that you use the following procedure when upgrading:
#
#                          (1) Save off a copy of the existing BUF_WxTool under a different name.
#                          (2) Install Version 2.0 of the BUF_WxTool.
#                          (3) After installing Version 2.0, merge any local changes back into the tool
#                              (if needed).
# 
# Version 1.2 (02/23/05) - Updated for IFPS 16.2. Hail (A) is no longer a separate
#                          weather type in GFE, so removed separate handling of
#                          hail attributes from other thunderstorm attributes. This
#                          resulted in a simplification of the code, and the removal
#                          of all references to "hailstring". Also, fixed a potential
#                          bug which could result in "DmgW' not being assigned even if
#                          it was selected by the forecaster.
#
# Version 1.1 (03/21/03) - Changed the PoP thresholds to match the precision of the
#                          gridded data. For example, the threshold for Chc/Sct was
#                          25% before the change, with the change it's now 24.5%.
#
#                          The reason for the change: in Version 1.0, the tool would
#                          assign an incorrect Wx value for PoP values just below the
#                          thresholds; for example, "SChc" or "Iso" Wx would be
#                          assigned for PoPs in the 24.5%-24.9% range. With the change,
#                          the tool assigns the correct Wx for such PoP values - i.e.
#                          "Chc" or "Sct" Wx.
# -------------------------------------------------------------------------------------------
#
# ----- BEGIN SITE CONFIGURATION SECTION -----
#
# Here you can configure the minimum PoP threshold for SChc wx.
# The default is the ER-standard setting of 14.5%.
#
SChc_min_PoP_threshold = 14.5
#
# ------ END SITE CONFIGURATION SECTION ------

ToolType = "numeric"
WeatherElementEdited = "Wx"
from numpy import *

ScreenList = [""]

##Set up variables to be solicited from the user:

VariableList = [
         ("Qualifier\nType:", "Prob", "radio",
          ["Prob", "Cov"]),
         ("Alter\nTerms\nCat\nPoPs:", "None", "radio",
          ["None", "Ocnl", "Frq", "Brf", "Pds", "Inter"]),
         ("1st\nType:", "RW", "radio",
          ["RW", "SW", "R", "S", "ZR", "IP", "L", "ZL", "F"]),
         ("1st\nInten:", "-", "radio",
          ["--", "-", "m", "+"]),
         ("1st Type\nAlternate\nProb/Cov:", "None", "radio",
          ["None", "SChc", "Iso", "Chc", "Sct", "Lkly", "Num", "Patchy", "Areas", "Wide"]),
         ("2nd\nType:", "None", "radio",
          ["None", "RW", "SW", "R", "S", "ZR", "IP", "L", "ZL", "F"]),
         ("2nd\nInten:", "-", "radio",
          ["--", "-", "m", "+"]),
         ("2nd Type\nAlternate\nProb/Cov:", "None", "radio",
          ["None", "SChc", "Iso", "Chc", "Sct", "Lkly", "Num", "Patchy", "Areas", "Wide"]),
         ("3rd\nType:", "None", "radio",
          ["None", "RW", "SW", "R", "S", "ZR", "IP", "L", "ZL", "F"]),
         ("3rd\nInten:", "-", "radio",
          ["--", "-", "m", "+"]),
         ("3rd Type\nAlternate\nProb/Cov:", "None", "radio",
          ["None", "SChc", "Iso", "Chc", "Sct", "Lkly", "Num", "Patchy", "Areas", "Wide"]),
         ("Thunder?", "No", "radio",
          ["No", "Yes (T)", "Yes (T+)"]),
         ("Thunder\nAlternate\nProb/Cov:", "None", "radio",
          ["None", "SChc", "Iso", "Chc", "Sct", "Lkly", "Num"]),
         ("Tstm\nAttributes?", ["None"], "check",
          ["Small Hail", "Heavy Rain", "Gusty Winds", "Frequent Lightning", "Large Hail", \
           "Damaging Winds", "Tornadoes"])
        ]

# Set up Class
import SmartScript
# Hack to make Tkinter work:
import sys
sys.argv = [''] 
import Tkinter
import threading
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Required Method: Execute
    #  Called for each Point for each Grid to be edited
    # Fill in the arguments you want to use -- WeatherElement1, WeatherElement2...

    def execute(self, PoP, Wx, varDict):
        "Sets Wx Coverage within Active Edit Area according to PoP values"

        # Set up variables from the varDict

        qualifiertype = varDict["Qualifier\nType:"]
        alt_catpop_probcov = varDict["Alter\nTerms\nCat\nPoPs:"]
        type1 = varDict["1st\nType:"]
        intensity1 = varDict["1st\nInten:"]
        alt_probcov1 = varDict["1st Type\nAlternate\nProb/Cov:"]
        type2 = varDict["2nd\nType:"]
        intensity2 = varDict["2nd\nInten:"]
        alt_probcov2 = varDict["2nd Type\nAlternate\nProb/Cov:"]
        type3 = varDict["3rd\nType:"]
        intensity3 = varDict["3rd\nInten:"]
        alt_probcov3 = varDict["3rd Type\nAlternate\nProb/Cov:"]
        thunder = varDict["Thunder?"]
        alt_thunder_probcov = varDict["Thunder\nAlternate\nProb/Cov:"]
        attributes = varDict["Tstm\nAttributes?"]

        type = [type1, type2, type3]
        intensity = [intensity1, intensity2, intensity3]
        # Set up alt_probcov list (lists alternate probabilities/coverages for each Wx Type)
        alt_probcov = [alt_probcov1, alt_probcov2, alt_probcov3]

        # Block to check for invalid Wx types if the user selects areal coverage qualifiers, and a precip type
        # of R, S, ZR, IP, L, or ZL. In such cases, return an error message - unless a valid areal coverage
        # qualifier is specified as an alternate coverage. The check for valid alternate coverages was added
        # with version 2.1; previously, the tool would just ingore valid alternate coverage terms and return
        # an error message regardless.
        if qualifiertype == "Cov":
            for x in range(len(type)):
                if ((type[x] == "R") or (type[x] == "S") or (type[x] == "ZR") or (type[x] == "IP")):
                    if ((alt_probcov[x] == "None") or (alt_probcov[x] == "Iso") or (alt_probcov[x] == "Sct") \
                        or (alt_probcov[x] == "Num") or (alt_probcov[x] == "Patchy") or (alt_probcov[x] == "Areas")): 
                        print type[x], alt_probcov[x]
                        self.errorNotice('Invalid Wx Type', "Areal coverage qualifiers other than 'Wide' may not be used with a "\
                        "precip type of R, S, ZR, or IP.\nPlease re-run the tool using different qualifiers.") 
                        return Wx
                if ((type[x] == "L") or (type[x] == "ZL")):
                    if ((alt_probcov[x] == "None") or (alt_probcov[x] == "Iso") or (alt_probcov[x] == "Sct") or (alt_probcov[x] == "Num")):
                        print type[x], alt_probcov[x]
                        self.errorNotice('Invalid Wx Type', "Areal coverage qualifiers other than 'Patchy', 'Areas', or 'Wide' may not be used with "\
                        "a precip type of L or ZL.\nPlease re-run the tool using different qualifiers.") 
                        return Wx

        # If block to check for invalid Wx types if the user selects alternate prob/cov terms...
        for x in range(len(alt_probcov)):
            # Add check in case Fog is selected with no alternate qualifier
            if (type[x] == "F") and (alt_probcov[x] == "None"):
                    print type[x]
                    self.errorNotice('Invalid Wx Type', "You must use a qualifier of 'Patchy', 'Areas', or 'Wide' with F (Fog).\n"\
                    "Please re-run the tool using an allowed qualifier.")
                    return Wx
            # Added additional check to the following line to ensure that type[x] is also None. This prevents
            # the error message(s) below from appearing if a second/third pcpn type is set to "None", yet the
            # second/third alt_probcov is some value other than "None".
            if (alt_probcov[x] != "None") and (type[x] != "None"):
                if ((type[x] == "R") or (type[x] == "S") or (type[x] == "ZR") or (type[x] == "IP") or (type[x] == "L") or (type[x] == "ZL")) and \
                   ((alt_probcov[x] == "Iso") or (alt_probcov[x] == "Sct") or (alt_probcov[x] == "Num")):
                    print type[x]
                    self.errorNotice('Invalid Wx Type', "The 'Iso', 'Sct', or 'Num' qualifiers may not be used with a precip type of "\
                    "R, S, ZR, IP, L, or ZL.\nPlease re-run the tool using a different qualifier.")
                    return Wx
                if ((type[x] != "L") and (type[x] != "ZL") and (type[x] != "F")) and \
                   ((alt_probcov[x] == "Patchy") or (alt_probcov[x] == "Areas")):
                    print type[x]
                    self.errorNotice('Invalid Wx Type', "'Patchy' or 'Areas' qualifiers may not be used with a precip type of RW, SW, R, S, ZR, or IP.\n"\
                    "Please re-run the tool using a different qualifier.")
                    return Wx
                # Add check in case Fog is selected, and the qualifier is set to something other than 'Patchy', 'Areas',
                # or "Wide'.
                if (type[x] == "F") and \
                    ((alt_probcov[x] != "Patchy") and (alt_probcov[x] != "Areas") and (alt_probcov[x] != "Wide")):
                    print type[x]
                    self.errorNotice('Invalid Wx Type', "Qualifiers other than 'Patchy', 'Areas', or 'Wide' may not be used with F (Fog).\n"\
                    "Please re-run the tool using an allowed qualifier.")
                    return Wx
                    
        # Create Attributes String for later use

        if thunder == "No":
            attstring = "<NoAttr>"

        if thunder == "Yes (T)":
            # If (non-severe) thunder is selected, do not allow LgA, DmgW, or TOR attributes to be used
            if attributes == []:
                attstring = "<NoAttr>"
            else:
                attstring = ""
                invalid_attributes = 0
                
                for x in range(len(attributes)):
                    if attributes[x] == "Small Hail":
                        attstring += "SmA,"
                    if attributes[x] == "Heavy Rain":
                        attstring += "HvyRn,"
                    if attributes[x] == "Gusty Winds":
                        attstring += "GW,"
                    if attributes[x] == "Frequent Lightning":
                        attstring += "FL"

                    # Check for invalid Tstm attributes (cannot use LgA, DmgW, or TOR with T);
                    # if any are selected - display a pop-up message indicating that these are not valid
                    # (but let tool run, ignoring the invalid attributes)
                    if (attributes[x] == "Large Hail") or (attributes[x] == "Damaging Winds") or \
                       (attributes[x] == "Tornadoes"):
                        invalid_attributes = 1
                        pass

                if invalid_attributes == 1:
                    self.errorNotice('Invalid Thunderstorm Attribute(s):', "The 'Large Hail', 'Damaging Winds', and 'Tornadoes' attributes may not be used with "\
                    "Non-severe thunderstorms (T).\nThese attributes will not be included in the returned grid.")
                #print attstring

        if thunder == "Yes (T+)":
            # If severe thunder is selected, do not allow SmA or GW attributes to be used.
            # Also allow tornadoes to be used (both changes for Version 2.0)
            if attributes == []:
                attstring = "<NoAttr>"
            else:
                attstring = ""
                invalid_attributes = 0

                for x in range(len(attributes)):
                    if attributes[x] == "Heavy Rain":
                        attstring += "HvyRn,"
                    if attributes[x] == "Frequent Lightning":
                        attstring += "FL,"
                    if attributes[x] == "Large Hail":
                        attstring += "LgA," 
                    if attributes[x] == "Damaging Winds":
                        attstring += "DmgW,"
                    if attributes[x] == "Tornadoes":
                        attstring += "TOR,"
                    # Check for invalid Tstm attributes (cannot use SmA or GW with T+);
                    # if any are selected - display a pop-up message indicating that these are not valid
                    # (but let tool run, ignoring the invalid attributes)
                    if (attributes[x] == "Small Hail") or (attributes[x] == "Gusty Winds"):
                        invalid_attributes = 1
                        pass

                if invalid_attributes == 1:
                    self.errorNotice('Invalid Thunderstorm Attribute(s):', "The 'Small Hail' and 'Gusty Winds' attributes may not be used with "\
                    "Severe thunderstorms (T+).\nThese attributes will not be included in the returned grid.")
                #print attstring
        
        # Initialize the Wx values and keys
        wxValues = array(PoP.shape, int8)
        keys = []

        if qualifiertype == "Prob":

            wxValues = where(less(PoP, SChc_min_PoP_threshold), self.getByteValue("", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov), \
                       where(less(PoP, 24.5), self.getByteValue("SChc", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov), \
                       where(less(PoP, 54.5), self.getByteValue("Chc", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov), \
                       where(less(PoP, 74.5),self.getByteValue("Lkly", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov), \
                       where(less_equal(PoP, 100),self.getByteValue("Def", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov), \
                           self.getByteValue("", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov))))))

        if qualifiertype == "Cov":

            wxValues = where(less(PoP, SChc_min_PoP_threshold), self.getByteValue("", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov), \
                       where(less(PoP, 24.5), self.getByteValue("Iso", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov), \
                       where(less(PoP, 54.5), self.getByteValue("Sct", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov), \
                       where(less(PoP, 74.5),self.getByteValue("Num", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov), \
                       where(less_equal(PoP, 100),self.getByteValue("Wide", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov), \
                           self.getByteValue("", type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov))))))
        
        wxValues = wxValues.astype(int8)
        print "keys = ", keys  
        return (wxValues, keys)

    def getByteValue(self, prevail_cov, type, intensity, thunder, attstring, keys, alt_probcov, alt_catpop_probcov, alt_thunder_probcov):
        # Adapted from Convective Smart Tool (billingsley)

        # Use alt_catpop_probcov if set:
        if alt_catpop_probcov != "None":
            if (prevail_cov == "Def") or (prevail_cov == "Wide"):
                prevail_cov = alt_catpop_probcov

        # Use alt_thunder_probcov if set:
        if alt_thunder_probcov == "None":
            thunder_cov = prevail_cov
        else:
            thunder_cov = alt_thunder_probcov

        # Set type0cov, type1cov, type2cov
        if alt_probcov[0] == "None":
            type0cov = prevail_cov
        else:
            type0cov = alt_probcov[0]
        if alt_probcov[1] == "None":
            type1cov = prevail_cov
        else:
            type1cov = alt_probcov[1]
        if alt_probcov[2] == "None":
            type2cov = prevail_cov
        else:
            type2cov = alt_probcov[2]

        # Check for intensities with Fog - if intensity is very light (--), light (-) or moderate (m),
        # set to <NoInten>...but also display a pop-up message letting the user know these are not allowed.
        for x in range(len(type)):
            if type[x] == "F":
                if (intensity[x] == "--") or (intensity[x] == "-") or (intensity[x] == "m"):
                    intensity[x] = "<NoInten>"
                    self.errorNotice('Invalid Fog intensity:', "The '--', '-', and 'm' intensities may not be used with "\
                    "a weather type of Fog (F).\nThe intensity has been set to <NoInten> in the returned grid.")

         
        # Now create the uglyString...
        if type[1] == "None" and type[2] == "None":
            if prevail_cov == "":
                uglyString = ""
            else:
                if thunder == "Yes (T+)":
                    uglyString = thunder_cov + ":T:+:<NoVis>:" + attstring + "^" \
                                + type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>"
                elif thunder == "Yes (T)":
                    uglyString = thunder_cov + ":T:<NoInten>:<NoVis>:" + attstring + "^" \
                                + type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>"
                else:
                    uglyString = type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>"

                print uglyString
        if type[1] != "None" and type[2] == "None":
            if prevail_cov == "":
                uglyString = ""
            else:
                if thunder == "Yes (T+)":
                    uglyString = thunder_cov + ":T:+:<NoVis>:" + attstring + "^" \
                                + type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>^" \
                                + type1cov + ":" + type[1] + ":" + intensity[1] + ":<NoVis>:<NoAttr>"
                elif thunder == "Yes (T)":
                    uglyString = thunder_cov + ":T:<NoInten>:<NoVis>:" + attstring + "^" \
                                + type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>^" \
                                + type1cov + ":" + type[1] + ":" + intensity[1] + ":<NoVis>:<NoAttr>"
                else:
                    uglyString = type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>^" \
                                + type1cov + ":" + type[1] + ":" + intensity[1] + ":<NoVis>:<NoAttr>"
                print uglyString
        if type[1] == "None" and type[2] != "None":
            if prevail_cov == "":
                uglyString = ""
            else:
                if thunder == "Yes (T+)":
                    uglyString = thunder_cov + ":T:+:<NoVis>:" + attstring + "^" \
                                + type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>^" \
                                + type2cov + ":" + type[2] + ":" + intensity[2] + ":<NoVis>:<NoAttr>"
                elif thunder == "Yes (T)":
                    uglyString = thunder_cov + ":T:<NoInten>:<NoVis>:" + attstring + "^" \
                                + type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>^" \
                                + type2cov + ":" + type[2] + ":" + intensity[2] + ":<NoVis>:<NoAttr>"
                else:
                    uglyString = type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>^" \
                                + type2cov + ":" + type[2] + ":" + intensity[2] + ":<NoVis>:<NoAttr>"
                print uglyString
        if type[1] != "None" and type[2] != "None":
            if prevail_cov == "":
                uglyString = ""
            else:
                if thunder == "Yes (T+)":
                    uglyString = thunder_cov + ":T:+:<NoVis>:" + attstring + "^" \
                                + type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>^" \
                                + type1cov + ":" + type[1] + ":" + intensity[1] + ":<NoVis>:<NoAttr>^" \
                                + type2cov + ":" + type[2] + ":" + intensity[2] + ":<NoVis>:<NoAttr>"
                elif thunder == "Yes (T)":
                    uglyString = thunder_cov + ":T:<NoInten>:<NoVis>:" + attstring + "^" \
                                + type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>^" \
                                + type1cov + ":" + type[1] + ":" + intensity[1] + ":<NoVis>:<NoAttr>^" \
                                + type2cov + ":" + type[2] + ":" + intensity[2] + ":<NoVis>:<NoAttr>"
                else:
                    uglyString = type0cov + ":" + type[0] + ":" + intensity[0] + ":<NoVis>:<NoAttr>^" \
                                + type1cov + ":" + type[1] + ":" + intensity[1] + ":<NoVis>:<NoAttr>^" \
                                + type2cov + ":" + type[2] + ":" + intensity[2] + ":<NoVis>:<NoAttr>"
                print uglyString

        if "" == uglyString:
            uglyString = "<NoCov>:<NoWx>:<NoInten>:<NoVis>:<NoAttr>"
        return self.getIndex(uglyString, keys)

    def errorNotice(self, title=None, text=None, **kwargs):
        "Show the user a multi-line error message."
        # Title and kwargs are not used
        # They may be used if this method goes back to a Tk implementation
        if text is not None:
            self.statusBarMsg(text, "A")

# Tk implementation of errorNotice
# currently can't use due to short-lived nature of Jep scripts                
#    def errorNotice(self, title=None, text=None, **kwargs):
#        "Show the user a multi-line error message."
#        if msg is not None:
#            Exit = Tkinter.Tk()
#            Exit.title(title)
#            kwargs['command'] = Exit.destroy
#            kwargs['text'] = text
#            Tkinter.Button(Exit, **kwargs).pack()