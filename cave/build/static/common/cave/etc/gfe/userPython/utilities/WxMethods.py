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
# WxMethods
#
# Author: hansen
# ----------------------------------------------------------------------------
import string

##########################################################################
# WORKING WITH UGLY STRINGS
#
# Although methods are provided (below) for working with Wx values,
# they are prohibitively slow and we recommend working directly with
# the "ugly" Wx string.  See the SmartTools User Guide for more
# information as well as the examples/smartTools directory.
##########################################################################
def WxParts(wxValue):
    # Return a list of the components of the wxValue
    # For example: "
    wxKeys = string.split(wxValue, "^")
    parts = []
    for wxKey in wxKeys:
        tokens = string.split(wxKey,":")
        for token in tokens:
            parts.append(token)
    return parts

#################################
# CREATING WEATHER STRINGS
#
# To create a weather string use this method:
#   WxString(description)
#     where description is a text string with the parts of a weather
#     value in order:
#         coverage,
#         types(can be single or a list),
#         intensity,
#         visibility,
#         attr
#     the parts are separated by spaces
# Example:
#   Wx = WxString("Sct RW")
#   Wx = WxString("Sct RW -")
#
# If you want to append multiple weather strings together you must
# put a "^" in between them.
# Example:
#   Wx = WxString("Sct RW") + "^" + WxString("Ocnl S")
#
# WARNING:  If you recieve an error indicating there was an invalid weather
#   string, make sure you have requested a valid combination of weather
#   coverages, types, intensities, etc.  For example, only certain intensities
#   are allowed with certain types and the system will not accept invalid
#   combinations.  This information is specified in the Configuration for
#   Weather (serverConfig.py).

def WxString(description):
    # Return a string with the designated arguments

    #print "WxString desc ", description
    coverage, wxType, inten, vis, attr = \
               convertWxDescription(description)
    #print "WxString convert ", coverages, wxTypes, intens, vis, attrs


    # Handle wildcards
    if coverage == "*":
        coverage = "<NoCov>"
    if wxType == "*":
        wxType = "<NoWx>"
    if inten == "*":
        inten = "<NoInten>"
    if vis == "*":
        vis = "<NoVis>"
    if attr == '*':
        attr = ""

    # Create weather string
    # Handle special case of TRW
    if wxType == "TRW":
        t = coverage +":T:<NoInten>:"+vis+":"+attr
        rw = coverage +":RW:"+inten+":"+vis+":"+attr
        return t + "^" + rw
    else:
        return coverage +":"+wxType+":"+inten+":"+vis+":"+attr

#################################
# EXAMINING WEATHER STRINGS
#
# To examine a Weather value, use the methods
#
#    WxContains(WeatherValue, description)
#      Returns true if the weather value contains the arguments of
#      of the description which is a textstring with arguments
#      separated by spaces and can contain wildcards (*) and lists
#    Examples:
#      WxContains(Wx, "Sct RW")
#        true if Wx contains Sct RW somewhere with any
#           intensity, visibility or attributes
#      WxContains(Wx, "* RW")
#        true if Wx contains RW with any coverage, intensity, visibility,
#           or attributes
#
#    WxContainsExact(WeatherValue, exactComponent)
#      This is a less powerful, but faster method if you are only
#      looking for one certain coverage, type, intensity, attribute,
#      or visibility
#    Examples:
#      WxContainsExact(Wx, "Sct")
#      WxContainsExact(Wx, "RW")
#
#
def WxContains(Wx, description):
    # Return 1 if any combination of the arguments are found
    # in the given Weather String

    if Wx is None:
        return 0

    #print "WxContains Wx, desc ", Wx, description
    coverage, wxType, inten, vis, attr = convertWxDescription(description)
    #print "WxContains convert ", coverage, wxType, inten, vis, attr

    subkeys = string.split(Wx,"^")
    #print "WxContains ", subkeys
    for subkey in subkeys:
       # Check all permutations of args
       tokens = string.split(subkey, ":")
       coverageFound = wxTypeFound = intenFound = 0
       visFound = attrFound = 0
       if coverage == "*" or coverage in tokens:
           coverageFound = 1
       if wxType == "*" or wxType in tokens:
           wxTypeFound = 1
       if inten == "*" or inten in tokens:
           intenFound = 1
       if vis == "*" or vis in tokens:
           visFound = 1
       if attr == "*" or attr in tokens:
           attrFound = 1
       if coverageFound and wxTypeFound and intenFound \
          and visFound and attrFound:
           return 1
    return 0

def WxContainsExact(Wx, str):
    subkeys = string.split(Wx,"^")
    for subkey in subkeys:
        tokens = string.split(subkey, ":")
        if str in tokens:
            return 1
    return 0

#################################
# MODIFYING WEATHER STRINGS
#
# Use this method to modify an existing string by replacing one value
# with another
# Example:
#   Wx = WxModify(Wx, "RW", "S")
# This would change rain to snow in the designated weather string
#
# You can use argument designations such as: "type", "cov", "inten"
#   Wx = WxModify(Wx, "type", "S")
# This would change the type to S regardless of its current value

def WxModify(Wx, part, value):
    # Return a modified weather string.

    #print "WxModify part, value ", part, value

    index = -1
    if part == "cov":
        index=0
    if part == "type":
        index=1
    if part == "inten":
        index=2
    if part == "vis":
        index=3
    if part == "attr":
        index=4
    if index < 0:
        # Simply replace using the given strings
        return string.replace(Wx, part, value)
    # Replace according to weather value part
    subkeys = string.split(Wx, "^")
    #print "WxModify subkeys ", subkeys
    first = 1
    result = ""
    for subkey in subkeys:
        if not first:
            result = result + "^"
        first = 0
        keyParts = string.split(subkey,":")
        #print "WxModify keyParts, index ", keyParts, index
        keyParts[index] = value
        for keyPart in keyParts:
            result = result + keyPart
            if keyParts.index(keyPart) < len(keyParts)-1:
                result = result + ":"

    #print "WxModify result ", result
    return result

def convertWxDescription(description):
    parts = string.split(description)
    totalParts = 5 # One list for each
    for i in range(totalParts):
        i = i # to supress pychecker
        if len(parts) < totalParts:
            parts.append("*")
    return tuple(parts)


#####################################################################
#  The following methods have more functionality, but are much slower.
#
#################################
# CREATING WEATHER STRINGS
#
# This method is similar to WxString above, but allows multiple
#  weather strings to be put together:
#
# Examples:
#   Wx = ComplexWxString("Sct RW")
#   Wx = ComplexWxString("Sct RW -")
#
# If two weather types have the same coverage, intensity, etc. you can
# separate the wxTypes with commas.
# Example:
#   Wx = ComplexWxString("Sct RW,S -")
#
# This is equivalent to:
#   Wx = WxString("Sct RW -") + "^" + WxString("Sct S -")
#

def ComplexWxString(description):
    # Return a string with the designated arguments

    #print "WxString desc ", description
    coverages, wxTypes, intens, vis, attrs = \
               complexConvertWxDescription(description)
    #print "WxString convert ", coverages, wxTypes, intens, vis, attrs

    # Handle special case of TRW
    if "TRW" in wxTypes:
        wxTypes.remove("TRW")
        wxTypes.append("T")
        wxTypes.append("RW")

    # Handle wildcards
    if coverages[0] == "*":
        coverages[0] = "<NoCov>"
    if wxTypes[0] == "*":
        wxTypes[0] = "<NoWx>"
    if intens[0] == "*":
        intens[0] = "<NoInten>"
    if vis[0] == "*":
        vis[0] = "<NoVis>"
    # Create attr string
    if attrs[0] == '*':
        attrStr = ""
    else:
        first = 1
        attrStr = ""
        for attr in attrs:
            if not first:
                attrStr = attrStr + ","
            first = 0
            attrStr = attrStr + attr

    # Create weather string
    Wx = ""
    first = 1
    for wxType in wxTypes:
        if not first:
            Wx = Wx + "^"
        first = 0
        intensity = intens[0]
        if wxType == "T" and not intensity == "+":
            intensity = "<NoInten>"
        Wx = Wx + coverages[0]+":"+wxType+":"+intensity+":"+vis[0]+":"\
             +attrStr

    #print "WxString result ", Wx
    return Wx

#################################
# EXAMINING WEATHER STRINGS
#
# This method is similar to the WxContains methods, but allows
# multiple
#
#    ComplexWxContains(WeatherValue, description)
#      Returns true if the weather value contains the arguments of
#      of the description which is a textstring with arguments
#      separated by spaces and can contain wildcards (*) and lists
#    Examples:
#      ComplexWxContains(Wx, "Sct RW")
#        true if Wx contains Sct RW somewhere with any
#           intensity, visibility or attributes
#      ComplexWxContains(Wx, "Sct,WSct RW -"
#        true if Wx contains Sct RW - or WSct RW - with any
#           visibility or attributes
#      ComplexWxContains(Wx, "* RW,R")
#        true if Wx contains RW or R with any coverage, intensity,
#        visibility, or attributes
#


def ComplexWxContains(Wx, description):
    # Return 1 if any combination of the arguments are found
    # in the given Weather String

    if Wx is None:
        return 0

    #print "WxContains Wx, desc ", Wx, description
    coverages, wxTypes, intens, visibilities, attrs = \
               complexConvertWxDescription(description)
    #print "WxContains convert ", coverages, wxTypes, intens, vis, attrs

    subkeys = string.split(Wx,"^")
    #print "WxContains ", subkeys
    for subkey in subkeys:
        # Check all permutations of args
        tokens = string.split(subkey, ":")
        for coverage in coverages:
            for wxType in wxTypes:
                for inten in intens:
                    for vis in visibilities:
                        for attr in attrs:
                            coverageFound = wxTypeFound = intenFound = 0
                            visFound = attrFound = 0
                            if coverage == "*" or coverage in tokens:
                                coverageFound = 1
                            if wxType == "*" or wxType in tokens:
                                wxTypeFound = 1
                            if inten == "*" or inten in tokens:
                                intenFound = 1
                            if vis == "*" or vis in tokens:
                                visFound = 1
                            if attr == "*" or attr in tokens:
                                attrFound = 1
                            if coverageFound and wxTypeFound and intenFound \
                               and visFound and attrFound:
                                return 1
    return 0

def complexConvertWxDescription(description):
    # Returns lists for each Weather Key part:
    # coverages, types, etc.
    # The description must be in order and each part separated by spaces
    # Each part could be a * or a list
    parts = string.split(description)
    totalParts = 5 # One list for each
    for i in range(totalParts):
        i = i # to supress pychecker
        if len(parts) < totalParts:
            parts.append("*")

    resultList = []
    for part in parts:
        # Convert to a list if necessary
        if string.find(part, ",") >=0:
            elements = string.split(part,",")
            eleList = []
            for element in elements:
                element = string.strip(element)
                eleList.append(element)
            resultList.append(eleList)
        else:
            resultList.append([part])

    return resultList[0], resultList[1], resultList[2], resultList[3],\
           resultList[4]
