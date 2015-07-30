# #
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
# #

#
# Overrides xml files with each other and returns a dict
#
#
#
#    SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/12/13                      mnash          Initial Creation.
#    Apr 27, 2015     4259         njensen        Updated for new JEP API
#
#
#

from com.raytheon.uf.common.localization.overrides import XMLOverrider as JavaXMLOverrider
from com.raytheon.uf.common.localization import LocalizationContext
LocalizationType = LocalizationContext.LocalizationType

def override(name, loctype):
    """
    Takes a name (filename and localization path) and the localization type and finds the 
    file and overrides it using the Java overrider, and then takes that value and returns 
    a dictionary
    
    Args:
            name : the name and path of the file in localization
            loctype : a string representation of the localization type
    
    Returns:
            a dictionary representing the XML
    """
    # call into the Java XMLOverrider class which will do the combining for us!
    configurationNode = JavaXMLOverrider.override(LocalizationType.valueOf(loctype), name)
    finalDict = _internalOverride(configurationNode)
    return finalDict

def _internalOverride(configurationNode):
    """
    Takes a configuration node from java and returns a dictionary of dictionaries corresponding to an XML file
    
    Args: 
            configurationNode : the ConfigurationNode from Java
    
    Returns:
            a dictionary of dictionaries (key is the xml tag, value is its children or the value)
    """
    theDict = dict()
    for child in configurationNode.getChildren() :
        # if there are children, then we can't just make this a value, so it needs to be a dict
        if child.getChildrenCount() > 0 :
            d = dict()
            theDict[child.getName()] = _internalOverride(child, d)
        else :
            # if this key is already in the dictionary, add to the list if there is one, or make a new one
            if theDict.has_key(child.getName()) :
                if isinstance(theDict.__getitem__(child.getName()), list) :
                    theDict.__getitem__(child.getName()).append(child.getValue())
                else :
                    l = list()
                    l.append(theDict.__getitem__(child.getName()))
                    l.append(str(child.getValue()))
                    theDict[child.getName()] = l
            else :
                theDict[child.getName()] = str(child.getValue())
    return theDict
