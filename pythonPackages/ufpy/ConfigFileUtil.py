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

#
# A set of utility functions for dealing with configuration files.
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/27/10                      dgilling       Initial Creation.
#    
# 
#


def parseKeyValueFile(fileName):
    propDict= dict()
    
    try:
        propFile= open(fileName, "rU")
        for propLine in propFile:
            propDef= propLine.strip()
            if len(propDef) == 0:
                continue
            if propDef[0] in ( '#' ):
                continue
            punctuation= [ propDef.find(c) for c in ':= ' ] + [ len(propDef) ]
            found= min( [ pos for pos in punctuation if pos != -1 ] )
            name= propDef[:found].rstrip()
            value= propDef[found:].lstrip(":= ").rstrip()
            propDict[name]= value
        propFile.close()
    except:
        pass

    return propDict