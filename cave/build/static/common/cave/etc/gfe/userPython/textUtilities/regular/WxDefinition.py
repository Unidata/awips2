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
# Provides a python compatible wrapper to WxDefinition
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/10/08                      njensen       Initial Creation.
#    
# 
#

class WxDefinition:
    def __init__(self, javaDef):
        self.__def = javaDef
    
    def javaDefinition(self):
        return self.__def

    def typeDesc(self, wxType):
        return self.__def.typeDesc(wxType)
    
    def intensityDesc(self, wxType, inten):
        return self.__def.intensityDesc(wxType, inten)
    
    def attributeDesc(self, wxType, attr):
        return self.__def.attributeDesc(wxType, attr)
    
    def coverageDesc(self, wxType, cov):
        return self.__def.coverageDesc(wxType, cov)
    
    def visibilityDesc(self, wxType, vis):
        return self.__def.visibilityDesc(wxType, vis)
    
    