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

knownLevels = {"BASE": {"text" : "BASE",
                        "order" : 0,
                        "systemLevel" : True,
                        },
               "CONFIGURED": {"text" : "CONFIGURED",
                        "order" : 250,
                        "systemLevel" : True,
                        },
               "SITE": {"text" : "SITE",
                        "order" : 500,
                        "systemLevel" : False,
                        },
               "USER": {"text" : "USER",
                        "order" : 1000,
                        "systemLevel" : False,
                        },
               "UNKNOWN": {"text" : "UNKNOWN",
                           "order" : -1,
                           }
                }


class LocalizationLevel(object):

    def __init__(self, level, order=750, systemLevel=False):
        if knownLevels.has_key(level.upper()):
            self.text = level.upper()
            self.order = knownLevels[self.text]["order"]
            self.systemLevel = knownLevels[self.text]["systemLevel"]
        else:
            self.text = level.upper()
            self.order = int(order)
            self.systemLevel = systemLevel

    def getText(self):
        return self.text

    def setText(self, text):
        self.text = text

    def getOrder(self):
        return self.order

    def setOrder(self, order):
        self.order = int(order)
        
    def isSystemLevel(self):
        return self.systemLevel
        
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        return str(self.text)

