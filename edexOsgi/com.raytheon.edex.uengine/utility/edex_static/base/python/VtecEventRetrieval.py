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

from com.raytheon.edex.uengine.tasks.vtec import VtecEventQuery

#
# Request of Vtec
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/28/08        1100          bwoodle        Initial Creation.
#    
#

class VtecEventRetrieval():
    def __init__(self):
        self.query = VtecEventQuery()
        self.setTimeOut(10)
        
    def execute(self):
        return self.query.execute()
    
    def setFields(self,site,phenom,level):
        self.addField(self.query.SITE, site)
        self.addField(self.query.PHENOM, phenom)
        self.addField(self.query.LEVEL, level)
        
    def addProperty(self,key,value):
        self.query.addProperty(key,value)
        
    def addField(self,name,value):
        self.query.addField(name,value)
        
    def setTimeOut(self,time):
        self.query.setTimeOut(time*1000)
        
    def setClientID(self,clientID):
        self.query.setClientID(clientID)