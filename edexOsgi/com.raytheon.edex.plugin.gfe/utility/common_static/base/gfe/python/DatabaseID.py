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
# Provides a wrapper to the Java DatabaseID class
#   
#
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Apr 10, 2008           njensen   Initial Creation.
# Apr 02, 2010  4816     ryu       Add missing methods
# Jul 02, 2010  6350     ryu       Modified modelTime()
# Mar 12, 2015  3955     randerso  Added __eq__ and __ne__
# Feb 06, 2017  5959     randerso  Removed Java .toString() calls 
#
##

##
# This is a base file that is not intended to be overridden.
##



import AbsTime
import JUtil


class DatabaseID(JUtil.JavaWrapperClass):
    
    def __init__(self, db):
        self.__dbid = db
        
    def __str__(self):
        if not self.isValid():
            return "<Invalid>"
        return str(self.__dbid)
    
    def __repr__(self):
        return self.__str__()
    
    def __eq__(self, other):
        try:
            return self.toJavaObj().equals(other.toJavaObj())
        except:
            return False
    
    def __ne__(self, other):
        return not self.__eq__(other)
   
    def format(self):
        return self.__dbid.getFormat()
   
    def modelIdentifier(self):
        return self.__dbid.getModelId()
    
    def modelName(self):
        return self.__dbid.getModelName()
    
    def modelTime(self):
        d = self.__dbid.getModelDate()
        if d:
            return AbsTime.AbsTime(d)
        return AbsTime.AbsTime(0)
    
    def shortModelId(self):
        return self.__dbid.getShortModelId()
    
    def siteID(self):
        return self.__dbid.getSiteId()
    
    def type(self):
        return self.__dbid.getDbType()
    
    def isValid(self):
        if str(self.__dbid) == "":
            return False
        return self.__dbid.isValid()
    
    def toJavaObj(self):
        return self.__dbid
    
    
def databaseID_default():
    from com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID as JavaDbID
    jdb = JavaDbID()
    return DatabaseID(jdb)

def databaseID(id):
    from com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID as JavaDbID
    jdb = JavaDbID(id)
    return DatabaseID(jdb)
