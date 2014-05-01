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

from com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID as JavaParmID
import JUtil


class ParmID(JUtil.JavaWrapperClass):
    "Wrapper class for com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID"
    
    def __init__(self, name=None, model=None, dbid=None, level=None, jParmId=None):
        if jParmId is not None:
            self.__pid = jParmId
            
        elif name is None:
            if model is None and dbid is None and level is None:
                self.__pid = JavaParmId()
            else:
                raise ValueError, '"name" must be given if any arguments are supplied.'
        else:
            if dbid is None and model is None and level is None:
                # name is an ident string
                self.__pid = JavaParmID(name)
            elif dbid is None and model is None:
                raise ValueError, '"level" cannot be specified without "dbid" or "model".' 
            elif dbid is not None and model is not None:
                raise ValueError, '"model" and "dbid" cannot both be specified.'
            elif dbid is not None:
                # assume it is a DatabaseID.DatabaseID
                dbid = dbid.toJavaObj()
                if level is None:
                    self.__pid = JavaParmID(name, dbid)
                else:
                    self.__pid = JavaParmID(name, dbid, level)
            else:
                if level is None:
                    self.__pid = JavaParmID(name, model)
                else:
                    self.__pid = JavaParmID(name, model, level)

    def __str__(self):
        return str(self.__pid.toString())

    @staticmethod
    def defaultLevel():
        return JavaParmID.defaultLevel()
    
    def toJavaObj(self):
        return self.__pid
    
    def compositeNameUI(self):
        return self.__pid.compositeNameUI()
    
    def parmNameAndLevel(self, composite):
        retval = self.__pid.parmNameAndLevel(composite)
        retval = JUtil.javaStringListToPylist(retval)
        return retval

    def expressionName(self, topoID, mutableID, includeTime):
        return self.__pid.expressionName(topoID.toJavaObj(), mutableID.toJavaObj(), includeTime)
    
    def isValid(self):
        return self.__pid.isValid()
        
    def getParmName(self):
        return self.__pid.getParmName()
    
    def getParmLevel(self):
        return self.__pid.getParmLevel()
    
    def getDbId(self):
        return DatabaseID.DatabaseID(self.__pid.getDbId())
    
    def getCompositeName(self):
        return self.__pid.getCompositeName()
    
    def getShortParmId(self):
        return self.__pid.getShortParmId()
    
    def getParmId(self):
        return self.__pid.getParmId()
    
    @staticmethod
    def shortSerializer(parmID):
        return JavaParmID.shortSerializer(parmID.javaParmId())

    @staticmethod
    def shortDeserializer(parmIDasString):
        return ParmID(parmIDasString)
    
    def getUIFormattedString(self):
        return self.__pid.getUIFormattedString()
    