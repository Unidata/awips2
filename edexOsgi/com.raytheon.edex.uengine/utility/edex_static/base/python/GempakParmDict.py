#
# GempakParmDict
#   
# This code has been developed by the SIB for use in the AWIPS2 system.
# Creates a dictionary for GEMPAK GRIB names and fullnames from $GEMTBL/grid tables,
# and updates the dictionary when changes are made to the tables.
#
#     Usage:
#    import GempakParmDict
#    dataRequest = GempakParmDict.GempakParmDict()
#    return dataRequest.execute("UREL")
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    06/02/10        173_partC     mgamazaychikov       Initial Creation
#   
import os
import time
import pickle
from com.raytheon.uf.common.message.response import ResponseMessageGeneric

class GempakParmDict():
    """This module defines a dictionary for translating
    GEMPAK variables/parameter names to A2DB ones."""
    def __init__ (self):
        self.dictDirName  = "/usr1/" + os.getlogin() + "/to11dr11/workspace/build.edex/esb/data/utility/edex_static/base/ncep/dictionary/"
        self.dictName     = "gempakParmDict.pkl"
        self.tableDirName = "/export-1/cdbsrv/nawdev/nawips/gempak/tables/grid/"
        self.fullDictName = self.dictDirName + self.dictName
        
        #
        # create the dictionary if it does not exist
        #
        if not  self.__dictionaryExists ():
            self.__createDictionary()
        else:
            #
            # check if the update is needed
            #
            if self.__dictionaryUpdateNeeded():
                self.__updateDictionary()
        
        #
        # load the dictionary
        #
        self.gempakNames = []
        self.gempakNames = self.__loadDictionary()        
        
    def __dictionaryExists(self):        
        return os.access(self.fullDictName, os.F_OK)
    
    def __dictionaryUpdateNeeded(self):
        return os.stat(self.tableDirName).st_mtime > os.stat(self.fullDictName).st_mtime
    
    def __updateDictionary(self):
        self.__deleteDictionary()
        self.__createDictionary()    
    
    def __deleteDictionary(self):
        os.remove(self.fullDictName)
        
    def __createDictionary (self):
        dictDirName = self.dictDirName
        dictName    = self.dictName
        tableDirName = self.tableDirName
        files = [
          "wmogrib1.tbl", "wmogrib2.tbl", "wmogrib3.tbl",
          "wmogrib128.tbl", "wmogrib128.tbl", "wmogrib128.tbl", "wmogrib128.tbl",
          "ncepgrib1.tbl", "ncepgrib2.tbl",
          "ncepgrib129.tbl", "ncepgrib130.tbl", "ncepgrib133.tbl",
          "nmcgrib1.tbl", "nmcgrib2.tbl",
          "ecmwfgrib128.tbl", "gwcgrib2.tbl",
          "g2vars.tbl", "g2varswmo2.tbl", "g2varsncep1.tbl"
                 ]
        dictFinal = {}
        nameList = []
        abbrList = []
        g1NameStart = 4
        g1NameEnd = 38
        g1AbbrStart = 59
        g1AbbrEnd = 72
        g2NameStart = 16
        g2NameEnd = 47
        g2AbbrStart = 70
        g2AbbrEnd = 81
        for j,fi in enumerate(files):
            file = open(tableDirName + files[j])
            if files[j][:2] == 'g2':
               nameStart = g2NameStart
               nameEnd   = g2NameEnd
               abbrStart = g2AbbrStart
               abbrEnd   = g2AbbrEnd
            else:
               nameStart = g1NameStart
               nameEnd   = g1NameEnd
               abbrStart = g1AbbrStart
               abbrEnd   = g1AbbrEnd
            kk = 0
            ii = 0
            while 1:
                line = file.readline()
                # 
                # check if the end of file is reached
                #
                if not line:
                    break
                # 
                # if the end of file is not reached
                # check if the line is commented out with '!'
                #
                skipping = False
                if line[0] != '!':
                    name = line[nameStart:nameEnd].rstrip()
                    abbr = line[abbrStart:abbrEnd].rstrip()
                    # 
                    # if the line is not commented out with '!',
                    # add name and abbr to lists, 
                    # if the abbreviation is not already in the list
                    # and not an empty string or 'NONE'
                    #
                    if abbr in abbrList:
                        skipping = True
                    else:
                        if not abbr.isalnum():
                            if abbr.find('-') == -1:
                                skipping = True
                            else:
                                skipping = False
                        if abbr == 'NONE':
                            skipping = True
                    
                    if skipping:
                        pass
                        ii = ii + 1
                    else:
                        kk = kk + 1
                        nameList.append(name)
                        abbrList.append(abbr)
            file.close()
        dictFinal = dict(zip(abbrList, nameList))
        output = open(self.fullDictName, 'wb')
        pickle.dump(dictFinal, output, -1)
        output.close()
        
    def __loadDictionary(self):
        pklFile = open(self.fullDictName,'rb')
        nameDict =  pickle.load(pklFile)
        pklFile.close()
        return nameDict
    
    def execute(self, aParm):
        try:
            name = self.gempakNames[aParm]
        except KeyError:
            print "Accessing a non-existent dictionary key"
            name = '' 
        return (ResponseMessageGeneric(name))
    
        