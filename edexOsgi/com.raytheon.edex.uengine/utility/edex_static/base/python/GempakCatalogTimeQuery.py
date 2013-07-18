#
# GempakCatalogTimeQuery
#
# This code has been developed by the SIB for use in the AWIPS2 system.
# Performs a MetadataCatalogQuery for data from GEMPAK and returns
# all the available unique times for the table.
#
#     Usage:
#    import GempakCatalogQuery
#    query = GempakCatalogQuery.GempakCatalogQuery("bufrua")
#    return query.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    06/02/09        92            mgamazaychikov       Initial Creation
#    10/14/09        173           mgamazaychikov       Added code for satellite plugin
#    12/22/09        173_partB     mgamazaychikov       Added code for radar, mosaic mcidas plugin
#    06/02/10        173_partC     mgamazaychikov       Added code for grib plugin 
#    02/02/11                      mli                  add eventName for dynamic model names
#
from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from com.raytheon.edex.uengine.tasks.query import MetadataCatalogQuery
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert

class GempakCatalogTimeQuery():
    
    def __init__(self, pluginName):
        self.eventName = None
        self.queryResults = ArrayList()
        self.pluginName = pluginName
        self.query = MetadataCatalogQuery(pluginName)
        self.returnText = False
        self.query.setDistinctField("dataTime")
        
    def __isMixed(self, s):
        seen_upper = seen_lower = False
        for c in s:
            if c.isupper(): seen_upper = True
            if c.islower(): seen_lower = True
            if seen_upper and seen_lower:
                return True
        return False 
    
    def setReturnText (self):
        self.returnText = True
                
#
# Set parameters of the metadata catalog query
#
    def setParameters (self, aParms):
        if self.pluginName == "satellite":
            parms = aParms.split("|")
            self.query.addParameter("creatingEntity", parms[0])
            self.query.addParameter("physicalElement", parms[1])
            self.query.addParameter("sectorID", parms[2])
        elif self.pluginName == "mcidas":
            parms = aParms.split("|")
            self.query.addParameter("imageType", parms[0])
            self.query.addParameter("areaName", parms[1])
            self.query.addParameter("resolution", parms[2])
        elif self.pluginName == "radar":
            parms = aParms.split("|")
            theIcao = parms[0].lower()
            productCode = self.descriptionToCode (parms[1])
            self.query.addParameter("icao", theIcao)
            self.query.addParameter("productCode", productCode)
            self.query.addParameter("trueElevationAngle", parms[2])
        elif self.pluginName == "mosaic":
            parms = aParms.split("|")
            prodName = parms[0].upper()
            resolution = "%s" % ( int ( float ( parms[1] ) ) * 1000 )
            self.query.addParameter("prodName", prodName)
            self.query.addParameter("resolution", resolution)
        elif self.pluginName == "bufrua":
            self.query.addParameter("wmoHeader", "", "isNotNull")
        elif self.pluginName == "sfcobs":
            self.query.addParameter("wmoHeader", "", "isNotNull")
        elif self.pluginName == "grib":
            parms = aParms.split("|")
            self.query.addParameter("modelInfo.modelName",parms[0].upper() )
        elif self.pluginName == "ncgrib":
            parms = aParms.split("|")
            for ii in range(len(parms)):
                if ii == 0: 
                    if self.__isMixed(parms[0]):
                        self.query.addParameter("modelInfo.modelName", parms[0] )
                        #print "modelInfo.modelName set to", parms[0]
                    else:
                        self.query.addParameter("modelInfo.modelName",parms[0].upper() )
                        #print "modelInfo.modelName set to", parms[0].upper()
                elif ii == 1:
                    #print "setting eventName to", parms[1]
                    self.query.addParameter("modelInfo.eventName", parms[1])
            
#
# Returns a string with null response  
#
    def makeNullResponse(self):
        nullStr = "Database Query returned no results"
        if self.returnText:
            return nullStr
        else:
            return ResponseMessageGeneric(nullStr)    
#
# Generates a list of responses in XML format
#
    def makeResponse(self):
        convert = GempakConvert(self.pluginName, self.queryResults)
        convert2 = GempakConvert() 
        convertSt = convert.getStrings()
        returnStr = ''

        for s in convertSt:
            retTime = convert2.dbtimeToDattim(s)
            #
            # add only unique times
            #
            if returnStr.find(retTime) == -1:
                returnStr=returnStr+retTime+'|'
        
        #
        # sort times before returning
        #
        if self.returnText:
            return "|".join(sorted(returnStr[0:-1].split("|")))
        else:
            return ResponseMessageGeneric("|".join(sorted(returnStr[0:-1].split("|"))))
    
#
# Executes the query and calls appropriate response functions
#    
    def execute(self):
    	self.queryResults = self.query.execute()
        #
        # Make response based on the query results
        #        
        if self.queryResults is None:
            return self.makeNullResponse()
        else:
            return self.makeResponse()

#
# Return the product code given the description
#
    def descriptionToCode(self, aDescription):
            
        #
        # List of descriptions (keys in dictionary)
        #
        descriptionList = ["BREF 1.00", 
                           "BREF 2.00",
                           "VEL 1.00",
                           "TOPS 4.00", 
                           "SRVEL 1.00", 
                           "VIL 4.00", 
                           "PRCP1 2.00",
                           "PRCPT 2.00"]
        
        #
        # List of product codes (values in dictionary)
        #
        productCodeList = ["19",
                           "20", 
                           "27",
                           "41",
                           "56",
                           "57",
                           "78",
                           "80"]

        
        #
        # Create a dictionary for key-value relationship
        #
        
        theMap = {}
        for (key, value) in map(None, descriptionList, productCodeList):
            theMap[key] = value
        
        #
        # Return a value based on the key
        #
        
        return theMap[aDescription]
        