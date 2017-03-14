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
import getopt, sys, os, LogStream, numpy, time, JUtil

from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DBit
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData, ReferenceID



# -- module -----------------------------------------------------------------
# The setupTextEA program.  Sets up the text edit areas 
# -- implementation ---------------------------------------------------------
# The ifpServerText program requires the following command line:
# setupTextEA -h hostname -p rpcPortNumber [-u user]
#
#    -h host where the ifpServer is running
#    -p rpc port number for the ifpServer.
#    -u userid, defaults to GFETEST
# ---------------------------------------------------------------------------


class setupTextEA:

    def __init__(self):
        from com.raytheon.viz.gfe.core import DataManager
        from java.lang import System
        System.setProperty('user.name', 'GFETEST')        
        self.__host = None
        self.__port = None
        self.__user = 'GFETEST'        
        self.__dataMgr = DataManager.getInstance(None)

    def process(self):
        import TimeRange
        # get list of edit areas that are part of the Zones/FireWx group
        from com.raytheon.viz.gfe.smarttool import TextFileUtil, GridCycler
        textID = TextFileUtil.getTextFile('Zones', 'editAreaGroups')
        zoneList = []
        textFile = open(textID.getFile().getPath())
        textFile.readline()
        for line in textFile:
            zoneList.append(line.rstrip())
        textFile.close()
        textID = TextFileUtil.getTextFile('FireWxZones', 'editAreaGroups')
        textFile = open(textID.getFile().getPath())
        textFile.readline()
        for line in textFile:
            zoneList.append(line.rstrip())              
        textFile.close()        

        refMgr = self.__dataMgr.getRefManager()
        # make the basic edit areas that are required, go sequentially through
        # the zoneList        
        requiredEA = ["west_half","east_half","east_one_third",
          "west_one_third", "east_two_thirds","west_two_thirds",
          "east_one_quarter", "west_one_quarter", "east_three_quarters",
          "west_three_quarters","Superior"]
        for x in xrange(len(requiredEA)):
            refData = refMgr.loadRefSet(ReferenceID(zoneList[x]))
            ea = ReferenceData(refData)
            ea.setId(ReferenceID(requiredEA[x]))
            refMgr.saveRefSet(ea)                
            #ea = self.__client.getEditAreaPolygons(zoneList[x])
            #self.__client.saveEditArea(requiredEA[x], ea)
            LogStream.logEvent("Saved ", zoneList[x], "under", requiredEA[x])

        # special EAs (source,destination)
        special = [("ISC_Send_Area","FireArea"), ("ISC_Send_Area", "area3")]
        for s in special:
            refData = refMgr.loadRefSet(ReferenceID(s[0]))
            ea = ReferenceData(refData)
            ea.setId(ReferenceID(s[1]))
            refMgr.saveRefSet(ea)             
            #ea = self.__client.getEditAreaPolygons(s[0])
            #self.__client.saveEditArea(s[1], ea)
            LogStream.logEvent("Saved ", s[0], "under", s[1])


        # topography simulated based edit areas
        # area3 = whole area, AboveElev, BelowElev
        LogStream.logEvent("Calculating topo-dependent edit areas...")        
        topo = self.__dataMgr.getParmManager().getParmInExpr("Topo", True)
        topogrid = GridCycler.getInstance().getCorrespondingResult(
                                topo, TimeRange.allTimes().toJavaObj(), "TimeWtAverage")
        topogrid = topogrid[0].getGridSlice().getNDArray()
        iscSend = ReferenceID('ISC_Send_Area') 
        #wholeGrid = self.__client.getEditArea("ISC_Send_Area")
        wholeGrid = refMgr.loadRefSet(iscSend).getGrid().getNDArray()
        topoAve = 0
        count = 0
        minx, maxx, miny, maxy = self.__extremaOfSetBits(wholeGrid)
        for x in range(minx, maxx):
            for y in range(miny, maxy): 
                if wholeGrid[y,x] == 1:
                    count = count + 1
                    topoAve = topoAve + topogrid[y,x]
        topoAve = topoAve / count
        aboveGrid = wholeGrid * 0
        belowGrid = wholeGrid * 0
        for x in xrange(topogrid.shape[1]):
            for y in xrange(topogrid.shape[0]):
                if wholeGrid[y,x] == 1:
                    if topogrid[y,x] > topoAve:
                        aboveGrid[y,x] = 1
                    else:
                        belowGrid[y,x] = 1
        # area1 and area2 need to be "BelowElev", but should be different
        # than area3
        desiredCount = 2000
        count = 0
        area1 = wholeGrid * 0
        area2 = wholeGrid * 0
        for x in xrange(topogrid.shape[1]):
            if count < desiredCount:
                for y in xrange(topogrid.shape[0]): 
                    if wholeGrid[y,x] == 0 and topogrid[y,x] < topoAve:
                        area1[y,x] = 1
                        belowGrid[y,x] = 1
                        count = count + 1
        count = 0
        for x in xrange(topogrid.shape[1]):
            if count < desiredCount:
                for y in xrange(topogrid.shape[0]): 
                    if wholeGrid[y,x] == 0 and topogrid[y,x] < topoAve and \
                      area1[y,x] == 0:
                        area2[y,x] = 1
                        belowGrid[y,x] = 1
                        count = count + 1
                
        # save all topography-dependent edit areas
        self.__saveEA("area1", area1)
        LogStream.logEvent("Saved area1 based on area2, area3, and topo <", 
          topoAve)
        self.__saveEA("area2", area2)
        LogStream.logEvent("Saved area2 based on area1, area3, and topo <", 
          topoAve)
        self.__saveEA("AboveElev", aboveGrid)
        LogStream.logEvent("Saved AboveElev based on area3 > ", topoAve)
        self.__saveEA("BelowElev", belowGrid)
        LogStream.logEvent("Saved BelowElev based on area3 <= ", topoAve)
        self.__saveEA("Ridges", aboveGrid)
        LogStream.logEvent("Saved Ridges based on area3 > ", topoAve)
        self.__saveEA("Valleys", belowGrid)
        LogStream.logEvent("Saved Valleys based on area3 < ", topoAve)
        self.__saveEA("Inland", aboveGrid)
        LogStream.logEvent("Saved Ridges based on area3 > ", topoAve)
        self.__saveEA("Coastal", belowGrid)
        LogStream.logEvent("Saved Valleys based on area3 < ", topoAve)


        #city areas, which are a small part of other edit areas
        cityBased = [("area1",["city1","city2"]), ("area2", ["city3"]),
          ("area3",["city4", "area3_pt"])]
        for baseArea,cityAreas in cityBased:
            #wholeGrid = self.__client.getEditArea(baseArea)
            wholeGrid = refMgr.loadRefSet(ReferenceID(baseArea)).getGrid().getNDArray()
            minx, maxx, miny, maxy = self.__extremaOfSetBits(wholeGrid)
            cNumber = 0 
            print minx, maxx, miny, maxy, wholeGrid.shape
            for x in range(minx, maxx):
                for y in range(miny, maxy): 
                    if wholeGrid[y,x] == 1:
                        if cNumber >= len(cityAreas):
                            break
                        cityGrid = numpy.logical_and(wholeGrid, 0)
                        cityGrid[y,x] = 1
                        self.__saveEA(cityAreas[cNumber], cityGrid.astype('int8'))
                        LogStream.logEvent("Saved ", cityAreas[cNumber], 
                          "based on:", baseArea)
                        cNumber = cNumber + 1

        # special for ISC areas for CCF database source test
        #txt = self.__eagdb["ISC"]
        #iscList = cPickle.loads(txt)
        textID = TextFileUtil.getTextFile('ISC', 'editAreaGroups')
        iscList = []
        textFile = open(textID.getFile().getPath())
        textFile.readline()
        for line in textFile:
            iscList.append(line.rstrip())
        textFile.close()
        count = 0
        while count < 6:
            for i in iscList:
                if i == "ISC_Send_Area" or i == "ISC_Tool_Area":
                    continue                
                wholeGrid = refMgr.loadRefSet(ReferenceID(i)).getGrid().getNDArray()                
                minx, maxx, miny, maxy = self.__extremaOfSetBits(wholeGrid)
                if minx == -1:
                    continue
                ok = 1
                print minx, maxx, miny, maxy, wholeGrid.shape
                for x in range(minx, maxx):
                    if ok:
                        for y in range(miny, maxy):
                            if wholeGrid[y,x] == 1:
                                ptGrid = numpy.logical_and(wholeGrid, 0)
                                ptGrid[y,x] = 1
                                name = "isc" + `count`
                                self.__saveEA(name, ptGrid.astype('int8'))
                                requiredEA.append(name)
                                LogStream.logEvent("Saved ", name, 
                                  "based on ", i)
                                ok = 0
                                break
                    else:
                        break
                            
                count = count + 1
                if count > 6:
                    break
 
        

        # store an edit area group with all of the generated edit areas                
        requiredEA.append("FireArea")
        requiredEA.append("AboveElev")
        requiredEA.append("BelowElev")
        requiredEA.append("Valleys")
        requiredEA.append("Ridges")
        requiredEA.append("Inland")
        requiredEA.append("Coastal")
        requiredEA.append("city1")
        requiredEA.append("city2")
        requiredEA.append("city3")
        requiredEA.append("city4")
        requiredEA.append("area3")
        requiredEA.append("area2")
        requiredEA.append("area1")
        
        refMgr.saveGroup("GFETest", JUtil.pylistToJavaStringList(requiredEA))
        
        time.sleep(.5)


    def __saveEA(self, name, grid):
        #save edit area from a grid
        gloc = self.__dataMgr.getParmManager().compositeGridLocation()
        id = ReferenceID(name)
        # convert grid to polygons
        grid2d = Grid2DBit.createBitGrid(gloc.getNx().intValue(), gloc.getNy().intValue(), grid)
        refdata = ReferenceData(gloc, id, grid2d)
     
        # save the edit area
        self.__dataMgr.getRefManager().saveRefSet(refdata)
        #self.__client.saveEditArea(name, ea)
 
        

    def __extremaOfSetBits(self,mask):
        "Returns tuple of extrema of set bits (minx,maxx, miny,maxy)"
        xs = sum(mask)
        ys = sum(mask, 1)
        minx = maxx = miny = maxy = -1
        for x in range(xs.shape[0]):
            if xs[x] != 0:
                if minx == -1:
                    minx = x
                maxx = x
        for y in range(ys.shape[0]):
            if ys[y] != 0:
                if miny == -1:
                    miny = y
                maxy = y
        return (minx, maxx, miny, maxy)

        
    def __cmdLine(self):
        optlist, oargs = getopt.getopt(sys.argv[1:], "h:p:u:")
        for opt in optlist:
            if opt[0] == '-h':
                self.__host = opt[1]
            elif opt[0] == '-p':
                self.__port = int(opt[1])
            elif opt[0] == '-u':
                self.__user = opt[1]

        # sanity checks, make sure all required switches are specified
        if self.__host is None or self.__port is None:
            self.__usage()
            raise SyntaxWarning, "Error: Missing host or port"


    def __usage(self):
        print """
Usage: setupTextEA -h hostname -p rpcPortNumber [-u user]

    -h host where the ifpServer is running
    -p rpc port number for the ifpServer.
    -u userid, defaults to GFETEST

"""


def main():
    LogStream.logEvent("setupTextEA Starting")

    try:
        obj = setupTextEA()
        obj.process()
    except Exception, e:
        LogStream.logProblem(LogStream.exc())
        sys.exit(1)

    LogStream.logEvent("setupTextEA Finished")


if __name__ == "__main__":
    main()

