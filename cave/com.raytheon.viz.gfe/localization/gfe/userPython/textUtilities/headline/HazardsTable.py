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
# Port of A1 HazardsTable.py.
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    ??/??/??                      ????????       Initial Creation.
#    05/14/13        1842          dgilling       Use GFEVtecUtil to handle NEW
#                                                 ETN assignment.
#    09/24/13        1843          dgilling       Handle GetNextEtnResponse.
#    11/20/13        2490          randerso       Corrected error handling in __getActiveTable
#    
# 


import time, getopt, sys, copy, string, logging
import VTECTableUtil, VTECTable
import TimeRange, AbsTime, ActiveTableVtec
import JUtil
from java.util import ArrayList
from com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID as JavaDatabaseID
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
from com.raytheon.uf.common.dataplugin.gfe.discrete import DiscreteKey
from com.raytheon.uf.common.time import TimeRange as JavaTimeRange
from com.raytheon.viz.gfe.sampler import HistoSampler, SamplerRequest
from com.raytheon.viz.gfe.vtec import GFEVtecUtil
import cPickle

# This class makes an object that interfaces to the GFE hazard grid
# sampling code and the TimeCombine code and generates formatted
# hazard strings and VTEC strings for formatters. Alternate active tables
# may be defined for test purposes.
class HazardsTable(VTECTableUtil.VTECTableUtil):
    def __init__(self, ifpClient, editAreas, productCategory, 
      filterMethod, databaseID, siteID4, activeTableName = "", 
      vtecMode = None, samplingThreshold = (10, None), hazardEndTime=None,
      creationTime=None, dataMgr=None, accurateCities=False, cityEditAreas=[]):
        self.log = logging.getLogger("FormatterRunner.HazardsTable.HazardsTable")
#        self.log.setLevel(logging.DEBUG)


        VTECTableUtil.VTECTableUtil.__init__(self, None)

        # save data
        self.__ifpClient = ifpClient
        self.__databaseID = databaseID
        self.__dataMgr = dataMgr
        self.__editAreas = editAreas
        self.__pil = productCategory
        self.__siteID4 = siteID4
        self.__spcSiteID4 = "KWNS"
        self.__tpcSiteID4 = "KNHC"
        self.filterMethod = filterMethod
        self.__activeTable = None
        self.__allGEOActiveTable = None #not filtered by edit areas
        self.__activeTableName = activeTableName
        self.__vtecMode = vtecMode
        self.__etnCache = {}
        if hazardEndTime is None:
            self.__hazardEndTime = None
        else:
            self.__hazardEndTime = hazardEndTime.unixTime()

        # list of marine products
        self.__marineProds = ["CWF", "NSH", "GLF", "MWW", "OFF"]

        # list of phen/sig from national centers and "until further notice"
        self.__tpcKeys = self.__processJavaCollection(GFEVtecUtil.TROPICAL_PHENSIGS, self.__convertPhensig)
        self.__tpcBaseETN = '1001'
        self.__ncKeys = self.__processJavaCollection(GFEVtecUtil.NATIONAL_PHENSIGS, self.__convertPhensig)
        self.__ufnKeys = [('HU','A'), ('HU','S'), ('HU','W'), ('TR','A'), ('TR','W'), 
          ('TY','A'), ('TY','W')]
        
        self.__sitesIgnoreNatlEtn = self.__processJavaCollection(GFEVtecUtil.IGNORE_NATIONAL_ETN, str)
        
        self.__marineZonesPrefix = ["AM", "GM", "PZ", "PK", "PH", "PM", "AN",
          "PS", "SL"]   #list of zone name prefix that are marine zones
        
        # tuple of (% area coverage, numberGridCells)
        self.__samplingThreshold = \
          (samplingThreshold[0]/100.0, samplingThreshold[1])

        #determine creation time
        if creationTime is not None:
            self.__time = creationTime
        else:
            self.__time = time.time() #now time
        self.__time = (int(self.__time) / 60) * 60  #truncated to minute

        # accurate cities
        self.__accurateCities = accurateCities
        self.__cityEditAreas = cityEditAreas

        #convert edit areas to a single zone list
        self.__zoneList = self.__singleZoneList(editAreas)

        #sample, and merge vtec codes
        self.__rawAnalyzedTable = self.__analyzedTable(self.__zoneList, 
            self.filterMethod)

        #reorganize raw analyzed table into hazards by zone, might cause
        #change in combinations
        self.__hazardsByZoneDict = {}
        if len(self.__rawAnalyzedTable) > 0:
            # organize by id
            self.__hazardsByZoneDict = self.__organizeByZone(
              self.__rawAnalyzedTable)
            

            self.__hazardCombinations = self.__recombineZoneGroups(
              self.__hazardsByZoneDict, editAreas)
        else:
            # if we got an empty table, set the combos to what was specified
            self.__hazardCombinations = editAreas

        self.log.debug("RecombinedZoneGroups: initial: " + str(self.__editAreas) +
          " final: " + str(self.__hazardCombinations))

        self.__cityHazards = self.__createCityHazards()

    def activeTable(self):
        # Returns the raw active table as a list of dictionaries
        return self.__activeTable

    def rawAnalyzedTable(self):
        # Returns the raw analyzed table as a list of dictionaries
        return self.__rawAnalyzedTable

    def consolidatedTableByID(self):
        # Returns the raw analyzed table consolidated by geo IDs, i.e.,
        # the ['id'] field is a list of ids.
        return self.consolidateByID(self.__rawAnalyzedTable)

    def getHazardAreaCombinations(self):
        # Returns a list of combinations to use that are guaranteed to
        # not have different hazards within each combination.
        return self.__hazardCombinations

    def getHazardList(self, editAreaList):
        # Find the hazards that apply to the area and timeRange, and returns
        # a list of dictionaries. This function can take a single string or
        # a list.  Restriction: only looks at the first element in the list.
        # The returned list's 'id' field is a list of zones with that 
        # hazard.

        if type(editAreaList) is list and len(editAreaList):
            ea = editAreaList[0]
            eaList = editAreaList
        elif type(editAreaList) is str:
            ea = editAreaList
            eaList = [editAreaList]
        else:
            return []

        hazards = []

        if self.__hazardsByZoneDict.has_key(ea):
            haz = self.__hazardsByZoneDict[ea]
            for h in haz:
                # if a segment number is present copy while removing seg 
                # from the key
                if h.has_key('seg') and h['seg'] != "":
                    # make a copy and change the key if we need to
                    newH = copy.deepcopy(h)
                    newH['id'] = eaList  # preserve the old list of areas
                    # strip segments
                    if (newH['phen'], newH['sig']) not in self.__ncKeys or \
                       self.__siteID4 == 'PGUM':
                        if string.find(newH['phensig'], ":") >= 0:
                            newH['phensig'] = newH['phen'] + '.' + newH['sig']

                    hazards.append(newH)
                else:
                    # otherwise just append the hazard record
                    hazards.append(h)

        # Now consolidate this list of hazards with segment numbers removed.
        hazards = self.__consolidateTime(hazards)
        
        return hazards

    def getVTECString(self, fcstArea):
        # Returns a string containing the vtec strings for the given forecast
        # area and time range.

        # get the list of hazards for this fcst area and time range
        hazards = self.getHazardList(fcstArea)   #could sort in here

        # sort the list of hazards depending on the type of product
        if self.__pil in self.__marineProds:   # it's a marine product
            hazards.sort(self.__marineHazardsSort)
        else:   # non-marine product
            hazards.sort(self.__hazardsSort)
        # hazards need upgrade records to be paired up
        hazards = self.__pairUpgradeRecords(hazards)
            
        # get VTEC strings and VTEC records
        vtecStrings = []
        for h in hazards:
            vtecS = h['vtecstr']
            if len(vtecS) == 0:
                continue
            vtecStrings.append(vtecS)

        returnStr = ""
        for s in vtecStrings:
            returnStr = returnStr + s + '\n'
        return returnStr

    # Returns the cities associted with the hazards that could afflict
    # the cities in cityList
    def getCities(self, cityList, zoneHazards):
        if self.__cityHazards is None:
            return
        
        relevant = []
        compare = ('phen', 'sig', 'endTime')
        for p in self.__cityHazards:
            for h in zoneHazards:
                if self.hazardCompare(p, h, compare):
                    relevant.append(p)
                    break

        return self.__getCities(cityList, relevant)
    
    # Get cities associated with a VTEC with an EXP action
    # returns None if the grid is deleted
    def getCitiesForEXP(self, cityList, zone, phen, sig, expTime):
        if self.__cityHazards is None:
            return

        # check zone hazards for existence of the grid
        if expTime <= self.__time:
            for rec in self.__oldZoneTable:
                if rec['id'] == zone and \
                   rec['phen'] == phen and rec['sig'] == sig and \
                   rec['endTime'] == expTime:
                    break
            else:
                self.log.info("No grid found for " + \
                                   `phen` + "." + `sig` + \
                                   " expired at " + \
                                   time.asctime(time.gmtime(expTime)))
                return

        # filter by phen, sig, expTime
        matches = []
        for rec in self.__cityHazards:
            if rec['phen'] == phen and rec['sig'] == sig and \
               rec['endTime'] == expTime:
                matches.append(rec)

        return self.__getCities(cityList, matches)

    # Get cities that appear in both cityList and hazardList
    # Ordering of cities should be same as cityList
    def __getCities(self, cityList, hazardList):
        cities = []
        for city in cityList:
            for p in hazardList:
                if p['id'].upper() == city:
                    cities.append(city)
                    break
        return cities

    # Check the AT for the last issued records to determine cities
    # that were affected by the cancelled/expired events.
    # We could include cities from other events, in which case the result
    # is uncertain.

    def getCitiesFromPrevious(self, ugcList, checkedVTEC, ignoredVTEC=[]):

        # local function for dict key
        def event(rec):
            return rec['phen'], rec['sig'], rec['etn']

        # we only need the records from the lastest issuance of this product

        myRecords = filter(lambda x: x['officeid'] == self.__siteID4 and \
                                     x['pil'] == self.__pil and \
                                     x['id'] in ugcList,
                           self.__activeTable)

        lastIssued = []
        issueT = 0
        for rec in myRecords:
            it = rec['issueTime']
            if self.__time >= it > issueT:
                lastIssued = [rec]
                issueT = it
            elif it == issueT:
                lastIssued.append(rec)

        if not lastIssued:
            return None, 1

        # keep track of matches
        unmatched = {}
        for rec in checkedVTEC:
            unmatched[event(rec)] = ugcList[:]
        
        cities = []
        certain = 1
        compare = ('phen', 'sig', 'etn')

        for active in lastIssued:

            if active['act'] in ['CAN', 'EXP']:
                # this will definitely make the result uncertain
                certain = 0
                continue
            elif active['act'] in ['UPG']:
                continue

            match = 0
            for rec in checkedVTEC:
                if self.hazardCompare(active, rec, compare):
                    match = 1
                    break

            if match:
                try:
                    unmatched[event(active)].remove(active['id'])
                except ValueError:
                    certain = 0
                    self.log.error("Too many matches for %s.%s:%04d"\
                          % event(active)\
                          + " in zone %s" % active['id'])
                    
                if active.get('cities') is not None:
                    for city in active['cities']:
                        if city not in cities:
                            cities.append(city)
                else:
                    certain = 0
                    msg = "Active table record has no cities attribute."
                    self.log.error(msg)

            else:
                # see if it should be ignored
                for rec in ignoredVTEC:
                    if self.hazardCompare(active, rec, compare):
                        break
                else:
                    # This active record doesn't match checked or ignored
                    # VTEC list - flag the result as uncertain
                    certain = 0

        # check if all hazard/zone combinations have been covered
        # there should be nothing in unmatched dict

        for key, zones in unmatched.items():
            if len(zones) > 0:
                certain = 0
                break

        msg = []
        for key, zones in unmatched.items():
            if len(zones) > 0:
                msg.append("%s.%s:%d   " % key + str(zones))
        if len(msg):
            msg = '\n'.join(msg)
            self.log.error("The following hazard/zones are not found"
                                 " in active table:\n" + str(msg))

        return cities, certain      


    def __hazardsSort(self, a, b):
        # Returns 1, 0, or -1 depending on whether the first hazard
        # is considered higher, equal, or lower priority when compared to
        # the second as defined in the VTEC directive.
        #      1) action code [CAN, EXP, UPG, NEW, EXB, EXA, EXT, CON]
        #      2) significance (W, Y, A, O, S)
        #      3) start time
        #      4) phenomena (alphabetical)

        # check action code
        actionCodeOrder = ["CAN", "EXP", "UPG", "NEW", "EXB", "EXA",
                           "EXT", "CON"]
        try:
            aIndex = actionCodeOrder.index(a['act'])
            bIndex = actionCodeOrder.index(b['act'])
        except ValueError:
            self.log.error("Invalid action code in hazard %s %s", a, b)
            return 0

        if aIndex > bIndex:
            return 1
        elif aIndex < bIndex:
            return -1

        # check sig
        sigOrder = ["W", "Y", "A", "O", "S", "F"]
        try:
            aIndex = sigOrder.index(a['sig'])
            bIndex = sigOrder.index(b['sig'])
        except ValueError:
            self.log.error("Invalid sig code in hazard %s %s", a, b)
            return 0

        if aIndex > bIndex:
            return 1
        elif aIndex < bIndex:
            return -1

        # check startTime
        if a['startTime'] > b['startTime']:
            return 1
        elif a['startTime'] < b['startTime']:
            return -1

        # check phen
        if a['phen'] > b['phen']:
            return 1
        elif a['phen'] < b['phen']:
            return -1

        self.log.error("Hazards are identical in __hazardsSort %s %s", a, b)
        return 0
    
    def __marineHazardsSort(self, a, b):
        # Returns 1, 0, or -1 depending on whether the first MARINE hazard
        # is considered higher, equal, or lower priority when compared to
        # the second as defined in the VTEC directive.
        #      1) start time
        #      2) action code [CAN, EXP, UPG, NEW, EXB, EXA, EXT, CON]
        #      3) significance (W, Y, A, S)
        #      5) phenomena (alphabetical)

        # check startTime
        if a['startTime'] > b['startTime']:
            return 1
        elif a['startTime'] < b['startTime']:
            return -1

        # check action code
        actionCodeOrder = ["CAN", "EXP", "UPG", "NEW", "EXB", "EXA",
                           "EXT", "CON"]
        try:
            aIndex = actionCodeOrder.index(a['act'])
            bIndex = actionCodeOrder.index(b['act'])
        except ValueError:
            self.log.error("Invalid action code in hazard %s %s", a, b)
            return 0

        if aIndex > bIndex:
            return 1
        elif aIndex < bIndex:
            return -1


        # check sig
        sigOrder = ["W", "Y", "A", "S", "F"]
        try:
            aIndex = sigOrder.index(a['sig'])
            bIndex = sigOrder.index(b['sig'])
        except ValueError:
            self.log.error("Invalid sig code in hazard %s %s", a, b)
            return 0

        if aIndex > bIndex:
            return 1
        elif aIndex < bIndex:
            return -1

        # check phen
        if a['phen'] > b['phen']:
            return 1
        elif a['phen'] < b['phen']:
            return -1

        self.log.error("Marine Hazards are identical in __marineHazardsSort %s %s", a, b)
        return 0

    def __pairUpgradeRecords(self, hazardsList):
        # This method moves items in the hazardsList around such that
        # upgrades and downgrades are sequential (UPG, NEW), (CAN, NEW)
        # Hazard upgradeFrom fields records must match in the categories:
        # start, end, etn, phen, and sig.

        # get the list of upgraded or downgraded records
        upDownList = []
        for h in hazardsList:
            if h.has_key('upgradeFrom') or h.has_key('downgradeFrom'):
                upDownList.append(h)

        # temporarily remove these guys from the hazardsList
        for upDown in upDownList:
            hazardsList.remove(upDown)

        # Hunt down their counterparts and add the record in the correct slot
        for upDown in upDownList:
            # get the fields from the up/downgradeFrom record
            oldRec = {}   
            if upDown.has_key('upgradeFrom'):
                oldRec = upDown['upgradeFrom']
            elif upDown.has_key('downgradeFrom'):
                oldRec = upDown['downgradeFrom']

            # find its match
            foundMatch = 0  # set a flag
            for h in hazardsList:
                if oldRec['etn'] == h['etn'] and \
                   oldRec['phen'] == h['phen'] and oldRec['sig'] == h['sig']:
                    # found a match
                    hazardsList.insert(hazardsList.index(h) + 1, upDown) # insert after
                    foundMatch = 1
                    break  # done with this pass through hazardsList

            if foundMatch == 0:
                self.log.error("Match not found for upgrade/downgrade.")

        return hazardsList
        
    #-----------------------------------------------------------------
    # The following set of functions are utility functions.
    #-----------------------------------------------------------------

    # Pretty-print a time range or a time range list
    def __printTR(self, t):
        s = ""
        if type(t) is list:
            s = '['
            for e in t:
                s = s + '(' + time.asctime(time.gmtime(e[0])) + \
                    ',' + time.asctime(time.gmtime(e[1])) + '),'
            s = s + ']'
            return s
        else:    
            s = '(' + time.asctime(time.gmtime(t[0])) + \
                ',' + time.asctime(time.gmtime(t[1])) + ')'
            return s

    #Pretty-prints the hazard by zone table
    def __printHBZ(self, hazardsByZone):
        s = '\n'
        for id in hazardsByZone.keys():
            s = s + " Hazards for " + `id` + \
              self.printActiveTable(hazardsByZone[id])
        return s 

    #provides intersection of two time ranges
    def __timeIntersection(self, tr1, tr2):    #tr1, tr2 tuples (startT, endT)
        if tr1[0] < tr2[0]:
            startTime = tr2[0]
        else:
            startTime = tr1[0]
        if tr1[1] > tr2[1]:
            endTime = tr2[1]
        else:
            endTime = tr1[1]
        if startTime >= endTime:
            return None   # no intersection
        else:
            return (startTime, endTime)
    
    #provides the time ranges of non-intersection in tr1, based on
    #the time range tr2. Returns a list of 0, 1, or 2 items.
    def __nonTimeIntersection(self, tr1, tr2):
        #returns list of non intersections between tr1 and tr2 within tr1
        intersect = self.__timeIntersection(tr1, tr2)
        if intersect is None:
            return [tr1]
        #exact match
        if tr1 == tr2:
            return []
        #startT same
        elif tr1[0] == intersect[0]:
            return [(intersect[1], tr1[1])] 
        #endT same
        elif tr1[1] == intersect[1]:
            return [(tr1[0], intersect[0])]
        #middle
        else:
            return [(tr1[0], intersect[0]), (intersect[1], tr1[1])]

    # time contains, if time range (tr) contains time (t), return 1
    def __containsT(self, tr, t):
        return (t >= tr[0] and t < tr[1])

    # time overlaps, if tr1 overlaps tr2 (adjacent is not an overlap)
    def __overlaps(self, tr1, tr2):
        if self.__containsT(tr2, tr1[0]) or self.__containsT(tr1, tr2[0]):
            return 1
        return 0

    # hazard records' time overlaps
    def __hazardsOverlap(self, h1, h2):
        tr1 = (h1['startTime'], h1['endTime'])
        tr2 = (h2['startTime'], h2['endTime'])
        if self.__containsT(tr2, tr1[0]) or self.__containsT(tr1, tr2[0]):
            return 1
        return 0

    # time range is adjacent to each other
    def __isAdjacent(self, tr1, tr2):
        if tr1[0] == tr2[1] or tr1[1] == tr2[0]:
            return 1
        return 0

    # combine two time ranges
    def __combineTR(self, tr1, tr2):
        return (min(tr1[0], tr2[0]), max(tr1[1], tr2[1]))

    # prepare etn cache.  Adds new entries to the etn cache, but doesn't
    # figure out the etn values at this point.  Organizes the information
    # by phen.sig, then maintains a list of start/end/etn/ids
    def __prepETNCache(self, proposedRecord):

        phensig = (proposedRecord['phen'], proposedRecord['sig'])
        id = proposedRecord['id']
        if self.__etnCache.has_key(phensig):
            for start, end, etn, ids in self.__etnCache[phensig]:
                if proposedRecord['startTime'] == start and \
                  proposedRecord['endTime'] == end:
                    ids.append(id) # add the id
                    return #already in the cache
            times = self.__etnCache[phensig]
            times.append((proposedRecord['startTime'], proposedRecord['endTime'], 0, [id]))

        else:
            self.__etnCache[phensig] = [(proposedRecord['startTime'], 
              proposedRecord['endTime'], 0, [id])]
    
    # assign new etns to the etn cache. This is done after all requests
    # for new etns have been made
    def __assignNewETNs(self, activeTable):
        
        # go through each new phen,sig
        for phen, sig in self.__etnCache.keys():

            #determine the first new ETN to use if we need a new one
            etn_base = self.__highestETNActiveTable(phen, sig, 
              self.__allGEOActiveTable) 
            etn_base = int(etn_base) + 1   #the next one in sequence

            #sort the etn cache by (start, end, etn, ids)
            self.__etnCache[(phen, sig)].sort()  #sort the start,end,etn,ids

            # keep track of the ids that have been given each etn
            coverage = {}

            #process sequentially each (phen, sig). Entries in cache
            #are list of startT (0), endT (1), etn# (2), [id] (3).
            times = self.__etnCache[(phen, sig)]
            for x in xrange(len(times)):   
                s1, e1, etn1, ids = times[x]
                #if no etn, then use a new one
                if etn1 == 0:   #etn == 0?
                    etn1 = etn_base
                    etn_base = etn_base + 1
                    times[x] = (s1, e1, etn1, ids)
                    coverage[etn1] = ids[:]

                # the ids for which a record with etn1 already exists
                assigned = coverage[etn1]

                #search for all adjacent or overlapping, give it the same etn
                for y in xrange(x+1, len(times)):
                    s2, e2, etn2, ids2 = times[y]
                    if etn2 == 0 and \
                      (self.__isAdjacent((s1, e1), (s2, e2)) or\
                      self.__overlaps((s1, e1), (s2, e2))):

                        # check for potential ETN duplication
                        for id2 in ids2:
                            if id2 in assigned:
                                # cannot assign etn1 to this group since etn1
                                # is already assigned to a record for the zone
                                break
                        else:
                            # ok to assign etn1 to this group
                            etn2 = etn1  #reuse the etn
                            times[y] = (s2, e2, etn2, ids2)

                            # add the ids to assigned list
                            assigned.extend(ids2)

    # find highest etn in active table for phen/sig, returns it.
    # This method has been dramatically re-written for A2 to use
    # GFEVtecUtil to do preliminary ETN assignment instead of scrubbing
    # the whole set of ActiveTableRecords to calculate it. 
    def __highestETNActiveTable(self, phen, sig, activeTable):
        etn_base = 0
        phensig = (phen, sig)
        
        # find the max ETN...
        # 1. highest ETN period for non-tropical and all GUM products (tpcKeys)
        # or
        # 2. highest ETN > 1000 for the tropical, non-GUM products (tpcKeys)
        #
        # Local WFOs do not assign these numbers, so they should have
        # numbers < 1000
        if phensig not in self.__tpcKeys or self.__siteID4 in self.__sitesIgnoreNatlEtn:
            etn_base = GFEVtecUtil.getNextEtn(self.__siteID4, '.'.join(phensig), False).getNextEtn() - 1
        else:
            presentyear = time.gmtime(self.__time)[0]
            for active in activeTable:
                activeyear = time.gmtime(active['issueTime'])[0]
                activephensig = (active['phen'],active['sig'])
                if phensig == activephensig and presentyear == activeyear:
                    # causes failure if tropical hazards are less than 1001
                    if active['etn'] < int(self.__tpcBaseETN):
                        LogStream.logProblem("Incorrect ETN for tropical hazard.")
        return etn_base

    #determine the new etn to use, using the etn cache
    def __getNewETN(self, pRecord):
        key = (pRecord['phen'], pRecord['sig'])
        if self.__etnCache.has_key(key):
            times = self.__etnCache[key]
            for startT, endT, etn, ids in times:
                if pRecord['startTime'] == startT and pRecord['endTime'] == endT:
                    return etn
        return "???"  #should never get here



    #-----------------------------------------------------------------
    # The following set of functions are used to recombining
    # records from the raw analyzed table to keep the geographic
    # groups together.
    #-----------------------------------------------------------------

    def __singleZoneList(self, comboList):
        #Utility function to break apart a combinations list (list of list
        #of zones) into a set of single zones.  Returns the list of zones.
        newList = []
        for c in comboList:
            for z in c:
                newList.append(z)
        return newList

    # Returns a dictionary that is keyed on zonename, and contains a list
    # of all hazards for that zone.
    def __organizeByZone(self, hazardList):
        hazardsByZone = {}
        for h in hazardList:
            if hazardsByZone.has_key(h['id']):
                hazardsByZone[h['id']].append(h)
            else:
                hazardsByZone[h['id']] = [h]

        self.log.debug("HazardByZone: " + self.__printHBZ(hazardsByZone))
        return hazardsByZone

    # Returns a dictionary that is keyed on (phen, sig), and contains a list
    # of all hazards for each key value.
    def __organizeByPhenSig(self, hazardList):
        hazards = {}
        for h in hazardList:
            key = (h['phen'], h['sig'])
            hazards.setdefault(key, []).append(h)

        self.log.debug("HazardByPhenSig:"+ self.__printHBZ(hazards))
        return hazards


    #compares two lists of hazards (zone1, zone2) for two zones.  Returns
    #whether the same hazards exist in both zones.  Must be an exact
    #match (like a operator==)
    def __comboCompare(self, hazardsByZone, zone1, zone2):
        compareList = ['phen', 'sig', 'pil', 'startTime', 'endTime', 'officeid', 'act']
        if hazardsByZone.has_key(zone1) and hazardsByZone.has_key(zone2):
            list1 = hazardsByZone[zone1]
            list2 = hazardsByZone[zone2]
            if len(list1) != len(list2):
                return 0
            for i in range(len(list1)):
                found = 0
                for j in range(len(list2)):
                    if self.hazardCompare(list1[i], list2[j], compareList):
                        found = 1
                        break
                if found == 0:
                    return 0
            return 1

        elif not hazardsByZone.has_key(zone1) and \
          not hazardsByZone.has_key(zone2):
            return 1
        else:
            return 0
        

    #analyzes the hazardsByZone and the list of desired editArea combinations,
    #and ensures that the hazards are the same for every zone in each
    #combination.  If not, separates out those zones.  Returns the new
    #zone grouping.
    def __recombineZoneGroups(self, hazardsByZone, editAreas):
        outEditAreas = []
        for combo in editAreas:
            newCombo = [[combo[0]]]
            for i in range(1, len(combo)):
                found = 0
                for j in range(len(newCombo)):
                    if self.__comboCompare(hazardsByZone, newCombo[j][0], 
                      combo[i]):
                        newCombo[j].append(combo[i])
                        found = 1
                        break
                if found == 0:
                    newCombo.append([combo[i]])
            for nc in newCombo:
                outEditAreas.append(nc)

        return outEditAreas

    #--------------------------------------------------------------
    # The following methods sample Hazard grids, obtain the active
    # table,  and create the analyzed table (including injecting 
    # the vtec strings into the table.  
    #--------------------------------------------------------------

    def __analyzedTable(self, areas, filter):
        # main routine to obtain the analyzed table.  Analyzed table
        # is the composite between the proposed and active tables.
        # filter is the function that filters out the hazards that 
        # should be considered.

        # Sample the Hazards Grid
        atable = self.__getProposedTable(areas)
        self.log.info("Proposed Table length: " + str(len(atable)))
        self.log.debug("Sampled Proposed Table: " +
          self.printActiveTable(atable, combine=True))

        # Combine time entries
        atable = self.__timeCombine(atable)
        self.log.info("Time Combine Proposed Table length: " + str(len(atable)))
        self.log.info("Proposed Table:" +
          self.printActiveTable(atable, combine=True))

        # Get the active table from the IFPServer
        rawactTable = self.__getActiveTable()
        self.log.info("Raw Active Table: " +  
          self.printActiveTable(rawactTable, combine=True))
        if rawactTable is None:
            self.log.error("Unable to retrieve VTEC active table. " + 
              "Product VTEC codes may be suspect.")
            rawactTable = []
        self.log.info("Raw Active Table length: " + str(len(rawactTable)))

        # Do specific product filtering
        self.log.debug("Analyzed Table, prior to site/product filtering: " +
          self.printActiveTable(atable, combine=True))
        atable = filter(atable, allowedHazardsOnly=False)
        self.log.info(\
          "Filtered Analyzed Table length, prior to VTEC injection: " +  
          str(len(atable)))
                
        # Perform site filtering on the active table.  We keep
        # our site and SPC.
        allGEOTable = []
        siteFilter = [self.__siteID4, self.__spcSiteID4]
        for a in rawactTable:
            if a['officeid'] in siteFilter:
                allGEOTable.append(a)

        # Perform GEO (edit area) filtering on the active table.
        # Also filter for TEST mode
        self.__allGEOActiveTable = copy.deepcopy(allGEOTable)
        actTable = []
        for a in self.__allGEOActiveTable:
            if a['id'] not in self.__zoneList:
                continue   #skip over entries not in our zone list
            # If we are in TEST mode, filter out all except 'T'
            # Otherwise, filter out all 'T'
            testEntry = a['vtecstr'].find('/T.') == 0
            if self.__vtecMode == "T":
                if testEntry:
                    actTable.append(a)
            else:
                if not testEntry:
                    actTable.append(a)
        actTable = filter(actTable, allowedHazardsOnly=True)  #also filter the active table
               
        self.log.info("Filtered Active Table length: " +  str(len(actTable)))
        self.log.info("Filtered Active Table:" + 
          self.printActiveTable(actTable, combine=True))
        self.__activeTable = copy.deepcopy(actTable)

        # Merge the proposed and active tables, to arrive at the analyzed table
        atable = self.__mergeActiveProposed(atable, actTable, self.__pil, 
          areas)
        self.log.info("Analyzed Table length: " +  str(len(atable)))
        
        # Finished
        self.log.info("Analyzed Table: " + self.printActiveTable(atable,
          combine=True))
        return atable


    def __getActiveTable(self):
        #Uses the IFPClient interface to get the VTEC active table from
        #the server.   Returns None on failure.
        from com.raytheon.uf.common.activetable import ActiveTableMode
        
        try:
            if self.__activeTableName != "PRACTICE":
                table = self.__ifpClient.getVTECActiveTable(self.__dataMgr.getSiteID())
            else:
                table = self.__ifpClient.getVTECActiveTable(self.__dataMgr.getSiteID(), ActiveTableMode.PRACTICE)
            table = ActiveTableVtec.transformActiveTableToPython(table)            
            return table
              
        except:  
            import traceback  
            s = "Unable to access VTEC Active Table: "  
            self.log.exception(s)  
            raise Exception(s + traceback.format_exc())

    def __createCityHazards(self):
        if not self.__accurateCities:
            return None
        
        self.log.info("Evaluating hazards for cities.")

        # set up sample requests and get the ParmHistos
        eaMap = {}
        editAreas = []
        for ea in self.__cityEditAreas:
            ea, city = ea
            editAreas.append(ea)
            id = ea.getId().getName()
            eaMap[id] = city

        parmHistos = self.__doSamplingOfHazards(editAreas)

        # make proposed table
        pTable = self.__makeCityTable(parmHistos, eaMap)

        # consolidate
        pTable = self.__consolidateTime(pTable)

        # remove old - keep those ended within 30min
        cutoff = self.__time - 30*60
        pTable = filter(lambda x: x['endTime'] > cutoff, pTable)

        # handle UFN events - convert ending time to max
        for proposed in pTable:
            if (proposed['phen'], proposed['sig']) in self.__ufnKeys:
                proposed['startTime'] = self.__time   #now
                proposed['endTime'] = float(2**31-1)  #forever
                proposed['ufn'] = 1  #until further notice

        self.log.info("Hazards afflicting cities:"+ 
          self.printActiveTable(pTable, combine=True, idType='city'))

        return pTable

    # Create city hazard table from samples
    def __makeCityTable(self, parmHistos, eaMap):
        rval = []

        phIter = parmHistos.iterator()
        while phIter.hasNext():
            ph = phIter.next()            
            areaID = ph.area().getId().getName()
            areaPoints = ph.numberOfGridPoints()
            samples = ph.histoSamples()
            city = eaMap.get(areaID)

            for s in samples:
                areaTime = TimeRange.TimeRange(s.validTime()) # timerange
                histpairs = s.histogram()
                for p in histpairs:
                    subkeys = JUtil.javaObjToPyVal(p.value().discrete().getSubKeys())
                    for sk in subkeys:
                        # skip if no hazard
                        if sk == "<None>":
                            continue

                        d = {}
                        d['act'] = ''
                        d['id'] = city
                        d['phensig'] = sk
                        d['seg'] = 0  #normally zero, except if aux data
                        d['startTime'] = float(areaTime.startTime().unixTime())

                        # possibly shorten the endTime based on 
                        # self.__hazardEndTime
                        if self.__hazardEndTime is not None and \
                          areaTime.endTime().unixTime() > self.__hazardEndTime:
                            d['endTime'] = float(self.__hazardEndTime)
                        else:
                            d['endTime'] = float(areaTime.endTime().unixTime())

                        if VTECTable.VTECTable.has_key(sk[:4]):
                            d['phen'] = sk[:2]
                            d['sig'] = sk[3]
                        else:   # locally defined hazard
                            d['phen'] = sk
                            d['sig'] = ""   # empty significance

                        rval.append(d)

        return rval


    def __doSamplingOfHazards(self, editAreas):
        # Samples the Hazards Grid in the ifpServer.  Returns a list
        # of ParmHistos.

        # Determine the ParmID for Hazards out of the given database
        dbid = JavaDatabaseID(self.__databaseID)  
#        pid = filter(lambda x: str(x).find("Hazards") != -1,
#           self.__ifpClient.getParmList(self.__databaseID))[0]
        parmList = self.__ifpClient.getParmList(dbid)
        size = parmList.size()
        for x in range(size):
            p = parmList.get(x)
            if str(p).find("Hazards") != -1:
                pid = p
                break

        # TimeRange to sample
        # Use hazardEndTime if present
        if self.__hazardEndTime is not None:
            tr = TimeRange.TimeRange(AbsTime.AbsTime.current(), 
              AbsTime.AbsTime(self.__hazardEndTime))
        else: #(everything)
            tr = TimeRange.allTimes()

        # Determine the sampler request structures
        sampreqs = ArrayList()
        for ea in editAreas:
            if type(ea) is str:
                sampreqs.add(SamplerRequest(pid,
              	ReferenceID(ea), tr.toJavaObj()))
            else:
                sampreqs.add(SamplerRequest(pid, ea, tr.toJavaObj()))

        # Perform sampling
        hs = HistoSampler(self.__ifpClient, sampreqs)
        #parmHistos = hs.getParmHisto_SeqOf()
        parmHistos = hs.getParmHisto()

        return parmHistos

    # Create proposed table from samples
    def __makeProposedTable(self, parmHistos):
        rval = []
        size = parmHistos.size()
        #for ph in parmHistos:
        for x in range(size):
            ph = parmHistos.get(x)
            areaID = ph.area().getId().getName()
            areaPoints = ph.numberOfGridPoints()
            samples = ph.histoSamples()

            for s in samples:                
                areaTime = TimeRange.TimeRange(s.validTime()) # timerange
                histpairs = s.histogram()                                             
                for p in histpairs:
                    subkeys = p.value().discrete().getSubKeys()
                    sksize = subkeys.size()
                    for y in range(sksize):
                        sk = str(subkeys.get(y))
                        d = {}
                        d['id'] = areaID
                        d['officeid'] = self.__siteID4
                        d['pil'] = self.__pil
                        d['phensig'] = sk
                        d['seg'] = 0  #normally zero, except if aux data
                        d['startTime'] = float(areaTime.startTime().unixTime())

                        # possibly shorten the endTime based on 
                        # self.__hazardEndTime
                        if self.__hazardEndTime is not None and \
                          areaTime.endTime().unixTime() > self.__hazardEndTime:
                            d['endTime'] = float(self.__hazardEndTime)
                        else:
                            d['endTime'] = float(areaTime.endTime().unixTime())

                        d['areaPoints'] = areaPoints
                        d['valuePoints'] = p.count()
                        d['act'] = "???"   #Determined after merges
                        d['etn'] = "???"   #Mostly Determined after merges
                        if VTECTable.VTECTable.has_key(sk[:4]):
                            d['phen'] = sk[:2]
                            d['sig'] = sk[3]
                            d['hdln'] = VTECTable.VTECTable[sk[:4]]['hdln']
                        else:   # locally defined hazard
                            d['phen'] = sk
                            d['sig'] = ""   # empty significance
                            desc = \
                                 DiscreteKey.discreteDefinition(self.__dataMgr.getSiteID()).keyDesc(
                                 "Hazards_SFC", sk) 
                            d['hdln'] = desc

                        #special checks for aux data
                        auxindex = sk.find(':')
                        if auxindex != -1:
                            auxData = sk[auxindex+1:]
                            #national center uses:  aux data is the etn number
                            if (d['phen'], d['sig']) in self.__ncKeys:
                                try:
                                    number = int(auxData)
                                    #tropical events may be either seg or etn
                                    if (d['phen'], d['sig']) in self.__tpcKeys:
                                        if number >= int(self.__tpcBaseETN):
                                            d['etn'] = number
                                        else:
                                            d['seg'] = number
                                    else:
                                        d['etn'] = number
                                except:
                                    self.log.error("Bad auxData for ",
                                      "National Center:"+ auxData+ d)

                            #other aux data interpreted as segment number
                            else:
                                try:
                                    segment = int(auxData)
                                    d['seg'] = segment
                                except:
                                    self.log.error("Bad auxData for seg:"+
                                      auxData+ d)
                    rval.append(d)
        return rval


    # Gets the proposed hazards table from the server.
    # Note that proposed table has 'areaPoints', and 'valuePoints' within
    # it, which will be later stripped out.
    def __getProposedTable(self, editAreas):
        rval = []

        # set up sample requests and get the ParmHistos
        parmHistos = self.__doSamplingOfHazards(editAreas)

        # make proposed table
        pTable = self.__makeProposedTable(parmHistos)

        # handle UFN events - convert ending time to max
        for proposed in pTable:
            if (proposed['phen'], proposed['sig']) in self.__ufnKeys:
                proposed['startTime'] = self.__time   #now
                proposed['endTime'] = float(2**31-1)  #forever
                proposed['ufn'] = 1  #until further notice
        return pTable


    # Utility function to combine 
    def __timeReduce(self, atable, index):
        if index >= len(atable) - 1:
            return
        if atable[index]['endTime'] == atable[index + 1]['startTime']:
            atable[index]['endTime'] = atable[index + 1]['endTime']
            del atable[index + 1]
            self.__timeReduce(atable, index)

    # Remove any None Headlines
    def __stripNone(self, atable):
        # First punt any <None> headlines
        return filter(lambda x : x['phensig'] != '<None>', atable)

    # Remove any entries that are in the past
    def __stripOld(self, atable):
        now = self.__time
        return filter(lambda x : x['endTime'] > now, atable)

    # Truncate entries to current hour that start in the past
    # must call after stripOld
    def __truncateCurrentTime(self, atable):
        nowHour = int(self.__time / 3600) * 3600
        for a in atable:
            if a['startTime'] < nowHour:
                a['startTime'] =  nowHour
        return atable
    
    # Remove any entries that occupy less than the sampling threshold
    # of the area.  Threshold is met for a given % of the area covered
    # or a number of grid points covered. If None is given, then that
    # critera is not considered.
    def __coverageFilter(self, atable):
        percent = self.__samplingThreshold[0]
        points = self.__samplingThreshold[1]
        if percent is not None and points is not None:
            atable = filter(lambda x :
               x['valuePoints'] / float(x['areaPoints']) >= percent or \
               x['valuePoints'] >= points, atable)
        elif percent is not None:
            atable = filter(lambda x :
               x['valuePoints'] / float(x['areaPoints']) >= percent, atable)
        elif points is not None:
            atable = filter(lambda x : x['valuePoints'] >= points, atable)
        else:
            return []   #complete filtering

        for i in atable:
            del i['valuePoints']
            del i['areaPoints']
        return atable

    # Returns a set of values found under the specified key in atable.
    def __keySet(self, atable, key):
        tmp = map(lambda x : x[key], atable)
        rval = []
        for x in tmp:
            if x not in rval:
                rval.append(x)
        return rval

    # Assumes that atable is for a sinlge area
    def __compressTime(self, atable):
        # Sort by time
        atable.sort(lambda x, y: cmp(x['startTime'], y['startTime']))

        types = self.__keySet(atable, 'phensig')

        rval = []
        for t in types:
            a = filter(lambda x : x['phensig'] == t, atable)
            i = 0
            while i < len(a):
                self.__timeReduce(a, i)
                i = i + 1
            rval = rval + a

        rval.sort(lambda x, y: cmp(x['startTime'], y['startTime']))
        return rval

    def __consolidateTime(self, atable):
        actions = self.__keySet(atable, 'act')
        rval = []
        for i in actions:
            actT = filter(lambda x: x['act'] == i, atable)
            areas = self.__keySet(actT, 'id')
            for j in areas:
                a = filter(lambda x: x['id'] == j, actT)
                rval = rval + self.__compressTime(a)
        return rval

    def __timeCombine(self, atable):
        atable = self.__stripNone(atable)
        atable = self.__coverageFilter(atable)
        atable = self.__consolidateTime(atable)

        # for cities list - keep these records to check for existence of grid 
        self.__oldZoneTable = filter(lambda x:
                                       0 <= self.__time - x['endTime'] < 1800,
                                     atable)

        atable = self.__stripOld(atable)
        atable = self.__truncateCurrentTime(atable)
        return atable

    def __copyFields(self, record, fields):
        #copies the specified fields and returns a dictionary
        #containing those fields
        d = {}
        for f in fields:
            if record.has_key(f):
                d[f] = record[f]
        return d

    #-------------------------------------------------------------
    # The following functions handle the merging of the        
    # proposed and active tables. VTEC strings are calculated 
    # in these routines.
    #-------------------------------------------------------------

    # Converts active table EXP codes that are still in effect to CON
    # codes.  This simplifies the logic of VTEC comparisons.  Returns
    # the modified active table.
    def __convertEXPtoCON(self, aTable):
        for a in aTable:
            if a['act'] == 'EXP' and a['endTime'] > self.__time:
                a['act'] = 'CON'
        return aTable


    # Handles the special case SPC Watches, which are TO.A, SV.A
    # Logic: watch in active table that matches one in proposed table from
    # my office, if not, then "NEW" action code, copy the times (if within
    # 30 minutes) from the SPC active table match into the proposed table.
    # If match of active and proposed for my office, then do normal
    # logic - but still copy the times but from my active record for my office.
    # if within 30 minutes).
    def __handleSPCWatches(self, proposedTable, activeTable):
        compare = ['phen', 'sig', 'etn']
        for proposed in proposedTable:
            # TO.A, SV.A - are the watches originally from SPC
            if proposed['phen'] in ['TO', 'SV'] and proposed['sig'] == 'A':

                #attempt to find a match in the active table by my office
                #attempt to find a match in the active table by SPC
                #We don't care about the geography ('id') at this point.
                myActive = None
                spcActive = None
                for active in activeTable:
                    if self.hazardCompare(proposed, active, compare) and \
                      active['act'] not in ['CAN', 'UPG', 'EXP']:
                        if active['officeid'] == self.__siteID4:
                            myActive = copy.deepcopy(active)
                        elif active['officeid'] == self.__spcSiteID4:
                            spcActive = copy.deepcopy(active)
                    if myActive is not None and spcActive is not None:
                        break   #for effen - got what we want

                # This is a new watch that we haven't issued before
                if myActive is None:
                    proposed['act'] = "NEW"

                    #get the times from the SPC watch
                    if spcActive is not None:
                        activeStart = spcActive['startTime']
                        activeEnd = spcActive['endTime']
                    else:
                        self.log.error("Unable to match SPC watch for "+
                          self.printActiveTable(proposed))
                        activeStart = proposed['startTime']
                        activeEnd = proposed['endTime']  #failsafe code

                # we matched the active table, so we have issued it before
                # we get the times from our active watch
                else:
                    activeStart = myActive['startTime']
                    activeEnd = myActive['endTime']

                # we need to adjust the times possibly.  We compare active
                # vs. proposed, and within 30minutes, then we assume that
                # the time hasn't changed. Due to hourly grids, but less
                # than that SPC times, we copy over the active table times.
                deltaStart = abs(proposed['startTime'] - activeStart)
                deltaEnd = abs(proposed['endTime'] - activeEnd)
                if deltaStart < 1800:  #30 minutes
                    proposed['startTime'] = activeStart
                if deltaEnd < 1800:  #30 minutes
                    proposed['endTime'] = activeEnd
        return proposedTable

    # Checks for events that have merged together.  This could result
    # in dropped VTEC entries so we need to EXT one and CAN the other.
    # We remove entries from the active table (memory copy) and generate
    # additional CAN events.
    def __checkForMergedEvents(self, proposedTable, activeTable):

        compare = ['id','phen','sig','pil']

        createdCANEntries = []

        for proposed in proposedTable:
            matches = []
 
            #record match and time overlaps for real events
            for active in activeTable:
                if self.hazardCompare(proposed, active, compare) and \
                  active['act'] not in ['CAN','UPG','EXP'] and \
                  active['endTime'] > self.__time and \
                  proposed['startTime'] <= active['endTime'] and \
                  proposed['endTime'] >= active['startTime']:
                    matches.append(active)

            #if multiple records match, we have a merged event
            #we need to find the highest etn for the event matches
            if len(matches) > 1:
                self.log.debug("MERGE event: proposed="+ 
                  self.printActiveTable(proposed)+
                  " matches="+ self.printActiveTable(matches))
                highestETN = 0
                for m in matches:
                    highestETN = max(highestETN, m['etn'])

                # find all other entries (non highest etn) and generate
                # new CAN records, then remove the entries from activeTable
                for m in matches:
                    if m['etn'] != highestETN:
                        canEntry = copy.deepcopy(m)
                        canEntry['act'] = 'CAN'
                        createdCANEntries.append(canEntry)
                        self.log.debug("CAN event: %s%s%s", 
                          self.printActiveTable(canEntry),
                          " remEntry: ", self.printActiveTable(m))
                        del activeTable[activeTable.index(m)]

        #append the set of generated CAN events
        for c in createdCANEntries:
            proposedTable.append(c)

        #return the modified set of records
        return (proposedTable, activeTable)


    # Checks for "CON" continuation and "EXT" extended in time codes.
    # An event is considered continued two hazards have the same
    # id, phen, sig, and pil, and if the end times match.  An event
    # is considered to be extended in time if the event overlaps
    # in time.
    def __checkForCONEXT(self, proposedTable, activeTable):

        compare = ['id', 'phen', 'sig', 'pil', 'officeid']  #considered equal

        for proposed in proposedTable:

            if proposed['act'] == 'CAN':
                continue   #only occurs with merged events

            if len(proposed['sig']):   #is VTEC, must compare with active
                for active in activeTable:
                    if self.hazardCompare(proposed, active, compare) and \
                      active['act'] not in ['CAN', 'UPG', 'EXP']: 
#                      and not self.__separateETNtrack(proposed, active):

                        #convective watch (special case, also compare etn)
                        if proposed['phen'] in ['SV', 'TO'] and \
                          proposed['sig'] == "A" and \
                          proposed['etn'] != active['etn']:
                            continue  #allows CAN/NEW for new convect watches

                        # times exactly match
                        if proposed['startTime'] == active['startTime'] and \
                          proposed['endTime'] == active['endTime']:
                            proposed['act'] = 'CON'
                            proposed['etn'] = active['etn']
                            self.__copyTextFields(proposed, active)
                        
                        # start times both before current time, end
                        # times the same, CON state
                        elif self.__time >= proposed['startTime'] and \
                          self.__time >= active['startTime'] and \
                          proposed['endTime'] == active['endTime']:
                            proposed['act'] = 'CON'
                            proposed['etn'] = active['etn']
                            self.__copyTextFields(proposed, active)

                        # special case of event ended already, don't
                        # assign "EXT" even with overlap
                        elif self.__time >= active['endTime']:
                            pass   #force of a new event since it ended

                        # start and/or end times overlap, "EXT" case
                        # except when user changed the start time
                        # of an event has gone into effect.
                        elif self.__hazardsOverlap(proposed, active):
                            
                            if active['startTime'] <= self.__time:
                                if proposed['startTime'] <= self.__time or \
                                       active.has_key('conexted'):
                                    proposed['act'] = 'EXT'
                            else:
                                proposed['act'] = 'EXT'

                            if proposed['act'] == 'EXT':
                                active['conexted'] = 1
                                proposed['etn'] = active['etn']
                                self.__copyTextFields(proposed, active)

                                #save original time so we can later determine
                                #whether it is EXTENDED or SHORTENED
                                proposed['previousStart'] = active['startTime']
                                proposed['previousEnd'] = active['endTime']

            else: #is Local, no changes to local events
                pass

        for active in activeTable:
            if active.has_key('conexted'):
                del active['conexted']

        return proposedTable
 
    # Checks for CAN, EXP, UPG
    def __checkForCANEXPUPG(self, pTable, activeTable):
        compare1 = ['id', 'phen', 'sig']
        newEntries = []

        for active in activeTable:
            if active['officeid'] != self.__siteID4:
                continue   #for a different site

            if active['act'] in ['CAN', 'UPG', 'EXP']:
                continue   #skip these records, event already over

            if active['pil'] != self.__pil:
                continue   #skip these records, since it is for another prod

            cancel_needed = 1

            # determine if cancel is needed, cancel (CAN, EXP, UPG).
            # Cancel not needed if we have an entry in proposed that
            # is already in active and the times overlap, and the active
            # ending time is still in the future
            for proposed in pTable:
                if self.hazardCompare(active, proposed, compare1):
                    if self.__hazardsOverlap(proposed, active) and \
                      self.__time < active['endTime']:

                        # active event is in effect and proposed event is in future
                        # cancel active event
                        if active['startTime'] <= self.__time and \
                               proposed['startTime'] > self.__time:
                            break

                        #convective watch, also check etn
                        if proposed['phen'] in ['SV', 'TO'] and \
                          proposed['sig'] == 'A':
                            if proposed['etn'] == active['etn']:
                                cancel_needed = 0
                                break

                        else:
                            cancel_needed = 0
                            break

            # CAN's have three special forms. CAN when a product is no longer
            # in the proposed table, EXP when the product is no longer 
            # in the proposed table, and the end was within 30 min of now,
            # and UPG when the phen is the same, but 
            # sig is upgraded, and the VTEC is still in effect.
            #
            if cancel_needed == 1:

                # Case One - UPG
                # Area matches, phen matches, and we are going from an 
                # advisory to a watch, a watch to a warning, or an
                # advisory to a warning.

                for proposed in pTable:
                    #find matches in area, do phen later
                    if self.hazardCompare(active, proposed, ['id']):

                        #find overlaps in time
                        if self.__hazardsOverlap(proposed, active):

                            if self.__isUpgrade(proposed, active):
                                active['act'] = 'UPG'
                                active['seg'] = 0
                                if active not in newEntries:
                                    newEntries.append(active)
                                cancel_needed = 0
      
                # Case Two - EXP
                # If it wasn't an UPG, then check for EXP. EXP if entry
                # not in the proposed table, and current time is after
                # the EXP time.

                if cancel_needed == 1:
                    timeFromEnd = self.__time - active['endTime']   # +after
                    if timeFromEnd >= 0:
                        active['act'] = 'EXP'
                        active['seg'] = 0
                        if active not in newEntries:
                            newEntries.append(active)
                        cancel_needed = 0

                # Final Case - CAN
                # Only Allow "CAN" entries if the event is still ongoing, 
                # otherwise ignore the entry.
                if cancel_needed == 1:
                    if self.__time < active['endTime']:
                        active['act'] = 'CAN'
                        active['seg'] = 0
                        if active not in newEntries:
                            newEntries.append(active)
                        cancel_needed = 0


        # add in new entries, change any text to prevText, overviewText to
        # prevOverviewText. Strip out any VTEC coding from active table.
        for entry in newEntries:
            if entry.has_key('segText'):
                entry['prevText'] = entry['segText']
                del entry['segText'] 
            if entry.has_key('overviewText'):
                entry['prevOverviewText'] = entry['overviewText']
                del entry['overviewText'] 
            if entry.has_key('vtec'):
                entry['vtecstr'] = ""  #erase the VTEC string.
                del entry['overviewText'] 
            pTable.append(entry)
        return pTable


    ########################################################################
    # This function checks the pTable against the activeTable to determine #
    # EXA or EXB
    ########################################################################

    def __checkForEXAEXB(self, pTable, activeTable):
        compare1 = ['id', 'phen', 'sig', 'etn', 'pil', 'officeid']
        compare2 = ['phen', 'sig', 'pil']

        for proposed in pTable:

            # first check to see if we have already assigned "NEW".  This
            # is a special case for SPC watches that now appear in the
            # proposed table, but haven't been issued yet.  In this case,
            # we skip processing this record.
            if proposed['act'] != "???":
                continue

            # Assume first that this is EXA or EXB
            exaexb_flag = 1

            #if we find a match, and it overlaps in time, 
            #then it isn't an EXA, EXB
            for active in activeTable:
                if self.hazardCompare(proposed, active, compare1):
                    #if proposed['startTime'] <= active['endTime'] and 
                    #  proposed['endTime'] >= active['startTime'] and 
                    if self.__hazardsOverlap(proposed, active) and \
                      active['act'] not in ['CAN','EXP','UPG']:
                        exaexb_flag = 0
            
            # no match was found, thus this is either a EXA, or EXB,
            # match records with phen and sig the same
            if exaexb_flag == 1:
                #first check for EXA, must check ALL records before
                #deciding it isn't an EXA
                for active in activeTable:
                    if self.hazardCompare(proposed, active, compare2):
#                    and not self.__separateETNtrack(proposed, active):
                        if active['act'] not in ['CAN', 'UPG', 'EXP']:

                            #if times are identical, then we extended in area 
                            if proposed['startTime'] == active['startTime'] and \
                              proposed['endTime'] == active['endTime']:
                                if proposed['etn'] == "???" or \
                                  proposed['etn'] == active['etn']:
                                    proposed['exaexb'] = 'EXA'
                                    proposed['active'] = active
                                    break

                            #if start times are both in the past or
                            #current, but end times equal, then it is
                            #an EXA
                            elif proposed['startTime'] <= self.__time and \
                              active['startTime'] <= self.__time and \
                              proposed['endTime'] == active['endTime']:
                                if proposed['etn'] == "???" or \
                                  proposed['etn'] == active['etn']:
                                    proposed['exaexb'] = 'EXA'
                                    proposed['active'] = active
                                    break

                if proposed.has_key('exaexb'):
                    continue

                #if it isn't an EXA, now we check the records again, but
                #check for overlapping or adjacent times, that do
                #not occur in the past in the active table, but ensure
                #that there is an event in the proposed that overlaps
                #with time. Results in EXB
                if proposed['act'] == "???":
                    for active in activeTable:
                        if self.hazardCompare(proposed, active, compare2):
#                        and not self.__separateETNtrack(proposed, active):
                            if active['act'] not in ['CAN', 'UPG', 'EXP']:
                                #if self.__hazardsOverlap(proposed, active) and
                                if proposed['startTime'] <= active['endTime'] and \
                                  proposed['endTime'] >= active['startTime'] and \
                                  active['endTime'] > self.__time:
                                    if proposed['etn'] == "???" or \
                                      proposed['etn'] == active['etn']:
                                        #ensure record overlaps with proposed
                                        #event
                                        for p1 in pTable:
                                            if p1 == proposed:
                                                continue  #skip itself
                                            if self.hazardCompare(p1, proposed,
                                              compare2) and self.__hazardsOverlap(p1, proposed):
                                                proposed['exaexb'] = 'EXB'
                                                proposed['active'] = active
                                                break
                                        break

        # Now set the marked records to EXA/EXB unless
        # there is already a record with the same ETN
        # for the same phen/sig in the same zone

        # Organize hazards by zone
        hazardDict = self.__organizeByZone(pTable)
        for zone, hazards in hazardDict.iteritems():
            # then organize by hazard key
            hazards = self.__organizeByPhenSig(hazards)
            for key, hzds in hazards.iteritems():
                for proposed in hzds:

                    if proposed.has_key('exaexb'):
                        act = proposed.pop('exaexb')
                        active = proposed.pop('active')
                        # checking if the etn is used
                        for p in hzds:
                            if p['etn'] == active['etn'] and \
                                   p['act'] != '???':
                                break
                        else:
                            proposed['act'] = act
                            proposed['etn'] = active['etn']
                            self.__copyTextFields(proposed, active)

                            if act == 'EXB':
                                #save original time so we can later 
                                #determine whether it is EXTENDED 
                                #or SHORTENED
                                proposed['previousStart'] = active['startTime']
                                proposed['previousEnd'] = active['endTime']
        
        return pTable


    # Assigns NEW to remaining records.  Has to determine the appropriate
    # ETN number.
    def __checkForNEW(self, pTable, activeTable):
        compare = ['id', 'phen', 'sig', 'officeid']

        #check for any remaining records that have an undefined action
        #these records must be "NEW".  Need to allocate a new etn, except
        #in two cases: one is already identified in the proposed table,
        #existing record in active table (phen,sig,id) regardless of pil.
        #
        #Already identified are basic TO.A, SV.A using aux data fields,

        allowedActions = ['NEW','CON','EXT','EXA','EXB']

        for proposed in pTable:
            if proposed['act'] == '???':
                if proposed['etn'] == "???":
                    #check in active table for a match (from other product),
                    #with events that still are occurring
                    etn = 0
                    for act in activeTable:
                        if self.__hazardsOverlap(proposed, act) and \
                          act['act'] in allowedActions and \
                          self.hazardCompare(proposed, act, compare) and \
                          act['endTime'] > self.__time:
                            etn = act['etn']
                            break

                    #not found in active nor proposed, prep for new one
                    if etn == 0:
                        self.__prepETNCache(proposed)
                    else:
                        proposed['etn'] = etn   #match found in active table
                proposed['act'] = "NEW"

        # determine any new ETNs
        self.__assignNewETNs(activeTable)
        self.log.debug("New ETN cache: " + str(self.__etnCache))

        # process again for records that are now marked NEW, but no etn
        for proposed in pTable:
            if proposed['act'] == 'NEW' and proposed['etn'] == "???":
                proposed['etn'] = self.__getNewETN(proposed)

        return pTable


    # Eliminates EXP codes from the table (for marine).
    # Returns the filtered table.
    def __eliminateEXPCodes(self, pTable):
        rTable = []
        for h in pTable:
            #accept all non-EXP codes
            if h['act'] != 'EXP':
                rTable.append(h)

            #Convert EXP into CON codes for non-yet expired events (30min)
            #since marine does not permit EXP codes 
            elif h['endTime'] > self.__time:
                h['act'] = 'CON'  #convert to CON code
                rTable.append(h)

            #Ignore the events if at or after the EXP time
            else:
                pass

        return rTable

    # add in EXP codes (for events just about ready to expire)
    def __addEXPCodes(self, pTable):
        #looks for events that have "CON", but are within 30 minutes of
        #event ending time and converts those events to EXP. 
        for each_hazard in pTable:
            if each_hazard['act'] == 'CON':
                timeFromEnd = self.__time - each_hazard['endTime']   # +after
                if timeFromEnd >= -30*60 and timeFromEnd <= 0:
                    each_hazard['act'] = 'EXP'   #convert to expired
        return pTable
        
    # remove EXP (actual EXP codes) when another event of same phen/sig is
    # now ongoing, but only if same issuance year
    def __removeEXPWithOngoingCodes(self, pTable):
        compare = ['phen','sig','etn','id']
        tmp = []
        for h in pTable:
            #events with EXP, and after ending time
            removeIt = 0
            if h['act'] == 'EXP' and self.__time >= h['endTime']:
                hIssueT = h.get('issueTime', self.__time)
                hIssueYear = time.gmtime(hIssueT)[0]
                for h1 in pTable:
                    #active event with same phen/sig/etn
                    h1IssueT = h1.get('issueTime', self.__time)
                    h1IssueYear = time.gmtime(h1IssueT)[0]
                    if h1['act'] in ['CON','EXA','EXB','EXT'] and \
                      self.hazardCompare(h, h1, compare) and \
                      h1IssueYear == hIssueYear:
                        removeIt = 1
                        break
            if removeIt == 0:
                tmp.append(h)
        return tmp
                        
            
    # generate VTEC strings for hazards
    def __addVTECStrings(self, pTable):
        for h in pTable:
            # get the three middle characters of the product pil
            if h.has_key('pil'):
                prodCat = h['pil']
            else:
                prodCat = '???'

            # get the VTEC Mode
            if self.__vtecMode is None:
                h['vtecstr'] = ""
                continue

            # Phen and Significance
            phen = h['phen']
            sig = h['sig']
            if len(sig) == 0:    #local headline, non-VTEC
                h['vtecstr'] = ""
                continue     

            # get the office ID
            if h.has_key('officeid'):
                siteID = h['officeid']    #4letter id
            else:
                siteID = "????"

            # get the ETN
            if h.has_key('etn'):
                if type(h['etn']) is int:
                    ETN = "%04i" % h['etn']
                else:
                    ETN = h['etn']
            else:
                ETN = "????"

            # get the action
            if h.has_key('act'):
                action = h['act']
            else:
                action = "???"
        
            # adjust time of NEW events to ensure they don't start
            # earlier than now
            if h['startTime'] < self.__time:
                h['startTime'] = self.__time


            # use 00000000 or explicit times for the start time?
            if action is 'NEW' or \
              (action == 'EXT' and h['previousStart'] > self.__time) or \
              (action == 'EXB' and h['previousStart'] > self.__time) or \
              (h['startTime'] > self.__time):
                startStr = time.strftime("%y%m%dT%H%MZ-", 
                  time.gmtime(h['startTime']))
            else:
                startStr = "000000T0000Z-"  #ongoing

            # use 00000000 if event is "Until Further notice"
            if h.get('ufn', 0):
                endStr = "000000T0000Z/"
            else:
                endStr = time.strftime("%y%m%dT%H%MZ/", time.gmtime(h['endTime']))

            # format the beastly string
            vtec = '/' + self.__vtecMode + "." + action + "." +\
               siteID + '.' + phen + '.' + sig + '.' + ETN + '.' + \
               startStr + endStr
            h['vtecstr'] = vtec


    # Add in headlines if missing in the table, note that headlines
    # are not added for situations of merged events, i.e., an event
    # that has a CAN and a ongoing with same phen/sig and overlapping time.
    # Leaving 'hdln' blank indicates no headline and no mention in hazard
    # products.
    def __addHeadlinesIfMissing(self, pTable):
        compare = ['id','phen','sig','pil']
        ongoingAct = ['EXT','EXB','CON','NEW','EXA']
        for h in pTable:
            if h.has_key('hdln'):
                continue
            phensig = h['phen'] + '.' + h['sig']
            if VTECTable.VTECTable.has_key(phensig):

                #ongoing (merged) and CAN situation?
                mergedFound = 0
                for h1 in pTable:
                    if self.hazardCompare(h, h1, compare) and \
                      h['act'] == 'CAN' and h1['act'] in ongoingAct and \
                      h1['endTime'] > self.__time and \
                      h['startTime'] <= h1['endTime'] and \
                      h['endTime'] >= h1['startTime']:
                          mergedFound = 1
                          h['hdln'] = ""

                if mergedFound == 1:
                    h['hdln'] = ""
                else:
                    h['hdln'] = VTECTable.VTECTable[phensig]['hdln']
            else:
                h['hdln'] = ""   

     
    # isUpgrade(), indicates whether rec2 upgrades rec1, only looks
    # at act, phen and sig. Proposed gets NEW, EXA or EXB active gets UPG
    def __isUpgrade(self, proposed, active):
        # To change HazardsTable to have an UPG
        # only if the other hazard is a NEW, EXA or EXB and a CAN if the
        # associated hazard is CON or EXT.
        if proposed['act'] in ['CON', 'EXT']:
            return 0   #not an upgrade
        else:
            if VTECTable.checkForUpgrade(proposed['phen'], proposed['sig'], 
              active['phen'], active['sig']):
                return 1
            else:
                return 0   #not an upgrade
    
    # isDowngrade(), indicates whether rec2 downgrades rec1, only looks
    # at phen and sig. Proposed gets NEW, active gets CAN. 
    def __isDowngrade(self, proposed, active):
        if VTECTable.checkForDowngrade(proposed['phen'], proposed['sig'], 
          active['phen'], active['sig']):
            return 1
        else:
            return 0   #not an downgrade

    # Checks for records with the same phen/sig for the same geographical
    # area (id). Eliminates the records with the lower segment number with
    # same times.  Combines records with multiple segment numbers with 
    # different times. Result is only to have 1 record per ID for phen/sig.
    def __checkForMultipleSegsInSameID(self, pTable):

        #step 1: reorganize the proposed table by zone, then by phen/sig.
        #dict of zones, then dict of phensigs, value is list of records.
        #Also create dictionary of originally max segment numbers for phen/sig.
        orgHaz = {}
        orgMaxSeg = {}  #key:phensig, value: max seg number
        for p in pTable:
            phensig = (p['phen'], p['sig'])
            id = p['id']
            if orgHaz.has_key(id):
                psOrgHaz = orgHaz[id]
                if psOrgHaz.has_key(phensig):
                    records = psOrgHaz[phensig]
                    records.append(p)
                    orgHaz[id][phensig] = records
                else:
                    orgHaz[id][phensig] = [p]
            else:
                orgHaz[id] = {phensig: [p]}

            # tally the original max segment number per phen/sig
            if orgMaxSeg.has_key(phensig):
               orgMaxSeg[phensig] = max(p['seg'], orgMaxSeg[phensig])
            else:
               orgMaxSeg[phensig] = p['seg']
 

        #step 2: Check for multiple records for phensig and zone.
        #Mark records that can be combined (adjacent/overlap).
        for zone in orgHaz.keys():
            for phensig in orgHaz[zone].keys():
                records = orgHaz[zone][phensig]
                # if only 1 record, we have nothing to do
                if len(records) == 1:
                    continue
                records.sort(self.__hazardSortSTET)

                #find adjacent/overlapping, mark them as record number in
                #the dict entry 'rn', track overall tr in trDict (key is 'rn')
                trDict = {}
                for x in xrange(len(records)):
                    xtr = (records[x]['startTime'], records[x]['endTime'])

                    #search for adjacent/overlapping
                    for y in xrange(x+1, len(records)):
                        ytr = (records[y]['startTime'], records[y]['endTime'])
                        rny = records[y].get('rn', None)
                        if rny is None and (self.__isAdjacent(xtr, ytr) or \
                          self.__overlaps(xtr, ytr)):
                            rnx = records[x].get('rn', x)
                            records[y]['rn'] = rnx  #overlaps/adjacent,reuse rn
                            records[x]['rn'] = rnx  #assign to orig to match
                            if trDict.has_key(rnx):
                                trDict[rnx] = self.__combineTR(ytr,trDict[rnx])
                            else:
                                trDict[rnx] = self.__combineTR(xtr,ytr)

                maxSN = self.__maxSegNumber(orgHaz, phensig) #max seg num

                #now assign new segment numbers, reassign starting/ending
                #times for the adjacent/overlaps, delete the temp markers
                for x in xrange(len(records)):
                    rnx = records[x].get('rn', None)
                    if rnx is not None:
                        records[x]['seg'] = maxSN + rnx + 1
                        records[x]['startTime'] = trDict[rnx][0]
                        records[x]['endTime'] = trDict[rnx][1]
                        records[x]['phensig'] = records[x]['phen'] + '.' + \
                          records[x]['sig'] + ':' + `records[x]['seg']`
                        del records[x]['rn']

                #now eliminate records duplicate records
                newrecs = []
                for rec in records:
                    if rec not in newrecs:
                        newrecs.append(rec)
                orgHaz[zone][phensig] = newrecs

        #step 3: Expand back out to list
        updatedList = []
        for zone in orgHaz.keys():
            for phensig in orgHaz[zone].keys():
                records = orgHaz[zone][phensig]
                for r in records:
                    updatedList.append(r)

        #step 4: Combine new segments if possible. We can tell we have
        #generated new segments based on the orgMaxSeg dictionary. We assign
        #them the same segments.
        compare = ['pil','startTime','endTime','phen','sig']
        for x in xrange(len(updatedList)):
            p = updatedList[x]
            phensig = (p['phen'], p['sig'])
            if orgMaxSeg.has_key(phensig):
                orgMax = orgMaxSeg[phensig]
                if p['seg'] > orgMax:         #must be generated segment numb

                    #find matching records and assign all the same seg#
                    #and key
                    for y in xrange(x+1, len(updatedList)):
                        p1 = updatedList[y]
                        if self.hazardCompare(p, p1, compare) and \
                          p1['seg'] > orgMax:
                            p1['seg'] = p['seg']
                            p1['phensig'] = p1['phen'] + '.' + p1['sig'] + \
                              ':' + `p1['seg']`

        #step 5: Eliminate duplicate entries
        finalList = []
        for p in updatedList:
            if p not in finalList:
                finalList.append(p)

        return finalList

    # sort function: hazard records by starting time, then ending time
    def __hazardSortSTET(self, r1, r2):
        if r1['startTime'] < r2['startTime']:
            return -1
        elif r1['startTime'] > r2['startTime']:
            return 1
        else:
            if r1['endTime'] < r2['endTime']:
                return -1
            elif r1['endTime'] > r2['endTime']:
                return 1
            else:
                return 0

    # returns max segment number for zone, phen/sig directory (support routine)
    def __maxSegNumber(self, orgHaz, phensig):
        maxSegNumber = 0
        for zone in orgHaz.keys():
            if orgHaz[zone].has_key(phensig):
                entries = orgHaz[zone][phensig]
                for e in entries:
                    maxSegNumber = max(maxSegNumber, e['seg'])
        return maxSegNumber

    # check for valid etns for all national center products. if not, abort
    def __checkValidETNcw(self, pTable):
        errorLine = '**************************************************\n'
        for p in pTable:
            if (p['phen'],p['sig']) in self.__ncKeys and p['officeid'] != 'PGUM':
                try:
                    a = int(p['etn'])
                except:
                    raise Exception, "\n\n" + errorLine + "\n" +\
                      "ABORTING: Found National Hazard " + \
                      "with no ETN in grids. \n" + self.printActiveTable(p) + \
                      " Fix your grids by adding watch/storm number." + \
                      "\nFor tropical hazards, an override to MakeHazard" +\
                      "\n is likely to blame.\n" + errorLine

    # check for valid ETN/Actions in the analyzed table. Cannot have
    # a split ETN where one part of ongoing/NEW, and the other part
    # is being dropped (e.g., CAN, UPG). pTable is the analyzed active table.
    def __checkValidETNsActions(self, pTable):
        byZones = self.__organizeByZone(pTable)  
        compare = ['etn','phen','sig']
        errorLine = '**************************************************\n'
        currentYear = time.gmtime(self.__time)[0]
        for key in byZones:
            for h in byZones[key]:
                if (h['phen'], h['sig']) not in self.__ncKeys:
                    continue   #only interested in checking national keys
                if h['act'] in ['EXP','UPG','CAN']:
                    hissueTime = h.get('issueTime', 0)
                    hissueYear = time.gmtime(hissueTime)[0] #issueYear
                    for h1 in byZones[key]:
                        if self.hazardCompare(h, h1, compare) and \
                          h1['act'] in ['NEW','CON','EXA','EXT','EXB'] and \
                          currentYear == hissueYear:
                            raise Exception, "\n\n" + errorLine + "\n" +\
                             "ABORTING: Found VTEC Error"\
                             " with same ETN, same hazard, conflicting "\
                             "actions.\n" + self.printActiveTable(h) + \
                             self.printActiveTable(h1) + "\n" + \
                             "Fix, if convective watch, by coordinating "\
                             "with SPC. Otherwise serious software error.\n"\
                             "Cannot have new hazard with same ETN as one "\
                             "that is no longer in effect (EXP, UPG, CAN)."\
                             "\n" + errorLine

    # Remove EXP actions that are 30min past the end of event
    # The records were kept for conflict resolution for national events
    def __removeOverdueEXPs(self, pTable):
        newTable = []
        for p in pTable:
            if p['act'] == 'EXP' and \
               (self.__time - p['endTime']) >= 30*60:
                pass
            else:
                newTable.append(p)
                
        return newTable

    #ensure that we don't have two vtecs with same action code, same etns.
    #Switch the 2nd one to NEW.
    def __checkETNdups(self, pTable):
        keyetnmax = {}
        compare = ['etn', 'phen', 'sig', 'id']
        compare2 = ['phen', 'sig']
        for p in pTable:
            #look for all events to get max etn for each phen/sig
            vteckey = p['phen'] + p['sig']
            if not keyetnmax.has_key(vteckey):
                etn_max = 0
                for e in pTable:
                    if self.hazardCompare(p, e, compare2) and \
                       e['etn'] > etn_max:
                        etn_max = e['etn']
                keyetnmax[vteckey]= etn_max

        assigned = {}
        for p in pTable:
            #only look for EXT, EXA, EXB events
            if p['act'] in ['NEW', 'EXP', 'UPG', 'CAN', 'CON']:
                continue
            vteckey = p['phen'] + p['sig']

            for p1 in pTable:
                #check for matching id,etn,phen,sig,act combinations, these
                #are the ones that need to be reassigned. 
                if self.hazardCompare(p, p1, compare) and \
                  p['startTime'] > p1['endTime']:
                    #found a newer record that needs to be reassigned
                    #see if we have already reassigned one that overlaps in time
                    #  phensig startend etn   doublenested dictionary
                    akey = p['phen'] + p['sig']
                    tr = (p['startTime'], p['endTime'])
                    trs = assigned.get(akey, {})
                    etna = None
                    for tre in trs.keys():
                        if self.__overlaps(tr, tre):
                            etna = trs[tre]  #get previously reassigned
                            #update dictionary if time overlapped
                            trComb = self.__combineTR(tr, tre)
                            if tr != trComb:
                                del trs[tre]
                                trs[trComb] = etna
                                assigned[akey] = trs
                            break

                    if etna is not None:
                        p['act'] = 'NEW'
                        p['etn'] = etna

                    else:
                        #take the newest record and assign new and give new ETN
                        p['act'] = 'NEW'
                        p['etn'] = int(keyetnmax[vteckey]) + 1
                        trs[tr] = p['etn']   #new etn assigned
                        assigned[akey] = trs  #put back into dictionary
                        keyetnmax[vteckey]= p['etn']  #updated for new assign

    def __warnETNduplication(self, pTable):
        # Check should only operate on applicable VTEC products.
        if self.__pil not in \
                ['CFW', 'FFA', 'MWW', 'NPW', 'RFW', 'WSW']:
            return

        dups = []
        byZones = self.__organizeByZone(pTable)  
        for id, hazards in byZones.iteritems():
            visited = []
            for p in hazards:
                key = p['phen'], p['sig'], p['etn']
                if key in visited:
                    estr = "%s.%s:%d" % key
                    if estr not in dups:
                        dups.append(estr)
                else:
                    visited.append(key)

        if len(dups) > 0:
            errorLine = '\n******************************************************\n'
            LogStream.logProblem("Illegal ETN duplication is found for:\n", \
                                 dups, errorLine)

            # send message to GFE
            msg = "The formatted %s product contains a duplicate ETN.\n"\
                  "Please transmit the product and then open a trouble ticket with the NCF."\
                  % self.__pil
            os.system("sendGfeMessage -u -c GFE -m '" + msg + "'")

    # copy text/overviewText into record from active to proposed
    def __copyTextFields(self, proposed, active):
        if active.has_key("segText"):
            proposed['prevText'] = active['segText']
        if active.has_key("overviewText"):
            proposed['prevOverviewText'] = active['overviewText']

 
    # add upgrade/downgrade records from the active table
    def __addUpgradeDowngradeRec(self, proposedTable):
        compare = ['id', 'pil', 'officeid']
        fields = ['etn', 'startTime', 'endTime', 'phen', 'sig', 'phensig', 'act']
        for rec in proposedTable:
            if rec['act'] != 'NEW':
                continue
            for checkR in proposedTable:
                if checkR['act'] not in ['CAN', 'UPG']:
                    continue
                if self.__hazardsOverlap(checkR, rec) and \
                   self.hazardCompare(checkR, rec, compare):
                    ###################
                    if self.__isDowngrade(rec, checkR):
                       rec['downgradeFrom'] = self.__copyFields(checkR, fields)
                    elif self.__isUpgrade(rec, checkR):  
                       rec['upgradeFrom'] = self.__copyFields(checkR, fields)

        return proposedTable

        
    ############################################
    # 'inject' is the main function in vtec.py #
    ############################################

    def __mergeActiveProposed(self, pTable, activeTable, pil, areas):

        # convert active table EXP still in effect to CON
        activeTable = self.__convertEXPtoCON(activeTable)
        self.log.debug("After convertEXPtoCON: " +  
          self.printActiveTable(pTable, combine=True))

        # Special handling for the SPC watches (TO.A, SV.A)
        pTable = self.__handleSPCWatches(pTable, activeTable)
        self.log.debug("After handleSPCWatches: " + 
          self.printActiveTable(pTable, combine=True))

        # Drop multiple segments for same phen/sig in same "id"
        pTable = self.__checkForMultipleSegsInSameID(pTable)
        self.log.debug("After checkForMultipleSegsInSameID: " + 
          self.printActiveTable(pTable, combine=True))
       
        # Check for Merged Events
        pTable, activeTable = self.__checkForMergedEvents(pTable, activeTable)
        self.log.debug("After checkForMergedEvents: " + 
          self.printActiveTable(pTable, combine=True))

        # Check for CON and EXT actions
        pTable = self.__checkForCONEXT(pTable, activeTable)
        self.log.debug("After checkForCONEXT: " + 
          self.printActiveTable(pTable, combine=True))

        # Check for CAN, EXP, and UPG
        pTable = self.__checkForCANEXPUPG(pTable, activeTable)
        self.log.debug("After checkForCANEXPUPG: " + 
          self.printActiveTable(pTable, combine=True))

        # Check for EXA/EXB
        pTable = self.__checkForEXAEXB(pTable, activeTable)
        self.log.debug("After checkForEXAEXB: " + 
          self.printActiveTable(pTable, combine=True))

        # Assign NEW to remaining records
        pTable = self.__checkForNEW(pTable, activeTable)
        self.log.debug("After checkForNEW: " +  
          self.printActiveTable(pTable, combine=True))

        # Check for upgrades and downgrades, add records if needed
        pTable = self.__addUpgradeDowngradeRec(pTable)
        self.log.debug("After addUpgradeDowngradeRec: " + 
          self.printActiveTable(pTable, combine=True))

        # Convert ongoing events about ready to expire (still in the
        # proposed grids) to switch from CON to EXP
        pTable = self.__addEXPCodes(pTable)
        self.log.debug("After addEXPCodes: " + 
          self.printActiveTable(pTable, combine=True))

        # Eliminate any EXPs if other events (same phen/sig) in effect
        # at present time.
        pTable = self.__removeEXPWithOngoingCodes(pTable)
        self.log.debug("After removeEXPWithOngoingCodes: " + 
          self.printActiveTable(pTable, combine=True))

        # Ensure valid ETN/Actions - no EXP/CAN with valid same ETN
        # for national events
        self.__checkValidETNsActions(pTable)
        self.log.debug("After checkValidETNsActions:" +
          self.printActiveTable(pTable, combine=True))

        # Remove EXPs that are 30mins past the end of events
        pTable = self.__removeOverdueEXPs(pTable)
        self.log.debug("After removeOverdueEXPs:" +
          self.printActiveTable(pTable, combine=True))

        # Ensure that there are not ETN dups in the same segment w/diff
        # action codes
        self.__checkETNdups(pTable)
        self.log.debug("After checkETNdups:" +
          self.printActiveTable(pTable, combine=True))

        # Warn user about ETN duplication if any
        self.__warnETNduplication(pTable)

        # Complete the VTEC Strings
        self.__addVTECStrings(pTable)
        self.log.debug("After addVTECStrings: " + 
          self.printActiveTable(pTable, combine=True))

        #add in hdln entries if they are missing
        self.__addHeadlinesIfMissing(pTable)
        self.log.debug("After addHeadlinesIfMissing: " + 
          self.printActiveTable(pTable, combine=True))        

        # Ensure that all SV.A and TO.A have valid ETNs
        self.__checkValidETNcw(pTable)

        # Return pTable, which is essentially analyzedTable at this point
        return pTable

# This section no longer needed with tropical ETN consolidation
#    # is marine zone?
#    def __isMarineZone(self, id):
#        if id[0:2] in self.__marineZonesPrefix:
#            return True;
#        else:
#            return False;
#
#    # marine zones and non-marine zones for tpc phen/sigs follow their own
#    # sequence of ETNs and actions. This routine determines if separate
#    # ETNs/actions should occur between id1 and id2. Returns true if 
#    # separate ETN tracks are required - basically if id1 and id2 are one
#    # marine and the other not, and the phen/sigs are identical and are tpc
#    # phen/sigs. Also returns true if phen/sigs are not identical.  Otherwise
#    # returns false.  Only considers phen/sig/id.
#    def __separateETNtrack(self, rec1, rec2):
#        ps1 = (rec1['phen'], rec1['sig'])
#        ps2 = (rec2['phen'], rec2['sig'])
#        # same phen/sig
#        if ps1 == ps2:
#            # tropical?
#            if ps1 in self.__tpcKeys:
#                # one a marine zone, the other not?, that requires sepa track
#                return (self.__isMarineZone(rec1['id']) != \
#                  self.__isMarineZone(rec2['id']))
#            else:
#                return False   #same phen/sig, not tpc, so. non separate track
#        else:
#            return true;

    def __processJavaCollection(self, javaObj, processMethod=None):
        retVal = []
        iter = javaObj.iterator()
        while iter.hasNext():
            nextObj = iter.next()
            if processMethod is not None:
                nextObj = processMethod(nextObj)
            retVal.append(nextObj)
        return retVal

    def __convertPhensig(self, javaPhensig):
        phenSig = tuple(str(javaPhensig).split('.'))
        return phenSig
