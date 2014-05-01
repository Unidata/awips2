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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# PlotTPCEvents1
#
# This procedure synchonizes the hazards from TPC that are in the active table
# for some zones.
#
#
# Author: lefebvre/mathewson
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Hazards"]

from numpy import *
import SmartScript
import AbsTime
import HazardUtils
import LogStream, logging
import UFStatusHandler

PLUGIN_NAME = 'com.raytheon.viz.gfe'
CATEGORY = 'GFE'

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        self._dbss = dbss

        logging.basicConfig(level=logging.INFO)
        self.log = logging.getLogger("PlotSPCWatches")
        
        self.log.addHandler(UFStatusHandler.UFStatusHandler(PLUGIN_NAME, CATEGORY, level=logging.WARNING))    

    ##
    # getWatches returns ({activeWatches}, {cancelledWatches})
    # the returned records are not VTEC table entries, but rather
    # a dictionary with key of (phen,sig) and value of zone ids
    # 
    # @return: active watches and cancelled watches
    # @rtype: 2-tuple of dicts; each dict is keyed by a 2-tuple of strings,
    #         and contains lists of strings for its values.
    def getWatches(self):
        """
        getWatches returns ({activeWatches}, {cancelledWatches}).
        The returned records are dictionaries with keys of (phen, sig)
        and values of [zoneida, zoneidb,...].
        """
        
        active = {}  #add records
        cancel = {}  #remove records

        vtecTable = self.vtecActiveTable()        
        vtecTable = self._hazUtils._filterVTECBasedOnGFEMode(vtecTable)
        

        # step 1: just save the tropical hazards
        for v in vtecTable:

            # filter based on phen/sig
            phenSig = v['phen'] + "." + v['sig']

            # process only records in the phenSigList
            if not v['phen'] in ['HU','TR']:
                continue

            # only look at the KNHC records
            if v['officeid'] != 'KNHC':
                continue

            key = (v['phen'], v['sig'], v['etn'])

            # cancel events
            if v['act'] in ['CAN','EXP','UPG']:
                if not key in cancel:
                    cancel[key] = []
                cancel[key].append(v['id'])

            # active events
            elif v['act'] in ["NEW", "EXA", "EXB", "EXT", "CON"]:
                if not key in active:
                    active[key] = []
                active[key].append(v['id'])

        return (active, cancel)

    ##
    #   Remove all tropical grids from the Hazards inventory that contain
    #   the specified phen, sig for the given zone. zone can be a single
    #   zone name, or a list of zones.
    #
    # @param phen: Phenomenon code
    # @type phen: string 
    # @param sig: Significance code
    # @type sig: string
    # @param zones: Zones from which hazard should be removed
    # @type zones: list of strings or string
    def removeHazardByZone(self, phen, sig, zones):
        """
        Remove all tropical grids from the Hazards inventory that contain
        the specified phen, sig for the given zone. zone can be a single
        zone name, or a list of zones.
        """
        
        if type(zones) is not list:
            zonemask = self._hazUtils._makeMask([zones])
        else:
            zonemask = self._hazUtils._makeMask(zones)

        trList = self._hazUtils._getWEInventory("Hazards")
        for tr in trList:
            byteGrid, hazKey = self.getGrids("Fcst", "Hazards", "SFC", tr,
                                             mode="First", cache=0)
            uniqueKeys = self._hazUtils._getUniqueKeys(byteGrid, hazKey)

            startT = self._hazUtils._printTime(tr.startTime().unixTime())
            endT = self._hazUtils._printTime(tr.startTime().unixTime())

            for uKey in uniqueKeys:
                subKeys = self._hazUtils._getSubKeys(uKey)
                if subKeys is not None:
                    for subKey in subKeys:
                        hazPhen = self._hazUtils._keyPhen(subKey)
                        hazSig = self._hazUtils._keySig(subKey)  #may have seg
                        if phen == hazPhen and sig == hazSig[0]:
                            self._hazUtils._removeHazard("Hazards", tr, subKey,
                              mask=zonemask)
                            LogStream.logEvent("Remove: ", startT, endT, 
                              subKey, zones)
        return

    def execute(self, editArea, timeRange, varDict):
        # get the hazard utilities
        self._hazUtils = HazardUtils.HazardUtils(self._dbss, None)

        self.setToolType("numeric")

        #see if the Hazards WE is loaded in the GFE, if not abort the tool
        if not self._hazUtils._hazardsLoaded():
            self.statusBarMsg("Hazards Weather Element must be loaded in "+\
              "the GFE before running PlotTPCEvents.", "S")
            self.cancel()

        #ensure there are no temp grids loaded, refuse to run
        if self._hazUtils._tempWELoaded():
            self.statusBarMsg("There are temporary hazard grids loaded. " + \
                "Please merge all hazards grids before running PlotTPCEvents.", "S")
            self.cancel()

        if self.lockedByOther('Hazards', 'SFC'):
            self.statusBarMsg("There are conflicting locks (red locks - owned by others) on Hazards.  " + \
                "Please resolve these before running PlotTPCEvents", "S")
            self.cancel()

        # get just the tropical events
        (activeEvents, cancelEvents) = self.getWatches()

        # remove any cancelled watches from the grids, but for all times
        for phen,sig,etn in cancelEvents.keys():  
            self.removeHazardByZone(phen, sig, cancelEvents[(phen,sig,etn)])

        # remove any active events from the grid, to handle any slight grid
        # time differences from previous runs
        for phen,sig,etn in activeEvents.keys():
            self.removeHazardByZone(phen, sig, activeEvents[(phen,sig,etn)])

        #have to fake the start/ending times since tropical events
        #have no time information - changed to 48 hours in 9.3
        startT = int(AbsTime.current().unixTime()/3600) * 3600
        endT = startT + 48*3600

        # add active events 
        timeRange = self._hazUtils._makeTimeRange(startT, endT)
        for phen, sig, etn in activeEvents.keys():
            key = phen + '.' + sig + ':' + str(etn)
            zones = activeEvents[(phen, sig, etn)]
            zoneMask = self._hazUtils._makeMask(zones)
            self._hazUtils._addHazard("Hazards", timeRange, key, zoneMask)
            LogStream.logVerbose("Add:", self._hazUtils._printTime(startT),
              self._hazUtils._printTime(endT), key, zones)

        LogStream.logVerbose("PlotTPCEvents completed normally.", "(" + 
                             str(len(activeEvents.keys())) + " events)")
        return
