# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ZoneMap - Object that makes calculating masks over zones much faster.
#           This class creates an internal grid that uniquely identifies
#           the zone and determines whether it's a coastal or inland zone.
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# May  9, 2019 21020      tlefebvr    Original version
# May 16, 2019 21020      tlefebvr    Code review changes
# June 3, 2019 21020      tlefebvr    Added code to handle undefined edit are more
#                                     gracefully
# Aug 16, 2019 21020      tlefebvr    Code clean-up and added CoastalZone auto-
#                                     generation.
# Aug 20, 2019 21020      tlefebvr    Fixed issue when CoastalZone edit area was
#                                     missing.
# Aug 22, 2019 21020      tlefebvr    Minor code review changes.
# Apr  7, 2020 21682      tlefebvr    Minor change to getOverlappingZoneNames
# May  6, 2020 21682      tlefebvr    Code clean-up.
# May  6, 2020 21682      tlefebvr    Addressed code review comments.
# June 3, 2020 21682      tlefebvr    Addressed code review comments.

##################################################################################

import SmartScript
import numpy as np

class ZoneMap(SmartScript.SmartScript):
    def __init__(self, dbss):
        """
        Define some constants and make the coastal mask used to determine the zone type.
        """
        SmartScript.SmartScript.__init__(self, dbss)

        # List of valid state IDs.
        self._stateIDList = ['No State', 'SC', 'NC', 'NJ', 'TX', 'MD', 'VA', 'MA', 'RI', 'LA', 'ME',
                             'GA', 'MS', 'FL', 'AL', 'OK', 'DE', 'NY', 'CT', 'NH', 'AR', 'WV', 'PA',
                             'TN', 'MO', 'PR', 'VI', 'CA', 'HI']
        self._zoneMapName = "NHCZoneMap"
        self._objectCategory = "ZoneMap"
        self._coastalMask = None  # initialize before defining
        self._coastalMask = self.coastalZonesMask()
        self.getZoneMap()

    def getZoneMap(self):
        """
        Attempts to fetch the map from the server. If it's not found, it will
        create it from scratch and then save it for faster access later.
        It takes about 2-3 minutes to make the map and 2-3 seconds to fetch it.
        """
        # Try to fetch it from the server first for faster access
        try:
            self._zoneMap = self.getObject(self._zoneMapName, self._objectCategory)
        except:
            self._zoneMap = self.makeAndStoreCoastalZoneMap() # stored for later use
            
    def nhcMask(self):
        """
        Returns the mask corresponding to the national center area of responsibility
        """
        return (self._zoneMap > 0.0) & (self._zoneMap <= 100.0)

    def coastalZonesMask(self):
        """
        Return the coastal mask fetched in the constructor. Calculate this in real-time
        if the mask is None and save it as an edit area.
        """
        #If we calculated this before, just return it.
        if self._coastalMask is not None:
            return self._coastalMask
        # Otherwise try to fetch the Coastal Zone Edit area
        eaName = "WindWWEditAreaCoastalZones"
        try:
            self._coastalMask = self.encodeEditArea(self.getEditArea(eaName))
        # If the edit area was not found, re-create the edit area from the zone list.
        except AttributeError:
            import CoastalZoneDefinition
            self._coastalMask = self.empty(np.bool)
            CoastalZones = CoastalZoneDefinition.CoastalZoneList
            for zone in CoastalZones:
                try:
                    mask = self.encodeEditArea(self.getEditArea(zone))
                    self._coastalMask = self._coastalMask | mask
                except AttributeError:
                    self.statusBarMsg("Zone: " + zone + " was not found. Please remove from CoastZoneDefinition.", "S")

            ea = self.decodeEditArea(self._coastalMask)
            # Save the area for next time
            self.saveEditArea(eaName, ea)
            self.statusBarMsg(eaName + " was created from the zone list and must be saved under SITE!", "U")
        return self._coastalMask

    def makeAndStoreCoastalZoneMap(self):
        """
        Creates the zoneID grid (map) and returns it. Converts each zoneName to
        a zoneID and sets the grid to the zoneID over the zone's mask.
        """
        # Fetch the set of zones
        grid = self.empty()
        # For each zone store a number where that zone lives starting with 1.
        # Zero means no zone covers that point
        zoneList = self.getZoneList()
        for z in zoneList:
            mask = self.encodeEditArea(z)
            zoneID = self.zoneID(z, mask)
            grid[mask] = zoneID

        # Save the object for faster access next time
        self.saveObject(self._zoneMapName, grid, self._objectCategory)
        self.statusBarMsg("Saved ZoneMap.", "R")

        return grid

    def getZoneList(self):
        """
        Returns the entire list of editAreas configured for the system matching
        the pattern STZNNN, where ST is the stateID, Z is literal, NNN is the
        zone number. This filters out all but public zones.
        """
        zoneList = []
        eaList = self.editAreaList()
        for ea in eaList:
            if len(ea) == 6 and ea[0:2] in self._stateIDList and ea[2] == "Z":
                zoneList.append(ea)
        return zoneList

    def zoneName(self, zoneID):
        """
        Returns the zoneName for the specified zoneID
        """
        stateIndex = int(zoneID)
        zoneNum = (zoneID - stateIndex) * 1000
        zoneNum = int(self.round(zoneNum, "Nearest", 1))
        
        if stateIndex >= 100:
            stateIndex = stateIndex - 100

        zoneName = self._stateIDList[stateIndex] + "Z" + str(zoneNum).zfill(3)
        
        return zoneName

    def zoneID(self, zoneName, zoneMask=None):
        """
        Returns the zoneID. If the mask is not specified, it will fetch it.
        This is a performance enhancement for times when the mask is known.
        """
        if zoneMask is None:
            try:
                zoneMask = self.encodeEditArea(zoneName)
            except AttributeError:
                self.statusBarMsg(zoneName + " is not a valid edit area.", "S")
                return None

        stateID = zoneName[0:2]
        if stateID not in self._stateIDList:
            self.statusBarMsg("Unknown StateID: " + stateID, "S")
            return None
        try:
            zoneNum = int(zoneName[3:])
        except:
            self.statusBarMsg("Bad zone name: " + zoneName, "S")
            return None

        stateIndex = self._stateIDList.index(stateID)
        
        if self.isACoastalZone(zoneMask):
            zoneID = stateIndex + (zoneNum / 1000.0)
        else:
            zoneID = stateIndex + 100 + (zoneNum / 1000.0)

        return zoneID

    def isACoastalZone(self, zoneMask):
        """
        Returns True if the zone is a Coastal zone and False otherwise.
        """
        overlap = zoneMask & self._coastalMask

        return overlap.any()

    def zoneMapGrid(self):
        """
        Returns the zone map, a grid of zoneIDs
        """

        return self._zoneMap

    def getOverlappingZoneNames(self, mask):
        """
        Returns a list of zoneNames that overlap the specified mask
        """
        whereArray = np.extract(mask, self._zoneMap)
        
        uniqueList = np.unique(whereArray).tolist()
        zoneList = []
        for zoneID in uniqueList:
            zoneName = self.zoneName(zoneID)
            if "No State" in zoneName:
                continue
            zoneList.append(zoneName)

        return zoneList

    def getOverlappingZoneIDs(self, mask):
        """
        Returns a list of zoneIDs that overlap the specified mask
        """

        whereArray = np.extract(mask, self._zoneMap)

        uniqueList = np.unique(whereArray).tolist()
        zoneIDList = []
        for zoneID in uniqueList:
            if zoneID > 0.0:
                zoneIDList.append(zoneID)

        return zoneIDList

    def maskFromZoneList(self, zoneList):
        """
        Returns the mask that covers the specified list of zones.
        """
        mask = self.empty(np.bool)
        for zone in zoneList:
            zoneID = self.zoneID(zone)
            if zoneID is None:
                continue
            mask = mask | (self._zoneMap == zoneID)
        return mask

