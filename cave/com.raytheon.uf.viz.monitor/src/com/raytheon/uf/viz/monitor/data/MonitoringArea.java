/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.monitor.data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.monitor.data.ObConst;

/**
 * This class provides functions to return all zones in SAFESEAS monitoring
 * area, all zones in home WFO area of responsibility, and the associations
 * between zones and stations. It also provides function to determine the
 * associations between zones and moving buoys/ships.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009 1999       grichard    Initial creation.
 * 3/16/2009    2047       grichard    Add zone related routines.
 * Dec  9, 2009 3424       zhao        Added method getZoneId(platformId)
 * Nov. 1, 2012 1297       skorolev    Changed ArrayList to List.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public final class MonitoringArea {

    /**
     * Private constructor -- all contents must be public static
     */
    private MonitoringArea() {
    }

    /**
     * The map that contains the zoneIds for all platform identifiers
     */
    private static Map<String, List<String>> zoneMap = new HashMap<String, List<String>>();

    /**
     * The map that contains the platformIds for all zone identifiers
     */
    private static Map<String, List<String>> platformMap = new HashMap<String, List<String>>();

    /**
     * Get the monitoring area time window
     * 
     * @return
     */
    public static int getTimeWindow() {
        return ObConst.THREAT_INTERVAL_HOURS;
    }

    /**
     * Getter for zone map
     * 
     * @return
     */
    public static Map<String, List<String>> getZoneMap() {
        return zoneMap;
    }

    /**
     * Setter for zone map
     * 
     * @param zoneMap
     */
    public static void setZoneMap(Map<String, List<String>> zoneMap) {
        MonitoringArea.zoneMap = zoneMap;
    }

    /**
     * Getter for platform map
     * 
     * @return
     */
    public static Map<String, List<String>> getPlatformMap() {
        return platformMap;
    }

    /**
     * Setter for platform map
     * 
     * @param platformMap
     */
    public static void setPlatformMap(Map<String, List<String>> platformMap) {
        MonitoringArea.platformMap = platformMap;
    }

    /**
     * Getter for the List of all zones which associate with the given platform
     * 
     * @param platformId
     * @return
     */
    public static List<String> listZonesToPlatform(String platformId) {
        return getZoneMap().get(platformId);
    }

    // Getter for the List of all platforms which associate with the given zone
    public static List<String> listPlatformsToZone(String zoneId) {
        return getPlatformMap().get(zoneId);
    }

    /**
     * Returns the zone IDs that a caller-specified station is associated with
     * 
     * @param station
     * @return zones that the station is associated with
     */
    public static ArrayList<String> getZoneIds(String station) {
        ArrayList<String> theZones = new ArrayList<String>();
        for (String zone : platformMap.keySet()) {
            if (platformMap.get(zone).contains(station)) {
                theZones.add(zone);
            }
        }
        return theZones;
    }

    /**
     * @return list of stations
     */
    public static String getPlatformIdList() {
        StringBuilder stList = new StringBuilder();
        Set<String> stns = new HashSet<String>();
        for (String zone : platformMap.keySet()) {
            List<String> platfrms = platformMap.get(zone);
            stns.addAll(platfrms);
        }
        stList.append(stns.toString());
        stList.deleteCharAt(0);
        stList.deleteCharAt(stList.length() - 1);
        return stList.toString();
    }

}
