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
package com.raytheon.uf.viz.monitor.util;

import java.util.HashMap;

import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;

/**
 * MonitorConfiguration object stores configuration data common to all Monitors.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2009 2076       avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */
public class MonitorConfiguration {

    private String[] zoneList;

    private String[] stationList;

    private HashMap<String, String[]> zoneMap;

    private String[] columnList;

    /**
     * @param store
     *            HierarchicalPreferenceStore
     * @return zoneList String[]
     */
    public String[] getZoneList(HierarchicalPreferenceStore store) {
        if (zoneList == null) {
            String key = MonitorConfigConstants.AREA_IDS_KEY
                    + MonitorConfigConstants.ALL_ZONES_KEY;
            zoneList = store.getStringArray(
                    HierarchicalPreferenceStore.Level.COMBINED, key);
        }

        return zoneList.clone();
    }

    /**
     * @param store
     *            HierarchicalPreferenceStore
     * @return stationList String[]
     */
    public String[] getStationList(HierarchicalPreferenceStore store) {
        if (stationList == null) {
            String key = MonitorConfigConstants.AREA_IDS_KEY
                    + MonitorConfigConstants.STATION_LIST_KEY;
            stationList = store.getStringArray(
                    HierarchicalPreferenceStore.Level.COMBINED, key);
        }

        return stationList.clone();
    }

    /**
     * @param store
     *            HierarchicalPreferenceStore
     * @return columnList String[]
     */
    public String[] getColumnList(HierarchicalPreferenceStore store) {
        if (columnList == null) {
            String key = MonitorConfigConstants.COLUMN_LIST_KEY;
            columnList = store.getStringArray(
                    HierarchicalPreferenceStore.Level.COMBINED, key);
        }

        return columnList.clone();
    }

    /**
     * @param store
     *            HierarchicalPreferenceStore
     * @return zoneMap HashMap<String, String[]>
     */
    public HashMap<String, String[]> getZoneMap(
            HierarchicalPreferenceStore store) {
        if (zoneMap == null) {
            String key = MonitorConfigConstants.AREA_IDS_KEY
                    + MonitorConfigConstants.ACTIVE_ZONES_KEY;
            String[] zones = store.getStringArray(
                    HierarchicalPreferenceStore.Level.COMBINED, key);
            zoneMap = new HashMap<String, String[]>();

            for (int i = 0; i < zones.length; i++) {
                key = MonitorConfigConstants.AREA_IDS_KEY + zones[i]
                        + MonitorConfigConstants.STATIONS_KEY;
                String[] stations = store.getStringArray(
                        HierarchicalPreferenceStore.Level.COMBINED, key);

                zoneMap.put(zones[i], stations);
            }
        }

        return new HashMap<String, String[]>(zoneMap);
    }

    /**
     * Load the internal data structures from the HierarchicalPreferenceStore.
     * This is common data used by all monitors.
     * 
     * @param store
     *            HierarchicalPreferenceStore
     */
    public void reload(HierarchicalPreferenceStore store) {
        // Fill zoneList with all known zones.
        String key = MonitorConfigConstants.AREA_IDS_KEY
                + MonitorConfigConstants.ALL_ZONES_KEY;
        zoneList = store.getStringArray(
                HierarchicalPreferenceStore.Level.COMBINED, key);

        // Fill stationList with all known stations.
        key = MonitorConfigConstants.AREA_IDS_KEY
                + MonitorConfigConstants.STATION_LIST_KEY;
        stationList = store.getStringArray(
                HierarchicalPreferenceStore.Level.COMBINED, key);

        // Fill zoneMap with a mapping of all watched zones and their associated
        // stations.
        key = MonitorConfigConstants.AREA_IDS_KEY
                + MonitorConfigConstants.ACTIVE_ZONES_KEY;
        String[] zones = store.getStringArray(
                HierarchicalPreferenceStore.Level.COMBINED, key);
        zoneMap = new HashMap<String, String[]>();

        for (int i = 0; i < zones.length; i++) {
            key = MonitorConfigConstants.AREA_IDS_KEY + zones[i]
                    + MonitorConfigConstants.STATIONS_KEY;
            String[] stations = store.getStringArray(
                    HierarchicalPreferenceStore.Level.COMBINED, key);

            zoneMap.put(zones[i], stations);
        }

        // Fill columnList with all visible columns.
        key = MonitorConfigConstants.COLUMN_LIST_KEY;
        columnList = store.getStringArray(
                HierarchicalPreferenceStore.Level.COMBINED, key);
    }
}
