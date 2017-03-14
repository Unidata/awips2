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
package com.raytheon.viz.aviation.climatedata;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * This is the main data class for the Climate History Data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2010 #4549      lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ClimateHistoryData {
    /**
     * Map of station names (key) and station data (object).
     */
    private Map<String, List<StationData>> stationDataMap;

    /**
     * Constructor.
     */
    public ClimateHistoryData() {
        stationDataMap = new LinkedHashMap<String, List<StationData>>();
    }

    /**
     * Get the station data associated with the station name.
     * 
     * @param stationName
     *            The station name.
     * @return Array of station data.
     */
    public List<StationData> getStationData(String stationName) {
        return stationDataMap.get(stationName);
    }

    /**
     * Add station data.
     * 
     * @param stationName
     *            Station name.
     * @param stationDataArray
     *            Array of station data.
     */
    public void addStationData(String stationName,
            List<StationData> stationDataArray) {
        stationDataMap.put(stationName, stationDataArray);
    }

    /**
     * Get an array of station names in alphabetical order.
     * 
     * @return Array of station names.
     */
    public List<String> getStationNames() {
        List<String> names = new ArrayList<String>();
        Set<String> keys = stationDataMap.keySet();

        for (String key : keys) {
            names.add(key);
        }

        Collections.sort(names);

        return names;
    }
}
