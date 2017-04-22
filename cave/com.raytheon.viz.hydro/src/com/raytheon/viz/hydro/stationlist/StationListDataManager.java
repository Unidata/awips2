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
package com.raytheon.viz.hydro.stationlist;

import java.util.ArrayList;
import java.util.SortedMap;
import java.util.TreeMap;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Singleton class for managing database query calls.
 * StationListDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Sept2008   1507       dhladky     Initial creation.
 * 10/2/2008    1555       grichard    Support station selection.
 * 11/19/2008   1662       grichard    Support pt. precip. accum.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class StationListDataManager {

    private static final String STATION_QUERY = "select lid,name,lon,lat,hsa from location";

    private static StationListDataManager manager = null;

    private SortedMap<String, StationListData> stationData;

    /**
     * Private constructor.
     */
    private StationListDataManager() {
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized StationListDataManager getInstance() {
        if (manager == null) {
            manager = new StationListDataManager();
        }

        return manager;
    }

    /**
     * Get the station list data sorted by lid.
     */
    public SortedMap<String, StationListData> getStationListData() {

        if (stationData == null) {
            stationData = new TreeMap<String, StationListData>();

            try {
                ArrayList<Object[]> data = (ArrayList<Object[]>) DirectDbQuery
                        .executeQuery(STATION_QUERY, HydroConstants.IHFS,
                                QueryLanguage.SQL);
                for (Object[] rowData : data) {
                    /* Verify data are not null and lat/lon values are above 0 */
                    if (((rowData[2] != null) && (rowData[3] != null)) && 
                            ((Double)rowData[2] > 0) && ((Double)rowData[3] > 0)) {
                        if (rowData[1] == null) {
                            rowData[1] = "    ";
                        }
                        stationData.put((String) rowData[0], new StationListData(
                                (String) rowData[0], (String) rowData[1],
                                (Double) rowData[2], (Double) rowData[3],
                                (String) rowData[4]));
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return stationData;
    }
}
