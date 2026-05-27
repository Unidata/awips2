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
package com.raytheon.viz.hydrocommon;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Class for managing database query calls. HydroDataManager.java
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Sept2008   #1509      dhladky     Initial Creation
 * 09Sept2009   #2259      mpduff      Refactored to HydroCommon
 * Jun 08, 2016  5571      njensen     Fixed isAlertStation(String)
 * Feb 01, 2019  6951      dgilling    Added getLocationName, code cleanup.
 *
 * </pre>
 *
 * @author dhladky
 */

public class HydroDataCache {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static HydroDataCache cache = null;
    private HashMap<String, String> locationMap = null;
    private Map<String, String> peMap = null;
    private Map<String, String> tsMap = null;
    private Map<String, double[]> llMap = null;
    private Map<String, String> hsaMap = null;
    private Set<String> alertSet = null;

    private static final String PE_QUERY = "select pe,name from shefpe";

    private static final String TS_QUERY = "select ts,name from shefts";

    /* Private Constructor */
    private HydroDataCache() { }

    public static synchronized HydroDataCache getInstance() {
        if (cache == null) {
            cache = new HydroDataCache();
        }
        return cache;
    }

    /**
     * Get Location Data from the DB
     * @return ArrayList<String[]>
     */
    private List<String[]> getLocationData() {
        try {
            List<Object[]> data = DirectDbQuery.executeQuery(
                    "select lid, name from location", HydroConstants.IHFS,
                    QueryLanguage.SQL);
            if (data != null) {
                return data.stream().map(d -> {
                    return new String[] { d[0].toString(),
                            (d[1] != null) ? d[1].toString()
                                    : StringUtils.EMPTY };
                }).collect(Collectors.toList());
            }
        } catch (VizException e) {
            statusHandler.warn("Unable to retrieve data from location table.",
                    e);
        }

        return Collections.emptyList();
    }

    /**
     * Get the locationMap
     * @return HashMap
     */
    public Map<String, String> getLocationMap() {
        if (locationMap == null) {
            List<String[]> locData = getLocationData();
            if (!locData.isEmpty()) {
                locationMap = new HashMap<>();
                for (String[] row : locData) {
                    locationMap.put(row[0], row[1]);
                }
            } else {
                return Collections.emptyMap();
            }
        }

        return locationMap;
    }

    public Optional<String> getLocationName(String lid) {
        return Optional.ofNullable(getLocationMap().get(lid));
    }

    /**
     * Get the Physical Element Description
     *
     * @param pe - the physical element to look up
     *
     * @return The description
     */
    public String getPEDescription(String pe) {
        /* the first time get all data from the db and store for late use */
        if (peMap == null) {
            peMap = new HashMap<>();

            try {
                ArrayList<Object[]> data = (ArrayList<Object[]>)DirectDbQuery.executeQuery(PE_QUERY, HydroConstants.IHFS, QueryLanguage.SQL);;

                if (data == null) {
                    throw new VizException("Error querying ShefPE table");
                }

                for (int i = 0; i < data.size(); i++) {
                    Object[] row = data.get(i);
                    peMap.put((String)row[0], (String)row[1]);
                }
            } catch (VizException e) {
                statusHandler.warn("Error querying ShefPE table", e);
            }
        }

        return peMap.get(pe);
    }

    /**
     * Get the Type Source Description
     *
     * @param ts - the type source to look up
     *
     * @return The description
     */
    public String getTSDesc(String ts) {
        /* the first time get all data from the db and store for later use */
        if (tsMap == null) {
            tsMap = new HashMap<>();
            try {
                ArrayList<Object[]> data = (ArrayList<Object[]>)DirectDbQuery.executeQuery(TS_QUERY, HydroConstants.IHFS, QueryLanguage.SQL);

                if (data == null) {
                    throw new VizException("Error querying ShefTS table");
                }
                for (int i = 0; i < data.size(); i++) {
                    Object[] row = data.get(i);
                    if (row.length == 2) {
                        tsMap.put((String)row[0], (String)row[1]);
                    }
                }
            } catch (VizException e) {
                statusHandler.warn("Error querying ShefTS table", e);
            }
        }
        return tsMap.get(ts);
    }

    public double[] getLatLon(String lid) {
        double[] latlon = new double[2];
        if ((llMap == null) || (llMap.get(lid) == null)) {
            if (llMap == null) {
                llMap = new HashMap<>();
            }

            try {
                String query = "select lat, lon from location where lid = '" + lid + "'";
                ArrayList<Object[]> data = (ArrayList<Object[]>)DirectDbQuery.executeQuery(query, HydroConstants.IHFS, QueryLanguage.SQL);

                if (data == null) {
                    throw new VizException("Error querying Location table");
                }

                latlon[0] = (Double) data.get(0)[0];
                latlon[1] = (Double) data.get(0)[1];
                llMap.put(lid, latlon);
            } catch (VizException e) {
                statusHandler.warn(
                        "Error querying lat/lon data from location table", e);
            }
        }

        return llMap.get(lid);
    }

    /**
     * Get the HSA for the given location.
     *
     * @param lid
     *      The given location
     * @return
     *      The HSA
     */
    public String getHsa(String lid) {
        if ((hsaMap == null) || (hsaMap.get(lid) == null)) {
            if (hsaMap == null) {
                hsaMap = new HashMap<>();
            }
            if (lid != null) {
                try {
                    String query = "select hsa from location where lid = '"
                            + lid + "'";
                    ArrayList<Object[]> data = (ArrayList<Object[]>) DirectDbQuery
                            .executeQuery(query, HydroConstants.IHFS,
                                    QueryLanguage.SQL);
                    if (CollectionUtil.isNullOrEmpty(data)) {
                        throw new VizException("Error querying Location table");
                    }
                    String hsa = (String) data.get(0)[0];

                    hsaMap.put(lid, hsa);
                } catch (VizException e) {
                    statusHandler.warn(
                            "Error querying hsa information from location table",
                            e);
                }
            }
        }

        return hsaMap.get(lid);
    }

    /**
     * Determine if provided lid is an alert station.
     *
     * @return true if station is alert station
     */
    public boolean isAlertStation(String lid) {
        boolean isAlert = false;
        if (alertSet == null) {
            alertSet = new HashSet<>();

            String query = "select lid from telem where type = 'ALERT'";
            try {
                ArrayList<Object[]> rs = (ArrayList<Object[]>)DirectDbQuery.executeQuery(query, HydroConstants.IHFS, QueryLanguage.SQL);
                if (rs != null && !rs.isEmpty()) {
                    for (Object[] oa: rs) {
                        alertSet.add((String) oa[0]);
                    }
                }
            } catch (VizException e) {
                statusHandler.error("Error getting alert stations", e);
            }
        }

        if (alertSet.contains(lid)) {
            isAlert = true;
        }

        return isAlert;
    }
}
