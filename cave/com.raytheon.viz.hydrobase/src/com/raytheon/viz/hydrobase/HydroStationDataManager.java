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
package com.raytheon.viz.hydrobase;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.IGetSortType;

/**
 * HydroStationDataManager.java Used to query the various data tables related to
 * HydroStation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 16Oct2008    1636       askripsky   Initial Creation
 * Oct 27, 2011	11267	   lbousaidi   change showNoPost initial value to false
 *  									to match AWIPS I default setting
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class HydroStationDataManager {

    private static HydroStationDataManager manager = null;

    private ArrayList<HydroStationData> stationData;

    private ArrayList<String> hsaFilter = new ArrayList<String>();

    private double latCenter = 0;

    private double latOffset = 0;

    private double lonCenter = 0;

    private double lonOffset = 0;

    private boolean filterByLatLon = false;

    private boolean showPost = true;

    private boolean showNoPost = false;
    
    private IGetSortType sortType;

    /**
     * Private constructor.
     */
    private HydroStationDataManager() {
        if (stationData == null) {
            stationData = new ArrayList<HydroStationData>();
        }
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized HydroStationDataManager getInstance() {
        if (manager == null) {
            manager = new HydroStationDataManager();
        }

        return manager;
    }

    /**
     * Get the station list data sorted by lid.
     * 
     * @param resultCount
     * @param hours
     * @param duration
     * @param sortOrder
     * @param listType
     */
    public ArrayList<HydroStationData> getStationData(IGetSortType sortType) {
        ArrayList<HydroStationData> rval = new ArrayList<HydroStationData>();
        this.sortType = sortType;
        
        // Verify data is empty
        if (stationData == null) {
            stationData = new ArrayList<HydroStationData>();
        } else {
            stationData.clear();
        }

        StringBuffer hydroStationQuery = new StringBuffer();

        // Select Columns
        hydroStationQuery
                .append("Select lid, name, lat, lon, rb, state, county, stream");

        // Set tables and aliases
        hydroStationQuery.append(" FROM LocView");

        hydroStationQuery.append(getWhereClause());

        try {
            List<Object[]> data = DirectDbQuery.executeQuery(hydroStationQuery
                    .toString(), HydroConstants.IHFS,
                    DirectDbQuery.QueryLanguage.SQL);

            for (Object[] currData : data) {
                rval.add(new HydroStationData(sortType, currData));
            }

        } catch (VizException e) {
        }

        return rval;
    }

    private String getWhereClause() {
        StringBuffer rval = new StringBuffer();

        if (showPost) {
            if (showNoPost)
                rval.append(" WHERE post IS NOT NULL ");
            else
                rval.append(" WHERE post = 1 ");
        } else if (showNoPost) {
            rval.append(" WHERE post = 0 ");
        } else {
            rval.append(" WHERE post IS NULL ");
        }

        /*
         * add Service Backup to where clause. Service Backup operations were
         * altered to use the LocView hsa column instead of the wfo column on
         */
        rval.append(" AND lid IN (SELECT lid FROM LocView WHERE ");

        if (hsaFilter.size() == 0) {
            rval.append(" hsa IS NOT NULL ");
        } else {
            rval.append(" hsa IN (");

            for (String hsa : hsaFilter) {
                rval.append("'" + hsa + "'");
                rval.append(",");
            }
            // Take off trailing ,
            rval.setLength(rval.length() - 1);
            rval.append(")");
        }

        rval.append(" )");

        /* specify the lat-lon filter if it is enabled */

        if (filterByLatLon) {
            rval
                    .append(String
                            .format(
                                    " AND ((lat > %f) AND (lat < %f)) AND ((lon > %f) AND (lon < %f)) ",
                                    latCenter - latOffset, latCenter
                                            + latOffset, lonCenter - lonOffset,
                                    lonCenter + lonOffset));
        }
        
        /* Determine sort selection criteria */
        String sort = sortType.getSortType();
        if (sort.compareTo("State,County") == 0) {
            rval.append(" order by state, county asc, lid ");
        } else if (sort.compareTo("Name") == 0) {
            rval.append(" order by name ");
        } else // Sort by station is the default
        {
            rval.append(" order by lid");
        }
        

        return rval.toString();
    }
    
    /**
     * Check if the lid passed in is a river site or not.
     * 
     * @param lid
     *      The lid to check
     * @return
     *      True if site is a river site
     */
    public boolean isRiverSite(String lid) {
        boolean riverSite = false;
        
        String query = "select count(*) from riverstat where lid = '" + lid + "'";
        List<Object[]> rs;
        try {
            rs = DirectDbQuery.executeQuery(query, HydroConstants.IHFS, QueryLanguage.SQL);
        
            if ((rs != null) && (rs.size() > 0)) {
                long num = (Long) rs.get(0)[0];
                if (num > 0) {
                    riverSite = true;
                }
            }
        } catch (VizException e) {
            System.err.println("Error querying riverstat table");
        } 
        
        return riverSite;
    }
    
    /**
     * @return the latCenter
     */
    public double getLatCenter() {
        return latCenter;
    }

    /**
     * @param latCenter
     *            the latCenter to set
     */
    public void setLatCenter(double latCenter) {
        this.latCenter = latCenter;
    }

    /**
     * @return the latOffset
     */
    public double getLatOffset() {
        return latOffset;
    }

    /**
     * @param latOffset
     *            the latOffset to set
     */
    public void setLatOffset(double latOffset) {
        this.latOffset = latOffset;
    }

    /**
     * @return the lonCenter
     */
    public double getLonCenter() {
        return lonCenter;
    }

    /**
     * @param lonCenter
     *            the lonCenter to set
     */
    public void setLonCenter(double lonCenter) {
        this.lonCenter = lonCenter;
    }

    /**
     * @return the lonOffset
     */
    public double getLonOffset() {
        return lonOffset;
    }

    /**
     * @param lonOffset
     *            the lonOffset to set
     */
    public void setLonOffset(double lonOffset) {
        this.lonOffset = lonOffset;
    }

    /**
     * @return the filterByLatLon
     */
    public boolean isFilterByLatLon() {
        return filterByLatLon;
    }

    /**
     * @param filterByLatLon
     *            the filterByLatLon to set
     */
    public void setFilterByLatLon(boolean filterByLatLon) {
        this.filterByLatLon = filterByLatLon;
    }

    /**
     * @return the showPost
     */
    public boolean isShowPost() {
        return showPost;
    }

    /**
     * @param showPost
     *            the showPost to set
     */
    public void setShowPost(boolean showPost) {
        this.showPost = showPost;
    }

    /**
     * @return the showNoPost
     */
    public boolean isShowNoPost() {
        return showNoPost;
    }

    /**
     * @param showNoPost
     *            the showNoPost to set
     */
    public void setShowNoPost(boolean showNoPost) {
        this.showNoPost = showNoPost;
    }

    /**
     * @return the hsaFilter
     */
    public ArrayList<String> getHsaFilter() {
        return hsaFilter;
    }

    /**
     * @param hsaFilter
     *            the hsaFilter to set
     */
    public void setHsaFilter(ArrayList<String> hsaFilter) {
        this.hsaFilter = hsaFilter;
    }
}
