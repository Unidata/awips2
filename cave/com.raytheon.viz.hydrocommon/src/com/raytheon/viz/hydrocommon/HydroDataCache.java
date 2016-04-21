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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

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
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class HydroDataCache {   
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
    public ArrayList<Object[]> getLocationData() {

        ArrayList<Object[]> data;

        try {
            data = (ArrayList<Object[]>)DirectDbQuery.executeQuery("select lid, name from location", HydroConstants.IHFS, QueryLanguage.SQL);
            if (data != null) {
                return data;
            }
        } catch (VizException e) {

        }
        return null;
    }

    /**
     * Get the locationMap
     * @return HashMap
     */
    public HashMap<String, String> getLocationMap() {

        if (locationMap == null){

            ArrayList<Object[]> locData = getLocationData();
            locationMap = new HashMap<String, String>();

            for (Object[] row: locData){
                locationMap.put((String)row[0], (String)row[1]);
            }
        }

        return locationMap;
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
            peMap = new HashMap<String, String>();

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
                e.printStackTrace();
                System.err.println("Caught VizException");
                System.err.println("Error querying ShefPE table");
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
            tsMap = new HashMap<String, String>();
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
                e.printStackTrace();
                System.err.println("Caught VizException");
                System.err.println("Error querying ShefTS table");
            } 
        }
        return tsMap.get(ts);
    }

    public double[] getLatLon(String lid) {
        double[] latlon = new double[2];
        if ((llMap == null) || (llMap.get(lid) == null)) {
            if (llMap == null) {
                llMap = new HashMap<String, double[]>();
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
                e.printStackTrace();
                System.err.println("Caught VizException");
                System.err.println("Error querying ShefPE table");
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
                hsaMap = new HashMap<String, String>();
            }
            if (lid != null ) {
              try {
                String query = "select hsa from location where lid = '" + lid + "'";                             
                ArrayList<Object[]> data = (ArrayList<Object[]>)DirectDbQuery.executeQuery(query, HydroConstants.IHFS, QueryLanguage.SQL);
                if ((data == null) || (data.size() == 0)) {
                    throw new VizException("Error querying Location table");
                }
                String hsa = (String) data.get(0)[0];
                
                hsaMap.put(lid, hsa);
            } catch (VizException e) {
                e.printStackTrace();
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
            alertSet = new HashSet<String>();
            
            String query = "select lid from telem where type = 'ALERT'";
            try {
                ArrayList<Object[]> rs = (ArrayList<Object[]>)DirectDbQuery.executeQuery(query, HydroConstants.IHFS, QueryLanguage.SQL);
                if ((rs == null) || (rs.size() == 0)) {
                    for (Object[] oa: rs) {
                        alertSet.add((String) oa[0]);
                    }
                }
            } catch (VizException e) {
                e.printStackTrace();
            }
        }
        
        if (alertSet.contains(lid)) {
            isAlert = true;
        }
        
        return isAlert;
    }
}
