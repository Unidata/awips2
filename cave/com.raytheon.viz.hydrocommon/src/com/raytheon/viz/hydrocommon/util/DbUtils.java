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
package com.raytheon.viz.hydrocommon.util;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Hydro Database Utilities
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#      Engineer   Description
 * -----------------------------------------------------------
 * Jul 9, 2008  1194          mpduff    Initial creation.
 * Mar 7, 2014  16692                   lbousaidi Any Forecast source other than
 *                                      H*,P*,Q*,T* should be handled by fcstother.
 * Oct 10, 2015 17935                   special char (e.g apostrophe) can not be saved/updated in Hyrobase
 * Jan 15, 2016 DCS18180     JingtaoD   code improvement based on code review for DR17935
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DbUtils {
    private static final String SHEF_PROCOBS = "shef_procobs";

    private static ConcurrentHashMap<String, String> tableMap = null;

    private static ConcurrentHashMap<String, String> fcstTableMap = null;

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DbUtils.class);

    public static String getTableName(String pe, String ts) {
        populateMaps();
        String retVal = "procvalue";
        boolean treatProcessedAsObserverd = false;
        boolean matchFound = false;

        /*
         * if type-source indicates processed data, then check if processed data
         * being treated as observed
         */
        // if (pe.startsWith("P")) {
        if (ts.startsWith("P") || ts.startsWith("p")) {
            // tokenlen = strlen("shef_procobs");
            // get_apps_defaults("shef_procobs", &tokenlen, retstr, &retlen);
            String procObs = AppsDefaults.getInstance().getToken(SHEF_PROCOBS);

            if (procObs.equalsIgnoreCase("ON")) {
                treatProcessedAsObserverd = true;
            } else {
                return retVal;
            }
        }

        /* if type-source indicates contingency data */
        if (ts.toUpperCase().startsWith("C")) {
            retVal = "Contingencyvalue";
            return retVal;
        }

        /* if observed data or processed data being treated as observed */
        if (ts.toUpperCase().startsWith("R") || treatProcessedAsObserverd
                || ts.toUpperCase().startsWith("XX")) {

            Set<String> tableKeys = tableMap.keySet();
            for (String key : tableKeys) {
                if (key.substring(0, 1).equalsIgnoreCase(pe.substring(0, 1))) {
                    if (pe.startsWith("P") || pe.startsWith("p")) {
                        if (pe.equalsIgnoreCase("PA")
                                || pe.equalsIgnoreCase("PD")
                                || pe.equalsIgnoreCase("PE")
                                || pe.equalsIgnoreCase("PL")) {
                            retVal = "Pressure";
                        } else if (pe.equalsIgnoreCase("PP")) {
                            retVal = "Rawpp";
                        } else {
                            retVal = "Rawpc";
                        }
                        matchFound = true;
                        break;
                    } else {
                        retVal = tableMap.get(pe.substring(0, 1).toLowerCase());
                        matchFound = true;
                        break;
                    }
                }
            }
        } else if (ts.startsWith("F") || ts.startsWith("f")) { /*
                                                                * if forecast
                                                                * data
                                                                */
            retVal = fcstTableMap.get(pe.substring(0, 1).toLowerCase());
            if (retVal == null) {
                retVal = "Fcstother";
            }
            matchFound = true;
        } else { /* if type-source not valid */
            // TODO - Log error message - "Invalid type-source specified for
            // getTableName(): " + ts
            retVal = "INVALID";
        }

        if (!matchFound) {
            // TODO - Log error message - "Error finding table name match for
            // pe, ts: " + pe + ", " + ts"
            retVal = "INVALID";
        }

        return retVal;
    }

    /**
     * Get the HashMap of PE types to table name references
     * 
     * note: for TB, TV, NO; this function will return the temperature and
     * gatedam names, but the data are really stored in pairedvalue
     * 
     * @return the HashMap of PE references
     */
    public static Map<String, String> getTableMap() {
        return tableMap;
    }

    /**
     * Get the HashMap of forecast PE types to table name references
     * 
     * @return the HashMap of PE references
     */

    public static Map<String, String> getFcstTableMap() {
        return fcstTableMap;
    }

    private static void populateMaps() {
        if (tableMap == null) {
            tableMap = new ConcurrentHashMap<String, String>();
            tableMap.put("a", "Agricultural");
            tableMap.put("e", "Evaporation");
            tableMap.put("f", "Fishcount");
            tableMap.put("g", "Ground");
            tableMap.put("h", "Height");
            tableMap.put("i", "Ice");
            tableMap.put("l", "Lake");
            tableMap.put("m", "Moisture");
            tableMap.put("n", "Gatedam");
            /*
             * pressure, procvalue, rawpc, and rawpp are handled in code above
             */
            tableMap.put("p", "Precip");
            tableMap.put("q", "Discharge");
            tableMap.put("r", "Radiation");
            tableMap.put("s", "Snow");
            tableMap.put("t", "Temperature");
            tableMap.put("u", "Wind");
            tableMap.put("v", "Power");
            tableMap.put("w", "Waterquality");
            tableMap.put("x", "Weather");
            tableMap.put("y", "Yunique");
        }

        if (fcstTableMap == null) {
            fcstTableMap = new ConcurrentHashMap<String, String>();
            fcstTableMap.put("h", "Fcstheight");
            fcstTableMap.put("p", "Fcstprecip");
            fcstTableMap.put("q", "Fcstdischarge");
            fcstTableMap.put("t", "Fcsttemperature");
        }
    }

    /**
     * replace string fields in table class which contains apostrophe
     * 
     * @param curData
     *            , retData
     */
    public static <T extends Object> void escapeSpecialCharforData(T curData,
            T retData) {

        copyFields(curData, retData);

        Class<?> c = retData.getClass();

        Field fields[] = c.getDeclaredFields();

        for (Field f : fields) {
            try {
                if (f.getType().isAssignableFrom(String.class)) {

                    f.setAccessible(true);
                    if (f.get(retData) != null) {
                        String value = (String) f.get(retData).toString();

                        if (value != null) {
                            if (value.contains("'")) {
                                value = value.replace("'", "''");
                                f.set(retData, value);
                            }
                        }
                    }
                }
            } catch (IllegalAccessException e) {
                statusHandler.handle(Priority.ERROR,
                        "Error to escape special characters for object "
                                + curData.toString(), e);
            }
        }

    }

    /**
     * replace apostrophe for string
     * 
     * @param strValue
     * @return strValue
     */
    public static String escapeSpecialCharforStr(String strValue) {

        if (strValue != null) {
            if (strValue.contains("'")) {
                strValue = strValue.replace("'", "''");
            }
        }
        return strValue;
    }

    /**
     * Copy the fields in origData to copiedData, later replace special char in
     * copiedData object
     * 
     * @param origData
     * @param copiedData
     */
    public static <T extends Object> void copyFields(T origData, T copiedData) {

        Class<? extends Object> fromCopy = origData.getClass();
        Class<? extends Object> toCopy = copiedData.getClass();

        Field origFields[] = fromCopy.getDeclaredFields();

        Object value = null;

        for (Field f : origFields) {

            try {

                Field copiedField = toCopy.getDeclaredField(f.getName());

                f.setAccessible(true);
                copiedField.setAccessible(true);

                value = f.get(origData);
                copiedField.set(copiedData, value);

            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Error to copy object from " + origData.toString()
                                + "to" + copiedData.toString(), e);
            }

        }
    }

}
