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

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.RGBColors;

/**
 * Hydro Timeseries Color Utility Class
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Aug 22, 2008              mpduff       Initial creation
 * Dec 01, 2011  11464       mpduff       Made separate group and non-group
 *                                        color lists Using the RGB class
 *                                        instead of the list from A1. I ran
 *                                        across a group config file using a
 *                                        color not in the list which caused
 *                                        errors.
 * May 08, 2012  14958       wkwock       prevent go over the list in
 *                                        getGroupModeColor
 * Jul 11, 2016  19166       ksteinfeld   Prevent TS_COLOR_LIST array index
 *                                        exceeding max array size
 * Jun 27, 2018  6748        randerso     Refactored to match A1
 * Sep 21, 2018  7379        mduff        Support PDC Refactor.
 *
 * </pre>
 *
 * @author mpduff
 */

public class HydroUtils {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HydroUtils.class);

    private static final String SHEF_PROCOBS = "shef_procobs";

    private static final String ON = "ON";

    private static final String[] TScolors = new String[] { "DodgerBlue",
            "Yellow", "Cyan", "Green", "Purple", "Magenta", "Red", "Aquamarine",
            "SeaGreen", "maroon", "BlueViolet", "Coral", "HotPink",
            "MediumPurple", "DeepPink", "ForestGreen", "LimeGreen", "Orange",
            "AliceBlue", "SkyBlue", "White", "DarkGreen", "ForestGreen",
            "LightBlue", "Wheat", "DarkGreen", "DarkOrange", "DeepPink", "Gold",
            "GreenYellow", "DarkViolet", "IndianRed", "LawnGreen",
            "CornflowerBlue", "LimeGreen", "MediumBlue", "DarkGreen",
            "PaleGreen", "MediumOrchid", "SteelBlue" };

    /**
     * Number of defined colors
     */
    public static final int NUM_COLORS = TScolors.length;

    private static final RGB[] TS_COLOR_LIST = new RGB[NUM_COLORS];
    static {
        for (int i = 0; i < NUM_COLORS; i++) {
            TS_COLOR_LIST[i] = getColor(TScolors[i]);
        }
    }

    private static ConcurrentMap<String, String> tableMap = null;

    private static ConcurrentMap<String, String> fcstTableMap = null;

    /**
     * Get color by name
     *
     * @param colorName
     * @return the named color
     */
    public static RGB getColor(String colorName) {
        return RGBColors.getRGBColor(colorName);
    }

    /**
     * Get color by index
     *
     * @param index
     * @return the indexed color
     */
    public static RGB getColor(int index) {
        // make sure the index does not exceed max array size
        return TS_COLOR_LIST[index % NUM_COLORS];
    }

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
            String procObs = AppsDefaults.getInstance().getToken(SHEF_PROCOBS);

            if (ON.equalsIgnoreCase(procObs)) {
                treatProcessedAsObserverd = true;
            } else {
                if ("PP".equalsIgnoreCase(pe) && "PM".equalsIgnoreCase(ts)) {
                    retVal = "arealobs";
                }
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
                        if ("PA".equalsIgnoreCase(pe)
                                || "PD".equalsIgnoreCase(pe)
                                || "PE".equalsIgnoreCase(pe)
                                || "PL".equalsIgnoreCase(pe)) {
                            retVal = "Pressure";
                        } else if ("PP".equalsIgnoreCase(pe)) {
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
        } else if (ts.startsWith("F") || ts.startsWith("f")) {
            /* if forecast data */
            retVal = fcstTableMap.get(pe.substring(0, 1).toLowerCase());
            if (retVal == null) {
                retVal = "Fcstother";
            }
            matchFound = true;
        } else {
            /* if type-source not valid */
            statusHandler.warn(
                    "Invalid type-source specified for getTableName(): " + ts);
            retVal = "INVALID";
        }

        if (!matchFound) {
            statusHandler.warn("Error finding table name match for pe, ts: "
                    + pe + ", " + ts);
            retVal = "INVALID";
        }

        return retVal;
    }

    private static synchronized void populateMaps() {
        if (tableMap == null) {
            tableMap = new ConcurrentHashMap<>();
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
            fcstTableMap = new ConcurrentHashMap<>();
            fcstTableMap.put("h", "Fcstheight");
            fcstTableMap.put("p", "Fcstprecip");
            fcstTableMap.put("q", "Fcstdischarge");
            fcstTableMap.put("t", "Fcsttemperature");
        }
    }

}
