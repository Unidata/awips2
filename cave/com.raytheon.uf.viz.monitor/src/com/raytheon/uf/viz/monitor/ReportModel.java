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
package com.raytheon.uf.viz.monitor;

import java.util.HashMap;
import java.util.Map;

/**
 * ReportModel class is a singleton class that contains the report model for
 * this decision assistance tool.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2009 2047       grichard    Initial creation.
 * 3/3/2009     2047       grichard    Added unlimited visibility.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class ReportModel {
    /**
     * The static singleton instance.
     */
    private static ReportModel instance;

    /**
     * Visibility Look Up
     */
    private final Map<String, Float> reportableFloatVisibilityLookUp = new HashMap<String, Float>();

    /**
     * Visibility Look Up
     */
    private final Map<Integer, String> reportableStringVisibilityLookUp = new HashMap<Integer, String>();

    /**
     * Singleton constructor.
     * 
     * @return the observation station model.
     */
    public static synchronized ReportModel getInstance() {
        if (instance == null) {
            instance = new ReportModel();
        }

        return instance;
    }

    /**
     * Private constructor: Use getInstance().
     */
    private ReportModel() {
        // Initialize the look up table for string to float lookup.
        reportableFloatVisibilityLookUp.put("0", 0 / 16f);
        reportableFloatVisibilityLookUp.put("0SM", 0 / 16f);
        reportableFloatVisibilityLookUp.put("1/16", 1 / 16f);
        reportableFloatVisibilityLookUp.put("1/16SM", 1 / 16f);
        reportableFloatVisibilityLookUp.put("0100", 1 / 16f);
        reportableFloatVisibilityLookUp.put("1/8", 2 / 16f);
        reportableFloatVisibilityLookUp.put("1/8SM", 2 / 16f);
        reportableFloatVisibilityLookUp.put("0200", 2 / 16f);
        reportableFloatVisibilityLookUp.put("3/16", 3 / 16f);
        reportableFloatVisibilityLookUp.put("3/16SM", 3 / 16f);
        reportableFloatVisibilityLookUp.put("0300", 3 / 16f);
        reportableFloatVisibilityLookUp.put("1/4", 4 / 16f);
        reportableFloatVisibilityLookUp.put("1/4SM", 4 / 16f);
        reportableFloatVisibilityLookUp.put("0400", 4 / 16f);
        reportableFloatVisibilityLookUp.put("5/16", 5 / 16f);
        reportableFloatVisibilityLookUp.put("5/16SM", 5 / 16f);
        reportableFloatVisibilityLookUp.put("0500", 5 / 16f);
        reportableFloatVisibilityLookUp.put("3/8", 6 / 16f);
        reportableFloatVisibilityLookUp.put("3/8SM", 6 / 16f);
        reportableFloatVisibilityLookUp.put("0600", 6 / 16f);
        reportableFloatVisibilityLookUp.put("0700", 7 / 16f);
        reportableFloatVisibilityLookUp.put("1/2", 8 / 16f);
        reportableFloatVisibilityLookUp.put("1/2SM", 8 / 16f);
        reportableFloatVisibilityLookUp.put("0800", 8 / 16f);
        reportableFloatVisibilityLookUp.put("0900", 9 / 16f);
        reportableFloatVisibilityLookUp.put("5/8", 10 / 16f);
        reportableFloatVisibilityLookUp.put("5/8SM", 10 / 16f);
        reportableFloatVisibilityLookUp.put("1000", 10 / 16f);
        reportableFloatVisibilityLookUp.put("1100", 11 / 16f);
        reportableFloatVisibilityLookUp.put("3/4", 12 / 16f);
        reportableFloatVisibilityLookUp.put("3/4SM", 12 / 16f);
        reportableFloatVisibilityLookUp.put("1200", 12 / 16f);
        reportableFloatVisibilityLookUp.put("1300", 13 / 16f);
        reportableFloatVisibilityLookUp.put("7/8", 14 / 16f);
        reportableFloatVisibilityLookUp.put("7/8SM", 14 / 16f);
        reportableFloatVisibilityLookUp.put("1400", 14 / 16f);
        reportableFloatVisibilityLookUp.put("1500", 15 / 16f);
        reportableFloatVisibilityLookUp.put("1", 16 / 16f);
        reportableFloatVisibilityLookUp.put("1SM", 16 / 16f);
        reportableFloatVisibilityLookUp.put("1600", 16 / 16f);
        reportableFloatVisibilityLookUp.put("1700", 17 / 16f);
        reportableFloatVisibilityLookUp.put("1 1/8", 18 / 16f);
        reportableFloatVisibilityLookUp.put("1 1/8SM", 18 / 16f);
        reportableFloatVisibilityLookUp.put("1800", 18 / 16f);
        reportableFloatVisibilityLookUp.put("1900", 19 / 16f);
        reportableFloatVisibilityLookUp.put("1 1/4", 20 / 16f);
        reportableFloatVisibilityLookUp.put("1 1/4SM", 20 / 16f);
        reportableFloatVisibilityLookUp.put("2000", 20 / 16f);
        reportableFloatVisibilityLookUp.put("2100", 21 / 16f);
        reportableFloatVisibilityLookUp.put("1 3/8", 22 / 16f);
        reportableFloatVisibilityLookUp.put("1 3/8SM", 22 / 16f);
        reportableFloatVisibilityLookUp.put("2200", 22 / 16f);
        reportableFloatVisibilityLookUp.put("2300", 23 / 16f);
        reportableFloatVisibilityLookUp.put("1 1/2", 24 / 16f);
        reportableFloatVisibilityLookUp.put("1 1/2SM", 24 / 16f);
        reportableFloatVisibilityLookUp.put("2400", 24 / 16f);
        reportableFloatVisibilityLookUp.put("2500", 25 / 16f);
        reportableFloatVisibilityLookUp.put("1 5/8", 26 / 16f);
        reportableFloatVisibilityLookUp.put("1 5/8SM", 26 / 16f);
        reportableFloatVisibilityLookUp.put("2600", 26 / 16f);
        reportableFloatVisibilityLookUp.put("2700", 27 / 16f);
        reportableFloatVisibilityLookUp.put("1 3/4", 28 / 16f);
        reportableFloatVisibilityLookUp.put("1 3/4SM", 28 / 16f);
        reportableFloatVisibilityLookUp.put("2800", 28 / 16f);
        reportableFloatVisibilityLookUp.put("2900", 29 / 16f);
        reportableFloatVisibilityLookUp.put("1 7/8", 30 / 16f);
        reportableFloatVisibilityLookUp.put("1 7/8SM", 30 / 16f);
        reportableFloatVisibilityLookUp.put("3000", 30 / 16f);
        reportableFloatVisibilityLookUp.put("3100", 31 / 16f);
        reportableFloatVisibilityLookUp.put("2", 32 / 16f);
        reportableFloatVisibilityLookUp.put("2SM", 32 / 16f);
        reportableFloatVisibilityLookUp.put("3200", 32 / 16f);
        reportableFloatVisibilityLookUp.put("3300", 33 / 16f);
        reportableFloatVisibilityLookUp.put("3400", 34 / 16f);
        reportableFloatVisibilityLookUp.put("3500", 35 / 16f);
        reportableFloatVisibilityLookUp.put("2 1/4", 36 / 16f);
        reportableFloatVisibilityLookUp.put("2 1/4SM", 36 / 16f);
        reportableFloatVisibilityLookUp.put("3600", 36 / 16f);
        reportableFloatVisibilityLookUp.put("3700", 37 / 16f);
        reportableFloatVisibilityLookUp.put("2 3/8", 38 / 16f);
        reportableFloatVisibilityLookUp.put("2 3/8SM", 38 / 16f);
        reportableFloatVisibilityLookUp.put("3800", 38 / 16f);
        reportableFloatVisibilityLookUp.put("3900", 39 / 16f);
        reportableFloatVisibilityLookUp.put("2 1/2", 40 / 16f);
        reportableFloatVisibilityLookUp.put("2 1/2SM", 40 / 16f);
        reportableFloatVisibilityLookUp.put("4000", 40 / 16f);
        reportableFloatVisibilityLookUp.put("4100", 41 / 16f);
        reportableFloatVisibilityLookUp.put("4200", 42 / 16f);
        reportableFloatVisibilityLookUp.put("4300", 43 / 16f);
        reportableFloatVisibilityLookUp.put("2 3/4", 44 / 16f);
        reportableFloatVisibilityLookUp.put("2 3/4SM", 44 / 16f);
        reportableFloatVisibilityLookUp.put("4400", 44 / 16f);
        reportableFloatVisibilityLookUp.put("4500", 45 / 16f);
        reportableFloatVisibilityLookUp.put("4600", 46 / 16f);
        reportableFloatVisibilityLookUp.put("4700", 47 / 16f);
        reportableFloatVisibilityLookUp.put("3", 48 / 16f);
        reportableFloatVisibilityLookUp.put("3SM", 48 / 16f);
        reportableFloatVisibilityLookUp.put("4800", 48 / 16f);
        reportableFloatVisibilityLookUp.put("4900", 49 / 16f);
        reportableFloatVisibilityLookUp.put("5000", 50 / 16f);
        reportableFloatVisibilityLookUp.put("4", 64 / 16f);
        reportableFloatVisibilityLookUp.put("4SM", 64 / 16f);
        reportableFloatVisibilityLookUp.put("6000", 64 / 16f);
        reportableFloatVisibilityLookUp.put("7000", 70 / 16f);
        reportableFloatVisibilityLookUp.put("5", 80 / 16f);
        reportableFloatVisibilityLookUp.put("5SM", 80 / 16f);
        reportableFloatVisibilityLookUp.put("8000", 80 / 16f);
        reportableFloatVisibilityLookUp.put("6", 96 / 16f);
        reportableFloatVisibilityLookUp.put("6SM", 96 / 16f);
        reportableFloatVisibilityLookUp.put("9000", 96 / 16f);
        reportableFloatVisibilityLookUp.put("7", 112 / 16f);
        reportableFloatVisibilityLookUp.put("7SM", 112 / 16f);
        reportableFloatVisibilityLookUp.put("9999", 112 / 16f);
        reportableFloatVisibilityLookUp.put("8", 128 / 16f);
        reportableFloatVisibilityLookUp.put("8SM", 128 / 16f);
        reportableFloatVisibilityLookUp.put("9", 144 / 16f);
        reportableFloatVisibilityLookUp.put("9SM", 144 / 16f);
        reportableFloatVisibilityLookUp.put("10", 160 / 16f);
        reportableFloatVisibilityLookUp.put("10SM", 160 / 16f);
        reportableFloatVisibilityLookUp.put("11", 176 / 16f);
        reportableFloatVisibilityLookUp.put("11SM", 176 / 16f);
        reportableFloatVisibilityLookUp.put("15", 240 / 16f);
        reportableFloatVisibilityLookUp.put("15SM", 240 / 16f);

        // Initialize the look up table for integer to string lookup,
        // where the integer is scaled to sixteenths of a statute mile.
        reportableStringVisibilityLookUp.put(0, "0");
        reportableStringVisibilityLookUp.put(1, "1/16");
        reportableStringVisibilityLookUp.put(2, "1/8");
        reportableStringVisibilityLookUp.put(3, "3/16");
        reportableStringVisibilityLookUp.put(4, "1/4");
        reportableStringVisibilityLookUp.put(5, "5/16");
        reportableStringVisibilityLookUp.put(6, "3/8");
        reportableStringVisibilityLookUp.put(8, "1/2");
        reportableStringVisibilityLookUp.put(10, "5/8");
        reportableStringVisibilityLookUp.put(12, "3/4");
        reportableStringVisibilityLookUp.put(14, "7/8");
        reportableStringVisibilityLookUp.put(16, "1");
        reportableStringVisibilityLookUp.put(18, "1 1/8");
        reportableStringVisibilityLookUp.put(20, "1 1/4");
        reportableStringVisibilityLookUp.put(22, "1 3/8");
        reportableStringVisibilityLookUp.put(24, "1 1/2");
        reportableStringVisibilityLookUp.put(26, "1 5/8");
        reportableStringVisibilityLookUp.put(28, "1 3/4");
        reportableStringVisibilityLookUp.put(30, "1 7/8");
        reportableStringVisibilityLookUp.put(32, "2");
        reportableStringVisibilityLookUp.put(36, "2 1/4");
        reportableStringVisibilityLookUp.put(40, "2 1/2");
        reportableStringVisibilityLookUp.put(44, "2 3/4");
        reportableStringVisibilityLookUp.put(48, "3");
        reportableStringVisibilityLookUp.put(64, "4");
        reportableStringVisibilityLookUp.put(80, "5");
        reportableStringVisibilityLookUp.put(96, "6");
        reportableStringVisibilityLookUp.put(112, "7");
        reportableStringVisibilityLookUp.put(128, "8");
        reportableStringVisibilityLookUp.put(144, "9");
        reportableStringVisibilityLookUp.put(160, "10");
        reportableStringVisibilityLookUp.put(176, "11");
    }

    /**
     * Method that fetches the visibility map containing floats
     * 
     * @return the map containing floats
     */
    public Map<String, Float> getReportableFloatVisibilityLookUp() {
        return reportableFloatVisibilityLookUp;
    }

    /**
     * Method that fetches the visibility map containing strings
     * 
     * @return the map containing strings
     */
    public Map<Integer, String> getReportableStringVisibilityLookUp() {
        return reportableStringVisibilityLookUp;
    }

    /**
     * Method that converts units of meters into units of statute miles
     */
    public float metersToStatuteMiles(float vis) {
        return vis / 1609.347f;
    }

}
