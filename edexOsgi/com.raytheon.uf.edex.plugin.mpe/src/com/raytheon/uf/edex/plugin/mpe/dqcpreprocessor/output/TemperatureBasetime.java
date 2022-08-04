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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output;

import java.util.HashMap;
import java.util.Map;

/**
 * Enumeration representing the temperature basetime field. The temperature
 * basetime determines when the span of 24 hours that data is processed for with
 * a given day or days starts and ends.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2018 7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public enum TemperatureBasetime {
    BASETIME_12Z("12Z", 12, "DH12"),
    BASETIME_18Z("18Z", 18, "DH18"),
    BASETIME_06Z("06Z", 6, "DH06"),
    BASETIME_00Z("00Z", 0, "DH00");

    private static final Map<String, TemperatureBasetime> lookupMap;

    static {
        lookupMap = new HashMap<>(TemperatureBasetime.values().length, 1.0f);
        for (TemperatureBasetime value : TemperatureBasetime.values()) {
            lookupMap.put(value.getKey(), value);
        }
    }

    private final String key;

    private final int baseTime;

    private final String output;

    public static TemperatureBasetime lookupByKey(final String key) {
        return lookupMap.get(key);
    }

    private TemperatureBasetime(final String key, final int baseTime,
            final String output) {
        this.key = key;
        this.baseTime = baseTime;
        this.output = output;
    }

    /**
     * @return the key
     */
    public String getKey() {
        return key;
    }

    /**
     * @return the baseTime
     */
    public int getBaseTime() {
        return baseTime;
    }

    /**
     * @return the output
     */
    public String getOutput() {
        return output;
    }
}