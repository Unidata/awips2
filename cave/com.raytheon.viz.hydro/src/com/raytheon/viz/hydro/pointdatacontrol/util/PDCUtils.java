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
package com.raytheon.viz.hydro.pointdatacontrol.util;

import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 9, 2008            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PDCUtils {
    private static Map<String, String[]> presetData = new HashMap<String, String[]>();

    /**
     * Tokenize the preset string of key/value pairs.
     * 
     * @param presets
     *            The preset string list to be tokenized.
     * @return HashMap of preset data
     */
    public static Map<String, String[]> tokenizePresetString(String presets)
            throws ParseException {
        if ((presets == null) || (presets.length() < 1)) {
            throw new ParseException("Empty preset String", 0);
        }

        String[] parts = presets.split(";");

        for (String part : parts) {
            String[] pairs = part.split("=");
            String[] values = null;

            if (pairs.length > 1) {
                if (pairs[1].contains(",")) {
                    values = pairs[1].split(",");
                } else {
                    values = new String[] { pairs[1] };
                }

                presetData.put(pairs[0], values);
            }
        }

        return presetData;
    }

    /**
     * Get the refresh minutes from the apps defaults file.
     * 
     * @return The number of minutes between refresh
     */
    public static int getRefreshMinutes() {
        int refreshMinutes = AppsDefaults.getInstance().getInt(
                PDCConstants.HV_REFRESH_MINUTES, PDCConstants.REFRESH_MINUTES);

        return refreshMinutes;
    }

    /**
     * Get the Hours in Window value from the apps defaults file.
     * 
     * @return The hours in the window value
     */
    public static int getChangeHourWindow() {
        int hoursInWindow = AppsDefaults.getInstance().getInt(
                PDCConstants.HV_HOURS_IN_WINDOW, PDCConstants.MISSING_VALUE);

        return hoursInWindow;
    }

    public static double round(double value, int places) {
        double p = Math.pow(10, places);
        value *= p;
        double tmp = Math.round(value);
        return tmp / p;
    }

    /**
     * Convert duration int to String character.
     * 
     * @param dur
     *            The duration value
     * @return The single character duration value
     */
    public static String convertDur(int dur) {
        String value = null;

        switch (dur) {
        case 0:
            value = "I";
            break;
        case 1:
            value = "U";
            break;
        case 5:
            value = "E";
            break;
        case 10:
            value = "G";
            break;
        case 15:
            value = "C";
            break;
        case 30:
            value = "J";
            break;
        case 1001:
            value = "H";
            break;
        case 1002:
            value = "B";
            break;
        case 1003:
            value = "T";
            break;
        case 1004:
            value = "F";
            break;
        case 1006:
            value = "Q";
            break;
        case 1008:
            value = "A";
            break;
        case 1012:
            value = "K";
            break;
        case 1018:
            value = "L";
            break;
        case 2001:
            value = "D";
            break;
        case 2007:
            value = "W";
            break;
        // case 'N':
        // Not sure what to return. Shef maunal explanation:
        // N Mid month, duration for the period from the 1st day of the
        // month to and ending on the
        // 15th day of the same month
        // break;
        case 3001:
            value = "M";
            break;
        case 4001:
            value = "Y";
            break;
        case 5004:
            value = "P";
            break;
        // case 'V':
        // return 4001;
        // Shef manual:
        // V Variable period, duration defined separately (see Tables 11a
        // and 11b) 1/
        // break;
        case 5001:
            value = "S";
            break;
        case 5002:
            value = "R";
            break;
        case 5005:
            value = "X";
            break;
        // case 5001:
        // value = "Z";
        // break;
        }

        return value;
    }
}