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
package com.raytheon.edex.plugin.warning.util;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class will provide static utility methods related to UGC Headers.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 5, 2008				bwoodle	Initial creation
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */
public class UGCHeaderUtil {

    /** Another RE to Catch the Date portion of the UGC Header */
    private static final String DATESTR = "[0-9]{6}\\-";

    /** Parses a single county from a UGC Header, can be repeated */
    private static final String SINGLECOUNTY = "(?:([A-Z]{3}[0-9]{3})([\\-\\>]))|(?:([0-9]{3})[\\-\\>])";

    /**
     * Private utility method which will return an ArrayList of each zone
     * formatted from a UGC Header.
     * 
     * @param fips
     * @return
     */
    public static ArrayList<String> getUGCZones(String fips) {
        ArrayList<String> rval = new ArrayList<String>();
        String matchStr = "";

        Pattern pattern = Pattern.compile(DATESTR);
        Matcher matcher = pattern.matcher(fips);
        if (matcher.find()) {
            matchStr = fips.substring(0, fips.length() - 7);
        } else {
            matchStr = fips;
        }
        if (!matchStr.endsWith("-")) {
            matchStr += "-";
        }

        String currentState = "";
        String startOfSeries = "";
        pattern = Pattern.compile(SINGLECOUNTY);
        matcher = pattern.matcher(matchStr);
        while (matcher.find()) {
            if (matcher.group(1) != null && matcher.group(1).length() == 6) {
                currentState = matcher.group(1).substring(0, 3);
                rval.add(matcher.group(1));
                if (matcher.group(2).equals(">")) {
                    startOfSeries = matcher.group(1).substring(3, 6);
                }
            } else {
                rval.add(currentState + matcher.group(3));
                if (!startOfSeries.equals("")) {
                    int start = Integer.parseInt(startOfSeries);
                    int end = Integer.parseInt(matcher.group(3));
                    for (int i = start + 1; i < end; i++) {
                        String str = String.valueOf(i);
                        while (str.length() < 3) {
                            str = "0" + str;
                        }
                        rval.add(currentState + str);
                    }
                    startOfSeries = "";
                }
                if (matcher.group(2) != null && matcher.group(2).equals(">")) {
                    startOfSeries = matcher.group(1).substring(3, 6);
                }
            }
        }
        return rval;
    }

}
