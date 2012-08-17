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
package com.raytheon.uf.viz.points;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Several static utility methods and constants for use on points.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class PointUtilities {
    public static final char DELIM_CHAR = '~';

    public static final String DELIMITER = String.valueOf(DELIM_CHAR);

    public static final int MAX_LATITUDE = 90;

    public static final int MAX_LONGITUDE = 180;

    public static final double MINUTE_PER_DEGREE = 60;

    public static final double SECOND_PER_MINUTE = 60;

    private static final Pattern RedundantWhiteSpace = Pattern.compile("\\s+");

    private static final Pattern SingleSpace = Pattern.compile("\\s");

    private static final Pattern invalidFileName = Pattern
            .compile("[^A-Za-z0-9_ ]");

    private PointUtilities() {
        // Never need an instance of this class.
    }

    public static String removeRedundantWhiteSpace(String name) {
        Matcher matcher = RedundantWhiteSpace.matcher(name);
        String str = matcher.replaceAll(" ");
        return str;
    }

    /**
     * Replaces whitespace in name with delimiter.
     * 
     * @param name
     * @return str
     */
    public static String convertSpaceToDelimiter(String name) {
        Matcher matcher = SingleSpace.matcher(name);
        String str = matcher.replaceAll(DELIMITER);
        return str;
    }

    /**
     * Trim leading and trailing whitespace and remove redundant inner
     * whitespace.
     * 
     * @param str
     * @return
     */
    public static String trimAll(String str) {
        str = str.trim();
        str = PointUtilities.removeRedundantWhiteSpace(str);
        return str;
    }

    /**
     * This method defines a valid file name as any alpha-numeric name and
     * containing only underscores and spaces.
     * 
     * @param filename
     * @return
     */
    public static boolean isValidFileName(String filename) {
        boolean isValid = true;
        Matcher matcher = invalidFileName.matcher(filename);
        if (matcher.find()) {
            isValid = false;
        }
        return isValid;
    }
}
