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
package com.raytheon.viz.warngen.text;

import java.util.regex.Pattern;

/**
 * Common patterns.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2012   15332    jsanchez     Initial creation
 * Oct 18, 2012   15332    jsanchez     Replaced listOfAreaNamesPtrn with String pattern.
 * Mar 13, 2013  DR 15892  D. Friedman  Allow some punctuation in area names.
 * Apr 18, 2013  DR 16055  D. Friedman  Allow more than one contiguous space in areas.
 * Jan  6, 2014  DR 16627  Qinglu Lin   Updated listOfAreaName to get county name list locked when county abbreviation 
 *                                      is dropped, a name has more than one word, two names are separated by slash (/), 
 *                                      or a name has an apostrophe (') for Significant Weather Advisory, Special Weather 
 *                                      Statement, and Short Term Forecast.
 * Apr 29, 2014   3033     jsanchez     Added more patterns.
 * May  1, 2014  DR 16627  Qinglu Lin   Roll back the changes to listOfAreaName on January 6, 2014.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public interface ICommonPatterns {

    /** Start tag for locking */
    public static final String LOCK_START = "<L>";

    /** End tag for locking */
    public static final String LOCK_END = "</L>";

    public static final String NEWLINE = "\\n";

    // LOCK_END should not be found at the beginning since the previous line
    // should be blank.
    public static final String ugc = "(^(\\w{2}[CZ]\\d{3}\\S*-\\d{6}-)$|((\\d{3}-)*\\d{6}-)$|((\\d{3}-)+))";

    // LOCK_END can be added at the start of the line if a previous line has
    // been locked.
    public static final String listOfAreaName = "^(("
            + LOCK_END
            + "){0,1}((([\\?\\(\\)\\w\\.,/'-]+\\s{1,})+\\w{2}-)*(([\\?\\(\\)\\w\\.,/'-]+\\s{1,})+\\w{2}-)))";

    // LOCK_END should not be found at the beginning of a first bullet since the
    // previous line should be blank.
    public static final String firstBullet = "^(\\* (.*) (WARNING|ADVISORY)( FOR(.*)|\\.\\.\\.)"
            + NEWLINE + ")";

    // LOCK_END can be added at the start of the line if a previous line has
    // been locked.
    public static Pattern datePtrn = Pattern
            .compile(
                    "^(("
                            + LOCK_END
                            + "){0,1}\\d{3,4} (AM|PM) (\\w{3,4}) \\w{3} (\\w{3})\\s+(\\d{1,2}) (\\d{4})"
                            + NEWLINE + ")", Pattern.MULTILINE);

    public static final Pattern header = Pattern.compile(
            "^((THE NATIONAL WEATHER SERVICE IN .{1,} HAS (ISSUED A|EXTENDED THE))"
                    + NEWLINE + ")$", Pattern.MULTILINE);

    /*
     * LOCK_END should not be found at the beginning since the previous line
     */
    public static final Pattern secondBulletPtrn = Pattern
            .compile(
                    "\\* UNTIL \\d{3,4} (AM|PM) \\w{3,4}( \\w{6,9}){0,1}(\\/\\d{3,4} (AM|PM) \\w{3,4}( \\w{6,9}){0,1}\\/){0,1}"
                            + NEWLINE, Pattern.MULTILINE);

    public static final Pattern htecPtrn = Pattern
            .compile(
                    "^(("
                            + LOCK_END
                            + "){0,1}/[A-Za-z0-9]{5}.[0-3NU].\\w{2}.\\d{6}T\\d{4}Z.\\d{6}T\\d{4}Z.\\d{6}T\\d{4}Z.\\w{2}/"
                            + NEWLINE + ")", Pattern.MULTILINE);

    public static final Pattern vtecPtrn = Pattern
            .compile(
                    "^(("
                            + LOCK_END
                            + "){0,1}/[OTEX]\\.([A-Z]{3})\\.[A-Za-z0-9]{4}\\.[A-Z]{2}\\.[WAYSFON]\\.\\d{4}\\.\\d{6}T\\d{4}Z-\\d{6}T\\d{4}Z/"
                            + NEWLINE + ")", Pattern.MULTILINE);

    public static final Pattern tmlPtrn = Pattern
            .compile(
                    "^(("
                            + LOCK_END
                            + "){0,1}(TIME\\.\\.\\.MOT\\.\\.\\.LOC \\d{3,4}Z \\d{3}DEG \\d{1,3}KT(( \\d{3,4} \\d{3,5}){1,})(\\s*\\d{3,5} )*)\\s*"
                            + NEWLINE + ")", Pattern.MULTILINE);

    public static Pattern testPtrn = Pattern
            .compile("("
                    + "THIS IS A TEST MESSAGE\\. DO NOT TAKE ACTION BASED ON THIS MESSAGE\\."
                    + NEWLINE
                    + ")|"
                    + "("
                    + "THIS IS A TEST MESSAGE\\."
                    + ")|"
                    + "("
                    + "\\.\\.\\.THIS MESSAGE IS FOR TEST PURPOSES ONLY\\.\\.\\."
                    + NEWLINE + ")");

    public static final Pattern cta = Pattern.compile("("
            + "^(PRECAUTIONARY/PREPAREDNESS ACTIONS\\.\\.\\." + NEWLINE + ")"
            + ")" + "|(" + "^(&&" + NEWLINE + ")" + ")" + "|(" + "^(\\$\\$"
            + NEWLINE + ")" + ")", Pattern.MULTILINE);

    public static final Pattern latLonPtrn = Pattern.compile(
            "^((LAT\\.\\.\\.LON( \\d{3,4} \\d{3,5})+)" + NEWLINE
                    + ")(((\\s{5}( \\d{3,4} \\d{3,5})+)" + NEWLINE + ")+)?",
            Pattern.MULTILINE);

}
