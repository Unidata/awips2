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

    public static final String newline = "\\n";

    // LOCK_END should not be found at the beginning since the previous line
    // should be blank.
    public static final String ugc = "(^(\\w{2}[CZ]\\d{3}\\S*-\\d{6}-)$|((\\d{3}-)*\\d{6}-)$|((\\d{3}-)+))";

    // LOCK_END can be added at the start of the line if a previous line has
    // been locked.
    public static final String listOfAreaName = "^((" + LOCK_END
            + "){0,1}((([\\?\\(\\)\\w\\.,/'-])+(\\s{1,}\\w{2}){0,1}(\\s{1,}\\w{1,}([\\/\\']\\w{0,}\\s{0,}\\w{0,}){0,}){0,}-)*))";

    // LOCK_END should not be found at the beginning of a first bullet since the
    // previous line should be blank.
    public static final String firstBullet = "^(\\* (.*) (WARNING|ADVISORY)( FOR(.*)|\\.\\.\\.)"
            + newline + ")";

    // LOCK_END can be added at the start of the line if a previous line has
    // been locked.
    public static Pattern datePtrn = Pattern
            .compile(
                    "^(("
                            + LOCK_END
                            + "){0,1}\\d{3,4} (AM|PM) (\\w{3,4}) \\w{3} (\\w{3})\\s+(\\d{1,2}) (\\d{4})"
                            + newline + ")", Pattern.MULTILINE);
}
