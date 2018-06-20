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
package com.raytheon.viz.texteditor.qc;

import java.util.regex.Pattern;

/**
 * Utility class that contains common regexes and constants used by the
 * {@code IQCCheck} classes.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2017  #6251     dgilling     Initial creation
 *
 * </pre>
 *
 * @author dgilling
 */

public final class QCCheckConstants {

    /** AWIPS ID pattern (ex. SMWKEY) */
    public static final Pattern AWIPS_ID_PATTERN = Pattern
            .compile("(\\w{3})(\\w{3})");

    /** Match for "TTAAii CCCC DDHHMM BBB" line */
    public static final Pattern WMO_HEADER_PATTERN = Pattern.compile(
            "(\\w{2})\\w{2}\\d{2}\\s\\w{4}\\s\\S{6}((\\s(\\w{2})(\\w{1}))|)");

    public static final Pattern DATE_PATTERN = Pattern.compile(
            "(\\d{1,2})(\\d{2})\\s(AM|PM)\\s(\\w{3,4})\\s\\w{3}\\s(\\w{3})\\s{1,}(\\d{1,2})\\s(\\d{4})");

    /*
     * In order to keep this pattern simple, it does not exclude empty lines.
     * The empty line case must be handled separately.
     */
    public static final Pattern UGC_PATTERN = Pattern
            .compile("^(?:(?:[A-Z]{2}[CZ]\\d{3}-)?(?:\\d{3}-)*)*(?:\\d{6}-)?$");

    public static final Pattern VTEC_PATTERN = Pattern.compile(
            "/[OTEX]\\.([A-Z]{3})\\.[A-Za-z0-9]{4}\\.[A-Z]{2}\\.[WAYSFON]\\.\\d{4}\\.\\d{6}T\\d{4}Z-\\d{6}T\\d{4}Z/");

    public static final Pattern HVTEC_PATTERN = Pattern.compile(
            "/[A-Za-z0-9]{5}.[0-3NU].(\\w{2}).\\d{6}T\\d{4}Z.\\d{6}T\\d{4}Z.\\d{6}T\\d{4}Z.\\w{2}/");

    public static final Pattern FIRST_BULLET_PATTERN = Pattern.compile(
            "^\\*\\s(.*)\\s(WARNING|ADVISORY)(\\sFOR(.*)|...)",
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern SECOND_BULLET_PATTERN = Pattern.compile(
            "^\\*\\sUNTIL\\s(\\d{1,2})(\\d{2})\\s(AM|PM)\\s(\\w{3,4})",
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern THIRD_BULLET_PATTERN = Pattern.compile(
            "^\\*\\sAT\\s(\\d{1,2})(\\d{2})\\s(AM|PM)\\s(\\w{3,4})(.*)",
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final Pattern AREA_NAME_LIST_PATTERN = Pattern
            .compile("^([\\w\\s\\.'/]*-)");

    public static final Pattern LAT_LON_PATTERN = Pattern
            .compile("LAT...LON+(\\s\\d{3,4}\\s\\d{3,5}){1,}");

    public static final Pattern SUB_LAT_LON_PATTERN = Pattern.compile(
            "\\s{1,}\\d{3,4}\\s\\d{3,5}(|(\\s\\d{3,4}\\s\\d{3,5}){1,})");

    public static final Pattern LAT_LON_PAIR_PATTERN = Pattern
            .compile("(\\d{3,4})\\s(\\d{3,5})");

    public static final Pattern TIME_MOT_LOC_PATTERN = Pattern.compile(
            "TIME...MOT...LOC \\d{3,4}Z\\s\\d{3}DEG\\s\\d{1,3}KT((\\s\\d{3,4}\\s\\d{3,5}){1,})");

    public static final String TEST_MESSAGE_LABEL = "THIS IS A TEST MESSAGE.";

    public static final String TEST_MESSAGE_PATTERN = "THIS IS A TEST MESSAGE.\\p{Blank}{0,1}";

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     *
     */
    private QCCheckConstants() {
        throw new AssertionError();
    }
}
