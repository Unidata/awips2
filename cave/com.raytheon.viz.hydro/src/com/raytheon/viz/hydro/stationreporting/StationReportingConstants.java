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
package com.raytheon.viz.hydro.stationreporting;


/**
 * Common constants for station reporting.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer       Description
 * ------------ ---------- -----------    --------------------------
 * Aug 14, 2008            Eric Babin     Initial Creation
 * Oct 7, 2008             Adam Skripsky  Added Constants for parsing DB results.
 * 21 Feb 2010  2915       mpduff         Removed static SimpleDateFormats.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class StationReportingConstants {

    public static final String QC_PASSED = "QC_PASSED";

    public static final String QC_QUESTIONABLE = "QC_QUESTIONABLE";

    public static final String QC_FAILED = "QC_FAILED";

    public static final String QC_NOT_PASSED = "QC_NOT_PASSED";

    public static final String QC_NOT_FAILED = "QC_NOT_FAILED";

    public static final int DEFAULT_QC_VALUE = 1879048191;

    public static final int GOOD_QUESTIONABLE_THRESHOLD = 1610612736;

    public static final int QUESTIONABLE_BAD_THRESHOLD = 1073741824;

    public static final int LID_INDEX = 0;

    public static final int NAME_INDEX = 1;

    public static final int PE_INDEX = 2;

    public static final int DUR_INDEX = 3;

    public static final int TS_INDEX = 4;

    public static final int EXTREMUM_INDEX = 5;

    public static final int OBSTIME_INDEX = 6;

    public static final int VALUE_INDEX = 7;

    public static final int SHEF_QUAL_CODE_INDEX = 8;

    public static final int QUALITY_INDEX = 9;

    public static final int REVISION_INDEX = 10;

    public static final int PRODUCT_ID_INDEX = 11;

    public static final int PRODUCT_TIME_INDEX = 12;

    public static final int POSTING_TIME_INDEX = 13;

    public static final int TIME_LID_INDEX = 0;

    public static final int TIME_DCP_FREQUENCY_INDEX = 1;

    public static final int TIME_DCP_TIME_INDEX = 2;

    public static final int TIME_TELEM_FREQUENCY_INDEX = 3;

    public static enum ListType {
        ALL("All"), DURATION("Duration"), NEVER("Never");

        private String stringValue;

        private ListType(String value) {
            stringValue = value;
        }

        @Override
        public String toString() {
            return stringValue;
        }
    }

    public static enum SortOrder {
        LOCATION("Location"), TIME("Time"), TIME_REVERSE("Time Reverse");

        private String stringValue;

        private SortOrder(String value) {
            stringValue = value;
        }

        @Override
        public String toString() {
            return stringValue;
        }
    }

    public static enum Duration {
        DEFAULT("Default"), MIN("Min"), MAX("Max");

        private String stringValue;

        private Duration(String value) {
            stringValue = value;
        }

        @Override
        public String toString() {
            return stringValue;
        }
    }
}
