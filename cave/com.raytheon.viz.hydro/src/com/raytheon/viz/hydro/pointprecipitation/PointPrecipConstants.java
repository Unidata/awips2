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
package com.raytheon.viz.hydro.pointprecipitation;

import java.time.format.DateTimeFormatter;

import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Point Precipitation Accumulation constants.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2008 1617       grichard    Initial creation.
 * Oct 28, 2009 2257       mpduff      Added more constants.
 * Jan 31, 2019 6951       dgilling    Added more constants.
 * Mar 11, 2020 19533   mgamazaychikov Moved constants from PointPrecipConstants
 *                                     to CommonHydroConstants
 * </pre>
 *
 * @author grichard
 */

//public class PointPrecipConstants extends HydroConstants {
public class PointPrecipConstants {
    public static final float MISSING_PRECIP = -9999f;
    public static final char MISSING_CHAR = 'm';
    public static final int EXACT_ENDINGTIME_MATCH = -1;
    public static final int CLOSEST_ENDINGTIME_MATCH = -2;
    public static final int LATEST_ENDINGTIME_MATCH = -3;

    public static final char OK_CHAR = ' ';
    public static final char REJECTED_CHAR = 'r';

    /**
     * Token for minimum duration filled
     */
    public static final String HV_MIN_DUR_FILLED = "hv_min_dur_filled";

    /**
     * The durations accessible on the dialog.
     */
    protected static final int[] durationValues = { 1, 3, 6, 12, 24, 48, 72 };

    protected static final String[] DURATION_NAMES = { "1 Hour", "3 Hour",
            "6 Hour", "12 Hour", "24 Hour", "48 Hour", "72 Hour", "Other" };

    /**
     * The minimum percentage of the accumulation interval which must be covered
     * by precipitation reports.
     */
    public static final double MIN_PERCENT = 0.0;

    /**
     * Format string for percent in format header method.
     */
    public static final String FMT_PCT = "%-6.2f";

    /**
     * Format string for duration header/trailer in format header method.
     */
    public static final String FMT_DUR_HD_TR = "%-8.8s %-20.20s %-2.2s %-5.6s";

    /**
     * Format string for the key information.
     */
    public static final String FMT_KEY_INFO = "%-8.8s %-20.20s %-2.2s %-2.2s :";

    /**
     * Format string for durations in format header method.
     */
    public static final String FMT_DUR = "%2d %-3.4s";

    /**
     * Format string for durations trailer in format header method.
     */
    public static final String FMT_DUR_TR = "%-3s%-3.4s";

    /**
     * PC start time adjusted default hours.
     */
    public static final int DEFAULT_ADJUSTED_STARTTIME_HRS = 4;

    /**
     * Number of Durations.
     */
    public static final int NUMDURATIONS = 8;

    public static final int QC_NOT_FAILED = 105;

    public static final int PRECIP_TS_SINGLE = 0;

    public static final int PRECIP_NO_ACCUM = 1;

    public static final int REPORT_MISSING_BELOW_MIN_PERCENT = 4;

    public static final int PRECIP_PE_BEST = 8;

    public static final int PRECIP_PP = 16;

    public static final int PRECIP_PC = 32;

    public static final int PRECIP_TS_BEST = 64;

    public static final int PRECIP_TS_RANK = 128;

    public static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter
            .ofPattern(CommonHydroConstants.IHFS_DATE_FORMAT);

  /* Enumeration used by filterStationByHsa method */
    public static enum FilterStation {
        UseStation, IgnoreStation
    }

    public static enum RawPrecipTable {
        RawPrecip, CurRawPrecip
    }

    public static enum SortOption {
        SortByLocation, SortByValue
    }
}
