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
package com.raytheon.uf.common.hydro;

/**
 * Common Hydro Constants.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 26, 2016 5571       skorolev    Initial creation
 * Jul 01, 2016 4623       skorolev    Added PRECIP_TS_SINGLE
 * 
 * </pre>
 * 
 * @author skorolev
 */

public class CommonHydroConstants {

    public static class AppsDefaults {

        public static final String DB_NAME = "db_name";

        /* input data files location */
        public static final String GAGE_PP_DATA = "gage_pp_data";

        /* token which controls how PC precipitation totals are derived. */
        public static final String SUM_PC_REPORTS_TOKEN = "sum_pc_reports";

        /** The local 7am search window token */
        public static final String PPP_PPD_LOCAL_7AM_WINDOW = "ppp_ppd_local_7am_window";

        public AppsDefaults() {
        }
    }

    public static final String SUM_PC_REPORTS_TOKEN = "sum_pc_reports";

    public static final int PRECIP_TS_SINGLE = 0;

    public static final int PRECIP_NO_ACCUM = 1;

    public static final int REPORT_MISSING_BELOW_MIN_PERCENT = 4;

    public static final int PRECIP_PE_BEST = 8;

    public static final int PRECIP_PP = 16;

    public static final int PRECIP_PC = 32;

    public static final int PRECIP_TS_BEST = 64;

    public static final int PRECIP_TS_RANK = 128;

    public static final int DEFAULT_ADJUSTED_STARTTIME_HRS = 4;

    public static final int EXACT_ENDINGTIME_MATCH = -1;

    public static final int CLOSEST_ENDINGTIME_MATCH = -2;

    public static final int LATEST_ENDINGTIME_MATCH = -3;

    public static final int MISSING_VALUE = -9999;

    public static final char OK_CHAR = ' ';

    public static final char MISSING_CHAR = 'm';

    public static final char REJECTED_CHAR = 'r';

    public static final int LOCAL_5004_7AM_WINDOW = 0;

    public static final String PC = "PC";

    public static final String PP = "PP";

    public static final int DEFAULT_QC_VALUE = 1879048191;

    public static final String IHFS = "ihfs";

    public static final float MISSING_PRECIP = -9999f;

    /* MPE missing representation */
    public static final float MPE_MISSING = -999f;

    /**
     * Default value for summation of accumulated precipitation reports
     */
    public static final int DEFAULT_SUM_PC_REPORTS_VALUE = 0;

}
