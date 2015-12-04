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
package com.raytheon.viz.hydrocommon.whfslib;

import static com.raytheon.viz.hydrocommon.HydroConstants.MILLIS_PER_SECOND;
import static com.raytheon.viz.hydrocommon.util.QualityCodeUtil.QUESTIONABLE_BAD_THRESHOLD;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.IHourlyTS;
import com.raytheon.uf.common.dataplugin.shef.tables.Ingestfilter;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * PrecipUtil class is a singleton class that contains the utility methods for
 * hydrology.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 4, 2008  1662      grichard     Initial creation.
 * 11/19/2008   1662      grichard     Updated loadPeRaw.
 * 11/24/2008   1662      grichard     Added utility methods for raw precip.
 * 09/26/2012   15385     lbousaidi    fixed duplicate entries in gage table.
 * 11/04/2015   5100      bkowal       Fixes to handle records that spanned
 *                                     hour 24 to hour 1.
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public final class PrecipUtil {
    public static class total_precip {
        public String lid;

        public String PE;

        public String TS;

        public float value;

        public boolean summed_flag;

        public float hours_covered;

        public float percent_filled;

        public char value_indicator;

        public char qc;

        public DataErr err;

        public boolean reported_missing;

        public Date match_time;

        public total_precip() {
            lid = "";
            PE = "";
            TS = "";
            value = MISSING_PRECIP;
            summed_flag = false;
            hours_covered = 0;
            percent_filled = 0;
            value_indicator = MISSING_CHAR;
            qc = 'Z';
            err = new DataErr();
            reported_missing = false;
        }
    }

    public static class DataErr {
        public boolean negval;

        public boolean negdiff;

        public boolean largediff;

        public DataErr() {
            negval = false;
            negdiff = false;
            largediff = false;
        }
    }

    public static enum PrecipPEmode {
        PrecipPEbest, PrecipPEPP, PrecipPEPC
    }

    public static enum PrecipTSmode {
        PrecipTSbest, PrecipTSrank, PrecipTSsingle
    }

    /**
     * The static singleton instance.
     */
    private static PrecipUtil instance;

    /**
     * Singleton constructor.
     * 
     * @return the precipitation utility instance variable.
     */
    public static synchronized PrecipUtil getInstance() {
        if (instance == null) {
            instance = new PrecipUtil();
        }

        return instance;
    }

    private int previous_settings = 0;

    private PrecipPEmode pe_mode = PrecipPEmode.PrecipPEbest;

    private PrecipTSmode ts_mode = PrecipTSmode.PrecipTSsingle;

    private boolean report_miss_min_percent = false;

    private static final SimpleDateFormat sdf;

    private static final int MINUTES_PER_HOUR = 60;

    private static final int SECONDS_PER_MINUTE = 60;

    private static final int SECONDS_PER_HOUR = SECONDS_PER_MINUTE
            * MINUTES_PER_HOUR;

    public static final char MISSING_CHAR = 'm';

    public static final float MISSING_PRECIP = -9999f;

    public static final char OK_CHAR = ' ';

    public static final char REJECTED_CHAR = 'r';

    public static final int REPORT_MISSING_BELOW_MIN_PERCENT = 4;

    public static final int PRECIP_NO_ACCUM = 1;

    public static final int PRECIP_PC = 32;

    public static final int PRECIP_PE_BEST = 8;

    public static final int PRECIP_PP = 16;

    public static final int PRECIP_TS_BEST = 64;

    public static final int PRECIP_TS_RANK = 128;

    public static final int PRECIP_TS_SINGLE = 0;

    /**
     * Default value for summation of accumulated precipitation reports
     */
    public static final int DEFAULT_SUM_PC_REPORTS_VALUE = 0;

    /**
     * Token for accumulated precipitation totals
     */
    public static final String SUM_PC_REPORTS = "sum_pc_reports";

    static {
        sdf = new SimpleDateFormat("yyyy-MM-dd");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Private constructor: Use getInstance().
     */
    private PrecipUtil() {
    }

    /**
     * loadPeRaw
     * 
     * Method to load data from current precipitation table of IHFS database.
     * Combines legacy load_PC_raw and load_PP_raw methods in load_PCPP_data.
     * 
     * @param beginstr
     *            - string representing beginning time
     * @param endstr
     *            -- string representing ending time
     * @param locId
     *            -- location identifier
     * @param typeSource
     *            -- type source
     * @param pe
     *            -- physical element, either PC or PP
     * @return -- list of objects from IHFS database query
     */
    public ArrayList<Object[]> loadPeRaw(String beginstr, String endstr,
            String locId, java.util.List<String> typeSource,
            HydroConstants.PhysicalElement pe) {

        ArrayList<Object[]> retVal = null;
        String ts_clause = "";
        StringBuilder query = new StringBuilder();
        StringBuilder where = new StringBuilder();
        String lid = "";
        String value = "";
        String obstime = "";
        String ts = "";
        String physElt = "";
        String qcwhere = "";
        String dur = "";

        if ((typeSource != null) && !typeSource.isEmpty()) {
            ts_clause = build_ts_clause(typeSource);
            if (ts_clause == null) {
                return null;
            }
        }

        switch (pe) {
        case PC:
            query.append("select pc.lid, pc.pe, pc.dur, pc.ts, pc.extremum, pc.value, pc.shef_qual_code, pc.quality_code, pc.revision, pc.product_id, pc.producttime, pc.postingtime, pc.obstime, location.name from location, curpc pc where location.lid = pc.lid");
            lid = "pc.lid";
            value = "pc.value";
            obstime = "pc.obstime";
            ts = "pc.ts";
            physElt = " pc.";
            qcwhere = "";
            dur = "";
            break;
        case PP:
            query.append("select pp.lid, pp.pe, pp.dur, pp.ts, pp.extremum, pp.value, pp.shef_qual_code, pp.quality_code, pp.revision, pp.product_id, pp.producttime, pp.postingtime, pp.obstime, location.name from location, curpp pp where location.lid = pp.lid");
            lid = "pp.lid";
            value = "pp.value";
            obstime = "pp.obstime";
            ts = "pp.ts";
            physElt = " pp.";
            qcwhere = "pp.quality_code >= " + QUESTIONABLE_BAD_THRESHOLD;
            dur = "pp.dur";
            break;
        }

        if ((locId != null) && !locId.isEmpty() && (typeSource != null)
                && !typeSource.isEmpty()) {
            where.append(" AND ");
            where.append(lid + " = '");
            where.append(locId);
            where.append("' AND ");
            where.append(ts_clause);
            where.append(" AND " + value + " != '-9999.0' AND " + obstime
                    + " >= '");
            where.append(beginstr);
            where.append("' AND ");
            where.append(obstime + " <= '");
            where.append(endstr);
            switch (pe) {
            case PC:
                where.append("' ORDER BY " + obstime + " DESC ");
                break;
            case PP:
                where.append("' AND " + qcwhere + " ORDER BY " + dur
                        + " DESC, " + obstime + " DESC ");
                break;
            }
        } else if ((typeSource != null) && !typeSource.isEmpty()) {
            where.append(" AND ");
            where.append(ts_clause);
            where.append(" AND " + value + " != '-9999.0' AND " + obstime
                    + " >= '");
            where.append(beginstr);
            where.append("' AND ");
            where.append(obstime + " <= '");
            where.append(endstr);
            switch (pe) {
            case PC:
                where.append("' ORDER BY " + lid + " ASC, " + obstime
                        + " DESC ");
                break;
            case PP:
                where.append("' AND " + qcwhere + " ORDER BY " + lid + " ASC, "
                        + dur + " DESC, " + obstime + " DESC ");
                break;
            }
        } else if ((locId != null) && !locId.isEmpty()) {
            where.append(" AND ");
            where.append(lid + " = '");
            where.append(locId);
            where.append("' AND " + value + " != '-9999.0' AND " + obstime
                    + " >= '");
            where.append(beginstr);
            where.append("' AND ");
            where.append(obstime + " <= '");
            where.append(endstr);
            switch (pe) {
            case PC:
                where.append("' ORDER BY " + ts + " ASC, " + obstime + " DESC ");
                break;
            case PP:
                where.append("' AND " + qcwhere + " ORDER BY " + ts + " ASC, "
                        + dur + " DESC, " + obstime + " DESC ");
                break;
            }
        } else {
            where.append(" AND ");
            where.append(value + " != '-9999.0' AND " + obstime + " >= '");
            where.append(beginstr);
            where.append("' AND ");
            where.append(obstime + " <= '");
            where.append(endstr);
            switch (pe) {
            case PC:
                where.append("' ORDER BY " + lid + " ASC, " + ts + " ASC, "
                        + obstime + " DESC ");
                break;
            case PP:
                where.append("' AND " + qcwhere + " ORDER BY " + lid + " ASC, "
                        + ts + " ASC, " + dur + " DESC, " + obstime + " DESC ");
                break;
            }
        }

        if ((typeSource != null) && !typeSource.isEmpty()) {
            where.replace(where.indexOf(" id."), where.indexOf(" id.") + 4,
                    physElt);

        }

        query.append(where.toString());
        // Echo the query string to the console.
        System.out.println("Query = " + query.toString());

        try {
            retVal = (ArrayList<Object[]>) DirectDbQuery.executeQuery(
                    query.toString(), HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            e.printStackTrace();
        }

        return retVal;
    }

    /**
     * get_total_hourly_precip
     * 
     * DESCRIPTION: get_total_hourly_precip computes total precipitation based
     * on PP/PC data preprocessed by the gage precipitation processor and stored
     * in the HourlyPC, HourlyPP, and PPother tables.
     * 
     * @param hourlyPCList
     * @param pHourlyPCIdx
     *            current PC list index passed as an array to emulate passing by
     *            reference
     * @param hourlyPPList
     * @param pHourlyPPIdx
     *            current PP list index passed as an array to emulate passing by
     *            reference
     * @param ending_time
     * @param num_hours
     * @param min_percent
     * @param settings
     * @param advance
     * @param pc_records
     * @param pp_records
     * @return total_precip
     */
    public total_precip get_total_hourly_precip(
            ArrayList<Hourlypc> hourlyPCList, int[] pHourlyPCIdx,
            ArrayList<Hourlypp> hourlyPPList, int[] pHourlyPPIdx,
            Date ending_time, int num_hours, float min_percent, int settings,
            boolean advance, int[] pc_records, int[] pp_records) {

        /*
         * Make local copies of the pHourlyPCIdx, and pHourlyPPIdx pointers. The
         * originals will only be modified if the user has specified the advance
         * pointer option.
         */
        int pHourlyPCLoc = pHourlyPCIdx[0];
        int pHourlyPPLoc = pHourlyPPIdx[0];

        // Initialize the total_precip structure.
        total_precip total_precip = new total_precip();

        // Check the PC, PP, and PPother pointers for availability of data.
        if ((pHourlyPCLoc > hourlyPCList.size())
                && (pHourlyPPLoc > hourlyPPList.size())) {
            // Total precip stuct initialized to missing.
            return total_precip;
        }

        // Check the current settings against the settings used when this
        // routine was last called. If they are the same, then do not
        // process the settings.

        if (settings != previous_settings) {
            previous_settings = settings;
            // Check the settings flag to determine how to process these
            // precipitation reports.

            report_miss_min_percent = (settings & REPORT_MISSING_BELOW_MIN_PERCENT) != 0;
            if ((settings & PRECIP_PE_BEST) == PRECIP_PE_BEST) {
                pe_mode = PrecipPEmode.PrecipPEbest;
            } else if ((settings & PRECIP_PP) == PRECIP_PP) {
                pe_mode = PrecipPEmode.PrecipPEPP;
            } else if ((settings & PRECIP_PC) == PRECIP_PC) {
                pe_mode = PrecipPEmode.PrecipPEPC;
            }

            if ((settings & PRECIP_TS_BEST) == PRECIP_TS_BEST) {
                ts_mode = PrecipTSmode.PrecipTSbest;
            } else if ((settings & PRECIP_TS_RANK) == PRECIP_TS_RANK) {
                ts_mode = PrecipTSmode.PrecipTSrank;
            } else {
                ts_mode = PrecipTSmode.PrecipTSsingle;
            }
        }

        /*
         * Check the station id being pointed to by the HourlyPP and the
         * HourlyPC pointers. If these stations are not the same, then only
         * process the station which is lexically smaller.
         */
        int num_ts_pc = 0;
        int num_ts_pp = 0;
        ArrayList<Integer> pTsPc = new ArrayList<Integer>();
        ArrayList<Integer> pTsPp = new ArrayList<Integer>();
        Hourlypc pHourlyPC = null;
        if (pHourlyPCLoc < hourlyPCList.size()) {
            pHourlyPC = hourlyPCList.get(pHourlyPCLoc);
        }

        Hourlypp pHourlyPP = null;
        if (pHourlyPPLoc < hourlyPPList.size()) {
            pHourlyPP = hourlyPPList.get(pHourlyPPLoc);
        }

        if ((pHourlyPC != null) && (pHourlyPP != null)) {
            int status = pHourlyPC.getLid().compareTo(pHourlyPP.getLid());

            if (status == 0) {
                /* The stations are the same. */
                num_ts_pc = get_ts_count_hourly(hourlyPCList, pHourlyPCLoc,
                        pTsPc);
                num_ts_pp = get_ts_count_hourly(hourlyPPList, pHourlyPPLoc,
                        pTsPp);
                total_precip.lid = pHourlyPC.getLid();
            } else if (status < 0) {
                /*
                 * The PC lid is the smallest. Use it. Save the PP lid for later
                 * since there may be a matching PC lid further down the list.
                 */
                num_ts_pc = get_ts_count_hourly(hourlyPCList, pHourlyPCLoc,
                        pTsPc);
                total_precip.lid = pHourlyPC.getLid();
            } else {
                /*
                 * The PP lid is the smallest. Use it. Save the PC lid for later
                 * since there may be a matching PP lid further down the list.
                 */
                num_ts_pp = get_ts_count_hourly(hourlyPPList, pHourlyPPLoc,
                        pTsPp);
                total_precip.lid = pHourlyPP.getLid();
            }
        } else if (pHourlyPC == null) {
            /* There are no PC data. */
            num_ts_pp = get_ts_count_hourly(hourlyPPList, pHourlyPPLoc, pTsPp);
            total_precip.lid = pHourlyPP.getLid();
        } else {
            /* There are no PP data. */
            num_ts_pc = get_ts_count_hourly(hourlyPCList, pHourlyPCLoc, pTsPc);
            total_precip.lid = pHourlyPC.getLid();
        }

        // Retrieve the precip totals. If necessary process multiple TSs,
        // choosing
        // either the TS which produces the best precip coverage or the TS which
        // Has the highest rank.

        int num_pc_records = 0;
        int num_pp_records = 0;
        float best_pc_amount = MISSING_PRECIP;
        float best_pp_amount = MISSING_PRECIP;
        float pc_precip_amount;
        float pp_precip_amount;
        String best_pc_ts = null;
        String best_pp_ts = null;
        int best_covered = (int) MISSING_PRECIP;
        int[] best_pc_coverage = new int[] { (int) MISSING_PRECIP };
        int[] best_pp_coverage = new int[] { (int) MISSING_PRECIP };
        int[] pc_seconds_covered = new int[] { 0 };
        int[] pp_seconds_covered = new int[] { 0 };
        char[] best_pc_qc = new char[] { 'Z' };
        char[] best_pp_qc = new char[] { 'Z' };
        char[] temp_pc_qc = new char[] { 'Z' };
        char[] temp_pp_qc = new char[] { 'Z' };
        boolean[] best_reported_missing = new boolean[] { false };
        boolean[] reported_missing = new boolean[] { false };
        String lid;
        int pBestHourlyPCIdx;
        int pBestHourlyPPIdx;
        int[] ts_index = new int[] { 0 };
        switch (ts_mode) {
        // This is the case where the user wants to retrieve the precip total
        // for a single typesource.
        case PrecipTSsingle:

            if (num_ts_pc > 0) {
                num_pc_records = pTsPc.get(0);
                best_pc_ts = pHourlyPC.getTs();
                best_pc_amount = get_total_hourly_PC(hourlyPCList,
                        pHourlyPCLoc, ending_time, num_hours, num_pc_records,
                        best_pc_coverage, best_pc_qc);

            }

            if (num_ts_pp > 0) {
                num_pp_records = pTsPp.get(0);
                best_pp_ts = pHourlyPP.getTs();
                best_pp_amount = get_total_hourly_PP(hourlyPPList,
                        pHourlyPPLoc, ending_time, num_hours, num_pp_records,
                        best_pp_coverage, best_pp_qc, best_reported_missing);
            }

            break;

        case PrecipTSbest:

            // This is the case where the user wants to retrieve
            // the precipitation total for the typesource
            // which results in best precipitation total (most coverage)
            // for the user-specified interval.
            for (int i = 0; i < num_ts_pc; ++i) {
                num_pc_records = pTsPc.get(i);
                pc_precip_amount = get_total_hourly_PC(hourlyPCList,
                        pHourlyPCLoc, ending_time, num_hours, num_pc_records,
                        pc_seconds_covered, temp_pc_qc);

                if (pc_seconds_covered[0] > best_pc_coverage[0]) {
                    best_pc_coverage[0] = pc_seconds_covered[0];
                    best_pc_amount = pc_precip_amount;
                    best_pc_ts = pHourlyPC.getTs();
                    best_pc_qc[0] = temp_pc_qc[0];
                }

                pHourlyPC = hourlyPCList.get(++pHourlyPCLoc);
            }

            // When totaling the precipitation amount from the HourlyPPtable,
            // make sure that the TSs match.

            for (int i = 0; i < num_ts_pp; ++i) {
                num_pp_records = pTsPp.get(i);
                pp_precip_amount = get_total_hourly_PP(hourlyPPList,
                        pHourlyPPLoc, ending_time, num_hours, num_pp_records,
                        pp_seconds_covered, temp_pp_qc, reported_missing);

                if (pp_seconds_covered[0] > best_pp_coverage[0]) {
                    best_pp_coverage[0] = pp_seconds_covered[0];
                    best_pp_amount = pp_precip_amount;
                    best_pp_ts = pHourlyPP.getTs();
                    best_pp_qc = temp_pp_qc;
                    best_reported_missing[0] = reported_missing[0];
                }

                pHourlyPP = hourlyPPList.get(++pHourlyPPLoc);

            }

            break;

        case PrecipTSrank:

            // This case retrieves the precipitation total for the
            // highest ranking typesource.
            if (num_ts_pc > 0) {
                lid = pHourlyPC.getLid();
            } else {
                lid = pHourlyPP.getLid();
            }

            // Only perform TS ranking if for the given PE there are multiple
            // typesources.
            pBestHourlyPCIdx = pHourlyPCLoc;
            ts_index[0] = 0;

            List<Ingestfilter> pIngestNode = null;
            if (num_ts_pc > 1) {
                pIngestNode = get_ingest_info(lid);

                if (pIngestNode != null) {
                    pBestHourlyPCIdx = get_highest_ranking_hourly_ts(
                            hourlyPCList, pHourlyPCLoc, "PP", num_ts_pc,
                            ts_index, pTsPc, pIngestNode);
                }
            }

            // Get the total precipitation for the highest ranked ts.
            if (num_ts_pc > 0) {
                best_pc_amount = get_total_hourly_PC(hourlyPCList,
                        pBestHourlyPCIdx, ending_time, num_hours,
                        pTsPc.get(ts_index[0]), pc_seconds_covered, temp_pc_qc);
                best_pc_coverage[0] = pc_seconds_covered[0];
                best_pc_ts = hourlyPCList.get(pBestHourlyPCIdx).getTs();
                best_pc_qc = temp_pc_qc;
            }

            pBestHourlyPPIdx = pHourlyPPLoc;
            ts_index[0] = 0;

            if (num_ts_pp > 1) {

                if (pIngestNode == null) {
                    pIngestNode = get_ingest_info(lid);
                }

                if (pIngestNode != null) {
                    pBestHourlyPPIdx = get_highest_ranking_hourly_ts(
                            hourlyPPList, pHourlyPPLoc, "PP", num_ts_pp,
                            ts_index, pTsPp, pIngestNode);
                }
            }

            if (num_ts_pp > 0) {
                best_pp_amount = get_total_hourly_PP(hourlyPPList,
                        pBestHourlyPPIdx, ending_time, num_hours,
                        pTsPp.get(ts_index[0]), pp_seconds_covered, temp_pp_qc,
                        reported_missing);

                best_pp_coverage[0] = pc_seconds_covered[0];
                best_pp_ts = hourlyPPList.get(pBestHourlyPPIdx).getTs();
                best_pp_qc[0] = temp_pp_qc[0];
                best_reported_missing[0] = reported_missing[0];
            }

            if (pIngestNode != null) {
                pIngestNode.clear();
                pIngestNode = null;
            }

            break;

        default:

            // This case should not be reached. We should do something here
            // anyway.
            break;
        }

        if ((num_ts_pc > 0) && (num_ts_pp > 0)) {
            switch (pe_mode) {
            case PrecipPEbest:

                if ((best_pc_amount != MISSING_PRECIP)
                        || (best_pp_amount != MISSING_PRECIP)) {

                    /*
                     * Select the PE or PC estimate which provides the best
                     * precipitation amount estimate.
                     */
                    if (best_pc_coverage[0] > best_pp_coverage[0]) {
                        total_precip.value = best_pc_amount;
                        total_precip.PE = "PC";
                        total_precip.TS = best_pc_ts;
                        best_covered = best_pc_coverage[0];
                        total_precip.qc = best_pc_qc[0];
                        total_precip.summed_flag = false;
                    } else {
                        total_precip.value = best_pp_amount;
                        total_precip.PE = "PP";
                        total_precip.TS = best_pp_ts;
                        best_covered = best_pp_coverage[0];
                        total_precip.qc = best_pp_qc[0];
                        total_precip.summed_flag = num_hours > 1;
                        total_precip.reported_missing = best_reported_missing[0];
                    }
                }

                break;

            case PrecipPEPP:

                /* If there are PC and PP values, then use the PP value. */
                if ((num_ts_pp > 0)
                        && ((best_pp_amount != MISSING_PRECIP)
                                || (best_pp_qc[0] == 'M') || ((best_reported_missing[0]) && (best_pc_amount == MISSING_PRECIP)))
                        && (((best_pp_qc[0] != 'L') && (best_pp_qc[0] != 'C'))
                                || (best_pp_amount == best_pc_amount) || (best_pc_amount == MISSING_PRECIP)))

                {
                    total_precip.value = best_pp_amount;
                    total_precip.PE = "PP";
                    total_precip.TS = best_pp_ts;
                    best_covered = best_pp_coverage[0];
                    total_precip.qc = best_pp_qc[0];
                    total_precip.summed_flag = num_hours > 1;
                    total_precip.reported_missing = best_reported_missing[0];
                } else {
                    total_precip.value = best_pc_amount;
                    total_precip.PE = "PC";
                    total_precip.TS = best_pc_ts;
                    best_covered = best_pc_coverage[0];
                    total_precip.qc = best_pc_qc[0];
                    total_precip.summed_flag = false;
                }

                break;

            case PrecipPEPC:

                /*
                 * If there are totals from PC and PP, then use the PC value.
                 */
                if ((num_ts_pc > 0)
                        && ((best_pc_amount != MISSING_PRECIP) || (best_pc_qc[0] == 'M'))) {
                    total_precip.value = best_pc_amount;
                    total_precip.PE = "PC";
                    total_precip.TS = best_pc_ts;
                    best_covered = best_pc_coverage[0];
                    total_precip.qc = best_pc_qc[0];
                    total_precip.summed_flag = false;
                } else {
                    total_precip.value = best_pp_amount;
                    total_precip.PE = "PP";
                    total_precip.TS = best_pp_ts;
                    best_covered = best_pp_coverage[0];
                    total_precip.qc = best_pp_qc[0];
                    total_precip.summed_flag = num_hours > 1;
                    total_precip.reported_missing = best_reported_missing[0];
                }

                break;

            default:

                /* This case should not be reached. */
                break;
            }
        } else if (num_ts_pc > 0) {
            total_precip.value = best_pc_amount;
            total_precip.PE = "PC";
            total_precip.TS = best_pc_ts;
            best_covered = best_pc_coverage[0];
            total_precip.qc = best_pc_qc[0];
            total_precip.summed_flag = false;
        } else {
            total_precip.value = best_pp_amount;
            total_precip.PE = "PP";
            total_precip.TS = best_pp_ts;
            best_covered = best_pp_coverage[0];
            total_precip.qc = best_pp_qc[0];
            total_precip.summed_flag = num_hours > 1;
            total_precip.reported_missing = best_reported_missing[0];
        }

        if (total_precip.value != MISSING_PRECIP) {
            total_precip.hours_covered = (float) best_covered
                    / SECONDS_PER_HOUR;
            total_precip.percent_filled = (float) best_covered
                    / (num_hours * SECONDS_PER_HOUR);

            /* Do not allow a percentage filled of greater than 100%. */
            if (total_precip.percent_filled > 1.0f) {
                total_precip.percent_filled = 1.0f;
            }

            total_precip.value_indicator = OK_CHAR;

            /* Set the QC and error flags. */
            if (report_miss_min_percent) {
                if (total_precip.percent_filled < min_percent) {
                    total_precip.value = MISSING_PRECIP;
                    total_precip.value_indicator = REJECTED_CHAR;
                }
            }

            if ((total_precip.value < 0)
                    && (total_precip.value != MISSING_PRECIP)) {
                total_precip.value = MISSING_PRECIP;
                total_precip.err.negdiff = true;
                total_precip.value_indicator = MISSING_CHAR;
            }

        } else {
            total_precip.err.negval = true;
        }

        /*
         * If the user has requested it, advance the pointer. This enables
         * colooping, a feature which can save alot of CPU time.
         */
        if (advance) {
            switch (ts_mode) {
            case PrecipTSsingle:

                if (num_ts_pc > 0) {
                    pHourlyPCIdx[0]++;
                    pc_records[0] = pTsPc.get(0);
                }

                if (num_ts_pp > 0) {
                    pHourlyPPIdx[0]++;
                    pp_records[0] = pTsPp.get(0);
                }

                break;

            case PrecipTSbest:
            case PrecipTSrank:

                for (int k = 0; k < num_ts_pc; ++k) {
                    pHourlyPCIdx[0]++;
                    pc_records[0] += pTsPc.get(k);
                }

                for (int k = 0; k < num_ts_pp; ++k) {
                    pHourlyPPIdx[0]++;
                    pp_records[0] += pTsPp.get(k);
                }

                break;

            default:

                /* Should never reach this point. */
                break;
            }
        }

        pTsPc.clear();
        pTsPc = null;

        pTsPp.clear();
        pTsPp = null;

        return total_precip;
    }

    /**
     * build_ts_clause
     * 
     * @param ts
     * @return
     */
    public String build_ts_clause(List<String> ts) {
        if ((ts == null) || ts.isEmpty()) {
            return "";
        }
        StringBuilder tsClause = new StringBuilder("id.ts ");

        if (ts.get(0).startsWith("!")) {
            tsClause.append("not in ('");
            tsClause.append(ts.get(0).substring(1));
        } else {
            tsClause.append("in ('");
            tsClause.append(ts.get(0));
        }

        for (int i = 1; i < ts.size(); i++) {
            tsClause.append("', '");
            tsClause.append(ts.get(i));
        }
        tsClause.append("')");

        return tsClause.toString();
    }

    /**
     * compare_tsrank
     * 
     * @param ts1
     * @param ts2
     * @param pIngestHead
     * @param pe
     * @param dur
     * @param extremum
     * @return
     */
    public int compare_tsrank(String ts1, String ts2,
            List<Ingestfilter> pIngestHead, String pe, int dur, char extremum) {

        Ingestfilter ingestPtr = null;
        int rank1, rank2;
        int result;
        int status;

        /* initialize */
        rank1 = rank2 = 0;

        Iterator<Ingestfilter> ingestIter = pIngestHead.iterator();

        while (ingestIter.hasNext()) {
            ingestPtr = ingestIter.next();
            status = ingestPtr.getId().getPe().compareTo(pe);

            if ((status == 0) && (dur == ingestPtr.getId().getDur())
                    && (extremum == ingestPtr.getId().getExtremum().charAt(0))) {

                if (ingestPtr.getId().getTs().equals(ts1)) {
                    rank1 = ingestPtr.getTsRank();
                }

                if (ingestPtr.getId().getTs().equals(ts2)) {
                    rank2 = ingestPtr.getTsRank();
                }

                if ((rank1 != 0) && (rank2 != 0)) {
                    break;
                }
            }
        }

        if (rank1 > rank2) {
            result = 1;
        } else if (rank1 < rank2) {
            result = -1;
        } else {
            result = 0;
        }

        return (result);
    }

    /**
     * get_highest_ranking_hourly_ts
     * 
     * For a given LID, PE, this routine determines the highest ranked TS. This
     * routine is for the data in the HoulryPC and HourlyPP tables.
     * 
     * @param hourlyData
     * @param pHourlyIdx
     * @param pe
     * @param num_ts
     * @param pTsIndex
     * @param pTs
     * @param pIngestHead
     * @return pBestHourlyIdx
     */
    public int get_highest_ranking_hourly_ts(
            ArrayList<? extends IHourlyTS> hourlyData, int pHourlyIdx,
            String pe, int num_ts, int[] pTsIndex, ArrayList<Integer> pTs,
            List<Ingestfilter> pIngestHead) {

        int pBestHourlyIdx = -1;
        IHourlyTS pBestHourlyData = null;
        IHourlyTS pHourlyTS = null;

        for (int i = 0; i < num_ts; i++) {
            pHourlyTS = hourlyData.get(pHourlyIdx);

            if (i == 0) {
                pBestHourlyData = pHourlyTS;
                pBestHourlyIdx = pHourlyIdx;
                pTsIndex[0] = i;

            } else {
                int status = pe.compareTo("PP");

                if (status == 0) {
                    status = compare_tsrank(pHourlyTS.getTs(),
                            pBestHourlyData.getTs(), pIngestHead, pe, 1001, 'Z');
                } else {
                    status = compare_tsrank(pHourlyTS.getTs(),
                            pBestHourlyData.getTs(), pIngestHead, pe, 0, 'Z');
                }

                if (status < 0) {
                    pBestHourlyData = pHourlyTS;
                    pBestHourlyIdx = pHourlyIdx;
                    pTsIndex[0] = i;
                }

            }

            pHourlyIdx += pTs.get(i);
        }
        return pBestHourlyIdx;
    }

    /**
     * get_total_hourly_PC
     * 
     * @param hourlyPCList
     * @param pHourlyPCIdx
     * @param ending_time
     * @param num_hours
     * @param num_pc_records
     * @param seconds_covered
     * @param pc_qc
     * @return
     */
    public float get_total_hourly_PC(List<Hourlypc> hourlyPCList,
            int pHourlyPCIdx, Date ending_time, int num_hours,
            int num_pc_records, int[] seconds_covered, char[] pc_qc) {
        char shef_qc_char;
        float total = MISSING_PRECIP;
        int pEndPCIdx = -1;
        int pEndPrevIdx = -1;
        int pStartPCIdx = -1;
        int diff;
        int end_hour;
        boolean found;
        int hour_index;
        int num_days;
        int record_count;
        int start_hour;
        boolean value_found;
        Short end_value;
        Short start_value;
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Date end_date;
        Date pc_end_timet;
        Date pc_start_timet;
        Date pc_timet = new Date(0);
        Date start_date;

        pc_qc[0] = 'Z';

        /* Initialize the number of seconds covered to 0. */
        seconds_covered[0] = 0;

        if (pHourlyPCIdx >= hourlyPCList.size()) {
            /* No PC data was passed into this routine. */
            return MISSING_PRECIP;
        }

        /*
         * Determine the end date (YYYY-MM-DD) and hour of the accumulation
         * interval.
         */
        cal.setTime(ending_time);
        end_hour = cal.get(Calendar.HOUR_OF_DAY);

        /*
         * Check to make sure the end date is top of the hour. If not, then
         * adjust the date according to the number of minutes.
         */
        if (cal.get(Calendar.MINUTE) > 30) {
            ++end_hour;
        }

        /*
         * Determine if the end hour is 0. If it is, then set it to 24 and
         * decrease the date by one day.
         */
        if (end_hour == 0) {
            end_hour = 24;
            cal.add(Calendar.DAY_OF_MONTH, -1);
        }

        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        end_date = cal.getTime();

        /*
         * Determine the start date (YYYY-MM-DD) of the accumulation interval.
         */
        start_date = cal.getTime();

        diff = end_hour - num_hours;
        start_hour = diff % 24;

        if (start_hour <= 0) {
            start_hour += 24;
        }

        if (diff <= 0) {
            num_days = diff / 24 + 1;
            cal.add(Calendar.DAY_OF_MONTH, -num_days);
            start_date = cal.getTime();
        }

        /*
         * Load the record from the HourlyPC table corresponding to the
         * beginning date of the interval. Since we are dealing with the Hourly
         * tables, a value for the exact start date and hour must be available.
         */
        pStartPCIdx = pHourlyPCIdx;
        found = false;
        record_count = 0;

        for (int i = 0; i < num_pc_records; ++i) {
            record_count++;
            pc_timet = hourlyPCList.get(pStartPCIdx).getId().getObsdate();

            if (!pc_timet.before(start_date)) {
                found = true;
                break;
            }

            pStartPCIdx++;
        }

        if (!found) {
            /* No start date could be retrieved. */
            return MISSING_PRECIP;
        }

        if (pc_timet.after(start_date)) {
            /*
             * An exact match for the start date could not be found. Set the
             * starting hour to 1.
             */
            start_hour = 1;
        }

        pEndPCIdx = pStartPCIdx;

        if (pc_timet.after(end_date)) {
            /*
             * The starting time is after the ending time. Cannot compute a
             * PC-based precipitation total.
             */
            return MISSING_PRECIP;
        }

        if (!pc_timet.equals(end_date)) {
            pEndPrevIdx = pEndPCIdx;

            for (int i = record_count; i < num_pc_records; ++i) {
                pEndPCIdx++;
                ++record_count;
                pc_timet = hourlyPCList.get(pEndPCIdx).getId().getObsdate();

                if (end_date.equals(pc_timet)) {
                    break;
                } else if (pc_timet.after(end_date)) {
                    pEndPCIdx = pEndPrevIdx;
                    break;
                }

                pEndPrevIdx = pEndPCIdx;
                pEndPCIdx++; // RWA this seems like it shoudn't be here since
                // we also increment at the top of the loop

            }
        }

        /*
         * In the case where an exact match for the ending date could not be
         * found set the end hour to 24.
         */
        if ((pEndPCIdx == -1) || pc_timet.before(end_date)) {
            end_hour = 24;
            pEndPCIdx = pEndPrevIdx;
        }

        /*
         * The start and end dates have been retrieved. Compute the
         * precipitation total.
         */

        /*
         * Find the hour slot in the start date which is closest to the starting
         * time.
         */
        value_found = false;
        start_value = (short) MISSING_PRECIP;
        /*
         * Adjust for records returned that span more than one day. Simplest fix
         * without doing a significant rewrite of the algorithm. Not completely
         * optimal; however, this ensures that start->end is processed every
         * time.
         */
        if (start_hour == end_hour && start_hour == 1) {
            start_hour = 24;
            ++pStartPCIdx;
            pEndPCIdx += 2;
        }

        while ((pStartPCIdx < hourlyPCList.size())
                && ((pStartPCIdx != pEndPCIdx) || (start_hour < end_hour) || (start_hour == 24 && end_hour == 1))) {
            Hourlypc pStartPC = hourlyPCList.get(pStartPCIdx);
            start_value = get_hour_slot_value(pStartPC, start_hour);

            hour_index = start_hour - 1;
            shef_qc_char = pStartPC.getHourlyQc().charAt(hour_index);

            if ((start_value != null) && (start_value != MISSING_PRECIP)
                    && (shef_qc_char != 'B') && (shef_qc_char != 'R')) {
                /* The starting PC value has been found. */
                value_found = true;
                break;
            }

            ++start_hour;

            if (start_hour > 24) {
                start_hour = 1;
                pStartPCIdx++;
            }
        }

        if (!value_found) {
            /* The starting value could not be found. */
            return MISSING_PRECIP;
        }

        /*
         * Find the hour slot in the end date which is closest to the starting
         * time.
         */
        value_found = false;
        end_value = (short) MISSING_PRECIP;

        while ((pEndPCIdx > pStartPCIdx)
                || ((pEndPCIdx == pStartPCIdx) && (end_hour > start_hour))) {
            Hourlypc pEndPC = hourlyPCList.get(pEndPCIdx);
            hour_index = end_hour - 1;
            end_value = get_hour_slot_value(pEndPC, end_hour);

            shef_qc_char = pEndPC.getHourlyQc().charAt(hour_index);

            if ((end_value != null) && (end_value != MISSING_PRECIP)
                    && (shef_qc_char != 'B') && (shef_qc_char != 'R')) {
                value_found = true;

                /* Set the qc value to the end hour qc. */
                pc_qc[0] = shef_qc_char;
                break;
            }

            --end_hour;

            // if (end_hour < 0) { /* RWA this looked wrong */
            if (end_hour < 1) {
                end_hour = 24;
                pEndPCIdx--;
            }
        }

        if (!value_found) {
            /* The ending PC value could not be found. */
            return total;
        }

        /* Both the starting and the ending PC values have been found. */
        /* Compute the number of seconds in the interval. */
        pc_start_timet = hourlyPCList.get(pStartPCIdx).getId().getObsdate();
        cal.setTime(pc_start_timet);
        cal.add(Calendar.HOUR_OF_DAY, start_hour);
        pc_start_timet = cal.getTime();

        pc_end_timet = hourlyPCList.get(pEndPCIdx).getId().getObsdate();
        cal.setTime(pc_end_timet);
        cal.add(Calendar.HOUR_OF_DAY, end_hour);
        pc_end_timet = cal.getTime();

        seconds_covered[0] = (int) ((pc_end_timet.getTime() - pc_start_timet
                .getTime()) / MILLIS_PER_SECOND);

        /* The starting and ending values have been found. */
        total = end_value - start_value;
        total /= 100;

        return total;
    }

    /**
     * get_total_hourly_PP
     * 
     * @param hourlyPPList
     * @param pHourlyPPIdx
     * @param endingTime
     * @param num_hours
     * @param num_pp_records
     * @param seconds_covered
     * @param pp_qc
     * @param reported_missing
     * @return
     */
    public float get_total_hourly_PP(List<Hourlypp> hourlyPPList,
            int pHourlyPPIdx, Date endingTime, int num_hours,
            int num_pp_records, int[] seconds_covered, char[] pp_qc,
            boolean[] reported_missing) {
        /* Concept of an exact match does not apply here. BAL August 23, 2004. */
        /*
         * Ending time must be top of the hour. If it is not, then it is forced
         * to be top of the hour.
         */
        /*
         * Requires a list of PP data ordered by lid ascending, TS ascending,
         * duration descending, and obstime descending (only one TS).
         */

        char shef_qc_char;
        float total = MISSING_PRECIP;
        int end_hour_index;
        int hour;
        int hour_index;
        int i;
        int j;
        int num_minutes;
        int pp_count = 0;
        int start_hour_index;
        Short value;

        short[] pMinutes = null;
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Date date_timet;
        Date pp_date_timet;
        Date ending_time = new Date(endingTime.getTime());
        Date starting_time;

        /* Initialize the reported missing flag to 0. */
        reported_missing[0] = false;

        /* Initialize the seconds covered to 0. */
        seconds_covered[0] = 0;

        /* Check to make sure that there is data to process. */
        if (pHourlyPPIdx >= hourlyPPList.size()) {
            return MISSING_PRECIP;
        }

        /* Check to make sure that the ending time is top of hour. */
        /*
         * Create an array which has a number of elements equal to the number of
         * minutes between the starting and the ending times.
         */
        num_minutes = num_hours * MINUTES_PER_HOUR;

        pMinutes = new short[num_minutes + 1];

        if (pMinutes == null) {
            /* Return a missing precipitation value. */
            return MISSING_PRECIP;
        }

        /*
         * Check the ending time. Is it exactly on the top of the hour?
         */
        cal.setTime(ending_time);

        if (cal.get(Calendar.MINUTE) > 30) {
            cal.add(Calendar.MINUTE,
                    MINUTES_PER_HOUR - cal.get(Calendar.MINUTE));
            ending_time = cal.getTime();
        }

        /* Compute the starting time. */
        cal.add(Calendar.MINUTE, -num_minutes);
        starting_time = cal.getTime();

        /*
         * Determine the starting hour and ending hour indices in the the
         * minutes array.
         */
        start_hour_index = MINUTES_PER_HOUR;
        end_hour_index = num_minutes + 1;

        for (i = start_hour_index; (i <= end_hour_index)
                && (pp_count < num_pp_records); i += MINUTES_PER_HOUR) {
            /*
             * Compute the time in ticks of the hour being examined.
             */
            cal.setTime(starting_time);
            cal.add(Calendar.MINUTE, i);

            /*
             * Convert this time into a Year to Day time_t value.
             */
            hour = cal.get(Calendar.HOUR_OF_DAY);

            if (hour == 0) {
                hour = 24;
                cal.add(Calendar.HOUR, -1);
            }

            cal.set(Calendar.HOUR_OF_DAY, 0);
            cal.set(Calendar.MINUTE, 0);
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);

            date_timet = cal.getTime();

            while (pp_count < num_pp_records) {
                /*
                 * Check the corresponding slot in the HourlyPP table to for a
                 * PP value.
                 */
                Hourlypp pHourlyPP = hourlyPPList.get(pHourlyPPIdx);
                pp_date_timet = pHourlyPP.getId().getObsdate();

                if (date_timet.equals(pp_date_timet)) {
                    hour_index = hour - 1;
                    /* Retrieve the value from the appropriate hour slot. */
                    value = get_hour_slot_value(pHourlyPP, hour);
                    if (pHourlyPP.getHourlyQc().length() < 24) {
                        shef_qc_char = pHourlyPP.getHourlyQc().charAt(0);
                    } else {
                        shef_qc_char = pHourlyPP.getHourlyQc().charAt(
                                hour_index);
                    }

                    /* Check the value to determine if it is null. */
                    if ((value != null) && (shef_qc_char != 'B')
                            && (shef_qc_char != 'R')) {
                        pp_qc[0] = shef_qc_char;

                        if ((total == MISSING_PRECIP)
                                && ((value == MISSING_PRECIP) || ((value < 0) && (shef_qc_char == 'M')))) {
                            reported_missing[0] = true;
                        } else if (value >= 0) {

                            reported_missing[0] = false;

                            if (total == MISSING_PRECIP) {
                                total = value;
                            } else {
                                total += value;
                            }

                            /*
                             * Missing values DO NOT count towards the minutes
                             * array.
                             */
                            if (value >= 0) {
                                for (j = 0; j < MINUTES_PER_HOUR; ++j) {
                                    pMinutes[i - j] = 1;
                                }
                            }
                        }
                    }

                    break;

                } else if (pp_date_timet.before(date_timet)) {
                    /*
                     * The date of the HourlyPP record is earlier than the date
                     * of the hour being processed.
                     */
                    pHourlyPPIdx++;
                    ++pp_count;
                } else {
                    /*
                     * The date of the HourlyPP record is later than the date of
                     * the hour being processed. Get the next hour to process.
                     */
                    break;
                }
            }
        }

        /* Return the value and seconds covered to the user. */
        j = 0;

        for (i = 1; i <= num_minutes; ++i) {
            if (pMinutes[i] == 1) {
                j++;
            }
        }

        seconds_covered[0] = j * SECONDS_PER_MINUTE;

        if (total != MISSING_PRECIP) {
            total /= 100;
        }

        if (pMinutes != null) {
            pMinutes = null;
        }

        return total;
    }

    /**
     * get_ts_count_hourly
     * 
     * @param hourlyTSList
     * @param pHourlyTSIdx
     * @param pRecords
     * @return
     */
    public int get_ts_count_hourly(ArrayList<? extends IHourlyTS> hourlyTSList,
            int pHourlyTSIdx, ArrayList<Integer> pRecords) {
        String lid = null;
        String ts = null;

        int record_count = 0;
        int status;
        int ts_group_count = 0;

        if (pHourlyTSIdx < hourlyTSList.size()) {
            IHourlyTS pHourlyTS = hourlyTSList.get(pHourlyTSIdx);
            ++record_count;
            ++ts_group_count;

            lid = pHourlyTS.getLid();
            ts = pHourlyTS.getTs();

            pHourlyTSIdx++;
            while (pHourlyTSIdx < hourlyTSList.size()) {
                pHourlyTS = hourlyTSList.get(pHourlyTSIdx);

                status = lid.compareTo(pHourlyTS.getLid());

                if (status == 0) {
                    status = ts.compareTo(pHourlyTS.getTs());

                    if (status != 0) {
                        pRecords.add(record_count);
                        record_count = 1;
                        ts_group_count++;
                        ts = pHourlyTS.getTs();
                    } else {
                        ++record_count;
                    }

                    pHourlyTSIdx++;
                } else {
                    break;
                }
            }

            /* Add the last group being processed to the pRecords array. */
            pRecords.add(record_count);
        }

        return ts_group_count;
    }

    /**
     * buildWhereClause
     * 
     * @param query_begin_time
     * @param query_end_time
     * @param lid
     * @param ts
     * @return
     */
    private String buildWhereClause(Date query_begin_time, Date query_end_time,
            String lid, List<String> ts) {
        /*
         * Need special logic to account for accumulation intervals which start
         * at 00Z. This is because the 00Z PC value is actually placed in the 24
         * hour slot of the previous day.
         */

        Calendar pTm = null;
        pTm = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        pTm.setTime(query_begin_time);
        if (pTm.get(Calendar.HOUR_OF_DAY) == 0
                || pTm.get(Calendar.HOUR_OF_DAY) == 1) {
            pTm.add(Calendar.DAY_OF_MONTH, -1);
        }
        /* Need to convert the query begin and end times into dates. */
        String beginstr = sdf.format(pTm.getTime());

        pTm.setTime(query_end_time);
        if (pTm.get(Calendar.HOUR_OF_DAY) == 0) {
            pTm.add(Calendar.DAY_OF_MONTH, -1);
        }

        String endstr = sdf.format(pTm.getTime());

        /* consider according to whether type-source specified. */
        /* load data which is not missing value (-9999.0) */
        StringBuilder where = new StringBuilder("WHERE ");
        if (lid != null) {
            where.append("id.lid = '");
            where.append(lid);
            where.append("' AND ");
        }

        if ((ts != null) && (ts.size() > 0)) {
            where.append(build_ts_clause(ts));
            where.append(" AND ");
        }

        where.append("id.obsdate between '");
        where.append(beginstr);
        where.append("' AND '");
        where.append(endstr);
        where.append("' ORDER BY id.lid ASC, id.ts ASC, id.obsdate ASC");
        return where.toString();
    }

    /**
     * get_hour_slot_value
     * 
     * @param pHourlyPP
     * @param hour
     * @return
     */
    public Short get_hour_slot_value(IHourlyTS pHourlyPP, int hour) {
        Short precip_value = new Short((short) MISSING_PRECIP);

        /*
         * Depending on the hour, select the value in the correct hour slot in
         * the HourPC structure.
         */
        switch (hour) {
        case 1:

            precip_value = pHourlyPP.getHour1();
            break;

        case 2:

            precip_value = pHourlyPP.getHour2();
            break;

        case 3:

            precip_value = pHourlyPP.getHour3();
            break;

        case 4:

            precip_value = pHourlyPP.getHour4();
            break;

        case 5:

            precip_value = pHourlyPP.getHour5();
            break;

        case 6:

            precip_value = pHourlyPP.getHour6();
            break;

        case 7:

            precip_value = pHourlyPP.getHour7();
            break;

        case 8:

            precip_value = pHourlyPP.getHour8();
            break;

        case 9:

            precip_value = pHourlyPP.getHour9();
            break;

        case 10:

            precip_value = pHourlyPP.getHour10();
            break;

        case 11:

            precip_value = pHourlyPP.getHour11();
            break;

        case 12:

            precip_value = pHourlyPP.getHour12();
            break;

        case 13:

            precip_value = pHourlyPP.getHour13();
            break;

        case 14:

            precip_value = pHourlyPP.getHour14();
            break;

        case 15:

            precip_value = pHourlyPP.getHour15();
            break;

        case 16:

            precip_value = pHourlyPP.getHour16();
            break;

        case 17:

            precip_value = pHourlyPP.getHour17();
            break;

        case 18:

            precip_value = pHourlyPP.getHour18();
            break;

        case 19:

            precip_value = pHourlyPP.getHour19();
            break;

        case 20:

            precip_value = pHourlyPP.getHour20();
            break;

        case 21:

            precip_value = pHourlyPP.getHour21();
            break;

        case 22:

            precip_value = pHourlyPP.getHour22();
            break;

        case 23:

            precip_value = pHourlyPP.getHour23();
            break;

        case 24:
        case 0:
            precip_value = pHourlyPP.getHour24();
            break;

        default:
            break;
        }

        return precip_value;
    }

    /**
     * get_ingest_info
     * 
     * @param lid
     * @return
     */
    public List<Ingestfilter> get_ingest_info(String lid) {

        /*
         * Construct the where clause used to retrieve rows from the
         * IngestFilter table.
         */
        String where_clause = "WHERE lid = '" + lid
                + "' AND ingest = 'T' ORDER by ts ASC";

        List<Ingestfilter> pIngestHead = null;
        pIngestHead = IHFSDbGenerated.GetIngestFilter(where_clause);

        return pIngestHead;
    }

    /**
     * load_PC_hourly
     * 
     * @param query_begin_time
     * @param query_end_time
     * @param lid
     * @param ts
     * @return
     */
    public ArrayList<Hourlypc> load_PC_hourly(Date query_begin_time,
            Date query_end_time, String lid, List<String> ts) {

        ArrayList<Hourlypc> pHourlyPC = null;

        String where = buildWhereClause(query_begin_time, query_end_time, lid,
                ts);

        /* get the data */
        pHourlyPC = IHFSDbGenerated.GetHourlyPC(where);
        System.out.println("SELECT * FROM HourlyPC " + where);
        System.out.println(pHourlyPC.size()
                + " records retrieved from HourlyPC. ");
        return pHourlyPC;
    }

    /**
     * load_PP_hourly
     * 
     * @param query_begin_time
     * @param query_end_time
     * @param lid
     * @param ts
     * @return
     */
    public ArrayList<Hourlypp> load_PP_hourly(Date query_begin_time,
            Date query_end_time, String lid, List<String> ts) {

        ArrayList<Hourlypp> pHourlyPP = null;

        String where = buildWhereClause(query_begin_time, query_end_time, lid,
                ts);

        /* get the data */
        pHourlyPP = IHFSDbGenerated.GetHourlyPP(where);

        return pHourlyPP;
    }
}
