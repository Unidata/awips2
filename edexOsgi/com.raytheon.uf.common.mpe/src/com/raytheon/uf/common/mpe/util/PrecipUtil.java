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
package com.raytheon.uf.common.mpe.util;

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
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.data.PrecipModes;
import com.raytheon.uf.common.hydro.data.PrecipTotal;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;

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
 * 11/16/2015   5100      bkowal       Generated a better query to handle the case when
 *                                     the requested data spans two days.
 * Jun 08, 2016 5571      njensen      Moved precip mode enums to PrecipModes
 * Jul 22, 2016 4623      skorolev     Relocated to common. Cleanup.
 * 
 * </pre>
 * 
 * @author grichard
 */

public final class PrecipUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PrecipUtil.class);

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

    private int previousSettings = 0;

    private PrecipModes.PrecipPEmode peMode = PrecipModes.PrecipPEmode.PrecipPEbest;

    private PrecipModes.PrecipTSmode tsMode = PrecipModes.PrecipTSmode.PrecipTSsingle;

    private boolean reportMissMinPercent = false;

    private static final SimpleDateFormat sdf;

    static {
        sdf = new SimpleDateFormat("yyyy-MM-dd");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    private List<Ingestfilter> ingestList = null;

    /**
     * Private constructor: Use getInstance().
     */
    private PrecipUtil() {
    }

    /**
     * Gets Total HourlyPrecip
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
     * @return
     */
    public PrecipTotal getTotalHourlyPrecip(List<Hourlypc> hourlyPCList,
            int[] pHourlyPCIdx, List<Hourlypp> hourlyPPList,
            int[] pHourlyPPIdx, Date ending_time, int num_hours,
            float min_percent, int settings, boolean advance, int[] pc_records,
            int[] pp_records) {

        /*
         * Make local copies of the pHourlyPCIdx, and pHourlyPPIdx pointers. The
         * originals will only be modified if the user has specified the advance
         * pointer option.
         */
        int pHourlyPCLoc = pHourlyPCIdx[0];
        int pHourlyPPLoc = pHourlyPPIdx[0];

        // Initialize the totalPrecip structure.
        PrecipTotal totalPrecip = new PrecipTotal();

        // Check the PC, PP, and PPother pointers for availability of data.
        if ((hourlyPCList == null || hourlyPCList.isEmpty())
                && (hourlyPPList == null || hourlyPPList.isEmpty())) {
            // Total precip stuct initialized to missing.
            return totalPrecip;
        }

        /*
         * Check the current settings against the settings used when this
         * routine was last called. If they are the same, then do not process
         * the settings.
         */

        if (settings != previousSettings) {
            previousSettings = settings;

            /*
             * Check the settings flag to determine how to process these
             * precipitation reports.
             */
            reportMissMinPercent = (settings & CommonHydroConstants.REPORT_MISSING_BELOW_MIN_PERCENT) != 0;
            if ((settings & CommonHydroConstants.PRECIP_PE_BEST) == CommonHydroConstants.PRECIP_PE_BEST) {
                peMode = PrecipModes.PrecipPEmode.PrecipPEbest;
            } else if ((settings & CommonHydroConstants.PRECIP_PP) == CommonHydroConstants.PRECIP_PP) {
                peMode = PrecipModes.PrecipPEmode.PrecipPEPP;
            } else if ((settings & CommonHydroConstants.PRECIP_PC) == CommonHydroConstants.PRECIP_PC) {
                peMode = PrecipModes.PrecipPEmode.PrecipPEPC;
            }

            if ((settings & CommonHydroConstants.PRECIP_TS_BEST) == CommonHydroConstants.PRECIP_TS_BEST) {
                tsMode = PrecipModes.PrecipTSmode.PrecipTSbest;
            } else if ((settings & CommonHydroConstants.PRECIP_TS_RANK) == CommonHydroConstants.PRECIP_TS_RANK) {
                tsMode = PrecipModes.PrecipTSmode.PrecipTSrank;
            } else {
                tsMode = PrecipModes.PrecipTSmode.PrecipTSsingle;
            }
        }

        /*
         * Check the station id being pointed to by the HourlyPP and the
         * HourlyPC pointers. If these stations are not the same, then only
         * process the station which is lexically smaller.
         */
        int num_ts_pc = 0;
        int num_ts_pp = 0;
        List<Integer> pTsPc = new ArrayList<>();
        List<Integer> pTsPp = new ArrayList<>();
        Hourlypc pHourlyPC = null;
        if (hourlyPCList!=null && pHourlyPCLoc < hourlyPCList.size()) {
            pHourlyPC = hourlyPCList.get(pHourlyPCLoc);
        }

        Hourlypp pHourlyPP = null;
        if (hourlyPPList!=null && pHourlyPPLoc < hourlyPPList.size()) {
            pHourlyPP = hourlyPPList.get(pHourlyPPLoc);
        }

        if ((pHourlyPC != null) && (pHourlyPP != null)) {
            int status = pHourlyPC.getLid().compareTo(pHourlyPP.getLid());

            if (status == 0) {
                /* The stations are the same. */
                num_ts_pc = getTsCountHourly(hourlyPCList, pHourlyPCLoc, pTsPc);
                num_ts_pp = getTsCountHourly(hourlyPPList, pHourlyPPLoc, pTsPp);
                totalPrecip.lid = pHourlyPC.getLid();
            } else if (status < 0) {
                /*
                 * The PC lid is the smallest. Use it. Save the PP lid for later
                 * since there may be a matching PC lid further down the list.
                 */
                num_ts_pc = getTsCountHourly(hourlyPCList, pHourlyPCLoc, pTsPc);
                totalPrecip.lid = pHourlyPC.getLid();
            } else {
                /*
                 * The PP lid is the smallest. Use it. Save the PC lid for later
                 * since there may be a matching PP lid further down the list.
                 */
                num_ts_pp = getTsCountHourly(hourlyPPList, pHourlyPPLoc, pTsPp);
                totalPrecip.lid = pHourlyPP.getLid();
            }
        } else if (pHourlyPC == null) {
            /* There are no PC data. */
            num_ts_pp = getTsCountHourly(hourlyPPList, pHourlyPPLoc, pTsPp);
            totalPrecip.lid = pHourlyPP.getLid();
        } else {
            /* There are no PP data. */
            num_ts_pc = getTsCountHourly(hourlyPCList, pHourlyPCLoc, pTsPc);
            totalPrecip.lid = pHourlyPC.getLid();
        }

        /*
         * Retrieve the precip totals. If necessary process multiple TSs,
         * choosing either the TS which produces the best precip coverage or the
         * TS which Has the highest rank.
         */

        int num_pc_records = 0;
        int num_pp_records = 0;
        float best_pc_amount = CommonHydroConstants.MISSING_PRECIP;
        float best_pp_amount = CommonHydroConstants.MISSING_PRECIP;
        float pc_precip_amount;
        float pp_precip_amount;
        String best_pc_ts = null;
        String best_pp_ts = null;
        int best_covered = (int) CommonHydroConstants.MISSING_PRECIP;
        int[] best_pc_coverage = new int[] { (int) CommonHydroConstants.MISSING_PRECIP };
        int[] best_pp_coverage = new int[] { (int) CommonHydroConstants.MISSING_PRECIP };
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

        switch (tsMode) {
        /*
         * This is the case where the user wants to retrieve the precip total
         * for a single typesource.
         */
        case PrecipTSsingle:

            if (num_ts_pc > 0) {
                num_pc_records = pTsPc.get(0);
                best_pc_ts = pHourlyPC.getTs();
                best_pc_amount = getTotalHourlyPC(hourlyPCList, pHourlyPCLoc,
                        ending_time, num_hours, num_pc_records,
                        best_pc_coverage, best_pc_qc);

            }

            if (num_ts_pp > 0) {
                num_pp_records = pTsPp.get(0);
                best_pp_ts = pHourlyPP.getTs();
                best_pp_amount = getTotalHourlyPP(hourlyPPList, pHourlyPPLoc,
                        ending_time, num_hours, num_pp_records,
                        best_pp_coverage, best_pp_qc, best_reported_missing);
            }

            break;

        case PrecipTSbest:

            /*
             * This is the case where the user wants to retrieve the
             * precipitation total for the typesource which results in best
             * precipitation total (most coverage) for the user-specified
             * interval.
             */
            for (int i = 0; i < num_ts_pc; ++i) {
                num_pc_records = pTsPc.get(i);
                pc_precip_amount = getTotalHourlyPC(hourlyPCList, pHourlyPCLoc,
                        ending_time, num_hours, num_pc_records,
                        pc_seconds_covered, temp_pc_qc);

                if (pc_seconds_covered[0] > best_pc_coverage[0]) {
                    best_pc_coverage[0] = pc_seconds_covered[0];
                    best_pc_amount = pc_precip_amount;
                    best_pc_ts = pHourlyPC.getTs();
                    best_pc_qc[0] = temp_pc_qc[0];
                }

                pHourlyPC = hourlyPCList.get(++pHourlyPCLoc);
            }

            /*
             * When totaling the precipitation amount from the HourlyPPtable,
             * make sure that the TSs match.
             */
            for (int i = 0; i < num_ts_pp; ++i) {
                num_pp_records = pTsPp.get(i);
                pp_precip_amount = getTotalHourlyPP(hourlyPPList, pHourlyPPLoc,
                        ending_time, num_hours, num_pp_records,
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

            /*
             * This case retrieves the precipitation total for the highest
             * ranking typesource.
             */
            if (num_ts_pc > 0) {
                lid = pHourlyPC.getLid();
            } else {
                lid = pHourlyPP.getLid();
            }

            /*
             * Only perform TS ranking if for the given PE there are multiple
             * typesources.
             */
            pBestHourlyPCIdx = pHourlyPCLoc;
            ts_index[0] = 0;

            List<Ingestfilter> pIngestNode = null;
            pIngestNode = getIngestInfo(lid);

            if (num_ts_pc > 1 && pIngestNode != null) {
                pBestHourlyPCIdx = getHighestRankingHourlyTs(hourlyPCList,
                        pHourlyPCLoc, "PP", num_ts_pc, ts_index, pTsPc,
                        pIngestNode);
            }

            // Get the total precipitation for the highest ranked ts.
            if (num_ts_pc > 0) {
                best_pc_amount = getTotalHourlyPC(hourlyPCList,
                        pBestHourlyPCIdx, ending_time, num_hours,
                        pTsPc.get(ts_index[0]), pc_seconds_covered, temp_pc_qc);
                best_pc_coverage[0] = pc_seconds_covered[0];
                best_pc_ts = hourlyPCList.get(pBestHourlyPCIdx).getTs();
                best_pc_qc = temp_pc_qc;
            }

            pBestHourlyPPIdx = pHourlyPPLoc;
            ts_index[0] = 0;

            if (num_ts_pp > 1 && pIngestNode != null) {
                pBestHourlyPPIdx = getHighestRankingHourlyTs(hourlyPPList,
                        pHourlyPPLoc, "PP", num_ts_pp, ts_index, pTsPp,
                        pIngestNode);
            }

            if (num_ts_pp > 0) {
                best_pp_amount = getTotalHourlyPP(hourlyPPList,
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

            /*
             * This case should not be reached. We should do something here
             * anyway.
             */
            break;
        }

        if ((num_ts_pc > 0) && (num_ts_pp > 0)) {
            switch (peMode) {
            case PrecipPEbest:

                if ((best_pc_amount != CommonHydroConstants.MISSING_PRECIP)
                        || (best_pp_amount != CommonHydroConstants.MISSING_PRECIP)) {

                    /*
                     * Select the PE or PC estimate which provides the best
                     * precipitation amount estimate.
                     */
                    if (best_pc_coverage[0] > best_pp_coverage[0]) {
                        totalPrecip.value = best_pc_amount;
                        totalPrecip.setPe("PC");
                        totalPrecip.setTs(best_pc_ts);
                        best_covered = best_pc_coverage[0];
                        totalPrecip.qc = best_pc_qc[0];
                        totalPrecip.setSummedFlag(false);
                    } else {
                        totalPrecip.value = best_pp_amount;
                        totalPrecip.setPe("PP");
                        totalPrecip.setTs(best_pp_ts);
                        best_covered = best_pp_coverage[0];
                        totalPrecip.qc = best_pp_qc[0];
                        totalPrecip.setSummedFlag(num_hours > 1);
                        totalPrecip
                                .setReportedMissing(best_reported_missing[0]);
                    }
                }

                break;

            case PrecipPEPP:

                /* If there are PC and PP values, then use the PP value. */
                if ((num_ts_pp > 0)
                        && ((best_pp_amount != CommonHydroConstants.MISSING_PRECIP)
                                || (best_pp_qc[0] == 'M') || ((best_reported_missing[0]) && (best_pc_amount == CommonHydroConstants.MISSING_PRECIP)))
                        && (((best_pp_qc[0] != 'L') && (best_pp_qc[0] != 'C'))
                                || (best_pp_amount == best_pc_amount) || (best_pc_amount == CommonHydroConstants.MISSING_PRECIP)))

                {
                    totalPrecip.value = best_pp_amount;
                    totalPrecip.setPe("PP");
                    totalPrecip.setTs(best_pp_ts);
                    best_covered = best_pp_coverage[0];
                    totalPrecip.qc = best_pp_qc[0];
                    totalPrecip.setSummedFlag(num_hours > 1);
                    totalPrecip.setReportedMissing(best_reported_missing[0]);
                } else {
                    totalPrecip.value = best_pc_amount;
                    totalPrecip.setPe("PC");
                    totalPrecip.setTs(best_pc_ts);
                    best_covered = best_pc_coverage[0];
                    totalPrecip.qc = best_pc_qc[0];
                    totalPrecip.setSummedFlag(false);
                }

                break;

            case PrecipPEPC:

                /*
                 * If there are totals from PC and PP, then use the PC value.
                 */
                if ((num_ts_pc > 0)
                        && ((best_pc_amount != CommonHydroConstants.MISSING_PRECIP) || (best_pc_qc[0] == 'M'))) {
                    totalPrecip.value = best_pc_amount;
                    totalPrecip.setPe("PC");
                    totalPrecip.setTs(best_pc_ts);
                    best_covered = best_pc_coverage[0];
                    totalPrecip.qc = best_pc_qc[0];
                    totalPrecip.setSummedFlag(false);
                } else {
                    totalPrecip.value = best_pp_amount;
                    totalPrecip.setPe("PP");
                    totalPrecip.setTs(best_pp_ts);
                    best_covered = best_pp_coverage[0];
                    totalPrecip.qc = best_pp_qc[0];
                    totalPrecip.setSummedFlag(num_hours > 1);
                    totalPrecip.setReportedMissing(best_reported_missing[0]);
                }

                break;

            default:

                /* This case should not be reached. */
                break;
            }
        } else if (num_ts_pc > 0) {
            totalPrecip.value = best_pc_amount;
            totalPrecip.setPe("PC");
            totalPrecip.setTs(best_pc_ts);
            best_covered = best_pc_coverage[0];
            totalPrecip.qc = best_pc_qc[0];
            totalPrecip.setSummedFlag(false);
        } else {
            totalPrecip.value = best_pp_amount;
            totalPrecip.setPe("PP");
            totalPrecip.setTs(best_pp_ts);
            best_covered = best_pp_coverage[0];
            totalPrecip.qc = best_pp_qc[0];
            totalPrecip.setSummedFlag(num_hours > 1);
            totalPrecip.setReportedMissing(best_reported_missing[0]);
        }

        if (totalPrecip.value != CommonHydroConstants.MISSING_PRECIP) {
            totalPrecip.setHoursCovered((float) best_covered
                    / TimeUtil.SECONDS_PER_HOUR);
            totalPrecip.setPercentFilled((float) best_covered
                    / (num_hours * TimeUtil.SECONDS_PER_HOUR));

            /* Do not allow a percentage filled of greater than 100%. */
            if (totalPrecip.getPercentFilled() > 1.0f) {
                totalPrecip.setPercentFilled(1.0f);
            }

            totalPrecip.setValueIndicator(CommonHydroConstants.OK_CHAR);

            /* Set the QC and error flags. */
            if (reportMissMinPercent) {
                if (totalPrecip.getPercentFilled() < min_percent) {
                    totalPrecip.value = CommonHydroConstants.MISSING_PRECIP;
                    totalPrecip
                            .setValueIndicator(CommonHydroConstants.REJECTED_CHAR);
                }
            }

            if ((totalPrecip.value < 0)
                    && (totalPrecip.value != CommonHydroConstants.MISSING_PRECIP)) {
                totalPrecip.value = CommonHydroConstants.MISSING_PRECIP;
                totalPrecip.err.negdiff = true;
                totalPrecip
                        .setValueIndicator(CommonHydroConstants.MISSING_CHAR);
            }

        } else {
            totalPrecip.err.negval = true;
        }

        /*
         * If the user has requested it, advance the pointer. This enables
         * colooping, a feature which can save alot of CPU time.
         */
        if (advance) {
            switch (tsMode) {
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

        return totalPrecip;
    }

    /**
     * Builds TS Clause
     * 
     * @param ts
     * @param tsField
     * @return
     */
    public String buildTsClause(List<String> ts, String tsField) {
        if (ts == null || ts.isEmpty() || tsField == null || tsField.isEmpty()) {
            return "";
        }
        StringBuilder tsClause = new StringBuilder(tsField.trim() + " ");

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
     * Compares TS Rank
     * 
     * @param ts1
     * @param ts2
     * @param pIngestHead
     * @param pe
     * @param dur
     * @param extremum
     * @return
     */
    public int compareTsRank(String ts1, String ts2,
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
     * Gets Highest Ranking HourlyTs
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
    public int getHighestRankingHourlyTs(List<? extends IHourlyTS> hourlyData,
            int pHourlyIdx, String pe, int num_ts, int[] pTsIndex,
            List<Integer> pTs, List<Ingestfilter> pIngestHead) {

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
                    status = compareTsRank(pHourlyTS.getTs(),
                            pBestHourlyData.getTs(), pIngestHead, pe, 1001, 'Z');
                } else {
                    status = compareTsRank(pHourlyTS.getTs(),
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
     * Gets Total HourlyPC
     * 
     * @param hourlyPCList
     * @param pHourlyPCIdx
     * @param endingTime
     * @param numHours
     * @param numPcRecords
     * @param secondsCovered
     * @param pcQC
     * @return
     */
    public float getTotalHourlyPC(List<Hourlypc> hourlyPCList,
            int pHourlyPCIdx, Date endingTime, int numHours, int numPcRecords,
            int[] secondsCovered, char[] pcQC) {
        char shefQcChar;
        float total = CommonHydroConstants.MISSING_PRECIP;
        int pEndPCIdx = -1;
        int pEndPrevIdx = -1;
        int pStartPCIdx = -1;
        int diff;
        int endHour;
        boolean found;
        int hourIndex;
        int numDays;
        int recordCount;
        int startHour;
        boolean valueFound;
        Short endValue;
        Short startValue;
        Calendar cal = TimeUtil.newGmtCalendar();
        Date endDate;
        Date pcEndTimet;
        Date pcStartTimet;
        Date pcTimet = new Date(0);
        Date startDate;

        pcQC[0] = 'Z';

        /* Initialize the number of seconds covered to 0. */
        secondsCovered[0] = 0;

        if (pHourlyPCIdx >= hourlyPCList.size()) {
            /* No PC data was passed into this routine. */
            return CommonHydroConstants.MISSING_PRECIP;
        }

        /*
         * Determine the end date (YYYY-MM-DD) and hour of the accumulation
         * interval.
         */
        cal.setTime(endingTime);
        endHour = cal.get(Calendar.HOUR_OF_DAY);

        /*
         * Check to make sure the end date is top of the hour. If not, then
         * adjust the date according to the number of minutes.
         */
        if (cal.get(Calendar.MINUTE) > 30) {
            ++endHour;
        }

        /*
         * Determine if the end hour is 0. If it is, then set it to 24 and
         * decrease the date by one day.
         */
        if (endHour == 0) {
            endHour = 24;
            cal.add(Calendar.DAY_OF_MONTH, -1);
        }

        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        endDate = cal.getTime();

        /*
         * Determine the start date (YYYY-MM-DD) of the accumulation interval.
         */
        startDate = cal.getTime();

        diff = endHour - numHours;
        startHour = diff % 24;

        if (startHour <= 0) {
            startHour += 24;
        }

        if (diff <= 0) {
            numDays = diff / 24 + 1;
            cal.add(Calendar.DAY_OF_MONTH, -numDays);
            startDate = cal.getTime();
        }

        /*
         * Load the record from the HourlyPC table corresponding to the
         * beginning date of the interval. Since we are dealing with the Hourly
         * tables, a value for the exact start date and hour must be available.
         */
        pStartPCIdx = pHourlyPCIdx;
        found = false;
        recordCount = 0;

        for (int i = 0; i < numPcRecords; ++i) {
            recordCount++;
            pcTimet = hourlyPCList.get(pStartPCIdx).getId().getObsdate();

            if (!pcTimet.before(startDate)) {
                found = true;
                break;
            }

            pStartPCIdx++;
        }

        if (!found) {
            /* No start date could be retrieved. */
            return CommonHydroConstants.MISSING_PRECIP;
        }

        if (pcTimet.after(startDate) && endHour != 1) {
            /*
             * An exact match for the start date could not be found. Set the
             * starting hour to 1.
             */
            startHour = 1;
        }

        pEndPCIdx = pStartPCIdx;

        if (pcTimet.after(endDate)) {
            /*
             * The starting time is after the ending time. Cannot compute a
             * PC-based precipitation total.
             */
            return CommonHydroConstants.MISSING_PRECIP;
        }

        if (!pcTimet.equals(endDate)) {
            pEndPrevIdx = pEndPCIdx;

            for (int i = recordCount; i < numPcRecords; ++i) {
                pEndPCIdx++;
                ++recordCount;
                pcTimet = hourlyPCList.get(pEndPCIdx).getId().getObsdate();

                if (endDate.equals(pcTimet)) {
                    break;
                } else if (pcTimet.after(endDate)) {
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
        if ((pEndPCIdx == -1) || pcTimet.before(endDate)) {
            endHour = 24;
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
        valueFound = false;
        startValue = (short) CommonHydroConstants.MISSING_PRECIP;

        while ((pStartPCIdx < hourlyPCList.size())
                && ((pStartPCIdx != pEndPCIdx) || (startHour < endHour) || (startHour == 24 && endHour == 1))) {
            Hourlypc pStartPC = hourlyPCList.get(pStartPCIdx);
            startValue = getHourSlotValue(pStartPC, startHour);

            hourIndex = startHour - 1;
            shefQcChar = pStartPC.getHourlyQc().charAt(hourIndex);

            if ((startValue != null)
                    && (startValue != CommonHydroConstants.MISSING_PRECIP)
                    && (shefQcChar != 'B') && (shefQcChar != 'R')) {
                /* The starting PC value has been found. */
                valueFound = true;
                break;
            }

            ++startHour;

            if (startHour > 24) {
                startHour = 1;
                pStartPCIdx++;
            }
        }

        if (!valueFound) {
            /* The starting value could not be found. */
            return CommonHydroConstants.MISSING_PRECIP;
        }

        /*
         * Find the hour slot in the end date which is closest to the starting
         * time.
         */
        valueFound = false;
        endValue = (short) CommonHydroConstants.MISSING_PRECIP;

        while ((pEndPCIdx > pStartPCIdx)
                || ((pEndPCIdx == pStartPCIdx) && (endHour > startHour) || (startHour == 24 && endHour == 1))) {
            Hourlypc pEndPC = hourlyPCList.get(pEndPCIdx);
            hourIndex = endHour - 1;
            endValue = getHourSlotValue(pEndPC, endHour);

            shefQcChar = pEndPC.getHourlyQc().charAt(hourIndex);

            if ((endValue != null)
                    && (endValue != CommonHydroConstants.MISSING_PRECIP)
                    && (shefQcChar != 'B') && (shefQcChar != 'R')) {
                valueFound = true;

                /* Set the qc value to the end hour qc. */
                pcQC[0] = shefQcChar;
                break;
            }

            --endHour;

            // if (end_hour < 0) { /* RWA this looked wrong */
            if (endHour < 1) {
                endHour = 24;
                pEndPCIdx--;
            }
        }

        if (!valueFound) {
            /* The ending PC value could not be found. */
            return total;
        }

        /* Both the starting and the ending PC values have been found. */
        /* Compute the number of seconds in the interval. */
        pcStartTimet = hourlyPCList.get(pStartPCIdx).getId().getObsdate();
        cal.setTime(pcStartTimet);
        cal.add(Calendar.HOUR_OF_DAY, startHour);
        pcStartTimet = cal.getTime();

        pcEndTimet = hourlyPCList.get(pEndPCIdx).getId().getObsdate();
        cal.setTime(pcEndTimet);
        cal.add(Calendar.HOUR_OF_DAY, endHour);
        pcEndTimet = cal.getTime();

        secondsCovered[0] = (int) ((pcEndTimet.getTime() - pcStartTimet
                .getTime()) / TimeUtil.MILLIS_PER_SECOND);

        /* The starting and ending values have been found. */
        total = endValue - startValue;
        total /= 100;

        return total;
    }

    /**
     * Gets Total HourlyPP
     * 
     * @param hourlyPPList
     * @param pHourlyPPIdx
     * @param endingTime
     * @param numHours
     * @param numPpRecords
     * @param secondsCovered
     * @param ppQC
     * @param reportedMissing
     * @return
     */
    public float getTotalHourlyPP(List<Hourlypp> hourlyPPList,
            int pHourlyPPIdx, Date endingTime, int numHours, int numPpRecords,
            int[] secondsCovered, char[] ppQC, boolean[] reportedMissing) {
        /*
         * Concept of an exact match does not apply here. BAL August 23, 2004.
         * 
         * Ending time must be top of the hour. If it is not, then it is forced
         * to be top of the hour.
         * 
         * Requires a list of PP data ordered by lid ascending, TS ascending,
         * duration descending, and obstime descending (only one TS).
         */

        char shefQcChar;
        float total = CommonHydroConstants.MISSING_PRECIP;
        int endHourIndex;
        int hour;
        int hourIndex;
        int i;
        int j;
        int numMinutes;
        int ppCount = 0;
        int startHourIndex;
        Short value;

        short[] pMinutes = null;
        Calendar cal = TimeUtil.newGmtCalendar();
        Date dateTimet;
        Date ppDateTimet;
        Date timeEnding = new Date(endingTime.getTime());
        Date timeStarting;

        /* Initialize the reported missing flag to 0. */
        reportedMissing[0] = false;

        /* Initialize the seconds covered to 0. */
        secondsCovered[0] = 0;

        /* Check to make sure that there is data to process. */
        if (pHourlyPPIdx >= hourlyPPList.size()) {
            return CommonHydroConstants.MISSING_PRECIP;
        }

        /* Check to make sure that the ending time is top of hour. */
        /*
         * Create an array which has a number of elements equal to the number of
         * minutes between the starting and the ending times.
         */
        numMinutes = numHours * TimeUtil.MINUTES_PER_HOUR;

        pMinutes = new short[numMinutes + 1];

        if (pMinutes == null) {
            /* Return a missing precipitation value. */
            return CommonHydroConstants.MISSING_PRECIP;
        }

        /*
         * Check the ending time. Is it exactly on the top of the hour?
         */
        cal.setTime(timeEnding);

        if (cal.get(Calendar.MINUTE) > 30) {
            cal.add(Calendar.MINUTE,
                    TimeUtil.MINUTES_PER_HOUR - cal.get(Calendar.MINUTE));
            timeEnding = cal.getTime();
        }

        /* Compute the starting time. */
        cal.add(Calendar.MINUTE, -numMinutes);
        timeStarting = cal.getTime();

        /*
         * Determine the starting hour and ending hour indices in the the
         * minutes array.
         */
        startHourIndex = TimeUtil.MINUTES_PER_HOUR;
        endHourIndex = numMinutes + 1;

        for (i = startHourIndex; (i <= endHourIndex)
                && (ppCount < numPpRecords); i += TimeUtil.MINUTES_PER_HOUR) {
            /*
             * Compute the time in ticks of the hour being examined.
             */
            cal.setTime(timeStarting);
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

            dateTimet = cal.getTime();

            while (ppCount < numPpRecords) {
                /*
                 * Check the corresponding slot in the HourlyPP table to for a
                 * PP value.
                 */
                Hourlypp pHourlyPP = hourlyPPList.get(pHourlyPPIdx);
                ppDateTimet = pHourlyPP.getId().getObsdate();

                if (dateTimet.equals(ppDateTimet)) {
                    hourIndex = hour - 1;
                    /* Retrieve the value from the appropriate hour slot. */
                    value = getHourSlotValue(pHourlyPP, hour);
                    if (pHourlyPP.getHourlyQc().length() < 24) {
                        shefQcChar = pHourlyPP.getHourlyQc().charAt(0);
                    } else {
                        shefQcChar = pHourlyPP.getHourlyQc().charAt(hourIndex);
                    }

                    /* Check the value to determine if it is null. */
                    if ((value != null) && (shefQcChar != 'B')
                            && (shefQcChar != 'R')) {
                        ppQC[0] = shefQcChar;

                        if ((total == CommonHydroConstants.MISSING_PRECIP)
                                && ((value == CommonHydroConstants.MISSING_PRECIP) || ((value < 0) && (shefQcChar == 'M')))) {
                            reportedMissing[0] = true;
                        } else if (value >= 0) {

                            reportedMissing[0] = false;

                            if (total == CommonHydroConstants.MISSING_PRECIP) {
                                total = value;
                            } else {
                                total += value;
                            }

                            /*
                             * Missing values DO NOT count towards the minutes
                             * array.
                             */
                            if (value >= 0) {
                                for (j = 0; j < TimeUtil.MINUTES_PER_HOUR; ++j) {
                                    pMinutes[i - j] = 1;
                                }
                            }
                        }
                    }

                    break;

                } else if (ppDateTimet.before(dateTimet)) {
                    /*
                     * The date of the HourlyPP record is earlier than the date
                     * of the hour being processed.
                     */
                    pHourlyPPIdx++;
                    ++ppCount;
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

        for (i = 1; i <= numMinutes; ++i) {
            if (pMinutes[i] == 1) {
                j++;
            }
        }

        secondsCovered[0] = j * TimeUtil.SECONDS_PER_MINUTE;

        if (total != CommonHydroConstants.MISSING_PRECIP) {
            total /= 100;
        }

        if (pMinutes != null) {
            pMinutes = null;
        }

        return total;
    }

    /**
     * Gets TS Count Hourly
     * 
     * @param hourlyTSList
     * @param pHourlyTSIdx
     * @param pRecords
     * @return
     */
    public int getTsCountHourly(List<? extends IHourlyTS> hourlyTSList,
            int pHourlyTSIdx, List<Integer> pRecords) {
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
     * Builds HourlyHQL
     * 
     * @param query_begin_time
     * @param query_end_time
     * @param lid
     * @param ts
     * @param entityName
     * @param selectAdditional
     * @return
     */
    public String buildHourlyHQL(Date query_begin_time, Date query_end_time,
            String lid, List<String> ts, final String entityName,
            String selectAdditional) {

        final String orderBy = " ORDER BY b.id.lid ASC, b.id.ts ASC, b.id.obsdate ASC";

        StringBuilder fromList = new StringBuilder(
                " b.id.lid, b.id.ts, b.id.obsdate, %s, %s, b.hour1, ");
        fromList.append("b.hour2, b.hour3, b.hour4, b.hour5, b.hour6, b.hour7, b.hour8, b.hour9, b.hour10, ");
        fromList.append("b.hour11, b.hour12, b.hour13, b.hour14, b.hour15, b.hour16, b.hour17, b.hour18, ");
        fromList.append("b.hour19, b.hour20, b.hour21, b.hour22, b.hour23, ");
        if (selectAdditional != null
                && selectAdditional.trim().startsWith(", ") == false) {
            selectAdditional = ", " + selectAdditional;
        }

        Calendar pTm = null;
        pTm = TimeUtil.newGmtCalendar(query_begin_time);
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

        String where = null;
        String minuteOffsetStr = null;
        String hourlyQCStr = null;
        if (endstr.equals(beginstr)) {
            fromList.append("b.hour24 ");
            if (selectAdditional != null) {
                fromList.append(selectAdditional);
            }
            fromList.append(" FROM ").append(entityName).append(" b ");
            where = " b.id.obsdate = '" + beginstr + "'";
            minuteOffsetStr = "b.minuteOffset";
            hourlyQCStr = "b.hourlyQc";
        } else {
            fromList.append("a.hour24 ");
            if (selectAdditional != null) {
                fromList.append(selectAdditional);
            }
            fromList.append(" FROM ").append(entityName).append(" a, ")
                    .append(entityName).append(" b ");
            where = " a.id.lid = b.id.lid AND a.id.ts = b.id.ts AND a.id.obsdate = '"
                    + beginstr + "' AND b.id.obsdate = '" + endstr + "'";
            minuteOffsetStr = "substring(b.minuteOffset, 1, 23) || substring(a.minuteOffset, 24, 24)";
            hourlyQCStr = "substring(b.hourlyQc, 1, 23) || substring(a.hourlyQc, 24, 24)";
        }

        StringBuilder whereStr = new StringBuilder(where);
        if (lid != null) {
            whereStr.append(" AND ");
            whereStr.append("id.lid = '");
            whereStr.append(lid);
        }

        if ((ts != null) && (ts.size() > 0)) {
            whereStr.append(" AND ");
            whereStr.append(buildTsClause(ts, "b.id.ts"));
        }

        return new StringBuilder("SELECT")
                .append(String.format(fromList.toString(), minuteOffsetStr,
                        hourlyQCStr)).append("WHERE")
                .append(whereStr.toString()).append(orderBy).toString();
    }

    /**
     * Gets HourSlotValue
     * 
     * @param pHourlyPP
     * @param hour
     * @return
     */
    public Short getHourSlotValue(IHourlyTS pHourlyPP, int hour) {
        Short precip_value = new Short(
                (short) CommonHydroConstants.MISSING_PRECIP);

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
     * Gets IngestInfo
     * 
     * @param lid
     * @return
     */
    public List<Ingestfilter> getIngestInfo(String lid) {

        /*
         * Construct the where clause used to retrieve rows from the
         * IngestFilter table.
         */
        List<Ingestfilter> pIngestHead = new ArrayList<>();
        List<Ingestfilter> ilist = getIngestList();
        if (ilist != null) {
            for (Ingestfilter rec : ilist) {
                if (rec.getId().getLid().equals(lid)) {
                    pIngestHead.add(rec);
                }
            }
        }
        return pIngestHead;
    }

    public List<Ingestfilter> getIngestList() {
        return ingestList;
    }

    public void setIngestList(List<Ingestfilter> ingestList) {
        this.ingestList = ingestList;
    }

}
