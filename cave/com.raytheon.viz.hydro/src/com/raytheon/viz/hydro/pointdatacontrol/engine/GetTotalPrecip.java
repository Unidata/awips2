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
package com.raytheon.viz.hydro.pointdatacontrol.engine;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.shef.tables.Rawpc;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydro.pointdatacontrol.data.PrecipRecord;
import com.raytheon.viz.hydro.pointdatacontrol.data.PrecipTotal;
import com.raytheon.viz.hydro.pointdatacontrol.data.SitePrecipData;
import com.raytheon.viz.hydrocommon.HydroDataCache;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.datamanager.HydroCommonDataManager;
import com.raytheon.viz.hydrocommon.util.DurationUtils;

/**
 * Get the total precip for PC and PP reports from the rawPC, rawPP, curPC, and
 * curPP tables.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2010 4564       mpduff      Initial creation
 * 
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GetTotalPrecip {
    private static final String SUM_PC_REPORTS_TOKEN = "sum_pc_reports";

    private static final int PRECIP_NO_ACCUM = 1;

    private static final int REPORT_MISSING_BELOW_MIN_PERCENT = 4;

    private static final int PRECIP_PE_BEST = 8;

    private static final int PRECIP_PP = 16;

    private static final int PRECIP_PC = 32;

    private static final int PRECIP_TS_BEST = 64;

    private static final int PRECIP_TS_RANK = 128;

    private static final int DEFAULT_ADJUSTED_STARTTIME_HRS = 4;

    private static final int EXACT_ENDINGTIME_MATCH = -1;

    private static final int CLOSEST_ENDINGTIME_MATCH = -2;

    private static final int LATEST_ENDINGTIME_MATCH = -3;

    private ArrayList<GageData> reportList = null;

    private enum PrecipTsMode {
        PrecipTSbest, PrecipTSrank, PrecipTSsingle
    };

    private enum PrecipPeMode {
        PrecipPEbest, PrecipPEPP, PrecipPEPC
    };

    public GetTotalPrecip() {

    }

    /**
     * Produces a precipitation total based on the Raw precipitation tables.
     * These include the RawPP, RawPC, CurPP, and CurPC tables.
     * 
     * @param rawPCList
     * @param rawPPList
     * @param starting_time
     * @param ending_time
     * @param ending_time_match
     * @param min_percent
     * @param settings
     * @param advance
     * @return
     */
    public ArrayList<GageData> getTotalRawPrecip(ArrayList<Rawpc> rawPCList,
            ArrayList<Rawpp> rawPPList, Date startingTime, Date endingTime,
            int endingTimeMatch, float minPercent, int settings) {

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String lid = null;
        String prevLid = null;
        String pe = null;
        String ts = null;
        String prevTs = null;
        reportList = new ArrayList<GageData>();

        Map<String, SitePrecipData> pcPrecipData = new HashMap<String, SitePrecipData>();
        Map<String, SitePrecipData> ppPrecipData = new HashMap<String, SitePrecipData>();
        ArrayList<PrecipRecord> dataList = new ArrayList<PrecipRecord>();

        // process the pc data first
        pe = "PC";
        SitePrecipData precipData = new SitePrecipData();

        // process the first record
        Rawpc pc = null;
        if ((rawPCList != null) && (rawPCList.size() > 0)) {
            pc = rawPCList.get(0);
            prevLid = pc.getLid();
            prevTs = pc.getTs();
    
            precipData.setLid(prevLid);
        }
        
        precipData.setPe(pe);

        // process the remaining records
        for (int i = 1; i < rawPCList.size(); i++) {
            pc = rawPCList.get(i);
            lid = pc.getLid();
            ts = pc.getTs();
            if (lid.equals(prevLid)) {
                if (ts.equals(prevTs)) {
                    PrecipRecord rec = new PrecipRecord();
                    rec.setDate(pc.getObstime());
                    rec.setValue(pc.getValue());
                    rec.setPe(pe);
                    rec.setTs(pc.getTs());
                    rec.setShefQualCode(pc.getShefQualCode());
                    rec.setLid(pc.getLid());
                    rec.setDuration(pc.getDur());
                    dataList.add(rec);
                    prevLid = lid;
                    prevTs = ts;
                } else {
                    precipData.addTs(prevTs);
                    precipData.addData(prevTs, dataList);
                    prevTs = ts;

                    dataList = new ArrayList<PrecipRecord>();
                    PrecipRecord rec = new PrecipRecord();
                    rec.setDate(pc.getObstime());
                    rec.setValue(pc.getValue());
                    rec.setPe(pe);
                    rec.setTs(pc.getTs());
                    rec.setShefQualCode(pc.getShefQualCode());
                    rec.setLid(pc.getLid());
                    rec.setDuration(pc.getDur());
                    dataList.add(rec);
                    prevLid = lid;
                    prevTs = ts;
                }
            } else {
                precipData.addTs(prevTs);
                precipData.addData(prevTs, dataList);
                prevTs = ts;

                pcPrecipData.put(prevLid, precipData);
                // pcPrecipData.put(precipData.getLid(), precipData);

                precipData = new SitePrecipData(lid);
                dataList = new ArrayList<PrecipRecord>();

                precipData.setPe(pe);

                PrecipRecord rec = new PrecipRecord();
                rec.setDate(pc.getObstime());
                rec.setValue(pc.getValue());
                rec.setPe(pe);
                rec.setTs(pc.getTs());
                rec.setShefQualCode(pc.getShefQualCode());
                rec.setLid(pc.getLid());
                rec.setDuration(pc.getDur());
                dataList.add(rec);
                prevLid = lid;
                prevTs = ts;
            }
        }

        // Add the last set of data
        precipData.addData(ts, dataList);
        precipData.addTs(ts);

        // now process the pp data
        pe = "PP";
        precipData = new SitePrecipData();

        // process the first record
        Rawpp pp = null;
        if ((rawPPList != null) && (rawPPList.size() > 0)) {
            pp = rawPPList.get(0);
            prevLid = pp.getLid();
            prevTs = pp.getTs();

            precipData.setLid(prevLid);
        }
        
        precipData.setPe(pe);

        // process the remaining records
        for (int i = 1; i < rawPPList.size(); i++) {
            pp = rawPPList.get(i);
            lid = pp.getLid();
            ts = pp.getTs();
            if (lid.equals(prevLid)) {
                if (ts.equals(prevTs)) {
                    PrecipRecord rec = new PrecipRecord();
                    rec.setDate(pp.getObstime());
                    rec.setValue(pp.getValue());
                    rec.setPe(pe);
                    rec.setTs(pp.getTs());
                    rec.setShefQualCode(pp.getShefQualCode());
                    rec.setLid(pp.getLid());
                    rec.setDuration(pp.getDur());
                    dataList.add(rec);
                    prevLid = lid;
                    prevTs = ts;
                } else {
                    precipData.addTs(prevTs);
                    precipData.addData(prevTs, dataList);
                    prevTs = ts;

                    dataList = new ArrayList<PrecipRecord>();
                    PrecipRecord rec = new PrecipRecord();
                    rec.setDate(pp.getObstime());
                    rec.setValue(pp.getValue());
                    rec.setPe(pe);
                    rec.setTs(pp.getTs());
                    rec.setShefQualCode(pp.getShefQualCode());
                    rec.setLid(pp.getLid());
                    rec.setDuration(pp.getDur());
                    dataList.add(rec);
                    prevLid = lid;
                    prevTs = ts;
                }
            } else {
                precipData.addTs(prevTs);
                precipData.addData(prevTs, dataList);
                prevTs = ts;

                ppPrecipData.put(prevLid, precipData);

                precipData = new SitePrecipData(lid);
                dataList = new ArrayList<PrecipRecord>();

                precipData.setPe(pe);

                PrecipRecord rec = new PrecipRecord();
                rec.setDate(pp.getObstime());
                rec.setValue(pp.getValue());
                rec.setPe(pe);
                rec.setTs(pp.getTs());
                rec.setShefQualCode(pp.getShefQualCode());
                rec.setLid(pp.getLid());
                rec.setDuration(pp.getDur());
                dataList.add(rec);
                prevLid = lid;
                prevTs = ts;
            }
        }

        // Add the last set of data
        precipData.addData(ts, dataList);
        precipData.addTs(ts);

        int noAccumFlag = 0;
        int reportMissMinPercent = 0;
        PrecipPeMode peMode = PrecipPeMode.PrecipPEbest;
        PrecipTsMode tsMode = PrecipTsMode.PrecipTSsingle;

        String result = appsDefaults.getToken(SUM_PC_REPORTS_TOKEN);
        boolean sumPcReports = false;
        if (result.equalsIgnoreCase("YES")) {
            sumPcReports = true;
        }

        // Settings
        noAccumFlag = settings & PRECIP_NO_ACCUM;
        reportMissMinPercent = settings & REPORT_MISSING_BELOW_MIN_PERCENT;

        if ((settings & PRECIP_PE_BEST) == PRECIP_PE_BEST) {
            peMode = PrecipPeMode.PrecipPEbest;
        } else if ((settings & PRECIP_PP) == PRECIP_PP) {
            peMode = PrecipPeMode.PrecipPEPP;
        } else if ((settings & PRECIP_PC) == PRECIP_PC) {
            peMode = PrecipPeMode.PrecipPEPC;
        }

        if ((settings & PRECIP_TS_BEST) == PRECIP_TS_BEST) {
            tsMode = PrecipTsMode.PrecipTSbest;
        } else if ((settings & PRECIP_TS_RANK) == PRECIP_TS_RANK) {
            tsMode = PrecipTsMode.PrecipTSrank;
        } else {
            tsMode = PrecipTsMode.PrecipTSsingle;
        }

        /*
         * Retrieve the precip totals. If necessary process multiple TSs,
         * choosing either the TS which produces the best precip coverage or the
         * TS which has the highest rank.
         */
        Set<String> lidSet = new TreeSet<String>();
        lidSet.addAll(pcPrecipData.keySet());
        lidSet.addAll(ppPrecipData.keySet());

        SitePrecipData pcData = null;
        SitePrecipData ppData = null;
        String bestPcTs = null;
        String bestPpTs = null;
        double bestPcAmt = PDCConstants.MISSING_VALUE;
        double bestPpAmt = PDCConstants.MISSING_VALUE;
        Date bestMatchTime = null;
        PrecipTotal pcPrecipTotal = null;
        PrecipTotal ppPrecipTotal = null;
        PrecipTotal bestPrecipTotal = new PrecipTotal();

        Iterator<String> iter = lidSet.iterator();

        // Iterate through the full set of lids
        while (iter.hasNext()) {
            lid = iter.next();
            pcData = null;
            ppData = null;
            int bestPcCoverage = PDCConstants.MISSING_VALUE;
            int bestPpCoverage = PDCConstants.MISSING_VALUE;

            // make sure the lid is in the map
            if (pcPrecipData.containsKey(lid)) {
                pcData = pcPrecipData.get(lid);
            }

            // make sure the lid is in the map
            if (ppPrecipData.containsKey(lid)) {
                ppData = ppPrecipData.get(lid);
            }
            tsMode = PrecipTsMode.PrecipTSrank;
            switch (tsMode) {
            case PrecipTSsingle:
                if ((pcData != null) && (pcData.getTsCount() > 0)) {
                    pcPrecipTotal = getTotalRawPc(pcData.getData(pcData
                            .getTsList().get(0)), startingTime, endingTime,
                            sumPcReports);
                    bestPcAmt = pcPrecipTotal.getTotal();
                    bestPcCoverage = pcPrecipTotal.getSecondsCovered();
                    bestPcTs = pcData.getTsList().get(0);
                }

                if ((ppData != null) && (ppData.getTsCount() > 0)) {
                    // numPpRecords = pcData.getRecordCountList().get(0);
                    ppPrecipTotal = getTotalRawPp(ppData.getData(ppData
                            .getTsList().get(0)), startingTime, endingTime,
                            noAccumFlag, endingTimeMatch);

                    bestPpTs = ppData.getTsList().get(0);
                    bestPpCoverage = ppPrecipTotal.getSecondsCovered();
                    bestPpAmt = ppPrecipTotal.getTotal();
                    bestMatchTime = ppPrecipTotal.getMatchTime();
                }

                break;
            case PrecipTSbest:
                ArrayList<String> tsList = null;
                boolean summedFlag = false;
                if ((pcData != null) && (pcData.getTsCount() > 0)) {
                    tsList = pcData.getTsList();

                    for (int i = 0; i < tsList.size(); i++) {
                        ts = tsList.get(i);

                        pcPrecipTotal = getTotalRawPc(pcData.getData(ts),
                                startingTime, endingTime, sumPcReports);
                        if (pcPrecipTotal.getSecondsCovered() > bestPcCoverage) {
                            bestPcCoverage = pcPrecipTotal.getSecondsCovered();
                            bestPcAmt = pcPrecipTotal.getTotal();
                            bestPcTs = ts;
                        }
                    }
                }

                if ((ppData != null) && (ppData.getTsCount() > 0)) {
                    tsList = ppData.getTsList();
                    for (int i = 0; i < tsList.size(); i++) {
                        ts = tsList.get(i);
                        ppPrecipTotal = getTotalRawPp(ppData.getData(ts),
                                startingTime, endingTime, noAccumFlag,
                                endingTimeMatch);
                        if (ppPrecipTotal.getSecondsCovered() > bestPpCoverage) {
                            bestPpCoverage = ppPrecipTotal.getSecondsCovered();
                            bestPpAmt = ppPrecipTotal.getTotal();
                            bestMatchTime = ppPrecipTotal.getMatchTime();
                            bestPpTs = ts;

                            /* If value is > 0 summed flag = true; */
                            if (bestPpAmt > 0) {
                                summedFlag = true;
                            }
                        }

                        if (summedFlag) {
                            /*
                             * If an exact match has been found, then stop
                             * searching for the best PP rain amount.
                             */
                            break;
                        }
                    }
                }

                break;
            case PrecipTSrank:
                HydroCommonDataManager dman = HydroCommonDataManager
                        .getInstance();

                /*
                 * Get the TS rank info from the IngestFilter table for this
                 * station.
                 */

                /*
                 * Only perform TS ranking if there there are precipitation data
                 * from multiple PC/TS and PP/TS combinations.
                 */

                /* Retrieve the highest ranking PC typesource. */
                if ((pcData != null) && (pcData.getTsCount() > 0)) {
                    lid = pcData.getLid();
                    pe = pcData.getPe();
                    tsList = pcData.getTsList();
                    ArrayList<String> rs = dman.getTsRank(lid, pe);

                    for (int i = 0; i < rs.size(); i++) {
                        ts = rs.get(0);
                        if (tsList.contains(ts)) {
                            // Best ts found
                            break;
                        }
                    }

                    pcPrecipTotal = getTotalRawPc(pcData.getData(ts),
                            startingTime, endingTime, sumPcReports);

                    bestPcCoverage = pcPrecipTotal.getSecondsCovered();
                    bestPcAmt = pcPrecipTotal.getTotal();
                    bestPcTs = ts;
                }

                if ((ppData != null) && (ppData.getTsCount() > 0)) {
                    lid = ppData.getLid();
                    pe = ppData.getPe();
                    tsList = ppData.getTsList();
                    ArrayList<String> rs = dman.getTsRank(lid, pe);

                    for (int i = 0; i < rs.size(); i++) {
                        ts = rs.get(0);
                        if (tsList.contains(ts)) {
                            // Best ts found
                            break;
                        }
                    }

                    ppPrecipTotal = getTotalRawPp(ppData.getData(ts),
                            startingTime, endingTime, noAccumFlag,
                            endingTimeMatch);

                    bestPpCoverage = ppPrecipTotal.getSecondsCovered();
                    bestPpAmt = ppPrecipTotal.getTotal();
                    bestMatchTime = ppPrecipTotal.getMatchTime();
                    bestPpTs = ts;
                }

                break;
            }

            if (((pcData != null) && (pcData.getTsCount() > 0))
                    && ((ppData != null) && (ppData.getTsCount() > 0))) {
                switch (peMode) {
                case PrecipPEbest:
                    if ((bestPcAmt != PDCConstants.MISSING_VALUE)
                            || (bestPpAmt != PDCConstants.MISSING_VALUE)) {
                        /*
                         * Select the PE or PC estimate which provides the best
                         * precipitation amount estimate.
                         */
                        if (bestPcCoverage > bestPpCoverage) {
                            bestPrecipTotal.setPe("PC");
                            bestPrecipTotal.setTs(bestPcTs);
                            bestPrecipTotal.setTotal(bestPcAmt);
                            bestPrecipTotal.setSecondsCovered(bestPcCoverage);
                        } else {
                            bestPrecipTotal.setPe("PP");
                            bestPrecipTotal.setTs(bestPpTs);
                            bestPrecipTotal.setTotal(bestPpAmt);
                            bestPrecipTotal.setSecondsCovered(bestPpCoverage);
                            bestPrecipTotal.setMatchTime(bestMatchTime);
                        }
                    }

                    break;

                case PrecipPEPP:
                    /* If there are PC and PP values, then use the PP value. */
                    if ((ppData.getTsCount() > 0)
                            && (bestPpAmt != PDCConstants.MISSING_VALUE)) {
                        bestPrecipTotal.setTotal(bestPpAmt);
                        bestPrecipTotal.setPe("PP");
                        bestPrecipTotal.setTs(bestPpTs);
                        bestPrecipTotal.setSecondsCovered(bestPpCoverage);
                        bestPrecipTotal.setMatchTime(bestMatchTime);
                    } else {
                        bestPrecipTotal.setTotal(bestPcAmt);
                        bestPrecipTotal.setPe("PC");
                        bestPrecipTotal.setTs(bestPcTs);
                        bestPrecipTotal.setSecondsCovered(bestPcCoverage);
                    }
                    break;

                case PrecipPEPC:
                    /*
                     * If there are totals from PC and PP, then use the PC
                     * value.
                     */
                    if ((pcData.getTsCount() > 0)
                            && (bestPcAmt != PDCConstants.MISSING_VALUE)) {
                        bestPrecipTotal.setTotal(bestPcAmt);
                        bestPrecipTotal.setPe("PC");
                        bestPrecipTotal.setTs(bestPcTs);
                        bestPrecipTotal.setSecondsCovered(bestPcCoverage);
                    } else {
                        bestPrecipTotal.setTotal(bestPpAmt);
                        bestPrecipTotal.setPe("PP");
                        bestPrecipTotal.setTs(bestPpTs);
                        bestPrecipTotal.setSecondsCovered(bestPpCoverage);
                        bestPrecipTotal.setMatchTime(bestMatchTime);
                    }
                }
            } else if ((ppData != null) && (ppData.getTsCount() > 0)) {
                bestPrecipTotal.setTotal(bestPpAmt);
                bestPrecipTotal.setPe("PP");
                bestPrecipTotal.setTs(bestPpTs);
                bestPrecipTotal.setSecondsCovered(bestPpCoverage);
                bestPrecipTotal.setMatchTime(bestMatchTime);
            } else {
                bestPrecipTotal.setTotal(bestPcAmt);
                bestPrecipTotal.setPe("PC");
                bestPrecipTotal.setTs(bestPcTs);
                bestPrecipTotal.setSecondsCovered(bestPcCoverage);
            }

            if (bestPrecipTotal.getTotal() != PDCConstants.MISSING_VALUE) {
                bestPrecipTotal.setHoursCovered(bestPrecipTotal
                        .getSecondsCovered()
                        / PDCConstants.SECONDS_PER_HOUR);
                bestPrecipTotal
                        .setPercentFilled(bestPrecipTotal.getSecondsCovered()
                                / ((endingTime.getTime() - startingTime
                                        .getTime()) / PDCConstants.MILLIS_PER_SECOND));

                /* Do no allow for a percent filled of greater than 100%. */
                if (bestPrecipTotal.getPercentFilled() > 1.0) {
                    bestPrecipTotal.setPercentFilled(1.0);
                }

                bestPrecipTotal.setValueIndicator(PrecipTotal.OK_CHAR);

                /* Set the QC and error flags. */
                if (reportMissMinPercent > 0) {
                    if (bestPrecipTotal.getPercentFilled() < minPercent) {
                        bestPrecipTotal.setTotal(PDCConstants.MISSING_VALUE);
                        bestPrecipTotal
                                .setValueIndicator(PrecipTotal.REJECTED_CHAR);
                    }
                }

                if ((bestPrecipTotal.getTotal() < 0)
                        && (bestPrecipTotal.getTotal() != PDCConstants.MISSING_VALUE)) {
                    bestPrecipTotal.setTotal(PDCConstants.MISSING_VALUE);
                    bestPrecipTotal.getErr().setNegdiff(true);
                    bestPrecipTotal.setValueIndicator(PrecipTotal.MISSING_CHAR);
                }

            } else {
                bestPrecipTotal.getErr().setNegval(true);
            }
            GageData gd = loadGageData(bestPrecipTotal, lid, endingTime);
            reportList.add(gd);
        }

        return reportList;
    }

    /**
     * Get total raw PC data
     * 
     * @param pcDataList
     *            PrecipRecord list
     * @param startingTime
     *            starting time of period
     * @param endingTime
     *            ending time of period
     * @param sumPcReports
     *            sum the reports or not
     * @return PrecipTotal object
     */
    private PrecipTotal getTotalRawPc(ArrayList<PrecipRecord> pcDataList,
            Date startingTime, Date endingTime, boolean sumPcReports) {
        PrecipTotal precipTotal = null;
        
        /*
         * Determine which algorithm to use in deriving PC precip totals.
         */
        if (sumPcReports) {
            precipTotal = sumRawPc(pcDataList, startingTime, endingTime);
        } else {
            precipTotal = subtractRawPc(pcDataList, startingTime, endingTime);
        }
        
        return precipTotal;
    }

    /**
     * Get toal raw PP data
     * 
     * @param ppDataList
     *            PrecipRecord list
     * @param startingTime
     *            starting time of period
     * @param endingTime
     *            ending time of period
     * @param noAccumFlag
     *            accumulate the data or not
     * @param endingTimeMatch
     *            match the ending time or not
     * @return PrecipTotal object
     */
    private PrecipTotal getTotalRawPp(ArrayList<PrecipRecord> ppDataList,
            Date startingTime, Date endingTime, int noAccumFlag,
            int endingTimeMatch) {
        PrecipTotal precipTotal = new PrecipTotal();
        int numMinutes;
        int secondsCovered = 0;

        /* Check to make sure that there are data to process. */
        if ((ppDataList == null) || (ppDataList.size() == 0)) {
            precipTotal.setSecondsCovered(secondsCovered);
            precipTotal.setTotal(PDCConstants.MISSING_VALUE);

            return precipTotal;
        }

        numMinutes = (int) ((endingTime.getTime() - startingTime.getTime()) / PDCConstants.MILLIS_PER_MINUTE);
        ArrayList<Integer> minuteList = new ArrayList<Integer>(numMinutes);

        precipTotal = computeRawPpTotal(ppDataList, startingTime, endingTime,
                noAccumFlag, endingTimeMatch, minuteList);

        /* Return the value and seconds covered to the user. */
        if (precipTotal.isDurationMatchFound() == false) {
            int j = 0;

            for (int i = 0; i < minuteList.size(); i++) {
                if (minuteList.get(i) == 1) {
                    j++;
                }
            }

            secondsCovered = j * PDCConstants.SECONDS_PER_MINUTE;

        } else {
            secondsCovered = (int) ((endingTime.getTime() - startingTime
                    .getTime()) / PDCConstants.MILLIS_PER_SECOND);
        }

        precipTotal.setSecondsCovered(secondsCovered);

        return precipTotal;
    }

    /**
     * Compute the Raw PP Total preciptation.
     * 
     * @param ppDataList
     *            PP PrecipRecord list
     * @param startingTime
     *            Starting time of period
     * @param endingTime
     *            Ending time of period
     * @param noAccumFlag
     *            Accumulate or not
     * @param endingTimeMatch
     *            match the ending time or not
     * @param minuteList
     *            list of minutes in the time period
     * @return PrecipTotal object
     */
    private PrecipTotal computeRawPpTotal(ArrayList<PrecipRecord> ppDataList,
            Date startingTime, Date endingTime, int noAccumFlag,
            int endingTimeMatch, ArrayList<Integer> minuteList) {
        PrecipTotal precipTotal = new PrecipTotal();
        double total;
        boolean startIsWithin = false;
        boolean endIsWithin = false;
        boolean alreadyUsed = false;
        long reportStart;

        if ((ppDataList == null) || (minuteList == null)
                || (minuteList.size() == 0)) {
            precipTotal.setDurationMatchFound(false);
            return precipTotal;
        }

        /* Check for an exact match first. */
        precipTotal = findDurationMatch(ppDataList, startingTime, endingTime,
                noAccumFlag, endingTimeMatch);

        if (precipTotal.isDurationMatchFound()) {
            return precipTotal;
        }

        total = precipTotal.getTotal();

        /*
         * A duration match was not found. Check if the user will allow an
         * accumulation.
         */
        if (noAccumFlag == 0) {
            for (int i = 0; i < ppDataList.size(); i++) {
                PrecipRecord pRec = ppDataList.get(i);
                Date obstime = pRec.getDate();
                /* Only consider the PP value if it is valid. */
                if ((pRec.getValue() != PDCConstants.MISSING_VALUE)
                        && !pRec.getShefQualCode().startsWith("B")
                        && !pRec.getShefQualCode().startsWith("R")) {
                    /*
                     * Convert the SHEF duration code to an interval in seconds.
                     * Subtract this interval from the report's obstime to get
                     * the start time.
                     */
                    long durMillis = DurationUtils.durationToSeconds(pRec
                            .getDuration(), obstime.getTime());
                    /*
                     * Test whether the report's obstime is within the interval.
                     * If it is, then set the start flag to true.
                     */
                    if ((obstime.getTime() > startingTime.getTime())
                            && (obstime.getTime() <= endingTime.getTime())) {
                        endIsWithin = true;
                    } else {
                        endIsWithin = false;
                    }

                    reportStart = obstime.getTime() - durMillis;

                    /*
                     * Test whether the starting time of the report's
                     * observation is within the interval. If it is, then set
                     * the end flag to true.
                     */
                    if ((reportStart >= startingTime.getTime())
                            && (reportStart < endingTime.getTime())) {
                        startIsWithin = true;
                    } else {
                        startIsWithin = false;
                    }

                    /*
                     * Calculate the indexes in the minutes array corresponding
                     * to the starting and ending times.
                     */
                    int startmin = (int) ((reportStart - startingTime.getTime()) / PDCConstants.MILLIS_PER_MINUTE);
                    int endmin = (int) ((obstime.getTime() - startingTime
                            .getTime()) / PDCConstants.MILLIS_PER_MINUTE);

                    /*
                     * Check that the report's time period is completetly within
                     * the time period being considered.
                     */
                    if (pRec.getValue() >= 0.0) {
                        if (startIsWithin && endIsWithin) {
                            /*
                             * Check to determine if the portion of the
                             * accumulation interval covered by the report's
                             * duration has already been covered by a previous
                             * report.
                             */
                            alreadyUsed = false;

                            for (int j = startmin; j < endmin; j++) {
                                if (minuteList.get(j) == 1) {
                                    alreadyUsed = true;
                                    break;
                                }
                            }

                            /*
                             * The data being considered has a duration that
                             * fits in the duration being considered, and the
                             * duration 'slot' is available, so apply the data
                             * value to the total and set the array to indicate
                             * the slot is now not available.
                             */
                            if (!alreadyUsed) {
                                if (total == PDCConstants.MISSING_VALUE) {
                                    total = pRec.getValue();
                                } else {
                                    total += pRec.getValue();
                                }

                                for (int j = startmin; j < endmin; j++) {
                                    minuteList.add(j, 1);
                                }
                            }
                        } else if (pRec.getValue() == 0.0) {
                            if (startIsWithin && endIsWithin) {
                                /*
                                 * initialize the value as necessary and set
                                 * what time period the zero value covers.
                                 */
                                if (total == PDCConstants.MISSING_VALUE) {
                                    total = pRec.getValue();
                                }

                                if (startmin < 0) {
                                    startmin = 0;
                                }

                                if (endmin > minuteList.size()) {
                                    endmin = minuteList.size();
                                }

                                for (int j = startmin; j < endmin; j++) {
                                    minuteList.add(j, 1);
                                }
                            }
                        }
                    }
                }
            }
        }

        precipTotal.setTotal(total);
        precipTotal.setDurationMatchFound(false);

        return precipTotal;
    }

    /**
     * Find a duration match.
     * 
     * @param ppDataList
     *            PrecipRecord list
     * @param startingTime
     *            starting time of the period
     * @param endingTime
     *            ending time of the period
     * @param noAccumFlag
     *            accumulate the data or not
     * @param endingTimeMatch
     *            match the ending time or not
     * @return PrecipTotal object
     */
    private PrecipTotal findDurationMatch(ArrayList<PrecipRecord> ppDataList,
            Date startingTime, Date endingTime, int noAccumFlag,
            int endingTimeMatch) {
        PrecipTotal precipTotal = new PrecipTotal();
        int duration;
        boolean isEndtimeNear7am = false;
        boolean isObstimeNear7am = false;
        long diff;
        long prevDiff = Integer.MAX_VALUE;

        duration = (int) ((endingTime.getTime() - startingTime.getTime()) / PDCConstants.MILLIS_PER_SECOND);

        for (int i = 0; i < ppDataList.size(); i++) {
            PrecipRecord pRec = ppDataList.get(i);

            if ((pRec.getValue() != PDCConstants.MISSING_VALUE)
                    && (!pRec.getShefQualCode().startsWith("B"))
                    && (!pRec.getShefQualCode().startsWith("R"))) {
                /*
                 * Convert the SHEF duration code to an interval in seconds.
                 * Subtract this interval from the report's obstime to get the
                 * start time.
                 */
                long durMillis = DurationUtils.durationToSeconds(pRec
                        .getDuration(), pRec.getDate().getTime());

                if (((int) (durMillis / 1000)) == duration) {
                    switch (endingTimeMatch) {
                    case LATEST_ENDINGTIME_MATCH:
                        /*
                         * All done. The duration match with the most recent
                         * ending time has been found.
                         */
                        precipTotal.setTotal(pRec.getValue());
                        precipTotal.setMatchTime(pRec.getDate());
                        precipTotal.setDurationMatchFound(true);

                        return precipTotal;

                    case EXACT_ENDINGTIME_MATCH:
                        /*
                         * When accumulating for 24 hours and processing a 5004
                         * report with a duration of 24 hours and the ending
                         * time of the accumulation interval is within +/- 3
                         * hours of 7 AM local time, find the 5004 report whose
                         * obstime is closest to the ending time of the
                         * accumulation interval. Treat it like an exact match.
                         */
                        if (((pRec.getDuration() == 5004) || (pRec
                                .getDuration() == 2001))
                                && (duration == PDCConstants.SECONDS_PER_DAY)) {
                            isEndtimeNear7am = isNear7amLocal(endingTime, pRec
                                    .getDate());

                            if (isEndtimeNear7am == true) {
                                /*
                                 * If this is a 2001 report, make sure that its
                                 * obstime is within the specified number of
                                 * hours of 7am local. For a 5004 report we
                                 * already know that the obstime is close enough
                                 * to 7am local because it has a 24 hour
                                 * duration.
                                 */
                                isObstimeNear7am = true;

                                if (pRec.getDuration() == 2001) {
                                    isObstimeNear7am = isNear7amLocal(pRec
                                            .getDate(), pRec.getDate());
                                }

                                if (isObstimeNear7am) {
                                    precipTotal.setDurationMatchFound(true);
                                    diff = Math.abs(pRec.getDate().getTime()
                                            - endingTime.getTime());

                                    if (prevDiff >= diff) {
                                        precipTotal.setTotal(pRec.getValue());
                                        precipTotal
                                                .setMatchTime(pRec.getDate());
                                        prevDiff = diff;
                                    }
                                }
                            }
                        } else {
                            /*
                             * Does the obstime of the report match the ending
                             * time of the accumulation interval?
                             */
                            if (pRec.getDate().getTime() == endingTime
                                    .getTime()) {
                                precipTotal.setTotal(pRec.getValue());
                                precipTotal.setMatchTime(pRec.getDate());
                                precipTotal.setDurationMatchFound(true);

                                return precipTotal;
                            }
                        }

                        break;

                    case CLOSEST_ENDINGTIME_MATCH:
                        /*
                         * Find a report with a matching duration whose obstime
                         * is closest to the ending time of the accumulation
                         * interval.
                         */
                        diff = Math.abs(pRec.getDate().getTime()
                                - endingTime.getTime());

                        if (prevDiff >= diff) {
                            precipTotal.setTotal(pRec.getValue());
                            precipTotal.setMatchTime(pRec.getDate());
                            prevDiff = diff;
                        } else {
                            precipTotal.setDurationMatchFound(true);

                            return precipTotal;
                        }

                    default:
                        /*
                         * Find a report with a matching duration whose obstime
                         * is closest to the ending time of the accumulation
                         * interval and within the user specified hour window.
                         */
                        diff = Math.abs(pRec.getDate().getTime()
                                - endingTime.getTime());
                        if ((prevDiff >= diff)
                                && (diff <= (endingTimeMatch * PDCConstants.SECONDS_PER_HOUR))) {
                            precipTotal.setDurationMatchFound(true);
                            precipTotal.setTotal(pRec.getValue());
                            precipTotal.setMatchTime(pRec.getDate());
                            prevDiff = diff;
                        }
                    }
                }
            }
        }

        return precipTotal;
    }

    /**
     * Subtract the values for the given time period.
     * 
     * @param pcDataList
     * @param startingTime
     * @param endingTime
     * @return
     */
    private PrecipTotal subtractRawPc(ArrayList<PrecipRecord> pcDataList,
            Date startingTime, Date endingTime) {
        PrecipTotal precipTotal = new PrecipTotal();
        int adjStartHrs = DEFAULT_ADJUSTED_STARTTIME_HRS;
        int secondsCovered = 0;
        long adjustedStartTime = 0;
        long absoluteTimeDiff = 0;
        long endTimeDiff = Integer.MAX_VALUE;
        long beginTimeDiff = Integer.MAX_VALUE;
        long endMillis = 0; // last good data time
        long startMillis = 0;// first good data time
        double startValue = PDCConstants.MISSING_VALUE;
        double endValue = PDCConstants.MISSING_VALUE;
        boolean alertStationFlag = false;
        double total = 0;
        int i = 0;

        /*
         * Define an adjusted starting time to improve chances of getting a
         * meaningful start value.
         */
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String pcAdjustedStartTime = appsDefaults
                .getToken("adjust_PC_startingtime");
        if (pcAdjustedStartTime != null) {
            adjStartHrs = Integer.parseInt(pcAdjustedStartTime);
        }

        /* apply the start time adjustment */
        adjustedStartTime = startingTime.getTime()
                - (adjStartHrs * PDCConstants.MILLIS_PER_HOUR);

        /*
         * find the closest PC value to the ending time, but still later than
         * the starting time.  
         *       */
        if ( pcDataList!= null && pcDataList.size()>0) {
        	for (i = 0; i < pcDataList.size(); i++) {
        		PrecipRecord pRec = pcDataList.get(i);
        		Date obsTime = pRec.getDate();

        		if (startingTime.before(obsTime)) {
        			if (pRec.getValue() >= 0.0) {
        				absoluteTimeDiff = Math.abs(obsTime.getTime()
                            - endingTime.getTime());

        				if (absoluteTimeDiff < endTimeDiff) {
        					endTimeDiff = absoluteTimeDiff;
        					endValue = pRec.getValue();
        					endMillis = obsTime.getTime();
        				} else {
        					/* The best ending value has been found. */
        					break;
        				}
        			}
        		} else {
        			break;
        		}
        	}
        
        /*
         * if this is an ALERT station, then if no end_value was found, there
         * was no data before the starting_time, search for a begin value. if
         * this is not an ALERT station, then return with no value if no
         * end_value was found.
         */
        
        if ((pcDataList!=null) && !isAlertStation(pcDataList.get(0).getLid())) {
            alertStationFlag = false;
            if (endValue == PDCConstants.MISSING_VALUE) {
                precipTotal.setTotal(PDCConstants.MISSING_VALUE);
                precipTotal.setSecondsCovered(secondsCovered);

                return precipTotal;
            } else {
                alertStationFlag = true;
            } 

            /*
             * starting from the first record found above to look for the last
             * record which has value <= the end_value (not missing value) in
             * the precipitation duration plus the hour window
             */
            for (; i < pcDataList.size(); i++) {
                PrecipRecord pRec = pcDataList.get(i);
                Date obsTime = pRec.getDate();

                if (obsTime.getTime() >= adjustedStartTime) {
                    if ((pRec.getValue() >= 0.0)
                            && ((pRec.getValue() <= endValue) || (endValue == PDCConstants.MISSING_VALUE))) {
                        absoluteTimeDiff = Math.abs(obsTime.getTime()
                                - startingTime.getTime());

                        if (absoluteTimeDiff < beginTimeDiff) {
                            beginTimeDiff = absoluteTimeDiff;
                            startValue = pRec.getValue();
                            startMillis = obsTime.getTime();
                        } else {
                            /* The best starting value has been found. */
                           break;
                       }
                    }
                } else {
                    break;
                }
            } 

            /*
             * If there is only one report, this report may be before, within,
             * or after the requested time period. Treat these cases the same as
             * follows. If the station is an alert station than we assume that
             * the one report means that it is still reporting data, so the
             * value is not missing. Furthermore, we assume that the value is
             * 0.0. To ensure that the value is used, we set the period covered
             * to be the full period, even though that is not truly the case.
             */
            if (((startValue != PDCConstants.MISSING_VALUE) && (endValue == PDCConstants.MISSING_VALUE))
                    || ((startValue == PDCConstants.MISSING_VALUE) && (endValue != PDCConstants.MISSING_VALUE))) {
                if (alertStationFlag) {
                    secondsCovered = (int) ((endMillis - startMillis) / PDCConstants.MILLIS_PER_SECOND);
                    total = 0.0;
                }
            } else if ((startMillis >= startingTime.getTime())
                    && (endMillis <= endingTime.getTime())) {
                /*
                 * The two times are within the desired time period. This is the
                 * ideal case. The resulting value is safe to use whether it is
                 * zero or not. Allow more coverage tolerance for ALERT gages
                 */
                if (alertStationFlag) {
                    secondsCovered = (int) ((endingTime.getTime() - startingTime
                            .getTime()) / PDCConstants.MILLIS_PER_SECOND);
                } else {
                    secondsCovered = (int) ((endingTime.getTime() - startMillis) / PDCConstants.MILLIS_PER_SECOND);
                }

                total = endValue - startValue;
            } else if ((startMillis < startingTime.getTime())
                    && (endMillis > endingTime.getTime())) {
                /*
                 * The two times are both outside (before and after) the desired
                 * time period, i.e. no reports were found within the time
                 * period. This value can only be used if it is zero since any
                 * non-zero precip may have occurred after the time period, and
                 * we must not allow double counting of precip to occur.
                 */
                if ((endValue >= startValue)
                        && ((endValue - startValue) < 0.0001)) {
                    secondsCovered = (int) ((endingTime.getTime() - startingTime
                            .getTime()) / PDCConstants.MILLIS_PER_SECOND);
                    total = 0.0;
                }
            } else if ((startMillis < startingTime.getTime())
                    && (endMillis <= endingTime.getTime())) {
                /*
                 * The two times straddle the requested beginning time. This
                 * value may be used even if it is non-zero since we are
                 * assumming that the precip fell instantaneously at the end
                 * time and the end time is within the time period.
                 */
                secondsCovered = (int) ((endMillis - startingTime.getTime()) / PDCConstants.MILLIS_PER_SECOND);
                total = endValue - startValue;
            } else if ((startMillis >= startingTime.getTime())
                    && (endMillis > endingTime.getTime())) {
                /*
                 * The two times straddle the requested ending time. This value
                 * can only be used if the value is 0.0 since the end time is
                 * outside the time period, and we are assuming the rain falls
                 * instantaneously at the ending report time.
                 */
                if ((endValue >= startValue)
                        && ((endValue - startValue) < 0.0001)) {
                    if (alertStationFlag) {
                        secondsCovered = (int) ((endingTime.getTime() - startingTime
                                .getTime()) / PDCConstants.MILLIS_PER_SECOND);
                    } else {
                        secondsCovered = (int) ((endingTime.getTime() - startMillis) / PDCConstants.MILLIS_PER_SECOND);
                    }
                    total = 0.0;
                }
            }

            /* Set the return values */
            precipTotal.setSecondsCovered(secondsCovered);
            precipTotal.setTotal(total);
        }
        }//add for if

        return precipTotal;
    }

    /**
     * Sum the data values for the given time period.
     * 
     * @param pcDataList
     *            List of PreciRecord data objects
     * @param startingTime
     *            The starting time of the period
     * @param endingTime
     *            The ending time of the period
     * 
     * @return PrecipTotal data object
     */
    private PrecipTotal sumRawPc(ArrayList<PrecipRecord> pcDataList,
            Date startingTime, Date endingTime) {
        PrecipTotal precipTotal = new PrecipTotal();
        double total = PDCConstants.MISSING_VALUE;
        int secondsCovered = 0;
        long absoluteTimeDiff = 0;
        long endTimeDiff = Integer.MAX_VALUE;
        long beginTimeDiff = Integer.MAX_VALUE;
        boolean endValid = false;
        long startingMillis = 0;
        long endingMillis = 0;
        double beginValue;
        double endValue = PDCConstants.MISSING_VALUE;
        int i = 0;

        if (pcDataList == null) {
            precipTotal.setTotal(PDCConstants.MISSING_VALUE);
            precipTotal.setSecondsCovered(secondsCovered);

            return precipTotal;
        }

        /* Walk the list and get the sum */
        for (i = 0; i < pcDataList.size(); i++) {
            PrecipRecord pRec = pcDataList.get(i);
            Date obsTime = pRec.getDate();

            if (obsTime.after(startingTime)) {
                /* Check to make sure the value is valid. */
                if ((pRec.getValue() >= 0.0)
                        && (!pRec.getShefQualCode().startsWith("B"))
                        && (!pRec.getShefQualCode().startsWith("R"))) {
                    absoluteTimeDiff = Math.abs(obsTime.getTime()
                            - endingTime.getTime());

                    if (absoluteTimeDiff < endTimeDiff) {
                        endTimeDiff = absoluteTimeDiff;
                        endValue = pRec.getValue();
                        endingMillis = obsTime.getTime();
                        endValid = true;
                    } else {
                        /* The best ending value has been found. */
                        break;
                    }
                }
            } else {
                break;
            }
        }

        /*
         * if valid ending PC report cannot be found, then return with a missing
         * value
         */
        if (endValid == false) {
            precipTotal.setTotal(PDCConstants.MISSING_VALUE);
            precipTotal.setSecondsCovered(secondsCovered);

            return precipTotal;
        }

        /*
         * Walk through each PC report. If the PC report is valid, then compute
         * the precipitation total.
         */
        for (; i < pcDataList.size(); i++) {
            PrecipRecord pRec = pcDataList.get(i);
            Date obsTime = pRec.getDate();

            if (obsTime.getTime() >= startingTime.getTime()
                    - (12 * PDCConstants.MILLIS_PER_HOUR)) {
                if ((pRec.getValue() >= 0.0) && (pRec.getValue() <= endValue)
                        && !pRec.getShefQualCode().startsWith("B")
                        && !pRec.getShefQualCode().startsWith("R")) {
                    absoluteTimeDiff = Math.abs(obsTime.getTime()
                            - startingTime.getTime());

                    if (absoluteTimeDiff < beginTimeDiff) {
                        beginTimeDiff = absoluteTimeDiff;
                        beginValue = pRec.getValue();
                        startingMillis = obsTime.getTime();

                        if (total != PDCConstants.MISSING_VALUE) {
                            total += endValue - beginValue;
                        } else {
                            total = endValue - beginValue;
                        }

                        /* Figure out the coverage. */
                        if (secondsCovered > 0) {
                            secondsCovered += (endingMillis - startingMillis)
                                    / PDCConstants.MILLIS_PER_SECOND;
                        } else {
                            secondsCovered = (int) ((endingMillis - startingMillis) / PDCConstants.MILLIS_PER_SECOND);
                        }
                    } else {
                        /* The best starting value has been found. */
                        break;
                    }
                }

                /* Set the end values to the current values. */
                endValue = pRec.getValue();
                endingMillis = obsTime.getTime();
            } else {
                break;
            }
        }

        /* Set the return data */
        precipTotal.setTotal(total);
        secondsCovered = (int) ((endingMillis - startingMillis) / PDCConstants.MILLIS_PER_SECOND);
        precipTotal.setSecondsCovered(secondsCovered);

        return precipTotal;
    }

    /**
     * Check to see if the given lid is an Alert station.
     * 
     * @param lid
     *            The location ID
     * @return true if the station is an alert station
     */
    private boolean isAlertStation(String lid) {
        boolean isAlert = false;

        isAlert = HydroDataCache.getInstance().isAlertStation(lid);

        return isAlert;
    }

    /**
     * Is the provided time near 7am local time.
     * 
     * @param endingTime
     *            The ending time of the period
     * @param obstime
     *            The obs time
     * @return true if near 7am local
     */
    private boolean isNear7amLocal(Date endingTime, Date obstime) {

        int local7amWindow;
        Calendar localTime = null;
        long diff;

        localTime = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        localTime.setTime(obstime);
        localTime.set(Calendar.HOUR, 7);
        localTime.set(Calendar.MINUTE, 0);
        localTime.set(Calendar.SECOND, 0);

        diff = Math.abs(endingTime.getTime() - localTime.getTimeInMillis());
        /* Convert difference from milliseconds to seconds */
        diff /= PDCConstants.MILLIS_PER_SECOND;

        /* Obtain time window in units of hours */
        local7amWindow = getLocal7amSearchWindow();

        /* Convert time window from hours to seconds and compare with difference */
        if (diff <= (local7amWindow * PDCConstants.SECONDS_PER_HOUR)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Get a local 7am search window.
     * 
     * @return
     */
    private int getLocal7amSearchWindow() {
        int local7amWindow = PDCConstants.LOCAL_5004_7AM_WINDOW;

        String amString = AppsDefaults.getInstance().getToken(
                PDCConstants.PPP_PPD_LOCAL_7AM_WINDOW);
        if ((amString != null) && (amString.length() > 0)) {
            local7amWindow = Integer.parseInt(amString);
        }

        return local7amWindow;
    }

    /**
     * Create a GageData object from a PrecipTotal object. Method ported from
     * pointcontrol_derive.c in function load_precip_report
     * 
     * @param pt
     *            The PrecipTotal object
     * @param lid
     *            The location Id
     * @return The GageData Object
     */
    private GageData loadGageData(PrecipTotal pt, String lid, Date endingTime) {
        GageData gd = new GageData();
        gd.setLid(lid);
        gd.setPe(pt.getPe());
        gd.setDur((long) (pt.getHoursCovered() + 1000));
        gd.setTs(pt.getTs());
        gd.setExtremum("Z");
        gd.setProbability(-1);
        gd.setShefQualCode(pt.getQc());
        gd.setValue(pt.getTotal());
        gd.setValidtime(endingTime);
        gd.setBasistime(null);

        return gd;
    }
}
