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
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.shef.tables.Rawpc;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.data.PrecipModes.PrecipPEmode;
import com.raytheon.uf.common.hydro.data.PrecipModes.PrecipTSmode;
import com.raytheon.uf.common.hydro.data.PrecipRecord;
import com.raytheon.uf.common.hydro.data.PrecipTotal;
import com.raytheon.uf.common.hydro.data.SitePrecipData;
import com.raytheon.uf.common.hydro.engine.GetTotalPrecip;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.datamanager.HydroCommonDataManager;

/**
 * GetTotalPrecip for viz. Extracted from GetTotalPrecip.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 8, 2016  5571       njensen     Initial creation
 *
 * </pre>
 * 
 * @author njensen
 */

public class VizGetTotalPrecip extends GetTotalPrecip {

    protected List<GageData> reportList = null;

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
    public List<GageData> getTotalRawPrecip(List<Rawpc> rawPCList,
            List<Rawpp> rawPPList, Date startingTime, Date endingTime,
            int endingTimeMatch, float minPercent, int settings) {

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String lid = null;
        String prevLid = null;
        String pe = null;
        String ts = null;
        String prevTs = null;
        reportList = new ArrayList<>();

        Map<String, SitePrecipData> pcPrecipData = new HashMap<>();
        Map<String, SitePrecipData> ppPrecipData = new HashMap<>();
        List<PrecipRecord> dataList = new ArrayList<>();

        // process the pc data first
        pe = CommonHydroConstants.PC;
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

                    dataList = new ArrayList<>();
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
                dataList = new ArrayList<>();

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
        pe = CommonHydroConstants.PP;
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

                    dataList = new ArrayList<>();
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
                dataList = new ArrayList<>();

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
        PrecipPEmode peMode = PrecipPEmode.PrecipPEbest;
        PrecipTSmode tsMode = PrecipTSmode.PrecipTSsingle;

        String result = appsDefaults
                .getToken(CommonHydroConstants.SUM_PC_REPORTS_TOKEN);
        boolean sumPcReports = false;
        if (result.equalsIgnoreCase("YES")) {
            sumPcReports = true;
        }

        // Settings
        noAccumFlag = settings & CommonHydroConstants.PRECIP_NO_ACCUM;
        reportMissMinPercent = settings
                & CommonHydroConstants.REPORT_MISSING_BELOW_MIN_PERCENT;

        if ((settings & CommonHydroConstants.PRECIP_PE_BEST) == CommonHydroConstants.PRECIP_PE_BEST) {
            peMode = PrecipPEmode.PrecipPEbest;
        } else if ((settings & CommonHydroConstants.PRECIP_PP) == CommonHydroConstants.PRECIP_PP) {
            peMode = PrecipPEmode.PrecipPEPP;
        } else if ((settings & CommonHydroConstants.PRECIP_PC) == CommonHydroConstants.PRECIP_PC) {
            peMode = PrecipPEmode.PrecipPEPC;
        }

        if ((settings & CommonHydroConstants.PRECIP_TS_BEST) == CommonHydroConstants.PRECIP_TS_BEST) {
            tsMode = PrecipTSmode.PrecipTSbest;
        } else if ((settings & CommonHydroConstants.PRECIP_TS_RANK) == CommonHydroConstants.PRECIP_TS_RANK) {
            tsMode = PrecipTSmode.PrecipTSrank;
        } else {
            tsMode = PrecipTSmode.PrecipTSsingle;
        }

        /*
         * Retrieve the precip totals. If necessary process multiple TSs,
         * choosing either the TS which produces the best precip coverage or the
         * TS which has the highest rank.
         */
        Set<String> lidSet = new TreeSet<>();
        lidSet.addAll(pcPrecipData.keySet());
        lidSet.addAll(ppPrecipData.keySet());

        SitePrecipData pcData = null;
        SitePrecipData ppData = null;
        String bestPcTs = null;
        String bestPpTs = null;
        double bestPcAmt = CommonHydroConstants.MISSING_VALUE;
        double bestPpAmt = CommonHydroConstants.MISSING_VALUE;
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
            int bestPcCoverage = CommonHydroConstants.MISSING_VALUE;
            int bestPpCoverage = CommonHydroConstants.MISSING_VALUE;

            // make sure the lid is in the map
            if (pcPrecipData.containsKey(lid)) {
                pcData = pcPrecipData.get(lid);
            }

            // make sure the lid is in the map
            if (ppPrecipData.containsKey(lid)) {
                ppData = ppPrecipData.get(lid);
            }
            tsMode = PrecipTSmode.PrecipTSrank;
            switch (tsMode) {
            case PrecipTSsingle:
                if ((pcData != null) && (pcData.getTsCount() > 0)) {
                    pcPrecipTotal = getTotalRawPc(
                            pcData.getData(pcData.getTsList().get(0)),
                            startingTime, endingTime, sumPcReports);
                    bestPcAmt = pcPrecipTotal.getTotal();
                    bestPcCoverage = pcPrecipTotal.getSecondsCovered();
                    bestPcTs = pcData.getTsList().get(0);
                }

                if ((ppData != null) && (ppData.getTsCount() > 0)) {
                    // numPpRecords = pcData.getRecordCountList().get(0);
                    ppPrecipTotal = getTotalRawPp(
                            ppData.getData(ppData.getTsList().get(0)),
                            startingTime, endingTime, noAccumFlag,
                            endingTimeMatch);

                    bestPpTs = ppData.getTsList().get(0);
                    bestPpCoverage = ppPrecipTotal.getSecondsCovered();
                    bestPpAmt = ppPrecipTotal.getTotal();
                    bestMatchTime = ppPrecipTotal.getMatchTime();
                }

                break;
            case PrecipTSbest:
                List<String> tsList = null;
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
                    List<String> rs = dman.getTsRank(lid, pe);

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
                    List<String> rs = dman.getTsRank(lid, pe);

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
                    if ((bestPcAmt != CommonHydroConstants.MISSING_VALUE)
                            || (bestPpAmt != CommonHydroConstants.MISSING_VALUE)) {
                        /*
                         * Select the PE or PC estimate which provides the best
                         * precipitation amount estimate.
                         */
                        if (bestPcCoverage > bestPpCoverage) {
                            bestPrecipTotal.setPe(CommonHydroConstants.PC);
                            bestPrecipTotal.setTs(bestPcTs);
                            bestPrecipTotal.setTotal(bestPcAmt);
                            bestPrecipTotal.setSecondsCovered(bestPcCoverage);
                        } else {
                            bestPrecipTotal.setPe(CommonHydroConstants.PP);
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
                            && (bestPpAmt != CommonHydroConstants.MISSING_VALUE)) {
                        bestPrecipTotal.setTotal(bestPpAmt);
                        bestPrecipTotal.setPe(CommonHydroConstants.PP);
                        bestPrecipTotal.setTs(bestPpTs);
                        bestPrecipTotal.setSecondsCovered(bestPpCoverage);
                        bestPrecipTotal.setMatchTime(bestMatchTime);
                    } else {
                        bestPrecipTotal.setTotal(bestPcAmt);
                        bestPrecipTotal.setPe(CommonHydroConstants.PC);
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
                            && (bestPcAmt != CommonHydroConstants.MISSING_VALUE)) {
                        bestPrecipTotal.setTotal(bestPcAmt);
                        bestPrecipTotal.setPe(CommonHydroConstants.PC);
                        bestPrecipTotal.setTs(bestPcTs);
                        bestPrecipTotal.setSecondsCovered(bestPcCoverage);
                    } else {
                        bestPrecipTotal.setTotal(bestPpAmt);
                        bestPrecipTotal.setPe(CommonHydroConstants.PP);
                        bestPrecipTotal.setTs(bestPpTs);
                        bestPrecipTotal.setSecondsCovered(bestPpCoverage);
                        bestPrecipTotal.setMatchTime(bestMatchTime);
                    }
                }
            } else if ((ppData != null) && (ppData.getTsCount() > 0)) {
                bestPrecipTotal.setTotal(bestPpAmt);
                bestPrecipTotal.setPe(CommonHydroConstants.PP);
                bestPrecipTotal.setTs(bestPpTs);
                bestPrecipTotal.setSecondsCovered(bestPpCoverage);
                bestPrecipTotal.setMatchTime(bestMatchTime);
            } else {
                bestPrecipTotal.setTotal(bestPcAmt);
                bestPrecipTotal.setPe(CommonHydroConstants.PC);
                bestPrecipTotal.setTs(bestPcTs);
                bestPrecipTotal.setSecondsCovered(bestPcCoverage);
            }

            if (bestPrecipTotal.getTotal() != CommonHydroConstants.MISSING_VALUE) {
                bestPrecipTotal.setHoursCovered(bestPrecipTotal
                        .getSecondsCovered() / TimeUtil.SECONDS_PER_HOUR);
                bestPrecipTotal
                        .setPercentFilled(bestPrecipTotal.getSecondsCovered()
                                / ((endingTime.getTime() - startingTime
                                        .getTime()) / TimeUtil.MILLIS_PER_SECOND));

                /* Do no allow for a percent filled of greater than 100%. */
                if (bestPrecipTotal.getPercentFilled() > 1.0) {
                    bestPrecipTotal.setPercentFilled(1.0);
                }

                bestPrecipTotal.setValueIndicator(CommonHydroConstants.OK_CHAR);

                /* Set the QC and error flags. */
                if (reportMissMinPercent > 0) {
                    if (bestPrecipTotal.getPercentFilled() < minPercent) {
                        bestPrecipTotal
                                .setTotal(CommonHydroConstants.MISSING_VALUE);
                        bestPrecipTotal
                                .setValueIndicator(CommonHydroConstants.REJECTED_CHAR);
                    }
                }

                if ((bestPrecipTotal.getTotal() < 0)
                        && (bestPrecipTotal.getTotal() != CommonHydroConstants.MISSING_VALUE)) {
                    bestPrecipTotal
                            .setTotal(CommonHydroConstants.MISSING_VALUE);
                    bestPrecipTotal.getErr().setNegdiff(true);
                    bestPrecipTotal
                            .setValueIndicator(CommonHydroConstants.MISSING_CHAR);
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
