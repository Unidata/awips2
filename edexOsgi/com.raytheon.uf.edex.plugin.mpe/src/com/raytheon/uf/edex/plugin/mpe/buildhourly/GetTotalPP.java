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
package com.raytheon.uf.edex.plugin.mpe.buildhourly;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.shef.tables.Curpp;
import com.raytheon.uf.common.dataplugin.shef.tables.CurppId;
import com.raytheon.uf.common.dataplugin.shef.tables.IRawTS;
import com.raytheon.uf.common.dataplugin.shef.tables.Ingestfilter;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.dataplugin.shef.tables.RawppId;
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.data.PrecipModes.PrecipPEmode;
import com.raytheon.uf.common.hydro.data.PrecipModes.PrecipTSmode;
import com.raytheon.uf.common.hydro.data.PrecipRecord;
import com.raytheon.uf.common.hydro.data.PrecipTotal;
import com.raytheon.uf.common.hydro.data.SitePrecipData;
import com.raytheon.uf.common.hydro.engine.GetTotalPrecip;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.IngestFilterDao;

/**
 * Get the total precip for PC and PP reports from the rawPC, rawPP, curPC, and
 * curPP tables.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 26, 2016 5571       skorolev    Initial creation
 * Jun 08, 2016 5571       njensen     Use precip enums from PrecipModes
 * Jul 12, 2016 4619       bkowal      Moved {@link AppsDefaultsConversionWrapper} to common.
 * Jul 25, 2016 4623       skorolev    Replaced EdexPrecipTotal with PrecipTotal.
 * 
 * </pre>
 * 
 * @author skorolev
 */

public class GetTotalPP extends GetTotalPrecip {

    public IngestFilterDao ingestFilterDao = new IngestFilterDao();

    private List<PrecipTotal> reportList = null;

    /**
     * Constructor
     */
    public GetTotalPP() {
    }

    /**
     * Produces a precipitation total based on the Raw precipitation tables.
     * These include the RawPP, RawPC, CurPP, and CurPC tables. Requires the
     * Ingestfilter and telem tables in the IHFS database. The database must be
     * opened prior to calling this routine.
     * 
     * @param rawPCList
     * @param rawPPList
     * @param startingTime
     * @param endingTime
     * @param endingTimeMatch
     * @param minPercent
     * @param settings
     * @return
     */
    public List<PrecipTotal> getTotalPPRaw(List<IRawTS> rawPCList,
            List<IRawTS> rawPPList, Date startingTime, Date endingTime,
            int endingTimeMatch, float minPercent, int settings) {

        reportList = new ArrayList<>();

        Map<String, SitePrecipData> pcPrecipData = new HashMap<>();
        Map<String, SitePrecipData> ppPrecipData = new HashMap<>();

        Set<String> lidSet = new TreeSet<>();
        // process the pc data ----------------------
        if ((rawPCList != null) && (rawPCList.size() > 0)) {
            processPData(pcPrecipData, rawPCList, CommonHydroConstants.PC);
            lidSet.addAll(pcPrecipData.keySet());
        }
        // process the pp data ----------------------
        if ((rawPPList != null) && (rawPPList.size() > 0)) {
            processPData(ppPrecipData, rawPPList, CommonHydroConstants.PP);
            lidSet.addAll(ppPrecipData.keySet());
        }

        int noAccumFlag = 0;
        int reportMissMinPercent = 0;
        PrecipPEmode peMode = PrecipPEmode.PrecipPEbest;
        PrecipTSmode tsMode = PrecipTSmode.PrecipTSsingle;

        boolean sumPcReports = AppsDefaultsConversionWrapper
                .getPropertyAsBoolean(BuildHourlyConstants.AppsDefaults.SUM_PC_REPORTS_TOKEN);

        // Settings
        noAccumFlag = settings & BuildHourlyConstants.PRECIP_NO_ACCUM;
        reportMissMinPercent = settings
                & BuildHourlyConstants.REPORT_MISSING_BELOW_MIN_PERCENT;

        if ((settings & BuildHourlyConstants.PRECIP_PE_BEST) == BuildHourlyConstants.PRECIP_PE_BEST) {
            peMode = PrecipPEmode.PrecipPEbest;
        } else if ((settings & BuildHourlyConstants.PRECIP_PP) == BuildHourlyConstants.PRECIP_PP) {
            peMode = PrecipPEmode.PrecipPEPP;
        } else if ((settings & BuildHourlyConstants.PRECIP_PC) == BuildHourlyConstants.PRECIP_PC) {
            peMode = PrecipPEmode.PrecipPEPC;
        }

        if ((settings & BuildHourlyConstants.PRECIP_TS_BEST) == BuildHourlyConstants.PRECIP_TS_BEST) {
            tsMode = PrecipTSmode.PrecipTSbest;
        } else if ((settings & BuildHourlyConstants.PRECIP_TS_RANK) == BuildHourlyConstants.PRECIP_TS_RANK) {
            tsMode = PrecipTSmode.PrecipTSrank;
        } else {
            tsMode = PrecipTSmode.PrecipTSsingle;
        }

        /*
         * Retrieve the precip totals. If necessary process multiple TSs,
         * choosing either the TS which produces the best precip coverage or the
         * TS which has the highest rank.
         */
        SitePrecipData pcData = null;
        SitePrecipData ppData = null;
        String bestPcTs = null;
        String bestPpTs = null;
        double bestPcAmt = BuildHourlyConstants.MISSING_VALUE;
        double bestPpAmt = BuildHourlyConstants.MISSING_VALUE;
        Date bestMatchTime = null;
        PrecipTotal pcPrecipTotal = null;
        PrecipTotal ppPrecipTotal = null;

        Iterator<String> iter = lidSet.iterator();

        // Iterate through the full set of lids
        while (iter.hasNext()) {
            String lid = iter.next();
            PrecipTotal bestPrecipTotal = new PrecipTotal();
            // Set lid
            bestPrecipTotal.setLid(lid);

            pcData = null;
            ppData = null;
            int bestPcCoverage = BuildHourlyConstants.MISSING_VALUE;
            int bestPpCoverage = BuildHourlyConstants.MISSING_VALUE;

            // make sure the lid is in the map
            if (pcPrecipData.containsKey(lid)) {
                pcData = pcPrecipData.get(lid);
            }

            // make sure the lid is in the map
            if (ppPrecipData.containsKey(lid)) {
                ppData = ppPrecipData.get(lid);
            }

            String ts = null;
            String pe = null;

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
                    List<Ingestfilter> rs = getIngestFilterDao().getTs(lid, pe);

                    for (int i = 0; i < rs.size(); i++) {
                        ts = rs.toString();
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
                    List<Ingestfilter> rs = getIngestFilterDao().getTs(lid, pe);

                    for (int i = 0; i < rs.size(); i++) {
                        ts = rs.get(i).toString();
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
            }// end switch
             // if both not null
            if (((pcData != null) && (pcData.getTsCount() > 0))
                    && ((ppData != null) && (ppData.getTsCount() > 0))) {
                switch (peMode) {
                case PrecipPEbest:
                    if ((bestPcAmt != BuildHourlyConstants.MISSING_VALUE)
                            || (bestPpAmt != BuildHourlyConstants.MISSING_VALUE)) {
                        /*
                         * Select the PE or PC estimate which provides the best
                         * precipitation amount estimate.
                         */
                        if (bestPcCoverage > bestPpCoverage) {
                            bestPrecipTotal.setPe(BuildHourlyConstants.PC);
                            bestPrecipTotal.setTs(bestPcTs);
                            bestPrecipTotal.setTotal(bestPcAmt);
                            bestPrecipTotal.setSecondsCovered(bestPcCoverage);
                        } else {
                            bestPrecipTotal.setPe(BuildHourlyConstants.PP);
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
                            && (bestPpAmt != BuildHourlyConstants.MISSING_VALUE)) {
                        bestPrecipTotal.setTotal(bestPpAmt);
                        bestPrecipTotal.setPe(BuildHourlyConstants.PP);
                        bestPrecipTotal.setTs(bestPpTs);
                        bestPrecipTotal.setSecondsCovered(bestPpCoverage);
                        bestPrecipTotal.setMatchTime(bestMatchTime);
                    } else {
                        bestPrecipTotal.setTotal(bestPcAmt);
                        bestPrecipTotal.setPe(BuildHourlyConstants.PC);
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
                            && (bestPcAmt != BuildHourlyConstants.MISSING_VALUE)) {
                        bestPrecipTotal.setTotal(bestPcAmt);
                        bestPrecipTotal.setPe(BuildHourlyConstants.PC);
                        bestPrecipTotal.setTs(bestPcTs);
                        bestPrecipTotal.setSecondsCovered(bestPcCoverage);
                    } else {
                        bestPrecipTotal.setTotal(bestPpAmt);
                        bestPrecipTotal.setPe(BuildHourlyConstants.PP);
                        bestPrecipTotal.setTs(bestPpTs);
                        bestPrecipTotal.setSecondsCovered(bestPpCoverage);
                        bestPrecipTotal.setMatchTime(bestMatchTime);
                    }
                }
            } // only PP
            else if ((ppData != null) && (ppData.getTsCount() > 0)) {
                bestPrecipTotal.setTotal(bestPpAmt);
                bestPrecipTotal.setPe(BuildHourlyConstants.PP);
                bestPrecipTotal.setTs(bestPpTs);
                bestPrecipTotal.setSecondsCovered(bestPpCoverage);
                bestPrecipTotal.setMatchTime(bestMatchTime);
            } // only PC
            else {
                bestPrecipTotal.setTotal(bestPcAmt);
                bestPrecipTotal.setPe(BuildHourlyConstants.PC);
                bestPrecipTotal.setTs(bestPcTs);
                bestPrecipTotal.setSecondsCovered(bestPcCoverage);
            }
            // Calculate % of coverage
            if (bestPrecipTotal.getTotal() != BuildHourlyConstants.MISSING_VALUE) {
                double pcov = (double) bestPrecipTotal.getSecondsCovered()
                        / TimeUtil.SECONDS_PER_HOUR;
                bestPrecipTotal.setHoursCovered(pcov);
                double cov = (double) bestPrecipTotal.getSecondsCovered()
                        / ((endingTime.getTime() - startingTime.getTime()) / TimeUtil.MILLIS_PER_SECOND);
                bestPrecipTotal.setPercentFilled(cov);

                /* Do not allow for a percent filled of greater than 100%. */
                if (bestPrecipTotal.getPercentFilled() > 1.0) {
                    bestPrecipTotal.setPercentFilled(1.0);
                }

                bestPrecipTotal.setValueIndicator(BuildHourlyConstants.OK_CHAR);

                /* Set the QC and error flags. */
                if (reportMissMinPercent > 0) {
                    if (bestPrecipTotal.getPercentFilled() < minPercent) {
                        bestPrecipTotal
                                .setTotal(BuildHourlyConstants.MISSING_VALUE);
                        bestPrecipTotal
                                .setValueIndicator(BuildHourlyConstants.REJECTED_CHAR);
                    }
                }

                if ((bestPrecipTotal.getTotal() < 0)
                        && (bestPrecipTotal.getTotal() != BuildHourlyConstants.MISSING_VALUE)) {
                    bestPrecipTotal
                            .setTotal(BuildHourlyConstants.MISSING_VALUE);
                    bestPrecipTotal.getErr().setNegdiff(true);
                    bestPrecipTotal
                            .setValueIndicator(BuildHourlyConstants.MISSING_CHAR);
                }

            } else {
                bestPrecipTotal.getErr().setNegval(true);
            }

            reportList.add(bestPrecipTotal);
        } // end while

        return reportList;
    }

    /**
     * Process rawList = rawPCList or rawPPList. Fills pPrecipData =
     * pcPrecipData or ppPrecipData map.
     * 
     * @param pPrecipData
     * @param rawList
     * @param pe
     */
    private void processPData(Map<String, SitePrecipData> pPrecipData,
            List<IRawTS> rawList, String pe) {

        for (IRawTS p : rawList) {
            String lid = p.getLid();
            String ts = p.getTs();
            SitePrecipData precipData = pPrecipData.get(lid);

            if (precipData == null) {
                // new lid
                precipData = new SitePrecipData(lid);
                precipData.setPe(pe);
                pPrecipData.put(lid, precipData);
            }
            List<PrecipRecord> dataList = precipData.getData(ts);
            if (dataList == null) {
                // new ts for current lid
                dataList = new ArrayList<>();
                precipData.addData(ts, dataList);
                precipData.addTs(ts);
            }
            dataList.add(getNewPrecipRecord(p, pe));
        }
    }

    /**
     * Gets new Precipitation Record
     * 
     * @param rawData
     * @param pe
     * @return
     */
    private PrecipRecord getNewPrecipRecord(IRawTS rawData, String pe) {
        PrecipRecord rec = new PrecipRecord();
        rec.setDate(rawData.getObstime());
        rec.setValue(rawData.getValue());
        rec.setPe(pe);
        rec.setTs(rawData.getTs());
        rec.setShefQualCode(rawData.getShefQualCode());
        rec.setLid(rawData.getLid());
        rec.setDuration(rawData.getDur());
        return rec;
    }

    /**
     * Converts a Curpp object to a Rawpp object.
     * 
     * @param cpp
     *            The Curpp object to convert
     * @return The converted Rawpp object
     */
    public Rawpp convertCurpp2Rawpp(Curpp cpp) {
        Rawpp rpp = new Rawpp();

        RawppId rawId = new RawppId();
        CurppId curId = cpp.getId();

        rawId.setExtremum(curId.getExtremum());
        rawId.setLid(curId.getLid());
        rawId.setObstime(curId.getObstime());
        rawId.setTs(curId.getTs());
        rawId.setDur(curId.getDur());

        rpp.setId(rawId);

        rpp.setPe(cpp.getPe());
        rpp.setPostingtime(cpp.getPostingtime());
        rpp.setProductId(cpp.getProductId());
        rpp.setProducttime(cpp.getProducttime());
        rpp.setQualityCode(cpp.getQualityCode());
        rpp.setRevision(cpp.getRevision());
        rpp.setShefQualCode(cpp.getShefQualCode());
        rpp.setValue(cpp.getValue());
        return rpp;
    }

    public IngestFilterDao getIngestFilterDao() {
        return ingestFilterDao;
    }

}
