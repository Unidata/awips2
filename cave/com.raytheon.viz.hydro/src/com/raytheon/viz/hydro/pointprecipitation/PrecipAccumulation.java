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

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.BooleanUtils;

import com.raytheon.uf.common.hydro.data.PrecipRecord;
import com.raytheon.uf.common.hydro.data.PrecipTotal;
import com.raytheon.uf.common.hydro.engine.GetTotalPrecip;
import com.raytheon.uf.common.hydro.util.DurationUtils;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.hydrocommon.HydroConstants.PhysicalElement;

/**
 * Precipitation accumulation calculations are done here.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2009 2257       mpduff     Initial creation
 * Sep 29, 2010 4384       lbousaidi  Fixed bugs related to TS Lookup for PP and PC
 * May 26, 2016 5571       skorolev   {@link  DurationUtils} relocated to common. Cleanup.
 * Jan 31, 2019 6951       dgilling   Refactor based on GetTotalPrecip class.
 *
 * </pre>
 *
 * @author mpduff
 *
 */
public class PrecipAccumulation extends GetTotalPrecip {

    private final int endingTimeMatch;

    private final double minPercent;

    private final boolean reportMissMinPercent;

    private final boolean noAccumFlag;

    private final boolean sumPcReports;

    public PrecipAccumulation(int endingTimeMatch, double minPercent,
            boolean reportMissingBelowMinPercent, boolean precipNoAccumFlag) {
        super();

        /* Get the algorithm to use for totaling PC precipitation amounts. */
        this.sumPcReports = checkSumPcReports();

        this.endingTimeMatch = endingTimeMatch;
        this.minPercent = minPercent;
        this.reportMissMinPercent = reportMissingBelowMinPercent;
        this.noAccumFlag = precipNoAccumFlag;
    }

    /**
     * Produces a precipitation total based on the Raw precipitation tables.
     * These include the RawPP, RawPC, CurPP, and CurPC tables.
     *
     * This method in AWIPS 1 was in a precip library. We don't currently have
     * the precip library in place. The summation code for the point precip
     * accum is completely encapsulated in this class. If changes are needed in
     * the future see the legacy AWIPS code in function get_total_raw_precip in
     * whfs_lib/src/PrecipUtil/TEXT/get_total_precip.c for original algorithms
     *
     * Not using the settings as in the original code because for this dialog
     * the values are always PrecipPEBest and PrecipTSSingle
     *
     * @param pRawPC
     * @param pRawPP
     * @param startingTime
     * @param endingTime
     * @return
     */
    public Collection<TotalPrecip> getTotalRawPrecip(
            Map<String, List<PrecipRecord>> pRawData, PhysicalElement pe,
            LocalDateTime startingTime, LocalDateTime endingTime) {
        Collection<TotalPrecip> totalPrecipRecords = new ArrayList<>();

        for (Entry<String, List<PrecipRecord>> entry : pRawData.entrySet()) {
            String ts = entry.getKey();
            List<PrecipRecord> rawPrecipRecords = entry.getValue();

            TotalPrecip totalPrecip = new TotalPrecip();
            totalPrecip.setLid(rawPrecipRecords.get(0).getLid());
            long bestCoverage = 0;

            if (PhysicalElement.PC == pe) {
                PrecipTotal pcPrecipAmount = getTotalRawPc(rawPrecipRecords,
                        Date.from(startingTime.toInstant(ZoneOffset.UTC)),
                        Date.from(endingTime.toInstant(ZoneOffset.UTC)),
                        sumPcReports);
                bestCoverage = pcPrecipAmount.getSecondsCovered();

                totalPrecip.setValue((float) pcPrecipAmount.getTotal());
                totalPrecip.setPE("PC");
                totalPrecip.setTS(ts);
                totalPrecip.setSummed_flag(false);
            } else {
                PrecipTotal ppPrecipAmount = getTotalRawPp(rawPrecipRecords,
                        Date.from(startingTime.toInstant(ZoneOffset.UTC)),
                        Date.from(endingTime.toInstant(ZoneOffset.UTC)),
                        BooleanUtils.toInteger(noAccumFlag), endingTimeMatch);
                bestCoverage = ppPrecipAmount.getSecondsCovered();

                totalPrecip.setValue((float) ppPrecipAmount.getTotal());
                totalPrecip.setPE("PP");
                totalPrecip.setTS(ts);
                totalPrecip.setSummed_flag(ppPrecipAmount.isSummedFlag());
                totalPrecip.setMatch_time(ppPrecipAmount.getMatchTime());
            }

            if (PointPrecipConstants.MISSING_PRECIP != totalPrecip.getValue()) {
                totalPrecip.setHours_covered((float) bestCoverage
                        / (float) TimeUtil.SECONDS_PER_HOUR);
                totalPrecip.setPercent_filled((float) bestCoverage
                        / (float) Duration.between(startingTime, endingTime)
                                .getSeconds());

                /* Do no allow for a percent filled of greater than 100%. */
                if (totalPrecip.getPercent_filled() > 1.0) {
                    totalPrecip.setPercent_filled(1f);
                }

                totalPrecip.setValue_indicator(PointPrecipConstants.OK_CHAR);

                /* Set the QC and error flags. */
                if (reportMissMinPercent) {
                    if (totalPrecip.getPercent_filled() < minPercent) {
                        totalPrecip
                                .setValue(PointPrecipConstants.MISSING_PRECIP);
                        totalPrecip.setValue_indicator(
                                PointPrecipConstants.REJECTED_CHAR);
                    }
                }

                if ((totalPrecip.getValue() < 0f)
                        && (PointPrecipConstants.MISSING_PRECIP != totalPrecip
                                .getValue())) {
                    totalPrecip.setValue(PointPrecipConstants.MISSING_PRECIP);
                    totalPrecip.getErr().negdiff = true;
                    totalPrecip.setValue_indicator(
                            PointPrecipConstants.MISSING_CHAR);
                }
            } else {
                totalPrecip.getErr().negval = true;
            }

            totalPrecipRecords.add(totalPrecip);
        }

        return totalPrecipRecords;
    }

    /**
     * Checks if the sum_pc_reports token is set to "YES" or "NO". YES means to
     * compute PC-based precipitation totals by adding the PC reports within the
     * accumulation interval. NO means to subtract the value of the PC report
     * closest to the starting time from the value of the PC report closest to
     * the ending time.
     *
     * @return 1 if the sum_pc_reports token is "YES", 0 if "NO"
     */
    private boolean checkSumPcReports() {
        /*
         * Check the sum_pc_reports token. This will determine if PC-based
         * precipitation totals should be arrived at by a) adding all PC reports
         * over the user-specified interval or b) by subtracting the value of
         * the PC report closest to the starting time of the accumulation
         * interval from the value of the PC report closest to the ending time
         * of the accumulation interval.
         */
        return AppsDefaults.getInstance().getBoolean("sum_pc_reports", false);
    }
}
