/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.radar.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.Test;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord.ScanType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.radar.frame.RadarDataTime;

/**
 * Unit tests for {@link RadarDataCubeAdapter}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2025 2038488    mapeters    Initial creation
 * Jun 05, 2025 2038858    mapeters    Test improved multi-icao handling
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarDataCubeAdapter {

    private static final String KOAX = "koax";

    private static final String KDMX = "kdmx";

    /** Feb 19 2025 21:20:00 */
    private static final long SCAN_1_NORMAL_MS = 1_740_000_000_000L;

    /** Feb 19 2025 21:21:40 */
    private static final long SCAN_1_SAILS_MS = 1_740_000_100_000L;

    /** Feb 19 2025 21:28:20 */
    private static final long SCAN_2_NORMAL_MS = 1_740_000_500_000L;

    /** Feb 19 2025 21:30:00 */
    private static final long SCAN_2_SAILS_MS = 1_740_000_600_000L;

    /** Feb 19 2025 21:31:40 */
    private static final long SCAN_2_MRLE_1_MS = 1_740_000_700_000L;

    /** Feb 19 2025 21:33:20 */
    private static final long SCAN_2_MRLE_2_MS = 1_740_000_800_000L;

    @Test
    void testLinkSailsMrleTimesToNormalScanTimes1() {
        /*
         * SAILS and MRLE scans with corresponding normal scans -> appropriate
         * normal scan time is set on each SAILS/MRLE time
         */
        RadarDataTime scan1Normal = buildRdt(SCAN_1_NORMAL_MS, 1,
                ScanType.NORMAL);
        RadarDataTime scan1Sails = buildRdt(SCAN_1_SAILS_MS, 1, ScanType.SAILS);
        RadarDataTime scan2Normal = buildRdt(SCAN_2_NORMAL_MS, 2,
                ScanType.NORMAL);
        RadarDataTime scan2Mrle1 = buildRdt(SCAN_2_MRLE_1_MS, 2, ScanType.MRLE);
        RadarDataTime scan2Mrle2 = buildRdt(SCAN_2_MRLE_2_MS, 2, ScanType.MRLE);

        RadarDataCubeAdapter.linkSailsMrleTimesToNormalScanTimes(List.of(
                scan1Normal, scan1Sails, scan2Normal, scan2Mrle1, scan2Mrle2));

        assertNull(scan1Normal.getNormalScanMatchRef());
        assertEquals(SCAN_1_NORMAL_MS, scan1Sails.getNormalScanMatchRef());
        assertNull(scan2Normal.getNormalScanMatchRef());
        assertEquals(SCAN_2_NORMAL_MS, scan2Mrle1.getNormalScanMatchRef());
        assertEquals(SCAN_2_NORMAL_MS, scan2Mrle2.getNormalScanMatchRef());
    }

    @Test
    void testLinkSailsMrleTimesToNormalScanTimes2() {
        /*
         * Same as above but add extra SAILS and MRLE times that don't have
         * corresponding normal scan times -> minimum known SAILS offset is used
         * to guess extra SAILS' normal scan time, minimum known MRLE offset is
         * used to guess extra MRLE's normal scan time
         */
        long scan0SailsMs = SCAN_1_NORMAL_MS - 3 * TimeUtil.MILLIS_PER_MINUTE;
        RadarDataTime scan0Sails = buildRdt(scan0SailsMs, 0, ScanType.SAILS);
        RadarDataTime scan1Normal = buildRdt(SCAN_1_NORMAL_MS, 1,
                ScanType.NORMAL);
        RadarDataTime scan1Sails = buildRdt(SCAN_1_SAILS_MS, 1, ScanType.SAILS);
        RadarDataTime scan2Normal = buildRdt(SCAN_2_NORMAL_MS, 2,
                ScanType.NORMAL);
        RadarDataTime scan2Mrle1 = buildRdt(SCAN_2_MRLE_1_MS, 2, ScanType.MRLE);
        RadarDataTime scan2Mrle2 = buildRdt(SCAN_2_MRLE_2_MS, 2, ScanType.MRLE);
        long scan3MrleMs = SCAN_2_MRLE_1_MS + 7 * TimeUtil.MILLIS_PER_MINUTE;
        RadarDataTime scan3Mrle = buildRdt(scan3MrleMs, 3, ScanType.MRLE);

        RadarDataCubeAdapter.linkSailsMrleTimesToNormalScanTimes(
                List.of(scan0Sails, scan1Normal, scan1Sails, scan2Normal,
                        scan2Mrle1, scan2Mrle2, scan3Mrle));

        long minSailsOffset = SCAN_1_SAILS_MS - SCAN_1_NORMAL_MS;
        long minMrleOffset = SCAN_2_MRLE_1_MS - SCAN_2_NORMAL_MS;
        assertEquals(scan0SailsMs - minSailsOffset,
                scan0Sails.getNormalScanMatchRef());
        assertEquals(scan3MrleMs - minMrleOffset,
                scan3Mrle.getNormalScanMatchRef());
        /*
         * Ensure the offsets are different so that we are actually testing that
         * the right one is used
         */
        assertNotEquals(minSailsOffset, minMrleOffset);
    }

    @Test
    void testLinkSailsMrleTimesToNormalScanTimes3() {
        /*
         * Normal/SAILS combo for scan 1, and another normal/SAILS for scan 1 of
         * the following day -> each SAILS is linked to the correct day's normal
         * scan time
         */
        RadarDataTime scan1Normal = buildRdt(SCAN_1_NORMAL_MS, 1,
                ScanType.NORMAL);
        RadarDataTime scan1Sails = buildRdt(SCAN_1_SAILS_MS, 1, ScanType.SAILS);
        RadarDataTime nextDayScan1Normal = buildRdt(
                SCAN_1_NORMAL_MS + TimeUtil.MILLIS_PER_DAY, 1, ScanType.NORMAL);
        RadarDataTime nextDayScan1Sails = buildRdt(
                SCAN_1_SAILS_MS + TimeUtil.MILLIS_PER_DAY, 1, ScanType.SAILS);

        RadarDataCubeAdapter
                .linkSailsMrleTimesToNormalScanTimes(List.of(scan1Normal,
                        scan1Sails, nextDayScan1Normal, nextDayScan1Sails));

        assertEquals(SCAN_1_NORMAL_MS, scan1Sails.getNormalScanMatchRef());
        assertEquals(SCAN_1_NORMAL_MS + TimeUtil.MILLIS_PER_DAY,
                nextDayScan1Sails.getNormalScanMatchRef());
    }

    @Test
    void testLinkSailsMrleTimesToNormalScanTimes4() {
        /*
         * Normal/SAILS combo for KOAX scan 1, and normal/MRLE combo for KDMX
         * scan 1 -> each SAILS/MRLE is linked to the correct ICAO's normal scan
         * time
         */
        RadarDataTime scan1Normal = buildRdt(SCAN_1_NORMAL_MS, 1,
                ScanType.NORMAL);
        RadarDataTime scan1Sails = buildRdt(SCAN_1_SAILS_MS, 1, ScanType.SAILS);
        RadarDataTime kdmxScan1Normal = buildRdt(SCAN_2_NORMAL_MS, 1,
                ScanType.NORMAL, KDMX);
        RadarDataTime kdmxScan1Sails = buildRdt(SCAN_2_MRLE_1_MS, 1,
                ScanType.MRLE, KDMX);

        RadarDataCubeAdapter.linkSailsMrleTimesToNormalScanTimes(List
                .of(scan1Normal, scan1Sails, kdmxScan1Normal, kdmxScan1Sails));

        assertEquals(SCAN_1_NORMAL_MS, scan1Sails.getNormalScanMatchRef());
        assertEquals(SCAN_2_NORMAL_MS, kdmxScan1Sails.getNormalScanMatchRef());
    }

    @Test
    void testLinkSailsMrleTimesToNormalScanTimes5() {
        /*
         * Normal DataTime is passed in -> it's ignored and SAILS is still
         * linked to correct normal scan time
         */
        RadarDataTime rdtNormal = buildRdt(SCAN_1_NORMAL_MS, 1,
                ScanType.NORMAL);
        RadarDataTime rdtSails = buildRdt(SCAN_1_SAILS_MS, 1, ScanType.SAILS);
        DataTime dt = new DataTime(new Date(SCAN_2_NORMAL_MS));

        RadarDataCubeAdapter.linkSailsMrleTimesToNormalScanTimes(
                List.of(rdtNormal, dt, rdtSails));

        assertNull(rdtNormal.getNormalScanMatchRef());
        assertEquals(SCAN_1_NORMAL_MS, rdtSails.getNormalScanMatchRef());
    }

    @Test
    void testLinkSailsMrleTimesToNormalScanTimes6() {
        /*
         * SAILS scan without corresponding normal scan -> its normal scan time
         * is estimated based off the difference between the SAILS and normal
         * times for another volume scan for the same ICAO
         */
        RadarDataTime rdtSails1 = buildRdt(SCAN_1_SAILS_MS, 1, ScanType.SAILS);
        RadarDataTime rdtNormal2 = buildRdt(SCAN_2_NORMAL_MS, 2,
                ScanType.NORMAL);
        RadarDataTime rdtSails2 = buildRdt(SCAN_2_SAILS_MS, 2, ScanType.SAILS);

        RadarDataCubeAdapter.linkSailsMrleTimesToNormalScanTimes(
                List.of(rdtSails1, rdtNormal2, rdtSails2));

        long expectedScan1NormalMs = SCAN_1_SAILS_MS
                - (SCAN_2_SAILS_MS - SCAN_2_NORMAL_MS);
        assertEquals(expectedScan1NormalMs, rdtSails1.getNormalScanMatchRef());
    }

    @Test
    void testLinkSailsMrleTimesToNormalScanTimes7() {
        /*
         * SAILS scan without corresponding normal scan -> scans for a different
         * ICAO are ignored, so its normal scan time is left null
         */
        RadarDataTime rdtSails1 = buildRdt(SCAN_1_SAILS_MS, 1, ScanType.SAILS,
                KOAX);
        RadarDataTime rdtNormal2 = buildRdt(SCAN_2_NORMAL_MS, 2,
                ScanType.NORMAL, KDMX);
        RadarDataTime rdtSails2 = buildRdt(SCAN_2_SAILS_MS, 2, ScanType.SAILS,
                KDMX);

        RadarDataCubeAdapter.linkSailsMrleTimesToNormalScanTimes(
                List.of(rdtSails1, rdtNormal2, rdtSails2));

        assertNull(rdtSails1.getNormalScanMatchRef());
    }

    private static RadarDataTime buildRdt(long refMillis, int scanNum,
            ScanType scanType) {
        return buildRdt(refMillis, scanNum, scanType, KOAX);
    }

    private static RadarDataTime buildRdt(long refMillis, int scanNum,
            ScanType scanType, String icao) {
        RadarDataTime rdt = new RadarDataTime(
                new DataTime(new Date(refMillis)));
        rdt.setVolumeScanNumber(scanNum);
        rdt.setScanType(scanType);
        rdt.setIcao(icao);
        return rdt;
    }
}
