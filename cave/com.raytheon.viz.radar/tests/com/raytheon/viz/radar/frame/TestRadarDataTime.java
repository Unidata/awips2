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
package com.raytheon.viz.radar.frame;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Date;

import org.junit.jupiter.api.Test;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord.ScanType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Unit tests for {@link RadarDataTime}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2025 2038488    mapeters    Initial creation
 * Jun 05, 2025 2038858    mapeters    Test copy constructor and clone()
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarDataTime {

    private static final String TILT = "TILT";

    private static final String KOAX = "koax";

    private static final String KDMX = "kdmx";

    /** Feb 19 2025 21:20:00 */
    private static final long NORMAL_SCAN_MILLIS = 1_740_000_000_000L;

    /** Feb 19 2025 21:21:40 */
    private static final long SAILS_SCAN_MILLIS = 1_740_000_100_000L;

    /** Halfway between above normal and SAILS scans */
    private static final long IN_BETWEEN_SCAN_MILLIS = 1_740_000_050_000L;

    @Test
    void testGetMatchRef1() {
        // Normal scan time -> match ref is just that time
        RadarDataTime rdt = new RadarDataTime();
        rdt.setRefTime(new Date(NORMAL_SCAN_MILLIS));

        long actualMatchRef = rdt.getMatchRef();

        assertEquals(NORMAL_SCAN_MILLIS, actualMatchRef);
    }

    @Test
    void testGetMatchRef2() {
        /*
         * SAILS scan time with associated normal scan time set -> match ref is
         * halfway between SAILS and normal times
         */
        RadarDataTime rdt = new RadarDataTime();
        rdt.setRefTime(new Date(SAILS_SCAN_MILLIS));
        rdt.setNormalScanMatchRef(NORMAL_SCAN_MILLIS);

        long actualMatchRef = rdt.getMatchRef();

        assertEquals(IN_BETWEEN_SCAN_MILLIS, actualMatchRef);
    }

    @Test
    void testGetMatchValid1() {
        // Normal scan time -> match valid is just that time
        RadarDataTime rdt = new RadarDataTime();
        rdt.setRefTime(new Date(NORMAL_SCAN_MILLIS));

        long actualMatchValid = rdt.getMatchValid();

        assertEquals(NORMAL_SCAN_MILLIS, actualMatchValid);
    }

    @Test
    void testGetMatchValid2() {
        /*
         * SAILS scan time with associated normal scan time set -> match valid
         * is halfway between SAILS and normal times
         */
        RadarDataTime rdt = new RadarDataTime();
        rdt.setRefTime(new Date(SAILS_SCAN_MILLIS));
        rdt.setNormalScanMatchRef(NORMAL_SCAN_MILLIS);

        long actualMatchValid = rdt.getMatchValid();

        assertEquals(IN_BETWEEN_SCAN_MILLIS, actualMatchValid);
    }

    @Test
    void testIsSameScan1() {
        // Same ICAO and scan number and close times -> true
        RadarDataTime rdt1 = buildRdt(NORMAL_SCAN_MILLIS, 15, KOAX);
        RadarDataTime rdt2 = buildRdt(SAILS_SCAN_MILLIS, 15, KOAX);

        boolean isSameScan = rdt1.isSameScan(rdt2);
        boolean isSameScanReversed = rdt2.isSameScan(rdt1);

        assertTrue(isSameScan);
        assertTrue(isSameScanReversed);
    }

    @Test
    void testIsSameScan2() {
        // Same ICAO and close times but different scan numbers -> false
        RadarDataTime rdt1 = buildRdt(NORMAL_SCAN_MILLIS, 14, KOAX);
        RadarDataTime rdt2 = buildRdt(SAILS_SCAN_MILLIS, 15, KOAX);

        boolean isSameScan = rdt1.isSameScan(rdt2);
        boolean isSameScanReversed = rdt2.isSameScan(rdt1);

        assertFalse(isSameScan);
        assertFalse(isSameScanReversed);
    }

    @Test
    void testIsSameScan3() {
        // Same ICAO and scan number but far apart times -> false
        RadarDataTime rdt1 = buildRdt(NORMAL_SCAN_MILLIS, 15, KOAX);
        RadarDataTime rdt2 = buildRdt(
                NORMAL_SCAN_MILLIS + TimeUtil.MILLIS_PER_HOUR * 5, 15, KOAX);

        boolean isSameScan = rdt1.isSameScan(rdt2);
        boolean isSameScanReversed = rdt2.isSameScan(rdt1);

        assertFalse(isSameScan);
        assertFalse(isSameScanReversed);
    }

    @Test
    void testIsSameScan4() {
        // Same scan number and close times but different ICAOs -> false
        RadarDataTime rdt1 = buildRdt(NORMAL_SCAN_MILLIS, 15, KOAX);
        RadarDataTime rdt2 = buildRdt(NORMAL_SCAN_MILLIS, 15, KDMX);

        boolean isSameScan = rdt1.isSameScan(rdt2);
        boolean isSameScanReversed = rdt2.isSameScan(rdt1);

        assertFalse(isSameScan);
        assertFalse(isSameScanReversed);
    }

    @Test
    void testClone() {
        RadarDataTime rdt = new RadarDataTime();
        rdt.setRefTime(new Date(SAILS_SCAN_MILLIS));
        rdt.setLevel(0.5, TILT);
        rdt.setIcao(KDMX);
        rdt.setVolumeScanNumber(28);
        rdt.setElevationNumber(5);
        rdt.setScanType(ScanType.SAILS);
        rdt.setNormalScanMatchRef(NORMAL_SCAN_MILLIS);

        RadarDataTime clone = rdt.clone();

        assertEquals(new Date(SAILS_SCAN_MILLIS), clone.getRefTime());
        assertEquals(0.5, clone.getLevelValue());
        assertEquals(TILT, clone.getLevelType());
        assertEquals(KDMX, clone.getIcao());
        assertEquals(28, clone.getVolumeScanNumber());
        assertEquals(5, clone.getElevationNumber());
        assertEquals(ScanType.SAILS, clone.getScanType());
        assertEquals(NORMAL_SCAN_MILLIS, clone.getNormalScanMatchRef());
    }

    @Test
    void testCopyConstructor1() {
        // Pass RadarDataTime -> all fields are copied
        RadarDataTime rdt = new RadarDataTime();
        rdt.setRefTime(new Date(SAILS_SCAN_MILLIS));
        rdt.setLevel(0.5, TILT);
        rdt.setIcao(KDMX);
        rdt.setVolumeScanNumber(28);
        rdt.setElevationNumber(5);
        rdt.setScanType(ScanType.SAILS);
        rdt.setNormalScanMatchRef(NORMAL_SCAN_MILLIS);

        RadarDataTime copy = new RadarDataTime(rdt);

        assertEquals(new Date(SAILS_SCAN_MILLIS), copy.getRefTime());
        assertEquals(0.5, copy.getLevelValue());
        assertEquals(TILT, copy.getLevelType());
        assertEquals(KDMX, copy.getIcao());
        assertEquals(28, copy.getVolumeScanNumber());
        assertEquals(5, copy.getElevationNumber());
        assertEquals(ScanType.SAILS, copy.getScanType());
        assertEquals(NORMAL_SCAN_MILLIS, copy.getNormalScanMatchRef());
    }

    @Test
    void testCopyConstructor2() {
        // Pass DataTime -> only superclass fields are copied
        DataTime dt = new DataTime();
        dt.setRefTime(new Date(NORMAL_SCAN_MILLIS));
        dt.setLevel(1.5, TILT);

        RadarDataTime copy = new RadarDataTime(dt);

        assertEquals(new Date(NORMAL_SCAN_MILLIS), copy.getRefTime());
        assertEquals(1.5, copy.getLevelValue());
        assertEquals(TILT, copy.getLevelType());
    }

    private static RadarDataTime buildRdt(long refMillis, int scanNum,
            String icao) {
        RadarDataTime rdt = new RadarDataTime(
                new DataTime(new Date(refMillis)));
        rdt.setVolumeScanNumber(scanNum);
        rdt.setIcao(icao);
        return rdt;
    }
}
