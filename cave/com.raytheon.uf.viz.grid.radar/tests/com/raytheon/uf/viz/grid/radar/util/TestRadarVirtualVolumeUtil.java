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
package com.raytheon.uf.viz.grid.radar.util;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.raytheon.uf.common.time.DataTime;

/**
 * Unit tests for {@link RadarVirtualVolumeUtil}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2024 2037624    mapeters    Initial creation
 * Aug 14, 2024 2037631    mapeters    Handle RadarVirtualVolumeStatus
 *                                     constructor update
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarVirtualVolumeUtil {

    private static final String DT_1200 = "2024-01-01_12:00:00.0";

    private static final String DT_1206 = "2024-01-01_12:06:00.0";

    private final DataTime dt1200 = new DataTime(DT_1200);

    private final DataTime dt1206 = new DataTime(DT_1206);

    @Test
    void testBuildLegendText_fromTimeAndTilt1() {
        // Null time -> empty text
        String text = RadarVirtualVolumeUtil.buildLegendText(1.5d, null);

        assertEquals("", text);
    }

    @Test
    void testBuildLegendText_fromTimeAndTilt2() {
        // Null tilt -> empty text
        String text = RadarVirtualVolumeUtil.buildLegendText(null, dt1200);

        assertEquals("", text);
    }

    @Test
    void testBuildLegendText_fromTimeAndTilt3() {
        // Valid time/tilt -> properly formatted text
        String text = RadarVirtualVolumeUtil.buildLegendText(1.5d, dt1200);

        assertEquals("(12:00Z above 1.5)", text);
    }

    @Test
    void testBuildLegendText_fromStatus1() {
        // Null status -> empty text
        String text = RadarVirtualVolumeUtil.buildLegendText(null);

        assertEquals("", text);
    }

    @Test
    void testBuildLegendText_fromStatus2() {
        // Valid status -> properly formatted text
        RadarVirtualVolumeStatus status = new RadarVirtualVolumeStatus(dt1206,
                0.5, dt1200);
        String text = RadarVirtualVolumeUtil.buildLegendText(status);

        assertEquals("(12:00Z above 0.5)", text);
    }
}
