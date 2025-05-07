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
package com.raytheon.uf.viz.d2d.xy.adapters.crosssection;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.grid.radar.util.RadarVirtualVolumeStatus;

/**
 * Unit tests for {@link RadarCSFrameVirtualVolumeRenderable}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 23, 2024 2037631    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarCSFrameVirtualVolumeRenderable {

    private static final DataTime DT_1206 = new DataTime(
            "2024-01-01_12:06:00.0");

    private static final DataTime DT_1212 = new DataTime(
            "2024-01-01_12:12:00.0");

    static Stream<Arguments> provideParamsForGetExtraLegendText() {
        RadarVirtualVolumeStatus status = buildVirtualVolumeStatus(DT_1212, 1.5,
                DT_1206);

        return Stream.of(
                // Null status arg -> empty text
                Arguments.of(null, ""),
                // Valid status arg -> text generated
                Arguments.of(status, "(12:06Z above 1.5)"));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetExtraLegendText")
    void testGetExtraLegendText(RadarVirtualVolumeStatus status,
            String expectedText) {
        RadarCSFrameVirtualVolumeRenderable info = new RadarCSFrameVirtualVolumeRenderable(
                status, null);

        String text = info.getExtraLegendText();

        assertEquals(expectedText, text);
    }

    @Test
    void testConstructorSettingLine1() {
        RadarCSFrameVirtualVolumeRenderable info = new RadarCSFrameVirtualVolumeRenderable(
                null, null);

        assertNull(info.line);
    }

    @Test
    void testConstructorSettingLine2() {
        List<double[]> points = List.of(new double[] { 0, 0 },
                new double[] { 1, 1 });
        RadarCSFrameVirtualVolumeRenderable info = new RadarCSFrameVirtualVolumeRenderable(
                null, points);

        assertNotNull(info.line);
        assertEquals(points.size(), info.line.points.size());
        /*
         * DrawableLine adds z coord to points currently, just check that x/y
         * match
         */
        for (int i = 0; i < points.size(); ++i) {
            assertEquals(points.get(i)[0], info.line.points.get(i)[0]);
            assertEquals(points.get(i)[1], info.line.points.get(i)[1]);
        }
    }

    private static RadarVirtualVolumeStatus buildVirtualVolumeStatus(
            DataTime currScanTime, double currScanTilt, DataTime prevScanTime) {
        // This method just exists to let us keep the constructor protected
        RadarVirtualVolumeStatus status = mock(RadarVirtualVolumeStatus.class);
        lenient().when(status.getCurrScanTime()).thenReturn(currScanTime);
        when(status.getCurrScanTilt()).thenReturn(currScanTilt);
        when(status.getPrevScanTime()).thenReturn(prevScanTime);
        return status;
    }
}
