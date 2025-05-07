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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.RETURNS_MOCKS;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.grid.radar.RadarVirtualTimeAndSpace;

/**
 * Unit tests for {@link RadarVirtualVolumeStatus}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2024 2037624    mapeters    Initial creation
 * Aug 20, 2024 2037631    mapeters    Current scan time is now stored too
 *
 * </pre>
 *
 * @author mapeters
 */
@ExtendWith(MockitoExtension.class)
class TestRadarVirtualVolumeStatus {

    private final DataTime dt1206 = new DataTime("2024-01-01_12:06:00.0");

    private final DataTime dt1212 = new DataTime("2024-01-01_12:12:00.0");

    @Mock
    private IGridGeometryProvider space;

    @Test
    void testBuild1() {
        // Pass in time that has no records -> null
        List<AbstractRequestableData> dataList = List.of(buildData(dt1212, 0.5),
                buildData(dt1212, 1.5));

        RadarVirtualVolumeStatus status = RadarVirtualVolumeStatus
                .build(dataList, dt1206);

        assertNull(status);
    }

    @Test
    void testBuild2() {
        // No virtual records -> null
        List<AbstractRequestableData> dataList = List.of(buildData(dt1212, 0.5),
                buildData(dt1212, 1.5));

        RadarVirtualVolumeStatus status = RadarVirtualVolumeStatus
                .build(dataList, dt1212);

        assertNull(status);
    }

    @Test
    void testBuild3() {
        // 12:12 uses 12:06 records for 2.4/3.4 tilt -> fields set correctly
        List<AbstractRequestableData> dataList = List.of(buildData(dt1206, 2.4),
                buildData(dt1206, 3.4), buildData(dt1212, 0.5),
                buildData(dt1212, 1.5), buildVirtualData(dt1206, dt1212, 2.4),
                buildVirtualData(dt1206, dt1212, 3.4));

        RadarVirtualVolumeStatus status = RadarVirtualVolumeStatus
                .build(dataList, dt1212);

        assertEquals(dt1212, status.getCurrScanTime());
        assertEquals(dt1206, status.getPrevScanTime());
        assertEquals(1.5, status.getCurrScanTilt());
    }

    @Test
    void testBuild4() {
        /*
         * Same as above test but pass in a spatial data time -> fields still
         * set correctly, including that the current scan time is still
         * non-spatial
         */
        List<AbstractRequestableData> dataList = List.of(buildData(dt1206, 2.4),
                buildData(dt1206, 3.4), buildData(dt1212, 0.5),
                buildData(dt1212, 1.5), buildVirtualData(dt1206, dt1212, 2.4),
                buildVirtualData(dt1206, dt1212, 3.4));
        DataTime dt1212spatial = dt1212.clone();
        dt1212spatial.setLevel(0d, "TYPE");

        RadarVirtualVolumeStatus status = RadarVirtualVolumeStatus
                .build(dataList, dt1212spatial);

        assertEquals(dt1212, status.getCurrScanTime());
        assertEquals(dt1206, status.getPrevScanTime());
        assertEquals(1.5, status.getCurrScanTilt());
    }

    @Test
    void testBuild5() {
        // Setup virtual records for 12:12, but pass in 12:06 -> null
        List<AbstractRequestableData> dataList = List.of(buildData(dt1206, 2.4),
                buildData(dt1206, 3.4), buildData(dt1212, 0.5),
                buildData(dt1212, 1.5), buildVirtualData(dt1206, dt1212, 2.4),
                buildVirtualData(dt1206, dt1212, 3.4));

        RadarVirtualVolumeStatus status = RadarVirtualVolumeStatus
                .build(dataList, dt1206);

        assertNull(status);
    }

    private AbstractRequestableData buildData(DataTime dt, double tilt) {
        AbstractRequestableData data = mock(AbstractRequestableData.class,
                RETURNS_MOCKS);
        when(data.getTimeAndSpace()).thenReturn(new TimeAndSpace(dt, space));
        lenient().when(data.getLevel()).thenReturn(new Level(tilt + "TILT"));
        return data;
    }

    private AbstractRequestableData buildVirtualData(DataTime realDt,
            DataTime virtualDt, double tilt) {
        AbstractRequestableData data = mock(AbstractRequestableData.class,
                RETURNS_MOCKS);
        when(data.getTimeAndSpace()).thenReturn(
                new RadarVirtualTimeAndSpace(virtualDt, space, realDt));
        lenient().when(data.getLevel()).thenReturn(new Level(tilt + "TILT"));
        return data;
    }
}
