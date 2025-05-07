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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.RETURNS_MOCKS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedStatic;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.grid.radar.RadarVirtualTimeAndSpace;
import com.raytheon.viz.grid.record.RequestableDataRecord;

/**
 * Unit tests for {@link RadarVirtualVolumeRefreshListener}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2024 2037624    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarVirtualVolumeRefreshListener {

    private static MockedStatic<RadarVirtualVolumeUtil> utilMockedStatic;

    private final DataTime dt1200 = new DataTime("2024-01-01_12:00:00.0");

    private final DataTime dt1206 = new DataTime("2024-01-01_12:06:00.0");

    private final DataTime dt1212 = new DataTime("2024-01-01_12:12:00.0");

    private final RequestableDataRecord record1200 = buildRecord(dt1200, 1.5);

    private final RequestableDataRecord record1206 = buildRecord(dt1206, 1.5);

    private final RequestableDataRecord record1212 = buildRecord(dt1212, 1.5);

    /** Record with virtual time of 12:06Z and real time of 12:00Z */
    private final RequestableDataRecord record1206virt = buildVirtualRecord(
            dt1200, dt1206, 1.5);

    /** Record with virtual time of 12:12Z and real time of 12:06Z */
    private final RequestableDataRecord record1212virt = buildVirtualRecord(
            dt1206, dt1212, 1.5);

    @Mock
    private IGridGeometryProvider space;

    @BeforeAll
    static void setupBeforeAll() {
        utilMockedStatic = mockStatic(RadarVirtualVolumeUtil.class,
                CALLS_REAL_METHODS);
    }

    @AfterAll
    static void tearDownAfterAll() {
        utilMockedStatic.close();
    }

    @Test
    void testRefreshInternal1() {
        // No times/records -> nothing to refresh
        AbstractVizResource<?, ?> rsc = mock(AbstractVizResource.class);
        when(rsc.getDataTimes()).thenReturn(new DataTime[0]);
        RadarVirtualVolumeRefreshListener listener = new RadarVirtualVolumeRefreshListener(
                rsc, List::of);

        listener.refreshInternal();

        utilMockedStatic.verify(
                () -> RadarVirtualVolumeUtil.reloadFrames(any(), any()),
                never());
    }

    @Test
    void testRefreshInternal2() {
        /*
         * The refresh listener ensures that only the latest time uses the
         * virtual volume concept. Setup the resource with virtual records for
         * the two latest times, and verify that the second-latest time is
         * reloaded.
         */
        AbstractVizResource<?, ?> rsc = mock(AbstractVizResource.class);
        when(rsc.getDataTimes())
                .thenReturn(new DataTime[] { dt1200, dt1206, dt1212 });
        Supplier<Collection<? extends PluginDataObject>> pdoSupplier = () -> List
                .of(record1200, record1206, record1206virt, record1212,
                        record1212virt);
        RadarVirtualVolumeRefreshListener listener = new RadarVirtualVolumeRefreshListener(
                rsc, pdoSupplier);

        listener.refreshInternal();

        utilMockedStatic.verify(
                () -> RadarVirtualVolumeUtil.reloadFrames(Set.of(dt1206), rsc));
    }

    @Test
    void testRefreshInternal3() {
        /*
         * The refresh listener ensures that only the latest time uses the
         * virtual volume concept. Setup the resource with virtual records for
         * the two latest times, and verify that the second-latest time is
         * reloaded. This is the same as the previous test, except that the
         * resource's dataTimes have a spatial level set on them and the PDOs'
         * times don't, but we still need to reload the matching frame times.
         */
        List<DataTime> spatialFrameTimes = new ArrayList<>();
        for (DataTime time : List.of(dt1200, dt1206, dt1212)) {
            spatialFrameTimes.add(getSpatialTime(time, 0));
            spatialFrameTimes.add(getSpatialTime(time, 1));
        }
        AbstractVizResource<?, ?> rsc = mock(AbstractVizResource.class);
        when(rsc.getDataTimes())
                .thenReturn(spatialFrameTimes.toArray(DataTime[]::new));
        Supplier<Collection<? extends PluginDataObject>> pdoSupplier = () -> List
                .of(record1200, record1206, record1206virt, record1212,
                        record1212virt);
        RadarVirtualVolumeRefreshListener listener = new RadarVirtualVolumeRefreshListener(
                rsc, pdoSupplier);

        listener.refreshInternal();

        Set<DataTime> expectedReloadedFrameTimes = Set
                .of(getSpatialTime(dt1206, 0), getSpatialTime(dt1206, 1));
        utilMockedStatic.verify(() -> RadarVirtualVolumeUtil
                .reloadFrames(expectedReloadedFrameTimes, rsc));
    }

    private RequestableDataRecord buildRecord(DataTime dt, double tilt) {
        RequestableDataRecord record = mock(RequestableDataRecord.class,
                RETURNS_MOCKS);
        when(record.getDataTime()).thenReturn(dt);
        when(record.getTimeAndSpace()).thenReturn(new TimeAndSpace(dt, space));
        return record;
    }

    private RequestableDataRecord buildVirtualRecord(DataTime realDt,
            DataTime virtualDt, double tilt) {
        RequestableDataRecord record = mock(RequestableDataRecord.class,
                RETURNS_MOCKS);
        when(record.getDataTime()).thenReturn(virtualDt);
        when(record.getTimeAndSpace())
                .thenReturn(new RadarVirtualTimeAndSpace(virtualDt, space, realDt));
        return record;
    }

    private static DataTime getSpatialTime(DataTime time, double level) {
        time = time.clone();
        time.setLevel(level, "TYPE");
        return time;
    }
}
