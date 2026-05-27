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
package com.raytheon.uf.viz.grid.radar;

import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.Mockito.RETURNS_MOCKS;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * Unit tests for {@link RadarVolumeScanTracker}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2024 2037092    mapeters    Initial creation
 * Oct 14, 2024 2037939    mapeters    Minor cleanup
 *
 * </pre>
 *
 * @author mapeters
 */
@ExtendWith(MockitoExtension.class)
class TestRadarVolumeScanTracker {

    private final DataTime dtPrev = new DataTime("2024-01-01_12:00:00.0");

    private final DataTime dtCurr = new DataTime("2024-01-01_12:06:00.0");

    private RadarVolumeScanTracker tracker;

    private static Map<String, Object> buildTimeTiltMap(DataTime time,
            double tilt) {
        return Map.of(PluginDataObject.DATATIME_ID, time,
                RadarAdapter.TILT_QUERY, tilt);
    }

    protected DbQueryResponse buildDbQueryResponse() {
        List<Map<String, Object>> list = new ArrayList<>();

        list.add(buildTimeTiltMap(dtPrev, 0.5));
        list.add(buildTimeTiltMap(dtPrev, 1.5));
        list.add(buildTimeTiltMap(dtPrev, 2.4));
        list.add(buildTimeTiltMap(dtCurr, 0.5));

        DbQueryResponse resp = new DbQueryResponse();
        resp.setResults(list);
        return resp;
    }

    @BeforeEach
    public void setupBeforeEach() {
        try (MockedStatic<ProductAlertObserver> paoMockedStatic = mockStatic(
                ProductAlertObserver.class);
                MockedStatic<RequestRouter> rrMockedStatic = mockStatic(
                        RequestRouter.class)) {
            // These just disable things that would throw exceptions
            paoMockedStatic
                    .when(() -> ProductAlertObserver.addObserver(any(), any()))
                    .then(invocation -> null);
            rrMockedStatic.when(() -> RequestRouter.route(any()))
                    .thenReturn(new DbQueryResponse());

            tracker = new RadarVolumeScanTracker("koax",
                    Set.of(153, 94, 20, 19));
        }

        tracker = spy(tracker);
    }

    @Test
    public void testLoadVolumeScans() {
        // Test that scan times and latest tilt are populated correctly
        DbQueryResponse resp = buildDbQueryResponse();
        doReturn(resp).when(tracker).queryTimesAndTilts();

        tracker.loadVolumeScans();

        Set<DataTime> times = new TreeSet<>();
        times.add(dtPrev);
        times.add(dtCurr);

        assertEquals(times, tracker.scanTimes);
        assertEquals(0.5, tracker.latestTilt);
    }

    @Test
    public void testGetVirtualVolumeInfo1() {
        /*
         * Prev scan, current scan, and latest tilt available -> virtual volume
         * info returned
         */
        tracker.lastQueryTime = System.currentTimeMillis();
        tracker.scanTimes.add(dtPrev);
        tracker.scanTimes.add(dtCurr);
        tracker.latestTilt = 0.5;

        VirtualVolumeInfo result = tracker.getVirtualVolumeInfo();

        assertEquals(result.getPrevVolumeScan(), dtPrev);
        assertEquals(result.getLatestVolumeScan(), dtCurr);
        assertEquals(result.getLatestTilt(), 0.5);
        verify(tracker, never()).loadVolumeScans();
    }

    @Test
    public void testGetVirtualVolumeInfo2() {
        // Only 1 scan available -> null returned
        tracker.lastQueryTime = System.currentTimeMillis();
        tracker.scanTimes.add(dtPrev);
        tracker.latestTilt = 0.5;

        VirtualVolumeInfo result = tracker.getVirtualVolumeInfo();

        assertNull(result);
        verify(tracker, never()).loadVolumeScans();
    }

    @Test
    public void testGetVirtualVolumeInfo3() {
        // No scans/tilt available -> null returned
        tracker.lastQueryTime = System.currentTimeMillis();

        VirtualVolumeInfo result = tracker.getVirtualVolumeInfo();

        assertNull(result);
    }

    @Test
    public void testGetVirtualVolumeInfo4() {
        // Last query time was too long ago -> loadVolumeScans is called
        tracker.lastQueryTime = Long.MIN_VALUE;
        doNothing().when(tracker).loadVolumeScans();

        VirtualVolumeInfo result = tracker.getVirtualVolumeInfo();

        assertNull(result);
        verify(tracker).loadVolumeScans();
    }

    @Test
    public void testAlertArrived() {
        // alertArrived called -> processAlert called for each alert
        AlertMessage alert1 = mock(AlertMessage.class);
        AlertMessage alert2 = mock(AlertMessage.class);
        doNothing().when(tracker).processAlert(any());

        tracker.alertArrived(List.of(alert1, alert2));

        verify(tracker).processAlert(alert1);
        verify(tracker).processAlert(alert2);
    }

    @Test
    public void testProcessAlert1() {
        /*
         * icao/product code match and valid time/tilt -> processValidAlert
         * called
         */
        AlertMessage alert = new AlertMessage();
        alert.decodedAlert = Map.of(RadarAdapter.ICAO_QUERY, "koax",
                RadarAdapter.PRODUCT_CODE_QUERY, 153,
                PluginDataObject.DATATIME_ID, dtCurr, RadarAdapter.TILT_QUERY,
                0.5);

        tracker.processAlert(alert);

        verify(tracker).processValidAlert(dtCurr, 0.5);
    }

    @Test
    public void testProcessAlert2() {
        // icao doesn't match -> processValidAlert not called
        AlertMessage alert = new AlertMessage();
        alert.decodedAlert = Map.of(RadarAdapter.ICAO_QUERY, "kdmx",
                RadarAdapter.PRODUCT_CODE_QUERY, 153,
                PluginDataObject.DATATIME_ID, dtCurr, RadarAdapter.TILT_QUERY,
                0.5);

        tracker.processAlert(alert);

        verify(tracker, never()).processValidAlert(any(), anyDouble());
    }

    @Test
    public void testProcessAlert3() {
        // product code doesn't match -> processValidAlert not called
        AlertMessage alert = new AlertMessage();
        alert.decodedAlert = Map.of(RadarAdapter.ICAO_QUERY, "koax",
                RadarAdapter.PRODUCT_CODE_QUERY, 0,
                PluginDataObject.DATATIME_ID, dtCurr, RadarAdapter.TILT_QUERY,
                0.5);

        tracker.processAlert(alert);

        verify(tracker, never()).processValidAlert(any(), anyDouble());
    }

    @Test
    public void testProcessAlert4() {
        // missing data time -> processValidAlert not called
        AlertMessage alert = new AlertMessage();
        alert.decodedAlert = Map.of(RadarAdapter.ICAO_QUERY, "koax",
                RadarAdapter.PRODUCT_CODE_QUERY, 153, RadarAdapter.TILT_QUERY,
                0.5);

        tracker.processAlert(alert);

        verify(tracker, never()).processValidAlert(any(), anyDouble());
    }

    @Test
    public void testProcessAlert5() {
        // missing tilt -> processValidAlert not called
        AlertMessage alert = new AlertMessage();
        alert.decodedAlert = Map.of(RadarAdapter.ICAO_QUERY, "koax",
                RadarAdapter.PRODUCT_CODE_QUERY, 153,
                PluginDataObject.DATATIME_ID, dtCurr);

        tracker.processAlert(alert);

        verify(tracker, never()).processValidAlert(any(), anyDouble());
    }

    @Test
    public void testProcessValidAlert1() {
        // Process valid alert with empty times -> last query time reset
        tracker.lastQueryTime = 0;

        tracker.processValidAlert(dtCurr, 0.5);

        assertEquals(Long.MIN_VALUE, tracker.lastQueryTime);
    }

    @Test
    public void testProcessValidAlert2() {
        /*
         * Process valid alert with time that doesn't match latest -> last query
         * time reset
         */
        tracker.lastQueryTime = 0;
        tracker.scanTimes.add(dtPrev);

        tracker.processValidAlert(dtCurr, 0.5);

        assertEquals(Long.MIN_VALUE, tracker.lastQueryTime);
    }

    @Test
    public void testProcessValidAlert3() {
        /*
         * Process valid alert with time that matches latest -> latest tilt
         * updated and last query time NOT reset
         */
        tracker.lastQueryTime = 0;
        tracker.scanTimes.add(dtCurr);
        tracker.latestTilt = 0.5;

        tracker.processValidAlert(dtCurr, 1.5);

        assertEquals(0, tracker.lastQueryTime);
        assertEquals(tracker.latestTilt, 1.5);
    }

    @ParameterizedTest
    @CsvSource({
            // Icao and param match -> same instance
            "koax,RR,koax,RR,true",
            // Icao matches but param doesn't -> different instances
            "koax,RR,koax,RRV,false",
            // Param matches but icao doesn't -> different instances
            "koax,RR,kdmx,RR,false" })
    void testGetInstance(String icao1, String param1, String icao2,
            String param2, boolean expectedSame) {
        /*
         * Test that two calls to getInstance() return the same or different
         * instance, depending on if the passed args match
         */
        RadarVolumeScanTracker tracker1, tracker2;
        try (MockedStatic<RadarProductCodeMapping> pcMapStaticMock = mockStatic(
                RadarProductCodeMapping.class);
                MockedConstruction<RadarVolumeScanTracker> trackerMockedConstruction = mockConstruction(
                        RadarVolumeScanTracker.class)) {
            pcMapStaticMock.when(RadarProductCodeMapping::getInstance)
                    .then(RETURNS_MOCKS);
            tracker1 = RadarVolumeScanTracker.getInstance(icao1, param1);
            tracker2 = RadarVolumeScanTracker.getInstance(icao2, param2);
        }

        assertEquals(expectedSame, tracker1 == tracker2);
    }
}
