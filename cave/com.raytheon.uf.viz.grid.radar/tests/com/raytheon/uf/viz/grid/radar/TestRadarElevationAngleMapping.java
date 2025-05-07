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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.grid.radar.RadarElevationAngleMapping.VcpTime;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * Unit tests for {@link RadarElevationAngleMapping}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2024 2037939    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarElevationAngleMapping {

    private static final DataTime dt_30 = new DataTime("2024-01-01_12:30:00.0");

    private static final DataTime dt_36 = new DataTime("2024-01-01_12:36:00.0");

    private static final DataTime dt_39 = new DataTime("2024-01-01_12:39:00.0");

    private static final DataTime dt_42 = new DataTime("2024-01-01_12:42:00.0");

    private static final DataTime dt_48 = new DataTime("2024-01-01_12:48:00.0");

    private RadarElevationAngleMapping mapping;

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

            /*
             * Use constructor instead of getInstance so that each test starts
             * with a new instance
             */
            mapping = new RadarElevationAngleMapping("koax");
        }

        mapping = spy(mapping);
    }

    @Test
    public void testAlertArrived() {
        // alertArrived called -> processAlert called for each alert
        AlertMessage alert1 = mock(AlertMessage.class);
        AlertMessage alert2 = mock(AlertMessage.class);
        doNothing().when(mapping).processAlert(any());

        mapping.alertArrived(List.of(alert1, alert2));

        verify(mapping).processAlert(alert1);
        verify(mapping).processAlert(alert2);
    }

    @Test
    public void testProcessAlert1() {
        // icao matches and valid time/tilt -> processValidAlert called
        AlertMessage alert = new AlertMessage();
        alert.decodedAlert = Map.of(RadarAdapter.ICAO_QUERY, "koax",
                PluginDataObject.DATATIME_ID, dt_36, RadarAdapter.TILT_QUERY,
                0.5);

        mapping.processAlert(alert);

        verify(mapping).processValidAlert(dt_36, 0.5);
    }

    @Test
    public void testProcessAlert2() {
        // icao doesn't match -> processValidAlert not called
        AlertMessage alert = new AlertMessage();
        alert.decodedAlert = Map.of(RadarAdapter.ICAO_QUERY, "kdmx",
                PluginDataObject.DATATIME_ID, dt_36, RadarAdapter.TILT_QUERY,
                0.5);

        mapping.processAlert(alert);

        verify(mapping, never()).processValidAlert(any(), anyDouble());
    }

    @Test
    public void testProcessAlert4() {
        // missing data time -> processValidAlert not called
        AlertMessage alert = new AlertMessage();
        alert.decodedAlert = Map.of(RadarAdapter.ICAO_QUERY, "koax",
                RadarAdapter.TILT_QUERY, 0.5);

        mapping.processAlert(alert);

        verify(mapping, never()).processValidAlert(any(), anyDouble());
    }

    @Test
    public void testProcessAlert5() {
        // missing tilt -> processValidAlert not called
        AlertMessage alert = new AlertMessage();
        alert.decodedAlert = Map.of(RadarAdapter.ICAO_QUERY, "koax",
                PluginDataObject.DATATIME_ID, dt_36);

        mapping.processAlert(alert);

        verify(mapping, never()).processValidAlert(any(), anyDouble());
    }

    @Test
    public void testProcessValidAlert1() {
        // Process valid alert with empty times -> last query time reset
        mapping.lastQueryTime = 0;

        mapping.processValidAlert(dt_36, 0.5);

        assertEquals(Long.MIN_VALUE, mapping.lastQueryTime);
    }

    @Test
    public void testProcessValidAlert2() {
        /*
         * Process valid alert with VCP angle mapping but time that doesn't
         * match latest -> last query time reset
         */
        mapping.lastQueryTime = 0;
        mapping.addVcpTime(1, dt_30);
        mapping.vcpPrimaryToTrueMap.put(1, Map.of(1.5, 1.5f));

        mapping.processValidAlert(dt_36, 1.5);

        assertEquals(Long.MIN_VALUE, mapping.lastQueryTime);
    }

    @Test
    public void testProcessValidAlert3() {
        /*
         * Process valid alert with time that matches latest but no VCP angle
         * mapping -> last query time reset
         */
        mapping.lastQueryTime = 0;
        mapping.addVcpTime(1, dt_36);

        mapping.processValidAlert(dt_36, 1.5);

        assertEquals(Long.MIN_VALUE, mapping.lastQueryTime);
    }

    @Test
    public void testProcessValidAlert4() {
        /*
         * Process valid alert with time that matches latest and VCP angle
         * mapping -> query time NOT reset, nothing else done
         */
        mapping.lastQueryTime = 0;
        mapping.addVcpTime(1, dt_36);
        mapping.vcpPrimaryToTrueMap.put(1, Map.of(1.5, 1.5f));

        mapping.processValidAlert(dt_36, 1.5);

        assertEquals(0, mapping.lastQueryTime);
    }

    @ParameterizedTest
    @CsvSource({
            // Icaos match -> same instance
            "koax,koax,true",
            // Icaos don't match -> different instances
            "koax,kdmx,false" })
    void testGetInstance(String icao1, String icao2, boolean expectedSame) {
        /*
         * Test that two calls to getInstance() return the same or different
         * instance, depending on if the passed args match
         */
        RadarElevationAngleMapping mapping1, mapping2;
        try (MockedConstruction<RadarElevationAngleMapping> mappingMockedConstruction = mockConstruction(
                RadarElevationAngleMapping.class)) {
            mapping1 = RadarElevationAngleMapping.getInstance(icao1);
            mapping2 = RadarElevationAngleMapping.getInstance(icao2);
        }

        assertEquals(expectedSame, mapping1 == mapping2);
    }

    @Test
    void testGetTrueElevationAngle1() {
        /*
         * Setup 1.5->1.3 mapping and verify that mapping is used. Also verify
         * mapping isn't reloaded when last query time is very recent.
         */
        mapping.lastQueryTime = System.currentTimeMillis();
        mapping.vcpTimes.add(new VcpTime(1, dt_36));
        mapping.vcpPrimaryToTrueMap.put(1, Map.of(1.5, 1.3f));

        double trueAngle = mapping.getTrueElevationAngle(1.5, dt_36);

        assertEquals(1.3f, trueAngle);
        verify(mapping, never()).reloadMapping();
    }

    @Test
    void testGetTrueElevationAngle2() {
        /*
         * Verify that we fallback to the primary elevation angle when no
         * mapping exists for the requested angle. Also verify mapping isn't
         * reloaded when last query time is very recent.
         */
        mapping.lastQueryTime = System.currentTimeMillis();

        double trueAngle = mapping.getTrueElevationAngle(1.5, dt_36);

        assertEquals(1.5f, trueAngle);
        verify(mapping, never()).reloadMapping();
    }

    @Test
    void testGetTrueElevationAngle3() {
        // Last query time was too long ago -> loadMapping is called
        mapping.lastQueryTime = Long.MIN_VALUE;
        doNothing().when(mapping).reloadMapping();

        mapping.getTrueElevationAngle(0.5, dt_36);

        verify(mapping).reloadMapping();
    }

    @Test
    void testLoadMapping() {
        mapping.lastQueryTime = 0;
        RadarRecord record1 = new RadarRecord();
        record1.setVolumeCoveragePattern(2);
        record1.setPrimaryElevationAngle(3.4);
        record1.setTrueElevationAngle(3.1f);
        record1.setDataTime(dt_36);
        DbQueryResponse response = mock(DbQueryResponse.class);
        when(response.getEntityObjects(RadarRecord.class))
                .thenReturn(new RadarRecord[] { record1 });
        doReturn(response).when(mapping).queryIcaoRecords();
        mapping.vcpTimes.add(new VcpTime(1, dt_36));
        mapping.vcpPrimaryToTrueMap.put(1, Map.of(1.5, 1.3f));
        /*
         * These asserts just check that we set things up right so that the
         * below asserts are actually verifying that the mapping was reloaded
         */
        assertNull(mapping.getTrueAngle(mapping.getVcp(dt_36), 3.4));
        assertEquals(1.3f, mapping.getTrueAngle(mapping.getVcp(dt_36), 1.5));

        mapping.reloadMapping();

        assertTrue(mapping.lastQueryTime > 0);
        assertEquals(3.1f, mapping.getTrueElevationAngle(3.4, dt_36));
        assertNull(mapping.getTrueAngle(mapping.getVcp(dt_36), 1.5));
    }

    static Stream<Arguments> provideParamsForGetVcp() {
        List<VcpTime> vcpTimes36 = List.of(new VcpTime(1, dt_36));
        List<VcpTime> vcpTimes36_42 = List.of(new VcpTime(1, dt_36),
                new VcpTime(2, dt_42));

        return Stream.of(
                // Empty VCP times list -> -1
                Arguments.of(List.of(), dt_36, -1),

                /*
                 * Single entry in VCP times list -> its VCP is returned
                 * regardless of time
                 */
                Arguments.of(vcpTimes36, dt_30, 1),
                Arguments.of(vcpTimes36, dt_36, 1),
                Arguments.of(vcpTimes36, dt_42, 1),

                /*
                 * Multiple entries in VCP times list -> correct VCP is returned
                 * based on requested time
                 */
                Arguments.of(vcpTimes36_42, dt_30, 1),
                Arguments.of(vcpTimes36_42, dt_36, 1),
                Arguments.of(vcpTimes36_42, dt_39, 2),
                Arguments.of(vcpTimes36_42, dt_42, 2),
                Arguments.of(vcpTimes36_42, dt_48, 2));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetVcp")
    void testGetVcp(List<VcpTime> vcpTimes, DataTime time, int expectedVcp) {
        mapping.vcpTimes.addAll(vcpTimes);

        int vcp = mapping.getVcp(time);

        assertEquals(expectedVcp, vcp);
    }

    static Stream<Arguments> provideParamsForGetTrueAngle() {
        return Stream.of(
                // VCP not in map -> null
                Arguments.of(3, 1.5, null),
                // Angle not in map -> null
                Arguments.of(1, 0.5, null),
                // Get appropriate true angle for VCP 1
                Arguments.of(1, 1.5, 1.5f),
                // Get appropriate true angle for VCP 2
                Arguments.of(2, 1.5, 1.3f));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetTrueAngle")
    void testGetTrueAngle(int vcp, double primaryAngle,
            Float expectedTrueAngle) {
        mapping.vcpPrimaryToTrueMap.put(1, Map.of(1.5, 1.5f));
        mapping.vcpPrimaryToTrueMap.put(2, Map.of(1.5, 1.3f));

        Float trueAngle = mapping.getTrueAngle(vcp, primaryAngle);

        assertEquals(expectedTrueAngle, trueAngle);
    }

    static Stream<Arguments> provideParamsForAddVcpAngleMapping() {
        return Stream.of(
                /*
                 * Add 3.4->3.1 mapping to VCP 1 that already has 1.5->1.3
                 * mapping
                 */
                Arguments.of(1, 3.4, 3.1f,
                        Map.of(1, Map.of(1.5, 1.3f, 3.4, 3.1f))),
                // Add 3.4->3.1 mapping to new VCP 2
                Arguments.of(2, 3.4, 3.1f,
                        Map.of(1, Map.of(1.5, 1.3f), 2, Map.of(3.4, 3.1f))));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForAddVcpAngleMapping")
    void testAddVcpAngleMapping(int vcp, double primaryAngle, float trueAngle,
            Map<Integer, Map<Double, Double>> expectedMap) {
        mapping.vcpPrimaryToTrueMap.put(1, new HashMap<>(Map.of(1.5, 1.3f)));

        mapping.addVcpAngleMapping(vcp, primaryAngle, trueAngle);

        assertEquals(expectedMap, mapping.vcpPrimaryToTrueMap);
    }

    static Stream<Arguments> provideParamsForAddVcpTime() {
        return Stream.of(
                // Add VCP/time combo to empty list -> that VCP/time is added
                Arguments.of(List.of(), 1, dt_30, buildVcpTimes(1, dt_30)),
                /*
                 * Add VCP/time for same VCP as last stored entry -> last
                 * entry's time gets updated
                 */
                Arguments.of(buildVcpTimes(1, dt_30), 1, dt_36,
                        buildVcpTimes(1, dt_36)),
                /*
                 * Add VCP/time for same VCP as last stored entry when multiple
                 * entries are stored -> last entry's time gets updated
                 */
                Arguments.of(buildVcpTimes(1, dt_30, 2, dt_36), 2, dt_42,
                        buildVcpTimes(1, dt_30, 2, dt_42)),
                /*
                 * Add VCP/time for different VCP -> that VCP/time gets added on
                 * as new entry
                 */
                Arguments.of(buildVcpTimes(1, dt_30), 2, dt_36,
                        buildVcpTimes(1, dt_30, 2, dt_36)),
                /*
                 * Add VCP/time for same VCP as earlier stored entry but not the
                 * last entry -> combo gets added on as new entry
                 */
                Arguments.of(buildVcpTimes(1, dt_30, 2, dt_36), 1, dt_42,
                        buildVcpTimes(1, dt_30, 2, dt_36, 1, dt_42)));
    }

    @ParameterizedTest
    @MethodSource("provideParamsForAddVcpTime")
    void testAddVcpTime(List<VcpTime> initialVcpTimes, int vcp, DataTime time,
            List<VcpTime> expectedTimes) {
        mapping.vcpTimes.addAll(initialVcpTimes);

        mapping.addVcpTime(vcp, time);

        assertEquals(expectedTimes.size(), mapping.vcpTimes.size());
        for (VcpTime expected : expectedTimes) {
            VcpTime actual = mapping.vcpTimes.poll();
            assertTrue(EqualsBuilder.reflectionEquals(expected, actual));
        }
    }

    private static List<VcpTime> buildVcpTimes(int vcp, DataTime dt) {
        return List.of(new VcpTime(vcp, dt));
    }

    private static List<VcpTime> buildVcpTimes(int vcp1, DataTime dt1, int vcp2,
            DataTime dt2) {
        return List.of(new VcpTime(vcp1, dt1), new VcpTime(vcp2, dt2));
    }

    private static List<VcpTime> buildVcpTimes(int vcp1, DataTime dt1, int vcp2,
            DataTime dt2, int vcp3, DataTime dt3) {
        return List.of(new VcpTime(vcp1, dt1), new VcpTime(vcp2, dt2),
                new VcpTime(vcp3, dt3));
    }
}
