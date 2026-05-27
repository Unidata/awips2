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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord.ScanType;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.LevelNode;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.radar.ui.RadarDisplayControls;
import com.raytheon.viz.radar.ui.RadarDisplayManager;

/**
 * Unit tests for {@link RadarRequestableLevelNode}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2024 2037092    mapeters    Initial creation
 * Sep 16, 2024 2037941    mapeters    Virtual availability is no longer cached
 *
 * </pre>
 *
 * @author mapeters
 */
class TestRadarRequestableLevelNode {

    private static final String RR = "RR";

    private static final String RR_VIRT = "RRvirt";

    private static final String REFLECTIVITY = "Reflectivity";

    private static final String KOAX = "koax";

    private static final double TILT_2_4 = 2.4;

    private RadarRequestableLevelNode node;

    private RadarRequestableLevelNode virtualNode;

    private final DataTime currScanTime = new DataTime("2024-01-01_12:18:00.0");

    private final DataTime prevScanTime = new DataTime("2024-01-01_12:12:00.0");

    private final DataTime prevPrevScanTime = new DataTime(
            "2024-01-01_12:06:00.0");

    private final GridCoverage space = mock(GridCoverage.class);

    private final RadarVirtualTimeAndSpace virtualAvailability = new RadarVirtualTimeAndSpace(
            currScanTime, space, prevScanTime);

    private final RadarDisplayManager displayMgrEnabled = buildRadarDisplayManager(
            true);

    private final RadarDisplayManager displayMgrDisabled = buildRadarDisplayManager(
            false);

    private static final Map<String, RequestConstraint> expectedRcMap = Map.of(
            RadarAdapter.PLUGIN_NAME_QUERY,
            new RequestConstraint(RadarAdapter.RADAR_SOURCE),
            RadarAdapter.ICAO_QUERY, new RequestConstraint(KOAX),
            RadarAdapter.PRODUCT_CODE_QUERY,
            new RequestConstraint(Integer.toString(153)),
            RadarAdapter.TILT_QUERY,
            new RequestConstraint(Double.toString(TILT_2_4)));

    private static final Map<String, RequestConstraint> expectedVirtualRcMap;
    static {
        Map<String, RequestConstraint> rcMap = new HashMap<>(expectedRcMap);
        rcMap.put(RadarAdapter.SCAN_TYPE_QUERY,
                new RequestConstraint(ScanType.NORMAL.name()));
        expectedVirtualRcMap = Collections.unmodifiableMap(rcMap);
    }

    private MockedStatic<RadarRequestableDataFactory> dataFactoryMockedStatic;

    private RadarRequestableDataFactory dataFactory;

    private MockedStatic<RadarRequestableData> dataMockedStatic;

    private MockedStatic<RadarAdapter> adapterMockedStatic;

    @BeforeEach
    public void setupBeforeEach() throws Exception {
        dataFactory = mock(RadarRequestableDataFactory.class);
        when(dataFactory.getRadarRequestableData(any(), any()))
                .then(invocation -> {
                    RadarRecord record = invocation.getArgument(0);
                    String paramAbbrev = invocation.getArgument(1);

                    return new RadarRequestableData(record, paramAbbrev);
                });
        dataFactoryMockedStatic = mockStatic(RadarRequestableDataFactory.class);
        dataFactoryMockedStatic.when(RadarRequestableDataFactory::getInstance)
                .thenReturn(dataFactory);

        dataMockedStatic = TestRadarRequestableData
                .getMockedStatic("radar-koax", "dBZ", TILT_2_4, space);

        RadarAdapter adapter = mock(RadarAdapter.class);
        when(adapter.getCoverage(any())).thenReturn(space);
        adapterMockedStatic = mockStatic(RadarAdapter.class);
        adapterMockedStatic.when(RadarAdapter::getInstance).thenReturn(adapter);

        LevelNode levelNode = mock(LevelNode.class);
        Level level = mock(Level.class);
        when(level.getLevelonevalue()).thenReturn(TILT_2_4);
        when(levelNode.getLevel()).thenReturn(level);

        node = new RadarRequestableLevelNode(levelNode, KOAX, 153, RR,
                REFLECTIVITY);

        virtualNode = new RadarRequestableLevelNode(levelNode, KOAX, 153,
                RR_VIRT, REFLECTIVITY);
    }

    @AfterEach
    void tearDownAfterEach() throws VizException {
        dataFactoryMockedStatic.close();
        dataMockedStatic.close();
        adapterMockedStatic.close();
    }

    @Test
    void testConstructor1() {
        // Test constructor for standard node
        assertEquals(KOAX, node.icao);
        assertEquals(153, node.productCode);
        assertEquals(TILT_2_4, node.tilt);
        assertEquals(RR, node.paramAbbrev);
        assertEquals(RR, node.standardParamAbbrev);
        assertEquals(REFLECTIVITY, node.paramName);
        assertEquals(expectedRcMap, node.rcMap);
    }

    @Test
    void testConstructor2() {
        // Test constructor for virtual node
        assertEquals(KOAX, virtualNode.icao);
        assertEquals(153, virtualNode.productCode);
        assertEquals(TILT_2_4, virtualNode.tilt);
        assertEquals(RR_VIRT, virtualNode.paramAbbrev);
        assertEquals(RR, virtualNode.standardParamAbbrev);
        assertEquals(REFLECTIVITY, virtualNode.paramName);
        assertEquals(expectedVirtualRcMap, virtualNode.rcMap);
        assertEquals(RR, virtualNode.standardParamAbbrev);
    }

    @Test
    void testBuildRcMap1() {
        // Test buildRcMap for standard node
        Map<String, RequestConstraint> rcMap = node.buildRcMap(KOAX, 153,
                TILT_2_4);

        assertEquals(expectedRcMap, rcMap);
    }

    @Test
    void testBuildRcMap2() {
        // Test buildRcMap for virtual node
        Map<String, RequestConstraint> rcMap = virtualNode.buildRcMap(KOAX, 153,
                TILT_2_4);

        assertEquals(rcMap, expectedVirtualRcMap);
    }

    @Test
    void testGetAvailability1() throws DataCubeException {
        /*
         * Availability already cached in RadarUpdater -> return cached value
         */
        Set<TimeAndSpace> expectedAvail = Set.of(
                new TimeAndSpace(currScanTime, space),
                new TimeAndSpace(prevScanTime, space));
        RadarUpdater updaterMock = mock(RadarUpdater.class);
        when(updaterMock.getTimes(node)).thenReturn(expectedAvail);

        Set<TimeAndSpace> actualAvail;
        try (MockedStatic<RadarUpdater> updaterStaticMock = mockStatic(
                RadarUpdater.class)) {
            updaterStaticMock.when(RadarUpdater::getInstance)
                    .thenReturn(updaterMock);

            actualAvail = node.getAvailability(null, null);
        }

        assertEquals(expectedAvail, actualAvail);
    }

    @Test
    void testGetAvailability2() throws DataCubeException {
        /*
         * Setup standard node without cached availability. Setup catalog query
         * to return current scan time and previous scan time. Verify that
         * TimeAndSpace entries for those times are cached and returned.
         */
        RadarUpdater updaterMock = mock(RadarUpdater.class);
        when(updaterMock.getTimes(any())).thenReturn(null);

        Set<TimeAndSpace> actualAvail;
        try (MockedStatic<RadarUpdater> updaterStaticMock = mockStatic(
                RadarUpdater.class);
                MockedStatic<CatalogQuery> catalogQueryStaticMock = mockStatic(
                        CatalogQuery.class)) {
            updaterStaticMock.when(RadarUpdater::getInstance)
                    .thenReturn(updaterMock);
            catalogQueryStaticMock
                    .when(() -> CatalogQuery.performTimeQuery(node.rcMap, false,
                            null))
                    .thenReturn(new DataTime[] { currScanTime, prevScanTime });

            actualAvail = node.getAvailability(null, null);
        }

        Set<TimeAndSpace> expectedAvail = Set.of(
                new TimeAndSpace(currScanTime, space),
                new TimeAndSpace(prevScanTime, space));
        assertEquals(expectedAvail, actualAvail);
        verify(updaterMock).setTimes(node, actualAvail);
    }

    @Test
    void testGetAvailability3() throws DataCubeException {
        /*
         * Setup standard node with cached availability. Verify that cached
         * TimeAndSpace entries are returned.
         */
        Set<TimeAndSpace> expectedAvail = Set.of(
                new TimeAndSpace(currScanTime, space),
                new TimeAndSpace(prevScanTime, space));
        RadarUpdater updaterMock = mock(RadarUpdater.class);
        when(updaterMock.getTimes(any())).thenReturn(expectedAvail);

        Set<TimeAndSpace> actualAvail;
        try (MockedStatic<RadarUpdater> updaterStaticMock = mockStatic(
                RadarUpdater.class)) {
            updaterStaticMock.when(RadarUpdater::getInstance)
                    .thenReturn(updaterMock);
            actualAvail = node.getAvailability(null, null);
        }

        assertEquals(expectedAvail, actualAvail);
        /*
         * Nothing should be cached when we use the times that already were
         * cached
         */
        verify(updaterMock, never()).setTimes(any(), any());
    }

    @Test
    void testGetAvailability4() throws DataCubeException {
        /*
         * Setup virtual node with virtual availability, indicating that the
         * previous scan's data should be made virtually available for the
         * current scan's time. Setup catalog query to return previous scan
         * time. Verify that a normal TimeAndSpace entry is created for the
         * previous scan, and a VirtualTimeAndSpace entry for the current scan
         * time. Verify only non-virtual times are cached.
         */
        RadarUpdater updaterMock = mock(RadarUpdater.class);
        when(updaterMock.getTimes(any())).thenReturn(null);

        virtualNode = spy(virtualNode);
        doReturn(virtualAvailability).when(virtualNode)
                .getVirtualAvailability();

        Set<TimeAndSpace> actualAvail;
        try (MockedStatic<RadarUpdater> updaterStaticMock = mockStatic(
                RadarUpdater.class);
                MockedStatic<CatalogQuery> catalogQueryStaticMock = mockStatic(
                        CatalogQuery.class)) {
            updaterStaticMock.when(RadarUpdater::getInstance)
                    .thenReturn(updaterMock);
            catalogQueryStaticMock
                    .when(() -> CatalogQuery.performTimeQuery(virtualNode.rcMap,
                            false, null))
                    .thenReturn(new DataTime[] { prevScanTime });

            actualAvail = virtualNode.getAvailability(null, null);
        }

        Set<TimeAndSpace> expectedAvail = Set.of(
                new TimeAndSpace(prevScanTime, space),
                new RadarVirtualTimeAndSpace(currScanTime, space,
                        prevScanTime));
        // Virtual times aren't cached
        Set<TimeAndSpace> expectedCachedAvail = Set
                .of(new TimeAndSpace(prevScanTime, space));
        assertEquals(expectedAvail, actualAvail);
        verify(updaterMock).setTimes(virtualNode, expectedCachedAvail);
    }

    @Test
    void testGetAvailability5() throws DataCubeException {
        /*
         * Setup virtual node with virtual availability, indicating that the
         * previous scan's data should be made virtually available for the
         * current scan's time. Setup catalog query to return a time that is NOT
         * the previous scan time. Verify that a normal TimeAndSpace entry is
         * created for the returned time, and a VirtualTimeAndSpace entry is NOT
         * created since the previous scan is not available. Verify those times
         * are also cached.
         */
        RadarUpdater updaterMock = mock(RadarUpdater.class);
        when(updaterMock.getTimes(virtualNode)).thenReturn(null);

        virtualNode = spy(virtualNode);
        doReturn(virtualAvailability).when(virtualNode)
                .getVirtualAvailability();

        Set<TimeAndSpace> actualAvail;
        try (MockedStatic<RadarUpdater> updaterStaticMock = mockStatic(
                RadarUpdater.class);
                MockedStatic<CatalogQuery> catalogQueryStaticMock = mockStatic(
                        CatalogQuery.class)) {
            updaterStaticMock.when(RadarUpdater::getInstance)
                    .thenReturn(updaterMock);
            catalogQueryStaticMock
                    .when(() -> CatalogQuery.performTimeQuery(virtualNode.rcMap,
                            false, null))
                    .thenReturn(new DataTime[] { prevPrevScanTime });

            actualAvail = virtualNode.getAvailability(null, null);
        }

        Set<TimeAndSpace> expectedAvail = Set
                .of(new TimeAndSpace(prevPrevScanTime, space));
        assertEquals(expectedAvail, actualAvail);
        verify(updaterMock).setTimes(virtualNode, expectedAvail);
    }

    @Test
    void testGetAvailability6() throws DataCubeException {
        /*
         * Setup virtual node with cached non-virtual availability. Also set it
         * up to have virtual availability, indicating that the previous scan's
         * data should be made virtually available for the current scan's time.
         * Verify that the returned availability is the combination of the
         * cached availability and the virtual availability.
         */
        RadarUpdater updaterMock = mock(RadarUpdater.class);
        when(updaterMock.getTimes(any()))
                .thenReturn(Set.of(new TimeAndSpace(prevScanTime, space)));

        virtualNode = spy(virtualNode);
        doReturn(virtualAvailability).when(virtualNode)
                .getVirtualAvailability();

        Set<TimeAndSpace> actualAvail;
        try (MockedStatic<RadarUpdater> updaterStaticMock = mockStatic(
                RadarUpdater.class)) {
            updaterStaticMock.when(RadarUpdater::getInstance)
                    .thenReturn(updaterMock);

            actualAvail = virtualNode.getAvailability(null, null);
        }

        Set<TimeAndSpace> expectedAvail = Set.of(
                new TimeAndSpace(prevScanTime, space),
                new RadarVirtualTimeAndSpace(currScanTime, space,
                        prevScanTime));
        assertEquals(expectedAvail, actualAvail);
        /*
         * Nothing should be cached when we use the times that already were
         * cached
         */
        verify(updaterMock, never()).setTimes(any(), any());
    }

    @Test
    void testGetDataRequest1() {
        // Standard node -> constraints include passed in times
        Set<TimeAndSpace> requestedAvailability = new LinkedHashSet<>();
        requestedAvailability.add(new TimeAndSpace(currScanTime, space));
        requestedAvailability.add(new TimeAndSpace(prevScanTime, space));

        DbQueryRequest actualRequest = node.getDataRequest(null,
                requestedAvailability);

        String[] expectedTimeStrs = { currScanTime.toString(),
                prevScanTime.toString() };
        Map<String, RequestConstraint> expectedConstraints = new HashMap<>(
                expectedRcMap);
        expectedConstraints.put(PluginDataObject.DATATIME_ID,
                new RequestConstraint(expectedTimeStrs));
        assertEquals(expectedConstraints, actualRequest.getConstraints());
    }

    @Test
    void testGetDataRequest2() {
        // Virtual node with no virtual availability -> request times as normal
        virtualNode = spy(virtualNode);
        doReturn(null).when(virtualNode).getVirtualAvailability();
        Set<TimeAndSpace> requestedAvailability = new LinkedHashSet<>();
        requestedAvailability.add(new TimeAndSpace(currScanTime, space));
        requestedAvailability.add(new TimeAndSpace(prevScanTime, space));

        DbQueryRequest actualRequest = virtualNode.getDataRequest(null,
                requestedAvailability);

        Set<DataTime> expectedTimes = new HashSet<>();
        expectedTimes.add(currScanTime);
        expectedTimes.add(prevScanTime);
        String[] expectedTimeStrs = expectedTimes.stream()
                .map(DataTime::toString).toArray(String[]::new);
        Map<String, RequestConstraint> expectedConstraints = new HashMap<>(
                expectedVirtualRcMap);
        expectedConstraints.put(PluginDataObject.DATATIME_ID,
                new RequestConstraint(expectedTimeStrs));
        assertEquals(expectedConstraints, actualRequest.getConstraints());
    }

    @Test
    void testGetDataRequest3() {
        /*
         * Setup virtual node with virtual availability, indicating that the
         * node doesn't have data for the current volume scan, but should try to
         * create a virtual record for it from the previous volume scan data.
         *
         * When current volume scan time is passed into getDataRequest along
         * with other times, verify that constraint actually uses previous
         * volume scan time, since that time has the actual data that we want to
         * use for the current scan time.
         */
        virtualNode = spy(virtualNode);
        doReturn(virtualAvailability).when(virtualNode)
                .getVirtualAvailability();
        Set<TimeAndSpace> requestedAvailability = new LinkedHashSet<>();
        requestedAvailability.add(new TimeAndSpace(currScanTime, space));
        requestedAvailability.add(new TimeAndSpace(prevScanTime, space));
        requestedAvailability.add(new TimeAndSpace(prevPrevScanTime, space));

        DbQueryRequest actualRequest = virtualNode.getDataRequest(null,
                requestedAvailability);

        Set<DataTime> expectedTimes = new HashSet<>();
        expectedTimes.add(prevScanTime);
        expectedTimes.add(prevPrevScanTime);
        String[] expectedTimeStrs = expectedTimes.stream()
                .map(DataTime::toString).toArray(String[]::new);
        Map<String, RequestConstraint> expectedConstraints = new HashMap<>(
                expectedVirtualRcMap);
        expectedConstraints.put(PluginDataObject.DATATIME_ID,
                new RequestConstraint(expectedTimeStrs));
        assertEquals(expectedConstraints, actualRequest.getConstraints());
    }

    @Test
    void testGetDataRequest4() {
        /*
         * Setup virtual node with virtual availability, indicating that the
         * node doesn't have data for the current volume scan, but should try to
         * create a virtual record for it from the previous volume scan data.
         *
         * When only current volume scan time is passed into getDataRequest,
         * verify that constraint actually uses previous volume scan time, since
         * that time has the actual data that we want to use for the current
         * scan time.
         */
        virtualNode = spy(virtualNode);
        doReturn(virtualAvailability).when(virtualNode)
                .getVirtualAvailability();
        Set<TimeAndSpace> requestedAvailability = new LinkedHashSet<>();
        requestedAvailability.add(new TimeAndSpace(currScanTime, space));

        DbQueryRequest actualRequest = virtualNode.getDataRequest(null,
                requestedAvailability);

        String[] expectedTimeStrs = { prevScanTime.toString() };
        RequestConstraint expectedTimeConstraint = new RequestConstraint(
                expectedTimeStrs);
        Map<String, RequestConstraint> expectedConstraints = new HashMap<>(
                expectedVirtualRcMap);
        expectedConstraints.put(PluginDataObject.DATATIME_ID,
                expectedTimeConstraint);
        assertEquals(expectedConstraints, actualRequest.getConstraints());
    }

    @Test
    void testGetData1() throws Exception {
        /*
         * Setup normal node with DB data for current/previous scan being passed
         * into getData. Verify that non-virtual data is created from them, with
         * the correct standard parameter abbreviation and time-and-space that
         * matches the DB data.
         */
        DbQueryResponse resp = buildDbQueryResponse(
                List.of(currScanTime, prevScanTime));
        Set<TimeAndSpace> requestedAvail = Set.of(
                new TimeAndSpace(currScanTime, space),
                new TimeAndSpace(prevScanTime, space));

        Set<AbstractRequestableData> dataSet = node.getData(null,
                requestedAvail, resp);

        Set<TimeAndSpace> expectedTas = Set.of(
                new TimeAndSpace(currScanTime, space),
                new TimeAndSpace(prevScanTime, space));
        for (AbstractRequestableData data : dataSet) {
            assertTrue(data.getClass() == RadarRequestableData.class);
            assertTrue(RR.equals(data.getParameter()));
        }
        assertEquals(expectedTas.size(), dataSet.size());
        assertEquals(expectedTas,
                dataSet.stream().map(AbstractRequestableData::getTimeAndSpace)
                        .collect(Collectors.toSet()));
        verify(dataFactory, atLeastOnce()).getRadarRequestableData(any(),
                eq(RR));
        verify(dataFactory, never()).getRadarRequestableData(any(),
                eq(RR_VIRT));
    }

    @Test
    void testGetData2() throws Exception {
        /*
         * Setup virtual node with DB data for current/previous scan being
         * passed into getData, and no virtual availability. Verify that virtual
         * data objects are created from them, with the correct virtual
         * parameter abbreviation and time-and-space that matches the DB data
         * (no virtual times).
         */
        DbQueryResponse resp = buildDbQueryResponse(
                List.of(currScanTime, prevScanTime));
        Set<TimeAndSpace> requestedAvail = Set.of(
                new TimeAndSpace(currScanTime, space),
                new TimeAndSpace(prevScanTime, space));

        virtualNode = spy(virtualNode);
        doReturn(null).when(virtualNode).getVirtualAvailability();

        Set<AbstractRequestableData> dataSet = virtualNode.getData(null,
                requestedAvail, resp);

        Set<TimeAndSpace> expectedTas = Set.of(
                new TimeAndSpace(currScanTime, space),
                new TimeAndSpace(prevScanTime, space));
        for (AbstractRequestableData data : dataSet) {
            assertTrue(
                    data.getClass() == RadarVirtualVolumeRequestableData.class);
            assertTrue(RR_VIRT.equals(data.getParameter()));
        }
        assertEquals(expectedTas.size(), dataSet.size());
        assertEquals(expectedTas,
                dataSet.stream().map(AbstractRequestableData::getTimeAndSpace)
                        .collect(Collectors.toSet()));
        // Standard param abbrev should always be passed to data factory
        verify(dataFactory, atLeastOnce()).getRadarRequestableData(any(),
                eq(RR));
        verify(dataFactory, never()).getRadarRequestableData(any(),
                eq(RR_VIRT));
    }

    @Test
    void testGetData3() throws Exception {
        /*
         * Setup virtual node with DB data for previous scan being passed into
         * getData, virtual availability, and the current scan being requested
         * as well. Verify that one virtual data object is created for the
         * previous scan time, along with another virtual data object that
         * represents the previous scan time for the current scan time.
         */
        DbQueryResponse resp = buildDbQueryResponse(List.of(prevScanTime));
        Set<TimeAndSpace> requestedAvail = Set.of(
                new TimeAndSpace(currScanTime, space),
                new TimeAndSpace(prevScanTime, space));

        virtualNode = spy(virtualNode);
        doReturn(virtualAvailability).when(virtualNode)
                .getVirtualAvailability();

        Set<AbstractRequestableData> dataSet = virtualNode.getData(null,
                requestedAvail, resp);

        Set<TimeAndSpace> expectedTas = Set.of(
                new TimeAndSpace(prevScanTime, space),
                new RadarVirtualTimeAndSpace(currScanTime, space,
                        prevScanTime));
        for (AbstractRequestableData data : dataSet) {
            assertTrue(
                    data.getClass() == RadarVirtualVolumeRequestableData.class);
            assertTrue(RR_VIRT.equals(data.getParameter()));
        }
        assertEquals(expectedTas.size(), dataSet.size());
        assertEquals(expectedTas,
                dataSet.stream().map(AbstractRequestableData::getTimeAndSpace)
                        .collect(Collectors.toSet()));
        // Standard param abbrev should always be passed to data factory
        verify(dataFactory, atLeastOnce()).getRadarRequestableData(any(),
                eq(RR));
        verify(dataFactory, never()).getRadarRequestableData(any(),
                eq(RR_VIRT));
    }

    @Test
    void testGetData4() throws Exception {
        /*
         * Setup virtual node with DB data for previous scan being passed into
         * getData, virtual availability, but only the previous scan is being
         * requested. Verify that a virtual data object for the current scan
         * time is NOT created since it wasn't requested.
         */
        DbQueryResponse resp = buildDbQueryResponse(List.of(prevScanTime));
        Set<TimeAndSpace> requestedAvail = Set
                .of(new TimeAndSpace(prevScanTime, space));

        virtualNode = spy(virtualNode);
        doReturn(virtualAvailability).when(virtualNode)
                .getVirtualAvailability();

        Set<AbstractRequestableData> dataSet = virtualNode.getData(null,
                requestedAvail, resp);

        Set<TimeAndSpace> expectedTas = Set
                .of(new TimeAndSpace(prevScanTime, space));
        for (AbstractRequestableData data : dataSet) {
            assertTrue(
                    data.getClass() == RadarVirtualVolumeRequestableData.class);
            assertTrue(RR_VIRT.equals(data.getParameter()));
        }
        assertEquals(expectedTas.size(), dataSet.size());
        assertEquals(expectedTas,
                dataSet.stream().map(AbstractRequestableData::getTimeAndSpace)
                        .collect(Collectors.toSet()));
        // Standard param abbrev should always be passed to data factory
        verify(dataFactory, atLeastOnce()).getRadarRequestableData(any(),
                eq(RR));
        verify(dataFactory, never()).getRadarRequestableData(any(),
                eq(RR_VIRT));
    }

    @Test
    void testGetVirtualAvailability1() {
        // Standard node -> no virtual availability
        RadarVirtualTimeAndSpace virtAvail = node.getVirtualAvailability();

        assertNull(virtAvail);
    }

    @Test
    void testGetVirtualAvailability2() {
        // Virtual node with virtual volume disabled -> no virtual availability
        RadarVirtualTimeAndSpace virtAvail;
        try (MockedStatic<RadarDisplayManager> mockedStatic = mockStatic(
                RadarDisplayManager.class)) {
            mockedStatic.when(RadarDisplayManager::getInstance)
                    .thenReturn(displayMgrDisabled);
            virtAvail = virtualNode.getVirtualAvailability();
        }

        assertNull(virtAvail);
    }

    @Test
    void testGetVirtualAvailability3() {
        /*
         * Virtual node with virtual volume enabled, but volume scan tracker
         * returns null virtual volume info -> no virtual availability
         */
        RadarVirtualTimeAndSpace virtAvail;
        try (MockedStatic<RadarDisplayManager> mockedStatic = mockStatic(
                RadarDisplayManager.class);
                MockedStatic<RadarVolumeScanTracker> mockedStaticRVST = mockStatic(
                        RadarVolumeScanTracker.class)) {
            mockedStatic.when(RadarDisplayManager::getInstance)
                    .thenReturn(displayMgrEnabled);
            RadarVolumeScanTracker tracker = buildRadarVolumeScanTracker(null);
            mockedStatic
                    .when(() -> RadarVolumeScanTracker.getInstance(KOAX, RR))
                    .thenReturn(tracker);

            virtAvail = virtualNode.getVirtualAvailability();
        }

        assertNull(virtAvail);
    }

    @Test
    void testGetVirtualAvailability4() {
        /*
         * Virtual node with virtual volume enabled and volume scan tracker
         * returns virtual volume info, but the virtual volume info indicates
         * that the latest tilt matches the node's tilt -> no virtual
         * availability
         */
        RadarVirtualTimeAndSpace virtAvail;
        try (MockedStatic<RadarDisplayManager> mockedStatic = mockStatic(
                RadarDisplayManager.class);
                MockedStatic<RadarVolumeScanTracker> mockedStaticRVST = mockStatic(
                        RadarVolumeScanTracker.class)) {
            mockedStatic.when(RadarDisplayManager::getInstance)
                    .thenReturn(displayMgrEnabled);
            VirtualVolumeInfo virtVolInfo = new VirtualVolumeInfo(currScanTime,
                    prevScanTime, TILT_2_4);
            RadarVolumeScanTracker tracker = buildRadarVolumeScanTracker(
                    virtVolInfo);
            mockedStatic
                    .when(() -> RadarVolumeScanTracker.getInstance(KOAX, RR))
                    .thenReturn(tracker);

            virtAvail = virtualNode.getVirtualAvailability();
        }

        assertNull(virtAvail);

    }

    @Test
    void testGetVirtualAvailability5() {
        /*
         * Virtual node with virtual volume enabled, volume scan tracker returns
         * virtual volume info with latest tilt that's below the node's tilt ->
         * valid virtual availability
         */
        RadarVirtualTimeAndSpace virtAvail;
        try (MockedStatic<RadarDisplayManager> mockedStatic = mockStatic(
                RadarDisplayManager.class);
                MockedStatic<RadarVolumeScanTracker> mockedStaticRVST = mockStatic(
                        RadarVolumeScanTracker.class)) {
            mockedStatic.when(RadarDisplayManager::getInstance)
                    .thenReturn(displayMgrEnabled);
            VirtualVolumeInfo virtVolInfo = new VirtualVolumeInfo(currScanTime,
                    prevScanTime, 1.5);
            RadarVolumeScanTracker tracker = buildRadarVolumeScanTracker(
                    virtVolInfo);
            mockedStatic
                    .when(() -> RadarVolumeScanTracker.getInstance(KOAX, RR))
                    .thenReturn(tracker);

            virtAvail = virtualNode.getVirtualAvailability();
        }

        RadarVirtualTimeAndSpace expectedVirtAvail = new RadarVirtualTimeAndSpace(
                currScanTime, space, prevScanTime);
        assertEquals(expectedVirtAvail, virtAvail);
    }

    @Test
    void testGetIcao() {
        assertEquals(KOAX, node.getIcao());
        assertEquals(KOAX, virtualNode.getIcao());
    }

    @Test
    void testGetProductCode() {
        assertEquals(153, node.getProductCode());
        assertEquals(153, virtualNode.getProductCode());
    }

    @Test
    void testGetTilt() {
        assertEquals(TILT_2_4, node.getTilt());
        assertEquals(TILT_2_4, virtualNode.getTilt());
    }

    @Test
    void testGetParamAbbrev1() {
        assertEquals(RR, node.getParamAbbrev());
    }

    @Test
    void testGetParamAbbrev2() {
        assertEquals(RR_VIRT, virtualNode.getParamAbbrev());
    }

    @Test
    void testIsVirtualVolume1() {
        assertFalse(node.isVirtualVolume());
    }

    @Test
    void testIsVirtualVolume2() {
        assertTrue(virtualNode.isVirtualVolume());
    }

    @Test
    void testEquals() {
        assertNotEquals(node, virtualNode);
    }

    @Test
    void testCopyEquals1() {
        RadarRequestableLevelNode nodeCopy = new RadarRequestableLevelNode(
                node);

        assertEquals(nodeCopy, node);
    }

    @Test
    void testCopyEquals2() {
        RadarRequestableLevelNode nodeCopy = new RadarRequestableLevelNode(
                virtualNode);

        assertEquals(nodeCopy, virtualNode);
    }

    @Test
    void testCopyHashCode1() {
        RadarRequestableLevelNode nodeCopy = new RadarRequestableLevelNode(
                node);
        int expectedHash = node.hashCode();

        int copyHash = nodeCopy.hashCode();

        assertEquals(expectedHash, copyHash);
    }

    @Test
    void testCopyHashCode2() {
        RadarRequestableLevelNode nodeCopy = new RadarRequestableLevelNode(
                virtualNode);
        int expectedHash = virtualNode.hashCode();

        int copyHash = nodeCopy.hashCode();

        assertEquals(expectedHash, copyHash);
    }

    private RadarVolumeScanTracker buildRadarVolumeScanTracker(
            VirtualVolumeInfo virtualVolumeInfo) {
        RadarVolumeScanTracker tracker = mock(RadarVolumeScanTracker.class);
        when(tracker.getVirtualVolumeInfo()).thenReturn(virtualVolumeInfo);
        return tracker;
    }

    private DbQueryResponse buildDbQueryResponse(List<DataTime> times) {
        List<RadarRecord> records = new ArrayList<>();
        for (DataTime time : times) {
            RadarRecord record = new RadarRecord();
            time = time.clone();
            time.setLevel(TILT_2_4, RadarUtil.TILT);
            record.setDataTime(time);
            records.add(record);
        }
        DbQueryResponse resp = mock(DbQueryResponse.class);
        when(resp.getEntityObjects(RadarRecord.class))
                .thenReturn(records.toArray(RadarRecord[]::new));
        return resp;
    }

    private RadarDisplayManager buildRadarDisplayManager(
            boolean virtualVolumeEnabled) {
        RadarDisplayManager displayMgr = mock(RadarDisplayManager.class);
        RadarDisplayControls displayControls = mock(RadarDisplayControls.class);
        when(displayControls.isVirtualVolumeEnabled())
                .thenReturn(virtualVolumeEnabled);
        when(displayMgr.getCurrentSettings()).thenReturn(displayControls);
        return displayMgr;
    }
}
