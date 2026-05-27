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

import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.RETURNS_MOCKS;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.grid.radar.RadarVirtualTimeAndSpace;
import com.raytheon.uf.viz.grid.radar.util.RadarVirtualVolumeStatus;
import com.raytheon.uf.viz.grid.radar.util.RadarVirtualVolumeUtil;
import com.raytheon.uf.viz.xy.crosssection.rsc.AbstractCrossSectionResource;
import com.raytheon.viz.grid.record.RequestableDataRecord;
import com.raytheon.viz.radar.ui.RadarDisplayControls;
import com.raytheon.viz.radar.ui.RadarDisplayManager;

import tech.units.indriya.AbstractUnit;

/**
 * Unit tests for {@link RadarGridCSAdapter}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2024 2037092    mapeters    Initial creation
 * Jun 20, 2024 2037565    mapeters    Add getExtraLegendText() tests
 * Jul 24, 2024 2037624    mapeters    Update for code being extracted from adapter
 * Aug 09, 2024 2037698    bines       Update tests for virt and added testUseNearestNeighbor
 * Aug 20, 2024 2037631    mapeters    Add tests for new methods that support drawing virtual
 *                                     volume line on the graph
 * Oct 14, 2024 2037939    mapeters    Add getMetadataMaps() tests
 *
 * </pre>
 *
 * @author mapeters
 */
@ExtendWith(MockitoExtension.class)
class TestRadarGridCSAdapter {

    private static final DataTime DT_1206 = new DataTime(
            "2024-01-01_12:06:00.0");

    private static final DataTime DT_1212 = new DataTime(
            "2024-01-01_12:12:00.0");

    private static final GridCoverage space = mock(GridCoverage.class);

    private TestExtendedRadarGridCSAdapter adapter;

    /** Radar display manager with virtual volumes enabled */
    private final RadarDisplayManager displayMgrEnabled = buildRadarDisplayManager(
            true);

    private MockedStatic<RadarDisplayManager> displayManagerMockedStatic;

    private MockedStatic<LevelFactory> levelFactoryMockedStatic;

    @BeforeEach
    void setupBeforeEach() {
        /*
         * Lower minimum level count so we don't have to mock up 5+ levels to
         * get tests to work
         */
        System.setProperty("crosssection.min.level.count", "1");

        displayManagerMockedStatic = mockStatic(RadarDisplayManager.class);
        displayManagerMockedStatic.when(RadarDisplayManager::getInstance)
                .thenReturn(displayMgrEnabled);

        LevelFactory levelFactory = mock(LevelFactory.class);
        lenient().when(levelFactory.getLevel(eq(RadarUtil.TILT), anyDouble()))
                .then(invocation -> {
                    return getTiltLevel(invocation.getArgument(1));
                });
        levelFactoryMockedStatic = mockStatic(LevelFactory.class);
        levelFactoryMockedStatic.when(LevelFactory::getInstance)
                .thenReturn(levelFactory);

        adapter = spy(new TestExtendedRadarGridCSAdapter());
        lenient().doReturn("RRvirt").when(adapter).getParameterAbbrev();
    }

    @AfterEach
    void tearDownAfterEach() {
        displayManagerMockedStatic.close();
        levelFactoryMockedStatic.close();
        // Reset to default
        System.clearProperty("crosssection.min.level.count");
    }

    @Test
    void testSetResource() {
        // Verify resource field is set and listeners are registered
        AbstractCrossSectionResource rsc = mock(
                AbstractCrossSectionResource.class);

        try (MockedStatic<RadarVirtualVolumeUtil> utilMockedStatic = mockStatic(
                RadarVirtualVolumeUtil.class)) {

            adapter.setResource(rsc);

            assertSame(rsc, adapter.getResource());
            utilMockedStatic.verify(() -> RadarVirtualVolumeUtil
                    .registerVirtualVolumeListeners(eq(rsc), any()));
        }
    }

    @Test
    void testUseNearestNeighbor() {
        doReturn("HC").when(adapter).getParameterAbbrev();
        assertTrue(adapter.useNearestNeighbor());

        doReturn("HCvirt").when(adapter).getParameterAbbrev();
        assertTrue(adapter.useNearestNeighbor());

        doReturn("RR").when(adapter).getParameterAbbrev();
        assertFalse(adapter.useNearestNeighbor());

        doReturn("RRvirt").when(adapter).getParameterAbbrev();
        assertFalse(adapter.useNearestNeighbor());
    }

    @Test
    void testBuildExtraFrameRenderable1() {
        // Non-virtual param -> null
        when(adapter.getParameterAbbrev()).thenReturn("RR");

        RadarCSFrameVirtualVolumeRenderable renderable = adapter
                .buildExtraFrameRenderable(DT_1206, null, null, null, null);

        assertNull(renderable);
    }

    @Test
    void testBuildExtraFrameRenderable2() {
        // Virtual param but no virtual records -> null extra renderable
        when(adapter.getParameterAbbrev()).thenReturn("RRvirt");
        Map<Level, GridRecord> dataMap = new HashMap<>();
        addNonVirtualData(DT_1212, 0.5, dataMap);
        addNonVirtualData(DT_1212, 1.5, dataMap);

        RadarCSFrameVirtualVolumeRenderable renderable = adapter
                .buildExtraFrameRenderable(DT_1206, null, dataMap, null, null);

        assertNull(renderable);
    }

    @Test
    void testBuildExtraFrameRenderable3() throws VizException {
        /*
         * Virtual param and virtual records -> correct virtual volume
         * status/line calculated and passed to the extra renderable constructor
         */
        when(adapter.getParameterAbbrev()).thenReturn("RRvirt");
        Map<Level, GridRecord> dataMap = new HashMap<>();
        addNonVirtualData(DT_1212, 0.5, dataMap);
        addNonVirtualData(DT_1212, 1.5, dataMap);
        addVirtualData(DT_1212, DT_1206, 2.4, dataMap);
        addVirtualData(DT_1212, DT_1206, 3.4, dataMap);
        // y record of highest non-virtual tilt is needed to draw line
        GridRecord yRecordForCurrentTilt = mock(GridRecord.class);
        Map<Level, GridRecord> yMap = Map.of(getTiltLevel(1.5),
                yRecordForCurrentTilt);
        List<double[]> tiltLine = List.of(new double[] { 0d, 0d },
                new double[] { 1d, 1d });
        doReturn(tiltLine).when(adapter).getTiltLine(DT_1212, null,
                yRecordForCurrentTilt, null);

        // Mock construction to check that correct args are passed in
        try (MockedConstruction<RadarCSFrameVirtualVolumeRenderable> mockedConstructor = mockConstruction(
                RadarCSFrameVirtualVolumeRenderable.class, (mock, context) -> {
                    RadarVirtualVolumeStatus status = (RadarVirtualVolumeStatus) context
                            .arguments().get(0);
                    @SuppressWarnings("unchecked")
                    List<double[]> tiltLinePoints = (List<double[]>) context
                            .arguments().get(1);
                    assertNotNull(status);
                    assertEquals(DT_1212, status.getCurrScanTime());
                    assertEquals(1.5, status.getCurrScanTilt());
                    assertEquals(DT_1206, status.getPrevScanTime());
                    assertSame(tiltLine, tiltLinePoints);
                })) {

            RadarCSFrameVirtualVolumeRenderable renderable = adapter
                    .buildExtraFrameRenderable(DT_1212, null, dataMap, yMap,
                            null);

            /*
             * Just ensure constructor was called so that above asserts were
             * done and a renderable was created
             */
            assertTrue(!mockedConstructor.constructed().isEmpty());
            assertNotNull(renderable);
        }
    }

    @Test
    void testGetMetadataMaps1() throws VizException {
        // No virtual x records, nothing done
        RequestableDataRecord x0_5 = getRecord(0.5,
                new TimeAndSpace(DT_1212, space));
        RequestableDataRecord x1_5 = getRecord(1.5,
                new TimeAndSpace(DT_1212, space));
        adapter.addRecord(x0_5);
        adapter.addRecord(x1_5);
        RequestableDataRecord y0_5 = getRecord(0.5,
                new TimeAndSpace(DT_1212, space));
        RequestableDataRecord y1_5 = getRecord(1.5,
                new TimeAndSpace(DT_1212, space));
        adapter.yRecords.put(DT_1212, Set.of(y0_5, y1_5));

        Map<Level, GridRecord> expectedXMap = Map.of(x0_5.getLevel(), x0_5,
                x1_5.getLevel(), x1_5);
        Map<Level, GridRecord> expectedYMap = Map.of(y0_5.getLevel(), y0_5,
                y1_5.getLevel(), y1_5);

        Pair<Map<Level, GridRecord>, Map<Level, GridRecord>> actualMaps = adapter
                .getMetadataMaps(DT_1212);

        assertEquals(ImmutablePair.of(expectedXMap, expectedYMap), actualMaps);
    }

    @Test
    void testGetMetadataMaps2() throws VizException {
        // Virtual 1.5 x record, so 1.5 y record gets replaced with previous
        // scan record
        RequestableDataRecord x0_5 = getRecord(0.5,
                new TimeAndSpace(DT_1212, space));
        RequestableDataRecord x1_5virt = getRecord(1.5,
                new RadarVirtualTimeAndSpace(DT_1212, space, DT_1206));
        adapter.addRecord(x0_5);
        adapter.addRecord(x1_5virt);
        RequestableDataRecord y0_5 = getRecord(0.5,
                new TimeAndSpace(DT_1212, space));
        RequestableDataRecord y1_5 = getRecord(1.5,
                new TimeAndSpace(DT_1212, space));
        adapter.yRecords.put(DT_1212, Set.of(y0_5, y1_5));

        TimeAndSpace prevScanTas = new TimeAndSpace(DT_1206, space);
        GridRecord prevScanY1_5 = getRecord(1.5, prevScanTas);
        Set<GridRecord> prevScanYRecords = Set.of(getRecord(0.5, prevScanTas),
                prevScanY1_5);
        doReturn(prevScanYRecords).when(adapter).getYRecords(DT_1206);
        doCallRealMethod().when(adapter).getYRecords(DT_1212);

        Map<Level, GridRecord> expectedXMap = Map.of(x0_5.getLevel(), x0_5,
                x1_5virt.getLevel(), x1_5virt);
        Map<Level, GridRecord> expectedYMap = Map.of(y0_5.getLevel(), y0_5,
                prevScanY1_5.getLevel(), prevScanY1_5);

        Pair<Map<Level, GridRecord>, Map<Level, GridRecord>> actualMaps = adapter
                .getMetadataMaps(DT_1212);

        assertEquals(ImmutablePair.of(expectedXMap, expectedYMap), actualMaps);
    }

    private static RadarDisplayManager buildRadarDisplayManager(
            boolean virtualVolumeEnabled) {
        RadarDisplayManager displayMgr = mock(RadarDisplayManager.class);
        RadarDisplayControls displayControls = mock(RadarDisplayControls.class);
        when(displayControls.isVirtualVolumeEnabled())
                .thenReturn(virtualVolumeEnabled);
        when(displayMgr.getCurrentSettings()).thenReturn(displayControls);
        return displayMgr;
    }

    /**
     * Add a non-virtual record for the given time/tilt into the given dataMap.
     *
     * @param dt
     * @param tilt
     * @param dataMap
     */
    private static void addNonVirtualData(DataTime dt, double tilt,
            Map<Level, GridRecord> dataMap) {
        addData(tilt, new TimeAndSpace(dt, space), dataMap);
    }

    /**
     * Add a virtual record for the given times/tilt into the given dataMap.
     *
     * @param currScanTime
     * @param prevScanTime
     * @param tilt
     * @param dataMap
     */
    private static void addVirtualData(DataTime currScanTime,
            DataTime prevScanTime, double tilt,
            Map<Level, GridRecord> dataMap) {
        TimeAndSpace virtualTimeAndSpace = new RadarVirtualTimeAndSpace(
                currScanTime, space, prevScanTime);
        addData(tilt, virtualTimeAndSpace, dataMap);
    }

    private static void addData(double tilt, TimeAndSpace timeAndSpace,
            Map<Level, GridRecord> dataMap) {
        RequestableDataRecord rdr = getRecord(tilt, timeAndSpace);
        dataMap.put(rdr.getLevel(), rdr);
    }

    private static RequestableDataRecord getRecord(double tilt,
            TimeAndSpace timeAndSpace) {
        Level tiltLevel = getTiltLevel(tilt);
        AbstractRequestableData ard = mock(AbstractRequestableData.class,
                RETURNS_MOCKS);
        lenient().when(ard.getTimeAndSpace()).thenReturn(timeAndSpace);
        lenient().when(ard.getDataTime()).thenReturn(timeAndSpace.getTime());
        lenient().when(ard.getSpace()).thenReturn(timeAndSpace.getSpace());
        lenient().when(ard.getLevel()).thenReturn(tiltLevel);
        lenient().doReturn(AbstractUnit.ONE).when(ard).getUnit();
        RequestableDataRecord rdr;
        try {
            rdr = new RequestableDataRecord(ard);
        } catch (VizException e) {
            throw new RuntimeException(e);
        }
        return rdr;
    }

    private static Level getTiltLevel(double tilt) {
        return new Level(tilt + "TILT");
    }

    /**
     * Test subclass that just allows us to check values of protected fields.
     */
    private static class TestExtendedRadarGridCSAdapter
            extends RadarGridCSAdapter {

        private static final long serialVersionUID = 1L;

        private AbstractCrossSectionResource getResource() {
            return resource;
        }
    }
}