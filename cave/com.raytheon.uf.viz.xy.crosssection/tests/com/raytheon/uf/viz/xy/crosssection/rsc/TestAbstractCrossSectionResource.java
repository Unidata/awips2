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
package com.raytheon.uf.viz.xy.crosssection.rsc;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameData;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameRenderable;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;

/**
 * Unit tests for {@link AbstractCrossSectionResource}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2024 2037092    mapeters    Initial creation
 * May 29, 2024 2037244    mapeters    Add testRedraw
 * Jun 20, 2024 2037565    mapeters    Add remove()/getName() tests
 * Aug 20, 2024 2037631    mapeters    Float data is wrapped in new class now
 *
 * </pre>
 *
 * @author mapeters
 */
class TestAbstractCrossSectionResource {

    private static final String DT_1200_STR = "2024-01-01_12:00:00.0";

    private static final float[] slice = { 1f };

    private static final CrossSectionFrameData sliceList = new CrossSectionFrameData(
            List.of(slice), null);

    private static final Future<CrossSectionFrameData> sliceFuture = CompletableFuture
            .completedFuture(sliceList);

    private DataTime dt1200;

    private DataTime dt1200_0;

    private DataTime dt1200_1;

    private PluginDataObject pdo;

    private CrossSectionResourceData rscData;

    @BeforeEach
    public void setupBeforeEach() {
        dt1200 = new DataTime(DT_1200_STR);
        dt1200_0 = new DataTime(DT_1200_STR);
        dt1200_0.setLevel(0d, "AllLAT");
        dt1200_1 = new DataTime(DT_1200_STR);
        dt1200_1.setLevel(1d, "AllLAT");

        rscData = mock(CrossSectionResourceData.class);
        doReturn(List.of(dt1200_0, dt1200_1)).when(rscData)
                .getAffectedFrameTimes(dt1200);
        doReturn(List.of(dt1200_0)).when(rscData)
                .getAffectedFrameTimes(dt1200_0);
        doReturn(List.of(dt1200_1)).when(rscData)
                .getAffectedFrameTimes(dt1200_1);

        pdo = mock(PluginDataObject.class);
        when(pdo.getDataTime()).thenReturn(new DataTime(DT_1200_STR));
    }

    @Test
    public void testLoadSlice() throws VizException {
        /*
         * Adapter returns null data -> loadSlice returns empty data rather than
         * null (to indicate that we've tried loading the data)
         */
        AbstractCrossSectionAdapter<?> adapter = mock(
                AbstractCrossSectionAdapter.class);
        when(adapter.loadData(any(), any(), any())).thenReturn(null);
        AbstractCrossSectionResource rsc = new TestConcreteCrossSectionResource(
                mock(CrossSectionResourceData.class),
                mock(LoadProperties.class), adapter);
        CrossSectionDescriptor descriptor = mock(CrossSectionDescriptor.class);
        when(descriptor.getGraph(any()))
                .thenReturn(mock(CrossSectionGraph.class));
        rsc.setDescriptor(descriptor);

        CrossSectionFrameData slice = rsc.loadSlice(new DataTime());

        assertNotNull(slice);
        assertFalse(slice.hasData());
    }

    @Test
    public void testAddRecord1() {
        // Add record -> schedules data retrieval
        AbstractCrossSectionResource rsc = new TestConcreteCrossSectionResource(
                rscData, mock(LoadProperties.class),
                mock(AbstractCrossSectionAdapter.class));
        rsc.setDescriptor(mock(CrossSectionDescriptor.class));

        rsc.addRecord(pdo);

        assertTrue(rsc.dataRetrievalJob.times.contains(dt1200_0));
    }

    @Test
    public void testAddRecord2() {
        /*
         * Add record for existing time -> disposes that time's data and
         * schedules data retrieval
         */
        AbstractCrossSectionResource rsc = new TestConcreteCrossSectionResource(
                rscData, mock(LoadProperties.class),
                mock(AbstractCrossSectionAdapter.class), List.of(dt1200_0));
        rsc.setDescriptor(mock(CrossSectionDescriptor.class));
        rsc = spy(rsc);

        rsc.addRecord(pdo);

        assertTrue(rsc.dataRetrievalJob.times.contains(dt1200_0));
        verify(rsc).disposeFrame(dt1200_0, true);
    }

    @Test
    public void testRemove1() {
        /*
         * Remove non-spatial time -> data is cleared out for all frames for
         * that time, and that time is removed from the adapter since no frames
         * are left for it
         */
        AbstractCrossSectionResource rsc = new TestConcreteCrossSectionResource(
                rscData, mock(LoadProperties.class),
                mock(AbstractCrossSectionAdapter.class),
                List.of(dt1200_0, dt1200_1));

        rsc.sliceMap.put(dt1200_0, sliceFuture);
        rsc.sliceMap.put(dt1200_1, sliceFuture);
        rsc.dataRetrievalJob.times.add(dt1200_0);
        rsc.dataRetrievalJob.times.add(dt1200_1);

        rsc.remove(dt1200);

        assertTrue(rsc.getDataTimes().length == 0);
        assertTrue(rsc.sliceMap.isEmpty());
        assertTrue(rsc.dataRetrievalJob.times.isEmpty());
        verify(rsc.adapter).remove(dt1200);
    }

    @Test
    public void testRemove2() {
        /*
         * Remove spatial time that is the only frame for that time -> data is
         * cleared out for that frame, and the non-spatial version of that time
         * is removed from the adapter
         */
        AbstractCrossSectionResource rsc = new TestConcreteCrossSectionResource(
                rscData, mock(LoadProperties.class),
                mock(AbstractCrossSectionAdapter.class), List.of(dt1200_0));

        rsc.sliceMap.put(dt1200_0, sliceFuture);
        rsc.dataRetrievalJob.times.add(dt1200_0);

        rsc.remove(dt1200_0);

        assertTrue(rsc.getDataTimes().length == 0);
        assertTrue(rsc.sliceMap.isEmpty());
        assertTrue(rsc.dataRetrievalJob.times.isEmpty());
        verify(rsc.adapter).remove(dt1200);
    }

    @Test
    public void testRemove3() {
        /*
         * Remove 1 of 2 frames for time -> data is removed for only that frame
         * and not the other, and the time is not removed from adapter since a
         * frame remains for it
         */
        AbstractCrossSectionResource rsc = new TestConcreteCrossSectionResource(
                rscData, mock(LoadProperties.class),
                mock(AbstractCrossSectionAdapter.class),
                List.of(dt1200_0, dt1200_1));

        rsc.sliceMap.put(dt1200_0, sliceFuture);
        rsc.sliceMap.put(dt1200_1, sliceFuture);
        rsc.dataRetrievalJob.times.add(dt1200_0);
        rsc.dataRetrievalJob.times.add(dt1200_1);

        rsc.remove(dt1200_0);

        assertArrayEquals(new DataTime[] { dt1200_1 }, rsc.getDataTimes());
        assertNull(rsc.sliceMap.get(dt1200_0));
        assertSame(sliceFuture, rsc.sliceMap.get(dt1200_1));
        assertFalse(rsc.dataRetrievalJob.times.contains(dt1200_0));
        assertTrue(rsc.dataRetrievalJob.times.contains(dt1200_1));
        verify(rsc.adapter, never()).remove(any());
    }

    @Test
    public void testRedraw() {
        /*
         * Verify that processed frame data is cleared and frame times are
         * scheduled for re-processing
         */
        AbstractCrossSectionResource rsc = new TestConcreteCrossSectionResource(
                mock(CrossSectionResourceData.class),
                mock(LoadProperties.class),
                mock(AbstractCrossSectionAdapter.class), List.of(dt1200_0));
        rsc.sliceMap.put(new DataTime(),
                CompletableFuture.completedFuture(null));

        rsc.redraw();

        assertTrue(rsc.sliceMap.isEmpty());
        assertTrue(rsc.dataRetrievalJob.times.contains(dt1200_0));
    }

    static Stream<Arguments> provideParamsForGetName() {
        DisplayTypeCapability imgDispCap = new DisplayTypeCapability();
        imgDispCap.setDisplayType(DisplayType.IMAGE);
        DisplayTypeCapability contourDispCap = new DisplayTypeCapability();
        contourDispCap.setDisplayType(DisplayType.CONTOUR);

        // Contour product without extra text
        Arguments args1 = Arguments.of("radar-koax", "", "Reflectivity",
                "LineA", "dBZ", contourDispCap,
                "radar-koax LineA Reflectivity (dBZ)");
        // Image product without extra text
        Arguments args2 = Arguments.of("radar-koax", "", "Reflectivity",
                "LineA", "dBZ", imgDispCap,
                "radar-koax LineA Reflectivity Img (dBZ)");
        // Contour product with extra text
        Arguments args3 = Arguments.of("Raob", "74389-72501", "Temperature",
                "LineC", "C", contourDispCap,
                "Raob LineC 74389-72501 Temperature (C)");
        // Image product with extra text
        Arguments args4 = Arguments.of("Raob", "62350-62370", "Temperature",
                "LineC", "C", imgDispCap,
                "Raob LineC 62350-62370 Temperature Img (C)");

        return Stream.of(args1, args2, args3, args4);
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetName")
    void testGetName(String source, String adapterExtraText, String param,
            String lineId, String unitStr, DisplayTypeCapability dispCap,
            String expectedName) {
        AbstractCrossSectionResource rsc = new TestConcreteCrossSectionResource(
                mock(CrossSectionResourceData.class),
                mock(LoadProperties.class),
                mock(AbstractCrossSectionAdapter.class));

        rsc = spy(rsc);
        rsc.setDescriptor(mock(CrossSectionDescriptor.class));
        when(rsc.getResourceData().getSource()).thenReturn(source);
        when(rsc.adapter.getExtraNameText()).thenReturn(adapterExtraText);
        when(rsc.getResourceData().getParameterName()).thenReturn(param);
        when(rsc.getDescriptor().getLineID()).thenReturn(lineId);
        doReturn(unitStr).when(rsc).getUnitString();
        doReturn(dispCap).when(rsc).getCapability(DisplayTypeCapability.class);

        String name = rsc.getName();

        assertEquals(expectedName, name);
    }

    /**
     * Concrete implementation of {@link AbstractCrossSectionResource} to allow
     * us to instantiate it for testing. Also allows us to populate its
     * protected dataTimes field.
     */
    private static class TestConcreteCrossSectionResource
            extends AbstractCrossSectionResource {

        public TestConcreteCrossSectionResource(CrossSectionResourceData data,
                LoadProperties props, AbstractCrossSectionAdapter<?> adapter) {
            this(data, props, adapter, List.of());
        }

        public TestConcreteCrossSectionResource(CrossSectionResourceData data,
                LoadProperties props, AbstractCrossSectionAdapter<?> adapter,
                Collection<DataTime> dts) {
            super(data, props, adapter);
            dataTimes.addAll(dts);
        }

        @Override
        public CrossSectionFrameRenderable getFrameRenderable(
                DataTime frameTime) {
            return null;
        }
    }
}
