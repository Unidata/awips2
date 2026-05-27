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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameData;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionImage;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;

/**
 * Unit tests for {@link CrossSectionIntermediateImageResource}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr  2, 2024 2037091    mapeters    Initial creation
 * May 22, 2024 2037092    mapeters    Add testCleanupPreviousMaps3
 * May 29, 2024 2037244    mapeters    Add testRedraw
 * Jun 17, 2024 2037092    mapeters    Replace deprecated code and refactor to
 *                                     prevent unnecessary stubbing errors
 * Aug 20, 2024 2037631    mapeters    Floats and images are now wrapped in new classes
 *
 * </pre>
 *
 * @author mapeters
 */
@ExtendWith(MockitoExtension.class)
public class TestCrossSectionIntermediateImageResource {

    private static final float[] prevSlice = { 0f };

    private static final float[] currSlice = { 1f };

    private static final CrossSectionFrameData prevSliceList = new CrossSectionFrameData(
            List.of(prevSlice), null);

    private static final CrossSectionFrameData currSliceList = new CrossSectionFrameData(
            List.of(currSlice), null);

    private static final Future<CrossSectionFrameData> prevSliceFuture = CompletableFuture
            .completedFuture(prevSliceList);

    private static final Future<CrossSectionFrameData> currSliceFuture = CompletableFuture
            .completedFuture(currSliceList);

    @Mock
    private CrossSectionImage prevImage;

    @Mock
    private CrossSectionImage currImage;

    @Mock
    private IGraphicsTarget target;

    private CrossSectionIntermediateImageResource rsc;

    private DataTime dt;

    @BeforeEach
    public void setupBeforeEach() throws Exception {
        dt = new DataTime("2024-03-26_12:00:00.0");
        dt.setLevel(0d, "LineA");

        CrossSectionResourceData rscData = mock(CrossSectionResourceData.class);
        LoadProperties props = new LoadProperties();
        AbstractCrossSectionAdapter<?> adapter = mock(
                AbstractCrossSectionAdapter.class);

        rsc = new TestExtendedCrossSectionIntermediateImageResource(rscData,
                props, adapter, List.of(dt));
    }

    @Test
    public void testGetOrCreateImage1() throws VizException {
        // Only previous image -> previous image is used
        rsc.previousImageMap.put(dt, prevImage);

        CrossSectionImage actualResult = rsc.getOrCreateImage(target,
                getPaintPropsForDt());

        assertSame(prevImage, actualResult);
    }

    @Test
    public void testGetOrCreateImage2() throws VizException {
        // Previous and current images -> current is used
        rsc.previousImageMap.put(dt, prevImage);
        rsc.imageMap.put(dt, currImage);

        CrossSectionImage actualResult = rsc.getOrCreateImage(target,
                getPaintPropsForDt());

        assertSame(currImage, actualResult);
    }

    @Test
    public void testGetOrCreateImage3() throws VizException {
        // Only previous data -> image will be created from previous data
        rsc.previousSliceMap.put(dt, prevSliceFuture);
        rsc = spy(rsc);
        doReturn(prevImage).when(rsc).constructImage(eq(dt), eq(prevSliceList),
                eq(target));

        CrossSectionImage actualResult = rsc.getOrCreateImage(target,
                getPaintPropsForDt());

        assertSame(prevImage, actualResult);
    }

    @Test
    public void testGetOrCreateImage4() throws VizException {
        // Only current data -> image will be created from current data
        rsc.sliceMap.put(dt, currSliceFuture);
        rsc = spy(rsc);
        doReturn(currImage).when(rsc).constructImage(eq(dt), eq(currSliceList),
                eq(target));

        CrossSectionImage actualResult = rsc.getOrCreateImage(target,
                getPaintPropsForDt());

        assertSame(currImage, actualResult);
    }

    @Test
    public void testGetOrCreateImage5() throws VizException {
        /*
         * Current data and previous data/image -> image will be created from
         * current data
         */
        rsc.previousSliceMap.put(dt, prevSliceFuture);
        rsc.previousImageMap.put(dt, prevImage);
        rsc.sliceMap.put(dt, currSliceFuture);
        rsc = spy(rsc);
        doReturn(currImage).when(rsc).constructImage(eq(dt), eq(currSliceList),
                eq(target));

        CrossSectionImage actualResult = rsc.getOrCreateImage(target,
                getPaintPropsForDt());

        assertSame(currImage, actualResult);
    }

    @Test
    public void testGetOrCreateImage6() throws VizException {
        // Paint props with null time -> null image
        PaintProperties paintPropsWithNullDt = mock(PaintProperties.class);

        CrossSectionImage actualResult = rsc.getOrCreateImage(target,
                paintPropsWithNullDt);

        assertNull(actualResult);
        verify(paintPropsWithNullDt, atLeastOnce()).getDataTime();
    }

    @Test
    public void testGetInspectData1() throws VizException {
        // Only previous data -> previous data returned
        rsc.previousSliceMap.put(dt, prevSliceFuture);

        float[] actualResult = rsc.getInspectData(dt);

        assertSame(prevSlice, actualResult);
    }

    @Test
    public void testGetInspectData2() throws VizException {
        // Previous and current data -> current data returned
        rsc.previousSliceMap.put(dt, prevSliceFuture);
        rsc.sliceMap.put(dt, currSliceFuture);

        float[] actualResult = rsc.getInspectData(dt);

        assertSame(currSlice, actualResult);
    }

    @Test
    public void testGetInspectData3() throws VizException {
        // No data -> null returned
        float[] actualResult = rsc.getInspectData(dt);

        assertNull(actualResult);
    }

    @Test
    public void testGetSliceData1() throws VizException {
        // Test pulling from main sliceMap when map arg is not passed
        rsc.previousSliceMap.put(dt, prevSliceFuture);
        rsc.sliceMap.put(dt, currSliceFuture);

        CrossSectionFrameData actualResult = rsc.getSliceData(dt);

        assertSame(currSliceList, actualResult);
    }

    @Test
    public void testGetSliceData2() throws VizException {
        // Test pulling from previousSliceMap when previousSliceMap is passed
        rsc.previousSliceMap.put(dt, prevSliceFuture);
        rsc.sliceMap.put(dt, currSliceFuture);

        CrossSectionFrameData actualResult = rsc.getSliceData(dt,
                rsc.previousSliceMap);

        assertSame(prevSliceList, actualResult);
    }

    @Test
    public void testDisposeFrames() throws VizException {
        // Previous and current images/data -> all cleared and images disposed
        rsc.previousImageMap.put(dt, prevImage);
        rsc.imageMap.put(dt, currImage);
        rsc.previousSliceMap.put(dt, prevSliceFuture);
        rsc.sliceMap.put(dt, currSliceFuture);

        rsc.disposeFrames();

        assertTrue(rsc.previousImageMap.isEmpty());
        assertTrue(rsc.imageMap.isEmpty());
        assertTrue(rsc.previousSliceMap.isEmpty());
        assertTrue(rsc.sliceMap.isEmpty());
        verify(prevImage).dispose();
        verify(currImage).dispose();
    }

    @Test
    public void testDisposeFrame1() throws VizException {
        // Pass onUpdate = false -> all data should be disposed
        rsc.previousImageMap.put(dt, prevImage);
        rsc.imageMap.put(dt, currImage);
        rsc.previousSliceMap.put(dt, prevSliceFuture);
        rsc.sliceMap.put(dt, currSliceFuture);

        rsc.disposeFrame(dt, false);

        assertTrue(rsc.previousImageMap.isEmpty());
        assertTrue(rsc.imageMap.isEmpty());
        assertTrue(rsc.previousSliceMap.isEmpty());
        assertTrue(rsc.sliceMap.isEmpty());
        verify(prevImage).dispose();
        verify(currImage).dispose();
    }

    @Test
    public void testDisposeFrame2() throws VizException {
        // Pass onUpdate = true -> only current data should be cleared/disposed
        rsc.previousImageMap.put(dt, prevImage);
        rsc.imageMap.put(dt, currImage);
        rsc.previousSliceMap.put(dt, prevSliceFuture);
        rsc.sliceMap.put(dt, currSliceFuture);

        rsc.disposeFrame(dt, true);

        assertSame(prevImage, rsc.previousImageMap.get(dt));
        assertTrue(rsc.imageMap.isEmpty());
        assertSame(prevSliceFuture, rsc.previousSliceMap.get(dt));
        assertTrue(rsc.sliceMap.isEmpty());
        verify(prevImage, never()).dispose();
        verify(currImage).dispose();
    }

    @Test
    public void testDataUpdateArrived() throws VizException {
        // Current data/images should be moved to previous maps
        when(rsc.getResourceData().getAffectedFrameTimes(dt))
                .thenReturn(List.of(dt));
        rsc.previousImageMap.put(dt, prevImage);
        rsc.imageMap.put(dt, currImage);
        rsc.previousSliceMap.put(dt, prevSliceFuture);
        rsc.sliceMap.put(dt, currSliceFuture);

        rsc.dataUpdateArrived(dt);

        assertNull(rsc.imageMap.get(dt));
        assertNull(rsc.sliceMap.get(dt));
        assertSame(rsc.previousImageMap.get(dt), currImage);
        assertSame(rsc.previousSliceMap.get(dt), currSliceFuture);
        verify(prevImage).dispose();
    }

    @Test
    public void testCleanupPreviousMaps1() {
        // Current and previous entries -> previous should be cleaned up
        rsc.previousImageMap.put(dt, prevImage);
        rsc.imageMap.put(dt, currImage);
        rsc.previousSliceMap.put(dt, prevSliceFuture);
        rsc.sliceMap.put(dt, currSliceFuture);

        rsc.cleanupPreviousMaps();

        assertNull(rsc.previousImageMap.get(dt));
        assertNull(rsc.previousSliceMap.get(dt));
        verify(prevImage).dispose();
    }

    @Test
    public void testCleanupPreviousMaps2() {
        // Only previous entries -> nothing should be done
        rsc.previousImageMap.put(dt, prevImage);
        rsc.previousSliceMap.put(dt, prevSliceFuture);

        rsc.cleanupPreviousMaps();

        assertNotNull(rsc.previousImageMap.get(dt));
        assertNotNull(rsc.previousSliceMap.get(dt));
    }

    @Test
    public void testRedraw() {
        /*
         * Verify that processed frame data is cleared and frame times are
         * scheduled for re-processing
         */
        rsc.previousImageMap.put(dt, prevImage);
        rsc.imageMap.put(dt, currImage);
        rsc.previousSliceMap.put(dt, prevSliceFuture);
        rsc.sliceMap.put(dt, currSliceFuture);

        rsc.redraw();

        assertTrue(rsc.previousImageMap.isEmpty());
        assertTrue(rsc.imageMap.isEmpty());
        assertTrue(rsc.previousSliceMap.isEmpty());
        assertTrue(rsc.sliceMap.isEmpty());
        assertTrue(rsc.dataRetrievalJob.times.contains(dt));
        verify(prevImage).dispose();
        verify(currImage).dispose();
    }

    @Test
    public void testGetFrameRenderable1() throws VizException {
        // No images for time -> null returned
        CrossSectionImage actualResult = rsc.getFrameRenderable(dt);

        assertNull(actualResult);
    }

    @Test
    public void testGetFrameRenderable2() throws VizException {
        // Only previous image -> previous image returned
        rsc.previousImageMap.put(dt, prevImage);

        CrossSectionImage actualResult = rsc.getFrameRenderable(dt);

        assertSame(prevImage, actualResult);
    }

    @Test
    public void testGetFrameRenderable3() throws VizException {
        // Previous and current images -> current image returned
        rsc.imageMap.put(dt, currImage);
        rsc.previousImageMap.put(dt, prevImage);

        CrossSectionImage actualResult = rsc.getFrameRenderable(dt);

        assertSame(currImage, actualResult);
    }

    private PaintProperties getPaintPropsForDt() {
        PaintProperties paintProps = mock(PaintProperties.class);
        when(paintProps.getDataTime()).thenReturn(dt);
        return paintProps;
    }

    /**
     * Sub-class of {@link CrossSectionIntermediateImageResource} for testing.
     * This just allows us to populate its protected dataTimes field.
     */
    private static class TestExtendedCrossSectionIntermediateImageResource
            extends CrossSectionIntermediateImageResource {

        public TestExtendedCrossSectionIntermediateImageResource(
                CrossSectionResourceData resourceData, LoadProperties props,
                AbstractCrossSectionAdapter<?> adapter,
                Collection<DataTime> dts) {
            super(resourceData, props, adapter);
            dataTimes.addAll(dts);
        }
    }
}
