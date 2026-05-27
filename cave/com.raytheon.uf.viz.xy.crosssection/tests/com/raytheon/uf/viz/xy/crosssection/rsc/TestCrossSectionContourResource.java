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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionContour;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameRenderable;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;

/**
 * Unit tests for {@link CrossSectionContourResource}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2024 2037631    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
@ExtendWith(MockitoExtension.class)
class TestCrossSectionContourResource {

    private DataTime dt1;

    private DataTime dt2;

    @Mock
    private CrossSectionContour contour1;

    @Mock
    private CrossSectionContour contour2;

    private CrossSectionContourResource rsc;

    @BeforeEach
    public void setupBeforeEach() throws Exception {
        dt1 = new DataTime("2024-03-26_12:00:00.0");
        dt1.setLevel(0d, "LineA");

        dt2 = new DataTime("2024-03-26_12:06:00.0");
        dt2.setLevel(0d, "LineA");

        CrossSectionResourceData rscData = mock(CrossSectionResourceData.class);
        LoadProperties props = new LoadProperties();
        AbstractCrossSectionAdapter<?> adapter = mock(
                AbstractCrossSectionAdapter.class);

        rsc = new CrossSectionContourResource(rscData, props, adapter);
    }

    @Test
    void testDisposeFrame1() {
        // Dispose frame that doesn't exist -> no exception
        rsc.disposeFrame(dt1, false);
    }

    @Test
    void testDisposeFrame2() {
        /*
         * Dispose dt1 frame -> verify it's removed from contours map and
         * contour is disposed, and dt2 mapping is fully left alone
         */
        rsc.contours.put(dt1, contour1);
        rsc.contours.put(dt2, contour2);

        rsc.disposeFrame(dt1, false);

        assertTrue(!rsc.contours.containsKey(dt1));
        verify(contour1).dispose();
        assertEquals(contour2, rsc.contours.get(dt2));
        verify(contour2, never()).dispose();
    }

    @Test
    void testDisposeFrames1() {
        // Dispose frames when contours map is empty -> no exception
        rsc.disposeFrames();
    }

    @Test
    void testDisposeFrames2() {
        /*
         * Setup multiple contour mappings -> verify map is cleared and all
         * contours are disposed
         */
        rsc.contours.put(dt1, contour1);
        rsc.contours.put(dt2, contour2);

        rsc.disposeFrames();

        assertTrue(rsc.contours.isEmpty());
        verify(contour1).dispose();
        verify(contour2).dispose();
    }

    @Test
    void testGetFrameRenderable1() {
        // Get renderable for frame that's not in map -> null returned
        CrossSectionFrameRenderable renderable = rsc.getFrameRenderable(dt1);

        assertNull(renderable);
    }

    @Test
    void testGetFrameRenderable2() {
        // Get renderable for frame in map -> corresponding value returned
        rsc.contours.put(dt1, contour1);
        rsc.contours.put(dt2, contour2);

        CrossSectionFrameRenderable renderable = rsc.getFrameRenderable(dt2);

        assertEquals(renderable, contour2);
    }
}
