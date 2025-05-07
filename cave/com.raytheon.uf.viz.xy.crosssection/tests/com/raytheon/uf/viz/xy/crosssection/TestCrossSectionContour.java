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
package com.raytheon.uf.viz.xy.crosssection;

import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.viz.core.contours.ContourGroup;

/**
 * Unit tests for {@link CrossSectionContour}.
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
public class TestCrossSectionContour {

    @Mock
    private ContourGroup group1;

    @Mock
    private ContourGroup group2;

    @Mock
    private CrossSectionFrameExtraRenderable extraRenderable;

    @Test
    void testDispose1() {
        // No renderables -> nothing is done, but no exceptions
        CrossSectionContour contour = new CrossSectionContour(null, null, null);

        contour.dispose();
    }

    @Test
    void testDispose2() {
        /*
         * Setup multiple groups -> verify they're all disposed
         */
        CrossSectionContour contour = new CrossSectionContour(null, null, null);
        contour.setContourGroup(1, group1);
        contour.setContourGroup(3, group2);

        contour.dispose();

        verify(group1).dispose();
        verify(group2).dispose();
    }

    @Test
    void testDispose3() {
        // Only extra renderable -> it's disposed
        CrossSectionContour contour = new CrossSectionContour(null, null,
                extraRenderable);

        contour.dispose();

        verify(extraRenderable).dispose();
    }

    @Test
    void testDispose4() {
        /*
         * Setup multiple groups and extra renderable -> verify they're all
         * disposed
         */
        CrossSectionContour contour = new CrossSectionContour(null, null,
                extraRenderable);
        contour.setContourGroup(1, group1);
        contour.setContourGroup(3, group2);

        contour.dispose();

        verify(group1).dispose();
        verify(group2).dispose();
        verify(extraRenderable).dispose();
    }

    @Test
    void testSetAndGetContourGroup1() {
        // Set new group -> verify that get returns it
        CrossSectionContour contour = new CrossSectionContour(null, null, null);

        contour.setContourGroup(1, group1);

        verify(group1, never()).dispose();
        assertSame(group1, contour.getContourGroup(1));
    }

    @Test
    void testSetAndGetContourGroup2() {
        /*
         * Set group for level that already has group -> verify that old group
         * is disposed and get returns new group
         */
        CrossSectionContour contour = new CrossSectionContour(null, null, null);
        contour.setContourGroup(1, group1);

        contour.setContourGroup(1, group2);

        verify(group1).dispose();
        verify(group2, never()).dispose();
        assertSame(group2, contour.getContourGroup(1));
    }

    @Test
    void testSetAndGetContourGroup3() {
        /*
         * Set group for level that doesn't have group, but another level has a
         * group -> verify that neither group is disposed, and get returns the
         * right group for both levels
         */
        CrossSectionContour contour = new CrossSectionContour(null, null, null);
        contour.setContourGroup(1, group1);

        contour.setContourGroup(2, group2);

        verify(group1, never()).dispose();
        verify(group2, never()).dispose();
        assertSame(group1, contour.getContourGroup(1));
        assertSame(group2, contour.getContourGroup(2));
    }
}
