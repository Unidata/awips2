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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.rsc.capabilities.ICapabilityProvider;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;

/**
 * Unit tests for {@link CrossSectionImage}.
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
class TestCrossSectionImage {

    @Mock
    private DrawableImage drawableImage;

    @Mock
    private CrossSectionFrameExtraRenderable extraRenderable;

    private ICapabilityProvider capProvider;

    @BeforeEach
    void setupBeforeEach() {
        ImagingCapability imgCap = mock(ImagingCapability.class);
        capProvider = mock(ICapabilityProvider.class);
        when(capProvider.getCapability(ImagingCapability.class))
                .thenReturn(imgCap);
    }

    @Test
    void testDispose1() {
        // No renderables -> just verify that no exception occurs
        CrossSectionImage image = new CrossSectionImage(null, capProvider,
                null);

        image.dispose();
    }

    @Test
    void testDispose2() {
        // Non-null image -> verify that it's disposed
        CrossSectionImage image = new CrossSectionImage(drawableImage,
                capProvider, null);

        image.dispose();

        verify(drawableImage).dispose();
    }

    @Test
    void testDispose3() {
        // Null image, but extra renderable -> verify that extra is disposed
        CrossSectionImage image = new CrossSectionImage(null, capProvider,
                extraRenderable);

        image.dispose();

        verify(extraRenderable).dispose();
    }

    @Test
    void testDispose4() {
        // Image and extra renderable -> verify that both are disposed
        CrossSectionImage image = new CrossSectionImage(drawableImage,
                capProvider, extraRenderable);

        image.dispose();

        verify(drawableImage).dispose();
        verify(extraRenderable).dispose();
    }
}
