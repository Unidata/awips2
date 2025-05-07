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

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ICapabilityProvider;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;

/**
 * A renderable image for a single cross section frame.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14, 2024 2037631    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class CrossSectionImage extends CrossSectionFrameRenderable {

    private final DrawableImage drawableImage;

    private final ImagingCapability imgCap;

    public CrossSectionImage(DrawableImage drawableImage,
            ICapabilityProvider capProvider,
            CrossSectionFrameExtraRenderable extraRenderable) {
        super(capProvider, extraRenderable);
        this.drawableImage = drawableImage;

        imgCap = capProvider.getCapability(ImagingCapability.class);
        if (!imgCap.isBrightnessSet()) {
            imgCap.setBrightness(0.5f);
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (drawableImage == null) {
            return;
        }

        IImage img = drawableImage.getImage();
        img.setBrightness(imgCap.getBrightness());
        img.setContrast(imgCap.getContrast());
        img.setInterpolated(imgCap.isInterpolationState());

        target.drawRaster(img, drawableImage.getCoverage(), paintProps);
    }

    @Override
    protected void disposeInternal() {
        if (drawableImage != null) {
            drawableImage.dispose();
        }
    }
}
