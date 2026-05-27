/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.truecolor.extension.generic;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension;

/**
 * 
 * Generic implementation of {@link ITrueColorImagingExtension} that can be used
 * with any {@link IGraphicsTarget} which is able to render images. This
 * implementation is quite slow and generates lower resolution images so that it
 * can try to keep up.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 06, 2016  5400     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GenericTrueColorImagingExtension extends
        GraphicsExtension<IGraphicsTarget> implements
        ITrueColorImagingExtension {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GenericTrueColorImagingExtension.class);

    @Override
    public boolean drawRasters(PaintProperties paintProps,
            DrawableImage... images) throws VizException {
        List<DrawableImage> newImages = new ArrayList<>();
        for (int i = 0; i < images.length; i += 1) {
            IImage image = images[i].getImage();
            if (image == null) {
                throw new IllegalArgumentException(this.getClass()
                        .getSimpleName() + " cannot handle null images");

            } else if (!(image instanceof GenericTrueColorImage)) {
                throw new IllegalArgumentException(this.getClass()
                        .getSimpleName()
                        + " cannot handle images of type: "
                        + image.getClass().getSimpleName());
            }
            GenericTrueColorImage tcImage = (GenericTrueColorImage) image;

            newImages.addAll(tcImage.getRenderedImages());
        }
        return target.drawRasters(paintProps,
                newImages.toArray(new DrawableImage[0]));
    }

    @Override
    public ITrueColorImage initializeRaster(int[] imageBounds,
            IExtent imageExtent) throws VizException {
        statusHandler.info(this.getClass().getSimpleName()
                + " is being used for " + target.getClass().getSimpleName()
                        + ". This extension does not generate imagery at full resolution.");
        return new GenericTrueColorImage(target, imageBounds, imageExtent);
    }

    @Override
    public int getCompatibilityValue(IGraphicsTarget target) {
        return Compatibilty.GENERIC;
    }

}
