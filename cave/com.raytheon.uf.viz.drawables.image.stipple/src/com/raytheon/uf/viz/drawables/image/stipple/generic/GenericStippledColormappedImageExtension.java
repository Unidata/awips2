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
package com.raytheon.uf.viz.drawables.image.stipple.generic;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.drawables.image.stipple.IStippledColormappedImage;
import com.raytheon.uf.viz.drawables.image.stipple.IStippledColormappedImageExtension;

/**
 * 
 * Generic implementation of {@link IStippledColormappedImageExtension}
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Nov 02, 2016  5957     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public class GenericStippledColormappedImageExtension
        extends GraphicsExtension<IGraphicsTarget>
        implements IStippledColormappedImageExtension {

    @Override
    public IStippledColormappedImage initializeRaster(
            IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters colorMapParameters) {
        return new GenericStippledColormappedImage(target, dataCallback,
                colorMapParameters);
    }

    @Override
    public int getCompatibilityValue(IGraphicsTarget target) {
        return Compatibilty.GENERIC;
    }

    @Override
    public boolean drawRasters(PaintProperties paintProps,
            DrawableImage... images) throws VizException {
        List<DrawableImage> renderables = new ArrayList<>();
        for (DrawableImage di : images) {
            if (di.getImage() instanceof GenericStippledColormappedImage) {
                renderables
                        .add(new DrawableImage(
                                ((GenericStippledColormappedImage) di
                                        .getImage()).getWrappedImage(),
                                di.getCoverage()));
            } else {
                throw new IllegalArgumentException(
                        this.getClass().getSimpleName()
                                + " cannot handle images of type: "
                                + di.getImage().getClass().getSimpleName());
            }
        }
        return target.drawRasters(paintProps,
                renderables.toArray(new DrawableImage[renderables.size()]));
    }

}
