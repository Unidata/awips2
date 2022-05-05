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

import java.awt.image.RenderedImage;
import java.util.List;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ext.colormap.ColormappedImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.drawables.image.stipple.IStippledColormappedImage;
import com.raytheon.uf.viz.drawables.image.stipple.Stippler;

/**
 * Generic implementation of {@link IStippledColormappedImage}. This
 * implementation just extends {@link ColormappedImage} and adds in an extra
 * step to process the generated image with the supplied stipple patterns.
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
public class GenericStippledColormappedImage extends ColormappedImage
        implements IStippledColormappedImage {

    private final InternalCallback callback;

    private List<byte[]> fillPatterns;

    public GenericStippledColormappedImage(IGraphicsTarget target,
            IColorMapDataRetrievalCallback callback,
            ColorMapParameters parameters) {
        this(target, new InternalCallback(callback), parameters);
    }

    private GenericStippledColormappedImage(IGraphicsTarget target,
            InternalCallback callback, ColorMapParameters parameters) {
        super(target, callback, parameters);
        this.callback = callback;
    }

    @Override
    public void setFillPatterns(List<byte[]> fillPatterns) {
        this.fillPatterns = fillPatterns;
    }

    @Override
    public RenderedImage getImage() throws VizException {
        RenderedImage image = super.getImage();
        if (image == null || fillPatterns == null) {
            callback.clear();
            return image;
        }
        image = Stippler.stipple(image, getColorMapParameters(),
                callback.getColorMapData(), fillPatterns);
        callback.clear();
        return image;
    }

    /**
     * This class is necessary because we need the raw data for for selecting a
     * fill pattern but the superclass grabs the data and throws it away. This
     * callback will hold the data temporarily so the callback is not used twice
     * in a row which could cause performance problems.
     */
    private static class InternalCallback
            implements IColorMapDataRetrievalCallback {

        private final IColorMapDataRetrievalCallback callback;

        private ColorMapData data;

        public InternalCallback(IColorMapDataRetrievalCallback callback) {
            this.callback = callback;
        }

        @Override
        public ColorMapData getColorMapData() throws VizException {
            if (data == null) {
                data = callback.getColorMapData();
            }
            return data;
        }

        public void clear() {
            this.data = null;
        }

    }

}
