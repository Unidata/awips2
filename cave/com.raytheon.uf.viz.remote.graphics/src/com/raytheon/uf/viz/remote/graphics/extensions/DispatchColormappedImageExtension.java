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
package com.raytheon.uf.viz.remote.graphics.extensions;

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.colormap.ColorMapDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.CreateColormappedImageEvent;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingColormappedImage;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchColormappedImageExtension extends
        AbstractDispatchingImageExtension implements IColormappedImageExtension {

    public class DispatchingColormappedCallback implements
            IColorMapDataRetrievalCallback {

        private IColorMapDataRetrievalCallback callback;

        private DispatchingColormappedImage<?> image;

        public DispatchingColormappedCallback(
                IColorMapDataRetrievalCallback callback) {
            this.callback = callback;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback#
         * getColorMapData()
         */
        @Override
        public ColorMapData getColorMapData() throws VizException {
            ColorMapData data = callback.getColorMapData();
            ColorMapDataEvent event = RemoteGraphicsEventFactory.createEvent(
                    ColorMapDataEvent.class, image);
            event.setColorMapData(data);
            image.dispatch(event);
            return data;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension
     * #
     * initializeRaster(com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback
     * , com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public IColormappedImage initializeRaster(
            IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters colorMapParameters) {
        IColormappedImageExtension targetExt;
        try {
            targetExt = target.getWrappedObject().getExtension(
                    IColormappedImageExtension.class);
        } catch (VizException e) {
            throw new RuntimeException(
                    "Could not get IColormappedImageExtension from targetObject",
                    e);
        }
        DispatchingColormappedCallback wrapper = new DispatchingColormappedCallback(
                dataCallback);
        IColormappedImage actualImage = targetExt.initializeRaster(wrapper,
                colorMapParameters);
        DispatchingColormappedImage<IColormappedImage> image = new DispatchingColormappedImage<IColormappedImage>(
                actualImage, DispatchColormappedImageExtension.class,
                target.getDispatcher(), colorMapParameters);
        wrapper.image = image;

        // Send creation event
        CreateColormappedImageEvent creation = RemoteGraphicsEventFactory
                .createEvent(CreateColormappedImageEvent.class, image);
        if (colorMapParameters != null) {
            creation.setColorMapParameters(DispatchingColormappedImage
                    .createColorMapParametersUpdateEvent(image));
        }
        target.dispatch(creation);

        // Return image
        return image;
    }

}
