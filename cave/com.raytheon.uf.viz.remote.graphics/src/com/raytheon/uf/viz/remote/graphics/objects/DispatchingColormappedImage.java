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
package com.raytheon.uf.viz.remote.graphics.objects;

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.colormap.ColorMapDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.CreateColormappedImageEvent;
import com.raytheon.uf.viz.remote.graphics.extensions.DispatchColormappedImageExtension;

/**
 * Dispatching colormapped image object created from graphics image and forwards
 * key events to remote clients
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

public class DispatchingColormappedImage extends
        AbstractDispatchingColormappedImage<IColormappedImage> implements
        IColormappedImage {

    public static class DispatchingColormappedCallback implements
            IColorMapDataRetrievalCallback {

        private IColorMapDataRetrievalCallback callback;

        private DispatchingColormappedImage image;

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

    /**
     * @param targetObject
     * @param dispatcher
     */
    public DispatchingColormappedImage(IColormappedImage targetObject,
            DispatchingColormappedCallback callback, Dispatcher dispatcher) {
        super(targetObject, DispatchColormappedImageExtension.class,
                dispatcher, targetObject.getColorMapParameters());
        callback.image = this;
        ColorMapParameters parameters = targetObject.getColorMapParameters();

        // Send creation event
        CreateColormappedImageEvent creation = RemoteGraphicsEventFactory
                .createEvent(CreateColormappedImageEvent.class, this);
        if (parameters != null) {
            creation.setColorMapParameters(createColorMapParametersUpdateEvent(parameters));
        }

        dispatch(creation);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IColormappedImage#getColorMapParameters
     * ()
     */
    @Override
    public ColorMapParameters getColorMapParameters() {
        return wrappedObject.getColorMapParameters();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IColormappedImage#setColorMapParameters
     * (com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public void setColorMapParameters(ColorMapParameters params) {
        ColorMapParameters parameters = getColorMapParameters();
        if (params != parameters) {
            if (parameters != null) {
                parameters.removeListener(this);
            }
            wrappedObject.setColorMapParameters(params);
            if (params != null) {
                params.addListener(this);
                dispatch(createColorMapParametersUpdateEvent(params));
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IColormappedImage#getValue(int,
     * int)
     */
    @Override
    public double getValue(int x, int y) {
        return wrappedObject.getValue(x, y);
    }

}
