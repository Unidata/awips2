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

import java.awt.image.RenderedImage;

import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.imagery.RenderedImageEvent;

/**
 * Dispatching image object created from existing image and forwards key events
 * to remote clients
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

public class DispatchingImage extends AbstractDispatchingImage<IImage> {

    public static class DispatchingRenderedImageCallback implements
            IRenderedImageCallback {

        private IRenderedImageCallback callback;

        private DispatchingImage image;

        public DispatchingRenderedImageCallback(IRenderedImageCallback callback) {
            this.callback = callback;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.data.IRenderedImageCallback#getImage()
         */
        @Override
        public RenderedImage getImage() throws VizException {
            RenderedImage image = callback.getImage();
            RenderedImageEvent event = RemoteGraphicsEventFactory.createEvent(
                    RenderedImageEvent.class, this.image);
            event.setRenderedImage(image);
            this.image.dispatch(event);
            return image;
        }
    }

    /**
     * @param targetObject
     * @param dispatcher
     */
    public DispatchingImage(IImage targetObject,
            Class<? extends IImagingExtension> extension,
            DispatchingRenderedImageCallback callback, Dispatcher dispatcher) {
        super(targetObject, extension, dispatcher);
        if (callback != null) {
            callback.image = this;
        }
    }

}
