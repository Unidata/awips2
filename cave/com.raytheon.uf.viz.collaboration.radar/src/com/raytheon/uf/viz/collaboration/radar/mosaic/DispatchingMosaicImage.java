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
package com.raytheon.uf.viz.collaboration.radar.mosaic;

import java.util.Arrays;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.imagery.PaintImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.imagery.PaintImagesEvent;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingColormappedImage;
import com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicImageExtension.IMosaicImage;

/**
 * Dispatching mosaic image object created from graphics image and forwards key
 * events to remote clients
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingMosaicImage extends
        DispatchingColormappedImage<IMosaicImage> implements IMosaicImage {

    private PaintImageEvent[] imagesToMosaic;

    private IExtent imageExtent;

    /**
     * @param targetObject
     * @param extensionClass
     * @param dispatcher
     */
    public DispatchingMosaicImage(IMosaicImage targetObject,
            Class<? extends IImagingExtension> extensionClass,
            Dispatcher dispatcher, ColorMapParameters parameters,
            IExtent imageExtent) {
        super(targetObject, extensionClass, dispatcher, parameters);
        this.imageExtent = imageExtent;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicImageExtension.IMosaicImage
     * #setImagesToMosaic(com.raytheon.uf.viz.core.DrawableImage[])
     */
    @Override
    public void setImagesToMosaic(DrawableImage... images) {
        wrappedObject.setImagesToMosaic(PaintImagesEvent
                .extractTargetImages(images));
        PaintImageEvent[] imagesToMosaic = PaintImagesEvent
                .toPaintEvents(images);
        if (Arrays.equals(imagesToMosaic, this.imagesToMosaic) == false) {
            this.imagesToMosaic = imagesToMosaic;
            UpdateImagesToMosaic event = RemoteGraphicsEventFactory
                    .createEvent(UpdateImagesToMosaic.class, this);
            event.setImagesToMosaic(imagesToMosaic);
            dispatch(event);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicImageExtension.IMosaicImage
     * #setImageExtent(com.raytheon.uf.viz.core.IExtent)
     */
    @Override
    public void setImageExtent(IExtent imageExtent) {
        wrappedObject.setImageExtent(imageExtent);
        if (imageExtent.equals(this.imageExtent) == false) {
            this.imageExtent = imageExtent;
            UpdateMosaicExtent extentUpdate = RemoteGraphicsEventFactory
                    .createEvent(UpdateMosaicExtent.class, this);
            extentUpdate.setExtent(imageExtent.clone());
            dispatch(extentUpdate);
        }
    }

}
