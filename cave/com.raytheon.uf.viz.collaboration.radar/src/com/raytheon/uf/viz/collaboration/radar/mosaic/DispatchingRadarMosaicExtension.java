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

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.colormap.UpdateColorMapEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.UpdateColorMapParametersEvent;
import com.raytheon.uf.viz.remote.graphics.extensions.AbstractDispatchingImageExtension;
import com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicImageExtension;

/**
 * Dispatching radar mosaic extension, creates dispatching IMosaicImage
 * implementation that forwards key image events
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingRadarMosaicExtension extends
        AbstractDispatchingImageExtension implements IRadarMosaicImageExtension {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicImageExtension#
     * initializeRaster(int[], com.raytheon.uf.viz.core.IExtent,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public IMosaicImage initializeRaster(int[] imageBounds,
            IExtent imageExtent, ColorMapParameters params) throws VizException {
        DispatchingMosaicImage image = new DispatchingMosaicImage(target
                .getWrappedObject()
                .getExtension(IRadarMosaicImageExtension.class)
                .initializeRaster(imageBounds, imageExtent, params),
                DispatchingRadarMosaicExtension.class, target.getDispatcher(),
                params, imageExtent);
        // Send creation event
        CreateMosaicImageEvent creation = RemoteGraphicsEventFactory
                .createEvent(CreateMosaicImageEvent.class, image);
        creation.setBounds(imageBounds);
        if (params != null) {
            creation.setColorMapParameters(UpdateColorMapParametersEvent
                    .createEvent(image, params));
            creation.setColorMap(UpdateColorMapEvent.createEvent(image,
                    params.getColorMap()));
        }
        if (imageExtent != null) {
            UpdateMosaicExtent extentUpdate = RemoteGraphicsEventFactory
                    .createEvent(UpdateMosaicExtent.class, image);
            extentUpdate.setExtent(imageExtent.clone());
            creation.setExtent(extentUpdate);
        }
        target.dispatch(creation);
        return image;
    }

}
