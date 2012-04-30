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
package com.raytheon.uf.viz.collaboration.pointdata.image;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.DispatchGraphicsTarget;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingImage;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension;

/**
 * Dispatching extension for point images
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 27, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingPointImageExtension extends
        GraphicsExtension<DispatchGraphicsTarget> implements
        IPointImageExtension {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.pointdata.drawables.IPointImageExtension#drawPointImages
     * (com.raytheon.uf.viz.core.drawables.PaintProperties,
     * com.raytheon.viz.pointdata.drawables.IPointImageExtension.PointImage[])
     */
    @Override
    public void drawPointImages(PaintProperties paintProps,
            PointImage... images) throws VizException {
        drawPointImages(paintProps, Arrays.asList(images));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.pointdata.drawables.IPointImageExtension#drawPointImages
     * (com.raytheon.uf.viz.core.drawables.PaintProperties,
     * java.util.Collection)
     */
    @Override
    public void drawPointImages(PaintProperties paintProps,
            Collection<PointImage> images) throws VizException {
        // Dispatch event
        DrawPointImagesEvent event = RemoteGraphicsEventFactory.createEvent(
                DrawPointImagesEvent.class, target);
        event.setAlpha(paintProps.getAlpha());
        event.setPointImages(images);
        target.dispatch(event);

        // Extract a non dispatching point image and draw to wrapped target
        List<PointImage> extracted = new ArrayList<PointImage>(images.size());
        for (PointImage image : images) {
            PointImage pi = new PointImage(
                    ((DispatchingImage) image.getImage()).getWrappedObject(),
                    image.getX(), image.getY());
            pi.setWidth(image.getWidth());
            pi.setHeight(image.getHeight());
            pi.setHorizontalAlignment(image.getHorizontalAlignment());
            pi.setVerticalAlignment(image.getVerticalAlignment());
            pi.setSiteId(image.getSiteId());
            extracted.add(pi);
        }

        target.getWrappedObject().getExtension(IPointImageExtension.class)
                .drawPointImages(paintProps, extracted);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension#
     * getCompatibilityValue(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public int getCompatibilityValue(DispatchGraphicsTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

}
