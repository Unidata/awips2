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
package com.raytheon.uf.viz.collaboration.pointdata.rendering;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.pointdata.image.DrawPointImagesEvent;
import com.raytheon.uf.viz.collaboration.pointdata.image.PointImageEvent;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.rsc.rendering.CollaborationRenderingHandler;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension.PointImage;

/**
 * Class for rending PointImage objects, handles DrawPointImagesEvent events
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 30, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PointImageRenderingHandler extends CollaborationRenderingHandler {

    @Subscribe
    public void drawPointImages(DrawPointImagesEvent event) {
        IGraphicsTarget target = getGraphicsTarget();
        PaintProperties newProps = new PaintProperties(getPaintProperties());
        newProps.setAlpha(event.getAlpha());
        Set<PointImageEvent> events = event.getImagesCopy();
        List<PointImage> images = new ArrayList<PointImage>(events.size());
        for (PointImageEvent pie : events) {
            IImage image = dataManager.getRenderableObject(pie.getImageId(),
                    IImage.class);
            if (image != null) {
                PointImage pi = new PointImage(image, pie.getX(), pie.getY());
                pi.setWidth(pie.getWidth());
                pi.setHeight(pie.getHeight());
                pi.setHorizontalAlignment(pie.getHorizontalAlignment());
                pi.setVerticalAlignment(pie.getVerticalAlignment());
                pi.setSiteId(pie.getSiteId());
                images.add(pi);
            }
        }

        try {
            target.getExtension(IPointImageExtension.class).drawPointImages(
                    newProps, images);
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

}
