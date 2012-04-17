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
package com.raytheon.uf.viz.collaboration.radar.rendering;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.radar.mesh.CreateRadarRadialMesh;
import com.raytheon.uf.viz.collaboration.radar.mosaic.CreateMosaicImageEvent;
import com.raytheon.uf.viz.collaboration.radar.mosaic.UpdateImagesToMosaic;
import com.raytheon.uf.viz.collaboration.radar.mosaic.UpdateMosaicExtent;
import com.raytheon.uf.viz.collaboration.ui.rsc.rendering.CollaborationRenderingHandler;
import com.raytheon.uf.viz.collaboration.ui.rsc.rendering.ImagingRenderingHandler;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.radar.rsc.image.IRadialMeshExtension;
import com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicImageExtension;
import com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicImageExtension.IMosaicImage;

/**
 * Rendering handler for radar graphics extension objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RadarGraphicsExtRenderingHandler extends
        CollaborationRenderingHandler {

    @Subscribe
    public void createRadarMesh(CreateRadarRadialMesh event)
            throws VizException {
        int meshId = event.getObjectId();
        IGraphicsTarget target = getTarget();
        dataManager.putRenderableObject(
                meshId,
                target.getExtension(IRadialMeshExtension.class).constructMesh(
                        event.getRadarRecord(), event.getTargetGeometry()));
    }

    @Subscribe
    public void createMosaicImage(CreateMosaicImageEvent event)
            throws VizException {
        int imageId = event.getObjectId();
        IGraphicsTarget target = getTarget();
        IExtent imageExtent = null;
        ColorMapParameters parameters = null;
        if (event.getExtent() != null) {
            imageExtent = event.getExtent().getIExtent();
        }
        if (event.getColorMapParameters() != null) {
            parameters = event.getColorMapParameters().asColorMapParameters();
        }
        dataManager.putRenderableObject(
                imageId,
                target.getExtension(IRadarMosaicImageExtension.class)
                        .initializeRaster(event.getBounds(), imageExtent,
                                parameters));
    }

    @Subscribe
    public void updateImagesToMosaic(UpdateImagesToMosaic event) {
        IMosaicImage image = dataManager.getRenderableObject(
                event.getObjectId(), IMosaicImage.class);
        if (image != null) {
            image.setImagesToMosaic(ImagingRenderingHandler.toDrawableImages(
                    event.getImagesToMosaic(), dataManager));
        }
    }

    @Subscribe
    public void updateMosaicImageExtent(UpdateMosaicExtent event) {
        IMosaicImage image = dataManager.getRenderableObject(
                event.getObjectId(), IMosaicImage.class);
        if (image != null) {
            image.setImageExtent(event.getIExtent());
        }
    }
}
