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
package com.raytheon.uf.viz.collaboration.ui.rsc.rendering;

import java.awt.image.RenderedImage;
import java.util.ArrayList;
import java.util.List;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.ISingleColorImageExtension;
import com.raytheon.uf.viz.core.drawables.ext.ISingleColorImageExtension.ISingleColorImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.remote.graphics.events.colormap.ColorMapDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.CreateColormappedImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.UpdateColorMapParametersEvent;
import com.raytheon.uf.viz.remote.graphics.events.imagery.CreateIImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.imagery.CreateSingleColorImage;
import com.raytheon.uf.viz.remote.graphics.events.imagery.PaintImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.imagery.PaintImagesEvent;
import com.raytheon.uf.viz.remote.graphics.events.imagery.RenderedImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.imagery.UpdateImageDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.imagery.UpdateSingleColorImage;
import com.raytheon.uf.viz.remote.graphics.events.mesh.CreateMeshEvent;
import com.raytheon.uf.viz.remote.graphics.events.mesh.ReprojectMeshEvent;

/**
 * Handles render events for imagery/mesh objects
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

public class ImagingRenderingHandler extends CollaborationRenderingHandler {

    @Subscribe
    public void renderImages(PaintImagesEvent event) throws VizException {
        PaintProperties paintProps = getPaintProperties();
        IGraphicsTarget target = getTarget();
        PaintImageEvent[] events = event.getImageEvents();
        DrawableImage[] images = toDrawableImages(events, dataManager);
        if (images.length > 0) {
            PaintProperties imageProps = new PaintProperties(paintProps);
            imageProps.setAlpha(event.getAlpha());
            target.drawRasters(imageProps, images);
        }
    }

    /**
     * Converts PaintImageEvent[] into DrawableImage[] by looking up image and
     * mesh objects from dataManager
     * 
     * @param events
     * @param dataManager
     * @return
     */
    public static DrawableImage[] toDrawableImages(PaintImageEvent[] events,
            CollaborationRenderingDataManager dataManager) {
        List<DrawableImage> images = new ArrayList<DrawableImage>(events.length);
        for (PaintImageEvent pie : events) {
            IImage image = dataManager.getRenderableObject(pie.getObjectId(),
                    IImage.class);
            if (image != null) {
                PixelCoverage coverage = new PixelCoverage(pie.getUl(),
                        pie.getUr(), pie.getLr(), pie.getLl());
                IMesh mesh = dataManager.getRenderableObject(pie.getMeshId(),
                        IMesh.class);
                if (mesh != null) {
                    coverage.setMesh(mesh);
                }
                images.add(new DrawableImage(image, coverage));
            } else {
                // TODO: Log?
            }
        }
        return images.toArray(new DrawableImage[images.size()]);
    }

    @Subscribe
    public void updateImageData(UpdateImageDataEvent event) {
        IImage image = dataManager.getRenderableObject(event.getObjectId(),
                IImage.class);
        if (image != null) {
            image.setBrightness(event.getBrightness());
            image.setContrast(event.getContrast());
            image.setInterpolated(event.isInterpolated());
        }
    }

    @Subscribe
    public void disposeImage(IImage image) {
        image.dispose();
    }

    // ================== RenderedImage events ==================

    public class RenderedImageDataCallback implements IRenderedImageCallback {
        private RenderedImage image;

        @Override
        public RenderedImage getImage() throws VizException {
            RenderedImage rval = image;
            if (image != null) {
                image = null;
            }
            return rval;
        }

        public void setData(RenderedImage image) {
            this.image = image;
        }
    }

    @Subscribe
    public void createImage(CreateIImageEvent event) {
        IGraphicsTarget target = getTarget();
        RenderedImageDataCallback callback = new RenderedImageDataCallback();
        IImage image = target.initializeRaster(callback);
        dataManager.putRenderableObject(event.getObjectId(), new Object[] {
                image, callback });
    }

    @Subscribe
    public void disposeCallback(RenderedImageDataCallback callback) {
        callback.setData(null);
    }

    @Subscribe
    public void handleRenderedImage(RenderedImageEvent event) {
        RenderedImageDataCallback callback = dataManager.getRenderableObject(
                event.getObjectId(), RenderedImageDataCallback.class);
        if (callback != null) {
            callback.setData(event.getRenderedImage());
        }
    }

    // ================== IColormappedImage events ==================

    /**
     * TODO: Manage ColorMaps better! (sharing and updating)
     * 
     * Creates an IColormappedImage object from the event
     * 
     * @param event
     * @throws VizException
     */
    @Subscribe
    public void createColormappedImage(CreateColormappedImageEvent event)
            throws VizException {
        IGraphicsTarget target = getTarget();
        int imageId = event.getObjectId();
        IColorMapDataRetrievalCallback callback = new ColorMapDataCallback();
        UpdateColorMapParametersEvent cmapEvent = event.getColorMapParameters();
        ColorMapParameters params = null;
        if (cmapEvent != null) {
            params = cmapEvent.asColorMapParameters();
        }
        IColormappedImage image = target.getExtension(
                IColormappedImageExtension.class).initializeRaster(callback,
                params);
        dataManager.putRenderableObject(imageId,
                new Object[] { image, callback });
    }

    @Subscribe
    public void dataArrived(ColorMapDataEvent event) {
        ColorMapDataCallback callback = dataManager.getRenderableObject(
                event.getObjectId(), ColorMapDataCallback.class);
        if (callback != null) {
            callback.setData(event.getColorMapData());
        }
    }

    @Subscribe
    public void disposeColorMapCallback(ColorMapDataCallback callback) {
        callback.setData(null);
    }

    // TODO: Put utility classes in same package

    public class ColorMapDataCallback implements IColorMapDataRetrievalCallback {
        private ColorMapData data;

        @Override
        public ColorMapData getColorMapData() throws VizException {
            ColorMapData rval = data;
            if (data != null) {
                data = null;
            }
            return rval;
        }

        public void setData(ColorMapData data) {
            this.data = data;
        }
    }

    // ================== IMesh events ==================

    @Subscribe
    public void createMesh(CreateMeshEvent event) throws VizException {
        IGraphicsTarget target = getTarget();
        // TODO: Should we cache or should we expect data provider or even
        // internal to the target?
        int meshId = event.getObjectId();
        IMesh mesh = target.getExtension(IMapMeshExtension.class)
                .constructMesh(event.getImageGeometry(),
                        event.getTargetGeometry());
        dataManager.putRenderableObject(meshId, mesh);
    }

    @Subscribe
    public void reprojectMesh(ReprojectMeshEvent event) throws VizException {
        IMesh mesh = dataManager.getRenderableObject(event.getObjectId(),
                IMesh.class);
        if (mesh != null) {
            mesh.reproject(event.getTargetGeometry());
        }
    }

    @Subscribe
    public void disposeMesh(IMesh mesh) {
        mesh.dispose();
    }

    // ================== ISingleColorImage events ==================

    @Subscribe
    public void createSingleColorImage(CreateSingleColorImage event)
            throws VizException {
        IGraphicsTarget target = getTarget();
        RenderedImageDataCallback callback = new RenderedImageDataCallback();
        int imageId = event.getObjectId();
        ISingleColorImage image = target.getExtension(
                ISingleColorImageExtension.class).constructImage(callback,
                event.getColor());
        dataManager.putRenderableObject(imageId,
                new Object[] { image, callback });
    }

    @Subscribe
    public void updateSingleColorImage(UpdateSingleColorImage event) {
        ISingleColorImage image = dataManager.getRenderableObject(
                event.getObjectId(), ISingleColorImage.class);
        if (image != null) {
            image.setColor(event.getColor());
        }
    }
}
