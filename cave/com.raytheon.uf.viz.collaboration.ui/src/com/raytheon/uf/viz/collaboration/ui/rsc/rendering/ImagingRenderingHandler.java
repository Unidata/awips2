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
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.DrawableColorMap;
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
import com.raytheon.uf.viz.remote.graphics.events.colormap.CreateColorMapEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.CreateColormappedImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.DrawColorRampEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.UpdateColorMapEvent;
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

    private Object colorMapLock = new Object();

    @Subscribe
    public void renderImages(PaintImagesEvent event) {
        PaintProperties paintProps = getPaintProperties();
        IGraphicsTarget target = getGraphicsTarget();
        PaintImageEvent[] events = event.getImageEvents();
        DrawableImage[] images = toDrawableImages(events, dataManager);
        if (images.length > 0) {
            PaintProperties imageProps = new PaintProperties(paintProps);
            imageProps.setAlpha(event.getAlpha());
            try {
                target.drawRasters(imageProps, images);
            } catch (VizException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
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
                int meshId = pie.getMeshId();
                if (meshId > -1) {
                    IMesh mesh = dataManager.getRenderableObject(
                            pie.getMeshId(), IMesh.class);
                    if (mesh != null) {
                        coverage.setMesh(mesh);
                    }
                }
                images.add(new DrawableImage(image, coverage));
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
        IGraphicsTarget target = getGraphicsTarget();
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
    public void createColormappedImage(CreateColormappedImageEvent event) {
        IGraphicsTarget target = getGraphicsTarget();
        int imageId = event.getObjectId();
        IColorMapDataRetrievalCallback callback = new ColorMapDataCallback();
        UpdateColorMapParametersEvent cmapParamEvent = event
                .getColorMapParameters();
        ColorMapParameters params = null;
        if (cmapParamEvent != null) {
            params = cmapParamEvent.getColorMapParameters();
            if (event.getColorMap() != null && params != null) {
                params.setColorMap(event.getColorMap().getColorMap());
            }
        }
        try {
            IColormappedImage image = target.getExtension(
                    IColormappedImageExtension.class).initializeRaster(
                    callback, params);
            dataManager.putRenderableObject(imageId, new Object[] { image,
                    callback });
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    @Subscribe
    public void updateColorMapParameters(UpdateColorMapParametersEvent event) {
        IColormappedImage image = dataManager.getRenderableObject(
                event.getObjectId(), IColormappedImage.class);
        if (image != null) {
            ColorMapParameters newParams = event.getColorMapParameters();
            synchronized (colorMapLock) {
                ColorMapParameters params = image.getColorMapParameters();
                if (params != null && newParams != null) {
                    newParams.setColorMap(params.getColorMap());
                }
                image.setColorMapParameters(newParams);
            }
        }
    }

    @Subscribe
    public void updateColorMap(UpdateColorMapEvent event) {
        IColormappedImage image = dataManager.getRenderableObject(
                event.getObjectId(), IColormappedImage.class);
        if (image != null) {
            IColorMap colorMap = event.getColorMap();
            synchronized (colorMapLock) {
                ColorMapParameters params = image.getColorMapParameters();
                if (params == null && colorMap != null) {
                    params = new ColorMapParameters();
                    params.setColorMap(colorMap);
                    image.setColorMapParameters(params);
                } else if (params != null) {
                    params.setColorMap(colorMap);
                }
            }
        }
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
    public void createMesh(CreateMeshEvent event) {
        IGraphicsTarget target = getGraphicsTarget();
        int meshId = event.getObjectId();
        try {
            IMesh mesh = target.getExtension(IMapMeshExtension.class)
                    .constructMesh(event.getImageGeometry(),
                            event.getTargetGeometry());
            dataManager.putRenderableObject(meshId, mesh);
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    @Subscribe
    public void reprojectMesh(ReprojectMeshEvent event) {
        IMesh mesh = dataManager.getRenderableObject(event.getObjectId(),
                IMesh.class);
        if (mesh != null) {
            try {
                mesh.reproject(event.getTargetGeometry());
            } catch (VizException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
    }

    @Subscribe
    public void disposeMesh(IMesh mesh) {
        mesh.dispose();
    }

    // ================== ISingleColorImage events ==================

    @Subscribe
    public void createSingleColorImage(CreateSingleColorImage event) {
        IGraphicsTarget target = getGraphicsTarget();
        RenderedImageDataCallback callback = new RenderedImageDataCallback();
        int imageId = event.getObjectId();
        try {
            ISingleColorImage image = target.getExtension(
                    ISingleColorImageExtension.class).constructImage(callback,
                    event.getColor());
            dataManager.putRenderableObject(imageId, new Object[] { image,
                    callback });
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    @Subscribe
    public void updateSingleColorImage(UpdateSingleColorImage event) {
        ISingleColorImage image = dataManager.getRenderableObject(
                event.getObjectId(), ISingleColorImage.class);
        if (image != null) {
            image.setColor(event.getColor());
        }
    }

    // ================== IColorMap events ==================

    @Subscribe
    public void handleCreateColorMap(CreateColorMapEvent event) {
        int objectId = event.getObjectId();
        dataManager.putRenderableObject(objectId, event.getColorMap());
    }

    @Subscribe
    public void drawColorRamp(DrawColorRampEvent event) {
        IGraphicsTarget target = getGraphicsTarget();
        IColorMap colorMap = dataManager.getRenderableObject(
                event.getColorMapId(), IColorMap.class);
        if (colorMap != null) {
            DrawableColorMap cmap = new DrawableColorMap(colorMap);
            cmap.alpha = event.getAlpha();
            cmap.brightness = event.getBrightness();
            cmap.contrast = event.getContrast();
            cmap.interpolate = event.isInterpolate();
            cmap.extent = event.getIExtent();
            try {
                target.drawColorRamp(cmap);
            } catch (VizException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
    }
}
