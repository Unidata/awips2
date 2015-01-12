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
package com.raytheon.uf.viz.collaboration.display.rsc.rendering;

import java.awt.image.RenderedImage;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.display.Activator;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicImageExtension;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicImageExtension.IMosaicImage;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicMaxValImageExtension;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicOrderedImageExtension;
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
import com.raytheon.uf.viz.remote.graphics.events.mesh.CloneMeshEvent;
import com.raytheon.uf.viz.remote.graphics.events.mesh.CreateMeshEvent;
import com.raytheon.uf.viz.remote.graphics.events.mosaic.CreateMosaicImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.mosaic.UpdateImagesToMosaic;
import com.raytheon.uf.viz.remote.graphics.events.mosaic.UpdateMosaicExtent;
import com.raytheon.uf.viz.remote.graphics.extensions.DispatchingMosaicMaxValExtension;
import com.raytheon.uf.viz.remote.graphics.extensions.DispatchingMosaicOrderedExtension;

/**
 * Handles render events for imagery/mesh objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 16, 2012           mschenke     Initial creation
 * Feb 21, 2014  2817     bsteffen     Fix mesh cloning.
 * Mar 06, 2014  2826     njensen      Added mosaicMemberMap and relevant safety
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ImagingRenderingHandler extends CollaborationRenderingHandler {

    private Object colorMapLock = new Object();

    /**
     * Map contains the images that combine to make a mosaic. This is
     * specifically tracked because there's a small window of opportunity where
     * a member image is disposed but the mosaic is told to paint and is not yet
     * aware of that member's disposal.
     */
    private Multimap<IMosaicImage, DrawableImage> mosaicMemberMap = ArrayListMultimap
            .create();

    @Subscribe
    public void renderImages(PaintImagesEvent event) {
        PaintProperties paintProps = getPaintProperties();
        IGraphicsTarget target = getGraphicsTarget();
        PaintImageEvent[] events = event.getObjects();
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
        synchronized (mosaicMemberMap) {
            if (image instanceof IMosaicImage) {
                // guava will kindly remove the key from the map when there are
                // no members left
                mosaicMemberMap.removeAll(image);
            } else {
                for (IMosaicImage mosaic : mosaicMemberMap.keySet()) {
                    boolean didRemove = false;
                    Collection<DrawableImage> collect = mosaicMemberMap
                            .get(mosaic);
                    Iterator<DrawableImage> itr = collect.iterator();
                    while (itr.hasNext()) {
                        DrawableImage itrImage = itr.next();
                        if (image.equals(itrImage.getImage())) {
                            itr.remove();
                            didRemove = true;
                        }
                    }
                    if (didRemove) {
                        mosaic.setImagesToMosaic(collect
                                .toArray(new DrawableImage[0]));
                    }
                }
            }
        }
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
    public void cloneMesh(CloneMeshEvent event) {
        IMesh mesh = dataManager.getRenderableObject(event.getSourceObjectId(),
                IMesh.class);
        if (mesh != null) {
            try {
                mesh = mesh.clone(event.getTargetGeometry());
                dataManager.putRenderableObject(event.getObjectId(), mesh);
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
            try {
                target.drawColorRamp(event.getDrawableColorMap(colorMap));
            } catch (VizException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
    }

    // ================== IMosaicImage events ==================

    @Subscribe
    public void createMosaicImage(CreateMosaicImageEvent event) {
        int imageId = event.getObjectId();
        IGraphicsTarget target = getGraphicsTarget();
        IExtent imageExtent = null;
        ColorMapParameters parameters = null;
        if (event.getExtent() != null) {
            imageExtent = event.getExtent().getExtent();
        }
        if (event.getColorMapParameters() != null) {
            parameters = event.getColorMapParameters().getColorMapParameters();
            if (event.getColorMap() != null && parameters != null) {
                parameters.setColorMap(event.getColorMap().getColorMap());
            }
        }
        Class<? extends IMosaicImageExtension> extensionClass = IMosaicImageExtension.class;
        if (DispatchingMosaicMaxValExtension.MOSAIC_TYPE.equals(event
                .getMosaicType())) {
            extensionClass = IMosaicMaxValImageExtension.class;
        } else if (DispatchingMosaicOrderedExtension.MOSAIC_TYPE.equals(event
                .getMosaicType())) {
            extensionClass = IMosaicOrderedImageExtension.class;
        }
        try {
            dataManager.putRenderableObject(
                    imageId,
                    target.getExtension(extensionClass).initializeRaster(
                            event.getBounds(), imageExtent, parameters));
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    @Subscribe
    public void updateImagesToMosaic(UpdateImagesToMosaic event) {
        IMosaicImage image = dataManager.getRenderableObject(
                event.getObjectId(), IMosaicImage.class);
        if (image != null) {
            DrawableImage[] drawables = ImagingRenderingHandler
                    .toDrawableImages(event.getImagesToMosaic(), dataManager);
            image.setImagesToMosaic(drawables);

            synchronized (mosaicMemberMap) {
                mosaicMemberMap.replaceValues(image, Arrays.asList(drawables));
            }
        }
    }

    @Subscribe
    public void updateMosaicImageExtent(UpdateMosaicExtent event) {
        IMosaicImage image = dataManager.getRenderableObject(
                event.getObjectId(), IMosaicImage.class);
        if (image != null) {
            image.setImageExtent(event.getExtent());
        }
    }

}
