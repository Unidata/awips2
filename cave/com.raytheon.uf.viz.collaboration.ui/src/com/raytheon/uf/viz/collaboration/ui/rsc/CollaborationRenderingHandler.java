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
package com.raytheon.uf.viz.collaboration.ui.rsc;

import java.io.File;
import java.io.IOException;
import java.nio.Buffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.remote.graphics.events.BeginFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.ColorMapDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.CreateColormappedImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.UpdateColorMapParametersEvent;
import com.raytheon.uf.viz.remote.graphics.events.fonts.CreateFontEvent;
import com.raytheon.uf.viz.remote.graphics.events.fonts.UpdateFontDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.imagery.PaintImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.imagery.PaintImagesEvent;
import com.raytheon.uf.viz.remote.graphics.events.imagery.UpdateImageDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.mesh.CreateMeshEvent;
import com.raytheon.uf.viz.remote.graphics.events.mesh.ReprojectMeshEvent;
import com.raytheon.uf.viz.remote.graphics.events.offscreen.CreateOffscreenImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.offscreen.RenderOffscreenEvent;
import com.raytheon.uf.viz.remote.graphics.events.offscreen.RenderOnscreenEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.AllocatePointsEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.CreateWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.RenderWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.SimpleWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.UpdateWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.WireframeShapeData;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.WireframeShapeData.Label;

/**
 * Class that handles rendering events for collaboration resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 9, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationRenderingHandler {

    private Map<Integer, Object[]> renderableObjectMap = new HashMap<Integer, Object[]>();

    private IGraphicsTarget target;

    private PaintProperties paintProps;

    private EventBus disposerRouter;

    public CollaborationRenderingHandler() {
        this.disposerRouter = new EventBus();
        this.disposerRouter.register(new CollaborationDisposingHandler());
    }

    /**
     * @param target
     * @param paintProps
     */
    public void beginRender(IGraphicsTarget target, PaintProperties paintProps) {
        this.target = target;
        this.paintProps = paintProps;
    }

    /**
     * Put a renderable object in the object map. If an object already exists
     * for that id, it will be sent to the disposer
     * 
     * @param objectId
     * @param obj
     */
    private void putRenderableObject(int objectId, Object obj) {
        if (obj != null) {
            Object[] objects = null;
            if (obj.getClass().isArray()) {
                objects = (Object[]) obj;
            } else {
                objects = new Object[] { obj };
            }
            Object[] oldValue = renderableObjectMap.put(objectId, objects);
            if (oldValue != null) {
                dispose(oldValue);
            }
        }
    }

    /**
     * Get a renderable object out of the object map as the objectType, if no
     * object exists or the object is not of type objectType, null is returned.
     * If the Object in the map is an Object[], the first object in the array
     * that is of type objectType will be returned
     * 
     * @param <T>
     * @param objectId
     * @param objectType
     * @return
     */
    private <T> T getRenderableObject(int objectId, Class<T> objectType) {
        T obj = null;
        Object[] toCheck = renderableObjectMap.get(objectId);
        if (toCheck != null) {
            for (Object check : toCheck) {
                if (objectType.isInstance(check)) {
                    obj = objectType.cast(check);
                    break;
                }
            }
        }
        return obj;
    }

    /**
     * Dispose all renderable object data
     */
    public void dispose() {
        for (Object[] obj : renderableObjectMap.values()) {
            dispose(obj);
        }
        renderableObjectMap.clear();
    }

    /**
     * Disposes a single renderable object by sending it to the disposer
     * EventBus
     * 
     * @param obj
     */
    private void dispose(Object[] objects) {
        for (Object toDispose : objects) {
            disposerRouter.post(toDispose);
        }
    }

    /**
     * General dispose of a renderable object event
     * 
     * @param event
     */
    @Subscribe
    public void disposeRenderable(DisposeObjectEvent event) {
        Object[] toDispose = renderableObjectMap.remove(event);
        if (toDispose != null) {
            dispose(toDispose);
        }
    }

    /**
     * Begin frame event, modifies the target extent
     * 
     * @param event
     */
    @Subscribe
    public void handleBeginFrame(BeginFrameEvent event) {
        double[] center = event.getExtentCenter();
        IExtent copy = paintProps.getView().getExtent().clone();
        if (center != null) {
            double[] currCenter = copy.getCenter();
            copy.shift(center[0] - currCenter[0], center[1] - currCenter[1]);
            copy.scaleAndBias(event.getExtentFactor() / copy.getScale(),
                    center[0], center[1]);
            target.updateExtent(copy);
            event.setExtentCenter(null);
            target.setNeedsRefresh(true);
        }
    }

    // ================== Common IImage events ==================

    @Subscribe
    public void renderImages(PaintImagesEvent event) throws VizException {
        PaintImageEvent[] events = event.getImageEvents();
        List<DrawableImage> images = new ArrayList<DrawableImage>(events.length);
        for (PaintImageEvent pie : events) {
            IImage image = getRenderableObject(pie.getObjectId(), IImage.class);
            if (image != null) {
                PixelCoverage coverage = new PixelCoverage(pie.getUl(),
                        pie.getUr(), pie.getLr(), pie.getLl());
                IMesh mesh = getRenderableObject(pie.getMeshId(), IMesh.class);
                if (mesh != null) {
                    coverage.setMesh(mesh);
                }
                images.add(new DrawableImage(image, coverage));
            } else {
                // TODO: Log?
            }
        }
        if (images.size() > 0) {
            PaintProperties imageProps = new PaintProperties(paintProps);
            imageProps.setAlpha(event.getAlpha());
            target.drawRasters(imageProps,
                    images.toArray(new DrawableImage[images.size()]));
        }
    }

    @Subscribe
    public void updateImageData(UpdateImageDataEvent event) {
        IImage image = getRenderableObject(event.getObjectId(), IImage.class);
        if (image != null) {
            image.setBrightness(event.getBrightness());
            image.setContrast(event.getContrast());
            image.setInterpolated(event.isInterpolated());
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
        putRenderableObject(imageId, new Object[] { image, callback });
    }

    @Subscribe
    public void dataArrived(ColorMapDataEvent event) {
        ColorMapDataCallback callback = getRenderableObject(
                event.getObjectId(), ColorMapDataCallback.class);
        if (callback != null) {
            callback.setData(event.getColorMapData());
        }
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
        // TODO: Should we cache or should we expect data provider or even
        // internal to the target?
        int meshId = event.getObjectId();
        IMesh mesh = target.getExtension(IMapMeshExtension.class)
                .constructMesh(event.getImageGeometry(),
                        event.getTargetGeometry());
        putRenderableObject(meshId, mesh);
    }

    @Subscribe
    public void reprojectMesh(ReprojectMeshEvent event) throws VizException {
        IMesh mesh = getRenderableObject(event.getObjectId(), IMesh.class);
        if (mesh != null) {
            mesh.reproject(event.getTargetGeometry());
        }
    }

    // ================== Offscreen image events ==================

    @Subscribe
    public void createOffscreenImage(CreateOffscreenImageEvent event)
            throws VizException {
        try {
            IImage offscreenImage = null;
            IOffscreenRenderingExtension ext = target
                    .getExtension(IOffscreenRenderingExtension.class);
            int[] dims = event.getDimensions();
            if (event.getBufferType() != null) {
                Class<? extends Buffer> bufferType = Class.forName(
                        event.getBufferType()).asSubclass(Buffer.class);
                if (event.getColorMapParamters() != null) {
                    offscreenImage = ext.constructOffscreenImage(bufferType,
                            dims, event.getColorMapParamters()
                                    .asColorMapParameters());
                } else {
                    offscreenImage = ext.constructOffscreenImage(bufferType,
                            dims);
                }
            } else {
                offscreenImage = ext.constructOffscreenImage(dims);
            }
            if (offscreenImage != null) {
                putRenderableObject(event.getObjectId(), offscreenImage);
            }
        } catch (ClassNotFoundException e) {
            throw new VizException("Could not find class for buffer type: "
                    + event.getBufferType(), e);
        }
    }

    @Subscribe
    public void renderOffscreen(RenderOffscreenEvent event) throws VizException {
        IImage offscreenImage = getRenderableObject(event.getObjectId(),
                IImage.class);
        if (offscreenImage != null) {
            if (event.getExtent() != null) {
                target.getExtension(IOffscreenRenderingExtension.class)
                        .renderOffscreen(offscreenImage, event.getIExtent());
            } else {
                target.getExtension(IOffscreenRenderingExtension.class)
                        .renderOffscreen(offscreenImage);
            }
        }
    }

    @Subscribe
    public void renderOnscreen(RenderOnscreenEvent event) throws VizException {
        target.getExtension(IOffscreenRenderingExtension.class)
                .renderOnscreen();
    }

    // ================== Wireframe shape events ==================

    @Subscribe
    public void createWireframeShape(CreateWireframeShapeEvent event) {
        int shapeId = event.getObjectId();
        IWireframeShape shape = null;
        if (event.getSimplificationLevel() != null) {
            if (event.isSpatialChopFlag() != null) {
                shape = target.createWireframeShape(event.isMutable(),
                        event.getGridGeometry(),
                        event.getSimplificationLevel(),
                        event.isSpatialChopFlag(), event.getIExtent());
            } else {
                shape = target
                        .createWireframeShape(event.isMutable(),
                                event.getGridGeometry(),
                                event.getSimplificationLevel());
            }
        } else {
            shape = target.createWireframeShape(event.isMutable(),
                    event.getGridGeometry());
        }
        putRenderableObject(shapeId, shape);
    }

    @Subscribe
    public void allocatePointsForShape(AllocatePointsEvent event) {
        IWireframeShape shape = getRenderableObject(event.getObjectId(),
                IWireframeShape.class);
        if (shape != null) {
            shape.allocate(event.getNumberOfPoints());
        }
    }

    @Subscribe
    public void updateWireframeShapeData(UpdateWireframeShapeEvent event) {
        IWireframeShape shape = getRenderableObject(event.getObjectId(),
                IWireframeShape.class);
        if (shape != null) {
            WireframeShapeData data = event.getWireframeData();
            for (Label label : data.getLabels()) {
                shape.addLabel(label.getText(), label.getPoint());
            }
            for (double[][] coords : data.getCoordinates()) {
                shape.addLineSegment(coords);
            }
        }
    }

    @Subscribe
    public void handleSimpleWireframeShapeEvent(SimpleWireframeShapeEvent event) {
        IWireframeShape shape = getRenderableObject(event.getObjectId(),
                IWireframeShape.class);
        if (shape != null) {
            switch (event.getAction()) {
            case CLEAR_LABELS:
                shape.clearLabels();
                break;
            case COMPILE:
                shape.compile();
                break;
            case RESET:
                shape.reset();
                break;
            }
        }
    }

    @Subscribe
    public void renderWireframeShape(RenderWireframeShapeEvent event)
            throws VizException {
        IWireframeShape shape = getRenderableObject(event.getObjectId(),
                IWireframeShape.class);
        if (shape != null) {
            IFont font = null;
            if (event.getFontId() != null) {
                font = getRenderableObject(event.getFontId(), IFont.class);
            }
            if (event.getAlpha() == null) {
                target.drawWireframeShape(shape, event.getColor(),
                        event.getLineWidth(), event.getLineStyle(), font);
            } else {
                target.drawWireframeShape(shape, event.getColor(),
                        event.getLineWidth(), event.getLineStyle(), font,
                        event.getAlpha());
            }
        }
    }

    // ================== Font events ==================

    @Subscribe
    public void createFont(CreateFontEvent event) throws VizException {
        int fontId = event.getObjectId();
        IFont font = null;
        if (event.getFontData() != null) {
            try {
                File fontFile = File.createTempFile(event.getFontName(), null);
                FileUtil.bytes2File(event.getFontData(), fontFile);
                font = target.initializeFont(fontFile, event.getFontSize(),
                        event.getFontStyle());
            } catch (IOException e) {
                throw new VizException("Unable to write font file: "
                        + e.getLocalizedMessage());
            }
        } else {
            font = target.initializeFont(event.getFontName(),
                    event.getFontSize(), event.getFontStyle());
        }
        font.setMagnification(event.getMagnification());
        font.setSmoothing(event.isSmoothing());
        font.setScaleFont(event.isScaleFont());
        putRenderableObject(fontId, font);
    }

    @Subscribe
    public void updateFont(UpdateFontDataEvent event) {
        IFont font = getRenderableObject(event.getObjectId(), IFont.class);
        if (font != null) {
            if (event.getMagnification() != null) {
                if (event.getScaleFont() != null) {
                    font.setMagnification(event.getMagnification(),
                            event.getScaleFont());
                } else {
                    font.setMagnification(event.getMagnification());
                }
            } else {
                if (event.getScaleFont() != null) {
                    font.setScaleFont(event.getScaleFont());
                } else if (event.getSmoothing() != null) {
                    font.setSmoothing(event.getSmoothing());
                }
            }
            font.setMagnification(event.getMagnification());
            font.setSmoothing(event.getSmoothing());
            font.setScaleFont(event.getScaleFont());
        }
    }
}
