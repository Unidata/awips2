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
package com.raytheon.uf.viz.remote.graphics;

import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.IColormappedDataPreparer;
import com.raytheon.uf.viz.core.data.IDataPreparer;
import com.raytheon.uf.viz.core.data.IImageDataPreparer;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.data.resp.NumericImageData;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.ImagingSupport;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtensionManager;
import com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.geom.PixelCoordinate;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.imagery.CreateIImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.BeginFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.EndFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.CreateWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.RenderWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.extensions.DispatchingImagingExtension;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingFont;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingImage;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingImage.DispatchingRenderedImageCallback;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingWireframeShape;
import com.vividsolutions.jts.geom.LinearRing;

/**
 * Graphics target that uses a dispatcher for dispatching graphics events. These
 * events involve rendering and creating render objects
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

public class DispatchGraphicsTarget extends DispatchingObject<IGraphicsTarget>
        implements IGraphicsTarget {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DispatchGraphicsTarget.class);

    private IFont defaultFont;

    private GraphicsExtensionManager extensionManager;

    public DispatchGraphicsTarget(IGraphicsTarget target, Dispatcher dispatcher) {
        super(target, dispatcher);
        extensionManager = new GraphicsExtensionManager(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#initializeRaster(com.raytheon
     * .uf.viz.core.data.IDataPreparer,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public IImage initializeRaster(IDataPreparer preparer,
            ColorMapParameters optionalParams) {
        IImage rval = null;
        if (optionalParams == null) {
            // Assume IImageDataPreparer
            final IImageDataPreparer imagePreparer = (IImageDataPreparer) preparer;
            rval = initializeRaster(new IRenderedImageCallback() {
                @Override
                public RenderedImage getImage() throws VizException {
                    return imagePreparer.prepareData().getImage();
                }
            });
        } else if (preparer instanceof IColormappedDataPreparer) {
            try {
                IColormappedImageExtension cmapExt = getExtension(IColormappedImageExtension.class);
                final IColormappedDataPreparer cmapPreparer = (IColormappedDataPreparer) preparer;
                rval = cmapExt.initializeRaster(
                        new IColorMapDataRetrievalCallback() {
                            @Override
                            public ColorMapData getColorMapData()
                                    throws VizException {
                                NumericImageData oldData = cmapPreparer
                                        .prepareData();
                                return new ColorMapData(
                                        oldData.getData(),
                                        new int[] {
                                                oldData.getDatasetBounds().width,
                                                oldData.getDatasetBounds().height });
                            }

                        }, optionalParams);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error constructing creating image", e);
            }
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#initializeRaster(com.raytheon
     * .uf.viz.core.data.IRenderedImageCallback)
     */
    @Override
    public IImage initializeRaster(IRenderedImageCallback imageCallback) {
        // Create callback wrapper
        DispatchingRenderedImageCallback wrappedCallback = new DispatchingRenderedImageCallback(
                imageCallback);
        // Create image from wrapped target and return DispatchingImage
        DispatchingImage image = new DispatchingImage(
                wrappedObject.initializeRaster(wrappedCallback),
                DispatchingImagingExtension.class, wrappedCallback,
                getDispatcher());
        // Send creation event
        dispatch(RemoteGraphicsEventFactory.createEvent(
                CreateIImageEvent.class, image));
        return image;
    }

    /**
     * @param font
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#initializeFont(java.lang.String)
     */
    public IFont initializeFont(String font) {
        return new DispatchingFont(wrappedObject.initializeFont(font),
                getDispatcher());
    }

    /**
     * @param fontName
     * @param size
     * @param styles
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#initializeFont(java.lang.String,
     *      float, com.raytheon.uf.viz.core.drawables.IFont.Style[])
     */
    public IFont initializeFont(String fontName, float size, Style[] styles) {
        return new DispatchingFont(wrappedObject.initializeFont(fontName, size,
                styles), getDispatcher());
    }

    /**
     * @param fontFile
     * @param size
     * @param styles
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#initializeFont(java.io.File,
     *      float, com.raytheon.uf.viz.core.drawables.IFont.Style[])
     */
    public IFont initializeFont(File fontFile, float size, Style[] styles) {
        return new DispatchingFont(wrappedObject.initializeFont(fontFile, size,
                styles), getDispatcher(), fontFile);
    }

    /**
     * @param image
     * @param extent
     * @param paintProps
     * @return
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawRaster(com.raytheon.uf.viz.core.drawables.IImage,
     *      com.raytheon.uf.viz.core.PixelCoverage,
     *      com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    public boolean drawRaster(IImage image, PixelCoverage extent,
            PaintProperties paintProps) throws VizException {
        DrawableImage di = new DrawableImage(image, extent);
        return drawRasters(paintProps, di);
    }

    /**
     * @param paintProps
     * @param images
     * @return
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawRasters(com.raytheon.uf.viz.core.drawables.PaintProperties,
     *      com.raytheon.uf.viz.core.DrawableImage[])
     */
    public boolean drawRasters(PaintProperties paintProps,
            DrawableImage... images) throws VizException {
        return ImagingSupport.drawRasters(this, paintProps, images);
    }

    /**
     * @param image
     * @param extent
     * @param paintProps
     * @param mode
     * @return
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawRaster(com.raytheon.uf.viz.core.drawables.IImage,
     *      com.raytheon.uf.viz.core.PixelCoverage,
     *      com.raytheon.uf.viz.core.drawables.PaintProperties,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode)
     */
    public boolean drawRaster(IImage image, PixelCoverage extent,
            PaintProperties paintProps, RasterMode mode) throws VizException {
        DrawableImage di = new DrawableImage(image, extent);
        di.setMode(mode);
        return drawRasters(paintProps, di);
    }

    /**
     * @param parameters
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawStrings(com.raytheon.uf.viz.core.DrawableString[])
     */
    public void drawStrings(DrawableString... parameters) throws VizException {
        IFont[] originalFonts = new IFont[parameters.length];
        for (int i = 0; i < parameters.length; ++i) {
            DrawableString string = parameters[i];
            originalFonts[i] = string.font;
            if (string.font instanceof DispatchingFont) {
                string.font = ((DispatchingFont) string.font)
                        .getWrappedObject();
            }
        }
        wrappedObject.drawStrings(parameters);
        for (int i = 0; i < parameters.length; ++i) {
            parameters[i].font = originalFonts[i];
        }
    }

    /**
     * @param parameters
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawStrings(java.util.Collection)
     */
    public void drawStrings(Collection<DrawableString> parameters)
            throws VizException {
        List<IFont> originalFonts = new ArrayList<IFont>(parameters.size());
        for (DrawableString string : parameters) {
            originalFonts.add(string.font);
            if (string.font instanceof DispatchingFont) {
                string.font = ((DispatchingFont) string.font)
                        .getWrappedObject();
            }
        }
        wrappedObject.drawStrings(parameters);
        int i = 0;
        for (DrawableString string : parameters) {
            string.font = originalFonts.get(i);
        }
    }

    /**
     * @param parameters
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#getStringsBounds(com.raytheon.uf.viz.core.DrawableString)
     */
    public Rectangle2D getStringsBounds(DrawableString parameters) {
        IFont font = parameters.font;
        if (font instanceof DispatchingFont) {
            parameters.font = ((DispatchingFont) font).getWrappedObject();
        }
        Rectangle2D rval = wrappedObject.getStringsBounds(parameters);
        parameters.font = font;
        return rval;
    }

    /**
     * @param parameters
     * @param string
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#getStringsBounds(com.raytheon.uf.viz.core.DrawableString,
     *      java.lang.String)
     */
    public Rectangle2D getStringsBounds(DrawableString parameters, String string) {
        IFont font = parameters.font;
        if (font instanceof DispatchingFont) {
            parameters.font = ((DispatchingFont) font).getWrappedObject();
        }
        Rectangle2D rval = wrappedObject.getStringsBounds(parameters, string);
        parameters.font = font;
        return rval;
    }

    /**
     * @param shape
     * @param alpha
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawShadedShape(com.raytheon.uf.viz.core.drawables.IShadedShape,
     *      float)
     */
    public void drawShadedShape(IShadedShape shape, float alpha)
            throws VizException {
        wrappedObject.drawShadedShape(shape, alpha);
    }

    /**
     * @param shape
     * @param alpha
     * @param brightness
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawShadedShape(com.raytheon.uf.viz.core.drawables.IShadedShape,
     *      float, float)
     */
    public void drawShadedShape(IShadedShape shape, float alpha,
            float brightness) throws VizException {
        wrappedObject.drawShadedShape(shape, alpha, brightness);
    }

    /**
     * @param alpha
     * @param brightness
     * @param shapes
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawShadedShapes(float,
     *      float, com.raytheon.uf.viz.core.drawables.IShadedShape[])
     */
    public void drawShadedShapes(float alpha, float brightness,
            IShadedShape... shapes) throws VizException {
        wrappedObject.drawShadedShapes(alpha, brightness, shapes);
    }

    /**
     * @param shape
     * @param color
     * @param lineWidth
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawWireframeShape(com.raytheon.uf.viz.core.drawables.IWireframeShape,
     *      org.eclipse.swt.graphics.RGB, float)
     */
    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth) throws VizException {
        DispatchingWireframeShape wrapper = (DispatchingWireframeShape) shape;
        shape = wrapper.getWrappedObject();
        wrappedObject.drawWireframeShape(shape, color, lineWidth);
        sendDrawWireframeShapeEvent(wrapper, color, lineWidth, null, null, null);
    }

    /**
     * @param shape
     * @param color
     * @param lineWidth
     * @param lineStyle
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawWireframeShape(com.raytheon.uf.viz.core.drawables.IWireframeShape,
     *      org.eclipse.swt.graphics.RGB, float,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle)
     */
    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle) throws VizException {
        DispatchingWireframeShape wrapper = (DispatchingWireframeShape) shape;
        shape = wrapper.getWrappedObject();
        wrappedObject.drawWireframeShape(shape, color, lineWidth, lineStyle);
        sendDrawWireframeShapeEvent(wrapper, color, lineWidth, lineStyle, null,
                null);
    }

    /**
     * @param shape
     * @param color
     * @param lineWidth
     * @param lineStyle
     * @param alpha
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawWireframeShape(com.raytheon.uf.viz.core.drawables.IWireframeShape,
     *      org.eclipse.swt.graphics.RGB, float,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle, float)
     */
    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, float alpha)
            throws VizException {
        DispatchingWireframeShape wrapper = (DispatchingWireframeShape) shape;
        shape = wrapper.getWrappedObject();
        wrappedObject.drawWireframeShape(shape, color, lineWidth, lineStyle,
                alpha);
        sendDrawWireframeShapeEvent(wrapper, color, lineWidth, lineStyle, null,
                alpha);
    }

    /**
     * @param shape
     * @param color
     * @param lineWidth
     * @param lineStyle
     * @param font
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawWireframeShape(com.raytheon.uf.viz.core.drawables.IWireframeShape,
     *      org.eclipse.swt.graphics.RGB, float,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle,
     *      com.raytheon.uf.viz.core.drawables.IFont)
     */
    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, IFont font)
            throws VizException {
        DispatchingWireframeShape wrapper = (DispatchingWireframeShape) shape;
        shape = wrapper.getWrappedObject();
        wrappedObject.drawWireframeShape(shape, color, lineWidth, lineStyle,
                ((DispatchingFont) font).getWrappedObject());
        sendDrawWireframeShapeEvent(wrapper, color, lineWidth, lineStyle, font,
                null);
    }

    /**
     * @param shape
     * @param color
     * @param lineWidth
     * @param lineStyle
     * @param font
     * @param alpha
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawWireframeShape(com.raytheon.uf.viz.core.drawables.IWireframeShape,
     *      org.eclipse.swt.graphics.RGB, float,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle,
     *      com.raytheon.uf.viz.core.drawables.IFont, float)
     */
    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, IFont font, float alpha)
            throws VizException {
        DispatchingWireframeShape wrapper = (DispatchingWireframeShape) shape;
        shape = wrapper.getWrappedObject();
        if (font instanceof DispatchingFont) {
            font = ((DispatchingFont) font).getWrappedObject();
        }
        wrappedObject.drawWireframeShape(shape, color, lineWidth, lineStyle,
                font, alpha);
        sendDrawWireframeShapeEvent(wrapper, color, lineWidth, lineStyle, font,
                alpha);
    }

    private void sendDrawWireframeShapeEvent(DispatchingWireframeShape shape,
            RGB color, float lineWidth, LineStyle lineStyle, IFont font,
            Float alpha) {
        shape.flushState();
        RenderWireframeShapeEvent event = RemoteGraphicsEventFactory
                .createEvent(RenderWireframeShapeEvent.class, shape);
        event.setColor(color);
        event.setLineWidth(lineWidth);
        event.setLineStyle(lineStyle);
        event.setAlpha(alpha);
        if (font instanceof DispatchingFont) {
            event.setFontId(((DispatchingFont) font).getObjectId());
        }
        dispatch(event);
    }

    /**
     * @param pe
     * @param color
     * @param lineWidth
     * @param alpha
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawRect(com.raytheon.uf.viz.core.IExtent,
     *      org.eclipse.swt.graphics.RGB, float, double)
     */
    public void drawRect(IExtent pe, RGB color, float lineWidth, double alpha)
            throws VizException {
        wrappedObject.drawRect(pe, color, lineWidth, alpha);
    }

    /**
     * @param pe
     * @param color
     * @param alpha
     * @param pattern
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawShadedRect(com.raytheon.uf.viz.core.IExtent,
     *      org.eclipse.swt.graphics.RGB, double, byte[])
     */
    public void drawShadedRect(IExtent pe, RGB color, double alpha,
            byte[] pattern) throws VizException {
        wrappedObject.drawShadedRect(pe, color, alpha, pattern);
    }

    /**
     * @param poly
     * @param color
     * @param alpha
     * @param pattern
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawShadedPolygon(com.vividsolutions.jts.geom.LinearRing,
     *      org.eclipse.swt.graphics.RGB, double, byte[])
     */
    public void drawShadedPolygon(LinearRing poly, RGB color, double alpha,
            byte[] pattern) throws VizException {
        wrappedObject.drawShadedPolygon(poly, color, alpha, pattern);
    }

    /**
     * @param circles
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawCircle(com.raytheon.uf.viz.core.DrawableCircle[])
     */
    public void drawCircle(DrawableCircle... circles) throws VizException {
        wrappedObject.drawCircle(circles);
    }

    /**
     * @param lines
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawLine(com.raytheon.uf.viz.core.DrawableLine[])
     */
    public void drawLine(DrawableLine... lines) throws VizException {
        wrappedObject.drawLine(lines);
    }

    /**
     * @param x1
     * @param y1
     * @param z1
     * @param radius
     * @param color
     * @param width
     * @param startAzimuth
     * @param arcWidth
     * @param lineStyle
     * @param includeSides
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawArc(double, double,
     *      double, double, org.eclipse.swt.graphics.RGB, float, int, int,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle, boolean)
     */
    public void drawArc(double x1, double y1, double z1, double radius,
            RGB color, float width, int startAzimuth, int arcWidth,
            LineStyle lineStyle, boolean includeSides) throws VizException {
        wrappedObject.drawArc(x1, y1, z1, radius, color, width, startAzimuth,
                arcWidth, lineStyle, includeSides);
    }

    /**
     * @param x1
     * @param y1
     * @param z1
     * @param radius
     * @param angle
     * @return
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#getPointOnCircle(double,
     *      double, double, double, double)
     */
    public double[] getPointOnCircle(double x1, double y1, double z1,
            double radius, double angle) throws VizException {
        return wrappedObject.getPointOnCircle(x1, y1, z1, radius, angle);
    }

    /**
     * @param mutable
     * @param descriptor
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#createWireframeShape(boolean,
     *      com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    public IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor) {
        // Create original
        IWireframeShape targetShape = wrappedObject.createWireframeShape(
                mutable, descriptor);
        // Create wrapped
        DispatchingWireframeShape dispatching = new DispatchingWireframeShape(
                targetShape, getDispatcher(), descriptor.getGridGeometry());
        // Dispatch creation event
        sendCreateWireframeShapeEvent(dispatching, mutable,
                descriptor.getGridGeometry(), null, null, null);
        return dispatching;
    }

    /**
     * @param mutable
     * @param descriptor
     * @param simplificationLevel
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#createWireframeShape(boolean,
     *      com.raytheon.uf.viz.core.drawables.IDescriptor, float)
     */
    public IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor, float simplificationLevel) {
        // Create original
        IWireframeShape targetShape = wrappedObject.createWireframeShape(
                mutable, descriptor, simplificationLevel);
        // Create wrapped
        DispatchingWireframeShape dispatching = new DispatchingWireframeShape(
                targetShape, getDispatcher(), descriptor.getGridGeometry());
        // Dispatch creation event
        sendCreateWireframeShapeEvent(dispatching, mutable,
                descriptor.getGridGeometry(), simplificationLevel, null, null);
        return dispatching;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#createWireframeShape(boolean,
     * org.geotools.coverage.grid.GeneralGridGeometry, float)
     */
    @Override
    public IWireframeShape createWireframeShape(boolean mutable,
            GeneralGridGeometry geom, float simplificationLevel) {
        // Create original
        IWireframeShape targetShape = wrappedObject.createWireframeShape(
                mutable, geom, simplificationLevel);
        // Create wrapped
        DispatchingWireframeShape dispatching = new DispatchingWireframeShape(
                targetShape, getDispatcher(), geom);
        // Dispatch creation event
        sendCreateWireframeShapeEvent(dispatching, mutable, geom,
                simplificationLevel, null, null);
        return dispatching;
    }

    /**
     * @param mutable
     * @param descriptor
     * @param simplificationLevel
     * @param spatialChopFlag
     * @param extent
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#createWireframeShape(boolean,
     *      com.raytheon.uf.viz.core.drawables.IDescriptor, float, boolean,
     *      com.raytheon.uf.viz.core.IExtent)
     */
    public IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor, float simplificationLevel,
            boolean spatialChopFlag, IExtent extent) {
        // Create original
        IWireframeShape targetShape = wrappedObject.createWireframeShape(
                mutable, descriptor, simplificationLevel, spatialChopFlag,
                extent);
        // Create wrapped
        DispatchingWireframeShape dispatching = new DispatchingWireframeShape(
                targetShape, getDispatcher(), descriptor.getGridGeometry());
        // Dispatch creation event
        sendCreateWireframeShapeEvent(dispatching, mutable,
                descriptor.getGridGeometry(), simplificationLevel,
                spatialChopFlag, extent);
        return dispatching;
    }

    /**
     * @param mutableFlag
     * @param geom
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#createWireframeShape(boolean,
     *      org.geotools.coverage.grid.GeneralGridGeometry)
     */
    public IWireframeShape createWireframeShape(boolean mutableFlag,
            GeneralGridGeometry geom) {
        // Create original
        IWireframeShape targetShape = wrappedObject.createWireframeShape(
                mutableFlag, geom);
        // Create wrapped
        DispatchingWireframeShape dispatching = new DispatchingWireframeShape(
                targetShape, getDispatcher(), geom);
        // Dispatch creation event
        sendCreateWireframeShapeEvent(dispatching, mutableFlag, geom, null,
                null, null);
        return dispatching;
    }

    /**
     * @param mutable
     * @param geom
     * @param simplificationLevel
     * @param spatialChopFlag
     * @param extent
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#createWireframeShape(boolean,
     *      org.geotools.coverage.grid.GeneralGridGeometry, float, boolean,
     *      com.raytheon.uf.viz.core.IExtent)
     */
    public IWireframeShape createWireframeShape(boolean mutable,
            GeneralGridGeometry geom, float simplificationLevel,
            boolean spatialChopFlag, IExtent extent) {
        // Create original
        IWireframeShape targetShape = wrappedObject.createWireframeShape(
                mutable, geom, simplificationLevel, spatialChopFlag, extent);
        // Create wrapped
        DispatchingWireframeShape dispatching = new DispatchingWireframeShape(
                targetShape, getDispatcher(), geom);
        // Dispatch creation event
        sendCreateWireframeShapeEvent(dispatching, mutable, geom,
                simplificationLevel, spatialChopFlag, extent);
        return dispatching;

    }

    private void sendCreateWireframeShapeEvent(DispatchingWireframeShape shape,
            boolean mutable, GeneralGridGeometry geom,
            Float simplificationLevel, Boolean spatialChopFlag, IExtent extent) {
        CreateWireframeShapeEvent event = RemoteGraphicsEventFactory
                .createEvent(CreateWireframeShapeEvent.class, shape);
        event.setGridGeometry(geom);
        event.setMutable(mutable);
        event.setIExtent(extent);
        event.setSimplificationLevel(simplificationLevel);
        event.setSpatialChopFlag(spatialChopFlag);
        dispatch(event);
    }

    /**
     * @param mutable
     * @param descriptor
     * @param tesselate
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#createShadedShape(boolean,
     *      com.raytheon.uf.viz.core.drawables.IDescriptor, boolean)
     */
    public IShadedShape createShadedShape(boolean mutable,
            IDescriptor descriptor, boolean tesselate) {
        return wrappedObject.createShadedShape(mutable, descriptor, tesselate);
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#init()
     */
    public void init() {
        wrappedObject.init();
        defaultFont = new DispatchingFont(wrappedObject.getDefaultFont(),
                getDispatcher());
    }

    private IExtent previousExtent;

    /**
     * @param display
     * @param isClearBackground
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#beginFrame(com.raytheon.uf.viz.core.drawables.IRenderableDisplay,
     *      boolean)
     */
    public void beginFrame(IRenderableDisplay display, boolean isClearBackground) {
        wrappedObject.beginFrame(display, isClearBackground);
        BeginFrameEvent beginFrame = RemoteGraphicsEventFactory.createEvent(
                BeginFrameEvent.class, this);
        IExtent curExtent = display.getExtent();
        if (previousExtent == null || curExtent.equals(previousExtent) == false) {
            beginFrame.setExtentFactor(curExtent.getScale());
            beginFrame.setExtentCenter(curExtent.getCenter());
        }
        dispatch(beginFrame);
        previousExtent = curExtent.clone();
    }

    private EndFrameEvent endFrame = RemoteGraphicsEventFactory.createEvent(
            EndFrameEvent.class, this);

    /**
     * 
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#endFrame()
     */
    public void endFrame() {
        wrappedObject.endFrame();
        dispatch(endFrame);
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#resize()
     */
    public void resize() {
        wrappedObject.resize();
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#dispose()
     */
    public void dispose() {
        wrappedObject.dispose();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#screenshot()
     */
    public BufferedImage screenshot() {
        return wrappedObject.screenshot();
    }

    /**
     * @param extent
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#setupClippingPlane(com.raytheon.uf.viz.core.IExtent)
     */
    public void setupClippingPlane(IExtent extent) {
        wrappedObject.setupClippingPlane(extent);
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#clearClippingPlane()
     */
    public void clearClippingPlane() {
        wrappedObject.clearClippingPlane();
    }

    /**
     * @param needsRefresh
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#setNeedsRefresh(boolean)
     */
    public void setNeedsRefresh(boolean needsRefresh) {
        wrappedObject.setNeedsRefresh(needsRefresh);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#isNeedsRefresh()
     */
    public boolean isNeedsRefresh() {
        return wrappedObject.isNeedsRefresh();
    }

    /**
     * @param backgroundColor
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#setBackgroundColor(org.eclipse.swt.graphics.RGB)
     */
    public void setBackgroundColor(RGB backgroundColor) {
        wrappedObject.setBackgroundColor(backgroundColor);
    }

    /**
     * @param isColorbarDisplayed
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#setUseBuiltinColorbar(boolean)
     */
    public void setUseBuiltinColorbar(boolean isColorbarDisplayed) {
        wrappedObject.setUseBuiltinColorbar(isColorbarDisplayed);
    }

    /**
     * @param colorMap
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawColorRamp(com.raytheon.uf.viz.core.DrawableColorMap)
     */
    public void drawColorRamp(DrawableColorMap colorMap) throws VizException {
        wrappedObject.drawColorRamp(colorMap);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#getDefaultFont()
     */
    public IFont getDefaultFont() {
        return defaultFont;
    }

    /**
     * @param coord
     * @param color
     * @param alpha
     * @param height
     * @param baseRadius
     * @param topRadius
     * @param sideCount
     * @param sliceCount
     * @param rotation
     * @param lean
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawCylinder(com.raytheon.uf.viz.core.geom.PixelCoordinate,
     *      org.eclipse.swt.graphics.RGB, float, double, double, double, int,
     *      int, double, double)
     */
    public void drawCylinder(PixelCoordinate coord, RGB color, float alpha,
            double height, double baseRadius, double topRadius, int sideCount,
            int sliceCount, double rotation, double lean) {
        wrappedObject.drawCylinder(coord, color, alpha, height, baseRadius,
                topRadius, sideCount, sliceCount, rotation, lean);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#getView()
     */
    @Override
    public IView getView() {
        return wrappedObject.getView();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#getViewType()
     */
    public String getViewType() {
        return wrappedObject.getViewType();
    }

    /**
     * @param x
     * @param y
     * @param z
     * @param color
     * @param pointStyle
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawPoint(double, double,
     *      double, org.eclipse.swt.graphics.RGB,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle)
     */
    public void drawPoint(double x, double y, double z, RGB color,
            PointStyle pointStyle) throws VizException {
        wrappedObject.drawPoint(x, y, z, color, pointStyle);
    }

    /**
     * @param locations
     * @param color
     * @param pointStyle
     * @param magnification
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawPoints(java.util.Collection,
     *      org.eclipse.swt.graphics.RGB,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle, float)
     */
    public void drawPoints(Collection<double[]> locations, RGB color,
            PointStyle pointStyle, float magnification) throws VizException {
        wrappedObject.drawPoints(locations, color, pointStyle, magnification);
    }

    /**
     * @param x
     * @param y
     * @param z
     * @param color
     * @param pointStyle
     * @param magnification
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawPoint(double, double,
     *      double, org.eclipse.swt.graphics.RGB,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle, float)
     */
    public void drawPoint(double x, double y, double z, RGB color,
            PointStyle pointStyle, float magnification) throws VizException {
        wrappedObject.drawPoint(x, y, z, color, pointStyle, magnification);
    }

    /**
     * @param updatedExtent
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#updateExtent(com.raytheon.uf.viz.core.IExtent)
     */
    public void updateExtent(IExtent updatedExtent) {
        wrappedObject.updateExtent(updatedExtent);
    }

    /**
     * @param offscreenImage
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#renderOffscreen(com.raytheon.uf.viz.core.drawables.IImage)
     */
    public void renderOffscreen(IImage offscreenImage) throws VizException {
        IOffscreenRenderingExtension extension = getExtension(IOffscreenRenderingExtension.class);
        extension.renderOffscreen(offscreenImage);
    }

    /**
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#renderOnscreen()
     */
    public void renderOnscreen() throws VizException {
        IOffscreenRenderingExtension extension = getExtension(IOffscreenRenderingExtension.class);
        extension.renderOnscreen();
    }

    /**
     * @param font
     * @param text
     * @param x
     * @param y
     * @param z
     * @param textStyle
     * @param color
     * @param horizontalAlignment
     * @param verticalAlignment
     * @param rotation
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawString(com.raytheon.uf.viz.core.drawables.IFont,
     *      java.lang.String, double, double, double,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle,
     *      org.eclipse.swt.graphics.RGB,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment,
     *      java.lang.Double)
     */
    public void drawString(IFont font, String text, double x, double y,
            double z, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment, Double rotation)
            throws VizException {
        if (font instanceof DispatchingFont) {
            font = ((DispatchingFont) font).getWrappedObject();
        }
        wrappedObject.drawString(font, text, x, y, z, textStyle, color,
                horizontalAlignment, verticalAlignment, rotation);
    }

    /**
     * @param font
     * @param text
     * @param x
     * @param y
     * @param z
     * @param textStyle
     * @param color
     * @param horizontalAlignment
     * @param rotation
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawString(com.raytheon.uf.viz.core.drawables.IFont,
     *      java.lang.String, double, double, double,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle,
     *      org.eclipse.swt.graphics.RGB,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment,
     *      java.lang.Double)
     */
    public void drawString(IFont font, String text, double x, double y,
            double z, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment, Double rotation)
            throws VizException {
        if (font instanceof DispatchingFont) {
            font = ((DispatchingFont) font).getWrappedObject();
        }
        wrappedObject.drawString(font, text, x, y, z, textStyle, color,
                horizontalAlignment, rotation);
    }

    /**
     * @param font
     * @param text
     * @param x
     * @param y
     * @param z
     * @param textStyle
     * @param colors
     * @param horizontalAlignment
     * @param verticalAlignment
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawStrings(com.raytheon.uf.viz.core.drawables.IFont,
     *      java.lang.String[], double, double, double,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle,
     *      org.eclipse.swt.graphics.RGB[],
     *      com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment)
     */
    public void drawStrings(IFont font, String[] text, double x, double y,
            double z, TextStyle textStyle, RGB[] colors,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment) throws VizException {
        if (font instanceof DispatchingFont) {
            font = ((DispatchingFont) font).getWrappedObject();
        }
        wrappedObject.drawStrings(font, text, x, y, z, textStyle, colors,
                horizontalAlignment, verticalAlignment);
    }

    /**
     * @param font
     * @param string
     * @param xPos
     * @param yPos
     * @param zPos
     * @param textStyle
     * @param color
     * @param horizontalAlignment
     * @param verticalAlignment
     * @param rotation
     * @param alpha
     * @param magnification
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawString(com.raytheon.uf.viz.core.drawables.IFont,
     *      java.lang.String, double, double, double,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle,
     *      org.eclipse.swt.graphics.RGB,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment,
     *      java.lang.Double, float, double)
     */
    public void drawString(IFont font, String string, double xPos, double yPos,
            double zPos, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment, Double rotation, float alpha,
            double magnification) throws VizException {
        if (font instanceof DispatchingFont) {
            font = ((DispatchingFont) font).getWrappedObject();
        }
        wrappedObject.drawString(font, string, xPos, yPos, zPos, textStyle,
                color, horizontalAlignment, verticalAlignment, rotation, alpha,
                magnification);
    }

    /**
     * @param font
     * @param text
     * @return
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#getStringBounds(com.raytheon.uf.viz.core.drawables.IFont,
     *      java.lang.String)
     */
    public Rectangle2D getStringBounds(IFont font, String text) {
        if (font instanceof DispatchingFont) {
            font = ((DispatchingFont) font).getWrappedObject();
        }
        return wrappedObject.getStringBounds(font, text);
    }

    /**
     * @param font
     * @param text
     * @param style
     * @return
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#getStringBounds(com.raytheon.uf.viz.core.drawables.IFont,
     *      java.lang.String[],
     *      com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle)
     */
    public Rectangle2D getStringBounds(IFont font, String[] text,
            TextStyle style) {
        if (font instanceof DispatchingFont) {
            font = ((DispatchingFont) font).getWrappedObject();
        }
        return wrappedObject.getStringBounds(font, text, style);
    }

    /**
     * @param colorMap
     * @param pixelExtent
     * @param blendAlpha
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawColorRamp(com.raytheon.uf.common.colormap.IColorMap,
     *      com.raytheon.uf.viz.core.IExtent, float)
     */
    public void drawColorRamp(IColorMap colorMap, IExtent pixelExtent,
            float blendAlpha) throws VizException {
        wrappedObject.drawColorRamp(colorMap, pixelExtent, blendAlpha);
    }

    /**
     * @param colorMap
     * @param pixelExtent
     * @param blendAlpha
     * @param brightness
     * @param contrast
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawColorRamp(com.raytheon.uf.common.colormap.IColorMap,
     *      com.raytheon.uf.viz.core.IExtent, float, float, float)
     */
    public void drawColorRamp(IColorMap colorMap, IExtent pixelExtent,
            float blendAlpha, float brightness, float contrast)
            throws VizException {
        wrappedObject.drawColorRamp(colorMap, pixelExtent, blendAlpha,
                brightness, contrast);
    }

    /**
     * @param colorMapParams
     * @param pixelExtent
     * @param blendAlpha
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawColorRamp(com.raytheon.uf.viz.core.drawables.ColorMapParameters,
     *      com.raytheon.uf.viz.core.IExtent, float)
     */
    public void drawColorRamp(ColorMapParameters colorMapParams,
            IExtent pixelExtent, float blendAlpha) throws VizException {
        wrappedObject.drawColorRamp(colorMapParams, pixelExtent, blendAlpha);
    }

    /**
     * @param colorMapParams
     * @param pixelExtent
     * @param blendAlpha
     * @param brightness
     * @param contrast
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawColorRamp(com.raytheon.uf.viz.core.drawables.ColorMapParameters,
     *      com.raytheon.uf.viz.core.IExtent, float, float, float)
     */
    public void drawColorRamp(ColorMapParameters colorMapParams,
            IExtent pixelExtent, float blendAlpha, float brightness,
            float contrast) throws VizException {
        wrappedObject.drawColorRamp(colorMapParams, pixelExtent, blendAlpha,
                brightness, contrast);
    }

    /**
     * @param x1
     * @param y1
     * @param z1
     * @param radius
     * @param color
     * @param width
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawCircle(double, double,
     *      double, double, org.eclipse.swt.graphics.RGB, float)
     */
    public void drawCircle(double x1, double y1, double z1, double radius,
            RGB color, float width) throws VizException {
        wrappedObject.drawCircle(x1, y1, z1, radius, color, width);
    }

    /**
     * @param x
     * @param y
     * @param z
     * @param radius
     * @param color
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawFilledCircle(double,
     *      double, double, double, org.eclipse.swt.graphics.RGB)
     */
    public void drawFilledCircle(double x, double y, double z, double radius,
            RGB color) throws VizException {
        wrappedObject.drawFilledCircle(x, y, z, radius, color);
    }

    /**
     * @param name
     * @return
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#buildColorMap(java.lang.String)
     */
    public IColorMap buildColorMap(String name) throws VizException {
        return wrappedObject.buildColorMap(name);
    }

    /**
     * @param x1
     * @param y1
     * @param z1
     * @param x2
     * @param y2
     * @param z2
     * @param color
     * @param width
     * @param lineStyle
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawLine(double, double,
     *      double, double, double, double, org.eclipse.swt.graphics.RGB, float,
     *      com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle)
     */
    public void drawLine(double x1, double y1, double z1, double x2, double y2,
            double z2, RGB color, float width, LineStyle lineStyle)
            throws VizException {
        wrappedObject.drawLine(x1, y1, z1, x2, y2, z2, color, width, lineStyle);
    }

    /**
     * @param x1
     * @param y1
     * @param z1
     * @param x2
     * @param y2
     * @param z2
     * @param color
     * @param width
     * @throws VizException
     * @deprecated
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawLine(double, double,
     *      double, double, double, double, org.eclipse.swt.graphics.RGB, float)
     */
    public void drawLine(double x1, double y1, double z1, double x2, double y2,
            double z2, RGB color, float width) throws VizException {
        wrappedObject.drawLine(x1, y1, z1, x2, y2, z2, color, width);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#getExtension(java.lang.Class)
     */
    @Override
    public <T extends IGraphicsExtensionInterface> T getExtension(
            Class<T> extensionClass) throws VizException {
        return extensionManager.getExtension(extensionClass);
    }

}
