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
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
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
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.IColormappedDataPreparer;
import com.raytheon.uf.viz.core.data.IDataPreparer;
import com.raytheon.uf.viz.core.data.IImageDataPreparer;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.data.resp.NumericImageData;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.FontType;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.ImagingSupport;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtensionManager;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.clipping.ClearClippingPane;
import com.raytheon.uf.viz.remote.graphics.events.clipping.SetupClippingPane;
import com.raytheon.uf.viz.remote.graphics.events.colormap.DispatchingColorMapManager;
import com.raytheon.uf.viz.remote.graphics.events.colormap.DrawColorRampEvent;
import com.raytheon.uf.viz.remote.graphics.events.drawables.DrawCircleEvent;
import com.raytheon.uf.viz.remote.graphics.events.drawables.DrawCirclesEvent;
import com.raytheon.uf.viz.remote.graphics.events.drawables.DrawLineEvent;
import com.raytheon.uf.viz.remote.graphics.events.drawables.DrawLinesEvent;
import com.raytheon.uf.viz.remote.graphics.events.drawables.DrawRectEvent;
import com.raytheon.uf.viz.remote.graphics.events.imagery.CreateIImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.points.DrawPointsEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.BeginFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.EndFrameEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.CreateShadedShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.CreateWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.DrawShadedShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.DrawShadedShapesEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.RenderWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.strings.DrawStringEvent;
import com.raytheon.uf.viz.remote.graphics.events.strings.DrawStringsEvent;
import com.raytheon.uf.viz.remote.graphics.extensions.DispatchingImagingExtension;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingFont;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingImage;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingImage.DispatchingRenderedImageCallback;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingShadedShape;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingWireframeShape;

/**
 * Graphics target that uses a dispatcher for dispatching graphics events. These
 * events involve rendering and creating render objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 28, 2012           mschenke    Initial creation
 * Apr 04, 2014  2920     bsteffen    Allow strings to use mulitple styles.
 * May 16, 2014  3163     bsteffen    Remove references to deprecated
 *                                    {@link IGraphicsTarget} methods.
 * Jul 28, 2014  3429     mapeters    Updated deprecated drawLine() calls.
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

    private RGB backgroundColor = new RGB(0, 0, 0);

    private GraphicsExtensionManager extensionManager;

    private Rectangle bounds;

    public DispatchGraphicsTarget(IGraphicsTarget target, Canvas canvas,
            Dispatcher dispatcher) {
        super(target, dispatcher);
        extensionManager = new GraphicsExtensionManager(this);
        bounds = canvas.getBounds();
        canvas.addListener(SWT.Resize, new Listener() {
            @Override
            public void handleEvent(Event event) {
                bounds = ((Canvas) event.widget).getBounds();
            }
        });
    }

    /**
     * @deprecated For general IImage construction, use
     *             {@link #initializeRaster(IRenderedImageCallback)}. Other
     *             image construction methods should be done through extensions
     */
    @Deprecated
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
    @Override
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
    @Override
    public IFont initializeFont(String fontName, float size, Style[] styles) {
        return new DispatchingFont(wrappedObject.initializeFont(fontName, size,
                styles), getDispatcher());
    }

    /*
     * @deprecated {@link #initializeFont(File,
     * com.raytheon.uf.viz.core.drawables.IFont.FontType, float,
     * com.raytheon.uf.viz.core.drawables.IFont.Style[])} should be used instead
     */
    @Deprecated
    public IFont initializeFont(File fontFile, float size, Style[] styles) {
        return initializeFont(fontFile, FontType.TRUETYPE, size, styles);
    }

    /**
     * @param fontFile
     * @param type
     * @param size
     * @param styles
     * @return
     * @see 
     *      com.raytheon.uf.viz.core.IGraphicsTarget#initializeFont(java.io.File,
     *      com.raytheon.uf.viz.core.drawables.IFont.FontType float,
     *      com.raytheon.uf.viz.core.drawables.IFont.Style[])
     */
    @Override
    public IFont initializeFont(File fontFile, FontType type, float size,
            Style[] styles) {
        return new DispatchingFont(wrappedObject.initializeFont(fontFile, type,
                size, styles), getDispatcher(), fontFile);
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
    @Override
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
    @Override
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
    @Override
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
    @Override
    public void drawStrings(DrawableString... parameters) throws VizException {
        drawStrings(Arrays.asList(parameters));
    }

    /**
     * @param parameters
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawStrings(java.util.Collection)
     */
    @Override
    public void drawStrings(Collection<DrawableString> parameters)
            throws VizException {
        List<IFont> originalFonts = new ArrayList<IFont>(parameters.size());
        for (DrawableString string : parameters) {
            originalFonts.add(string.font);
            if (string.font instanceof DispatchingFont) {
                DispatchingFont font = (DispatchingFont) string.font;
                font.flushState();
                string.font = font.getWrappedObject();
            }
        }
        wrappedObject.drawStrings(parameters);

        Iterator<DrawableString> stringIter = parameters.iterator();
        Iterator<IFont> fontIter = originalFonts.iterator();
        while (stringIter.hasNext() && fontIter.hasNext()) {
            stringIter.next().font = fontIter.next();
        }

        DrawStringsEvent event = RemoteGraphicsEventFactory.createEvent(
                DrawStringsEvent.class, this);
        DrawStringEvent[] strings = new DrawStringEvent[parameters.size()];
        int i = 0;
        for (DrawableString param : parameters) {
            strings[i] = RemoteGraphicsEventFactory.createEvent(
                    DrawStringEvent.class, this);
            strings[i].setDrawableString(param);
            ++i;
        }
        event.setObjects(strings);
        dispatch(event);
    }

    /**
     * @param parameters
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#getStringsBounds(com.raytheon.uf.viz.core.DrawableString)
     */
    @Override
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
    @Override
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
    @Override
    public void drawShadedShape(IShadedShape shape, float alpha)
            throws VizException {
        drawShadedShape(shape, alpha, 1.0f);
    }

    /**
     * @param shape
     * @param alpha
     * @param brightness
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawShadedShape(com.raytheon.uf.viz.core.drawables.IShadedShape,
     *      float, float)
     */
    @Override
    public void drawShadedShape(IShadedShape shape, float alpha,
            float brightness) throws VizException {
        drawShadedShapes(alpha, brightness, shape);
    }

    /**
     * @param alpha
     * @param brightness
     * @param shapes
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawShadedShapes(float,
     *      float, com.raytheon.uf.viz.core.drawables.IShadedShape[])
     */
    @Override
    public void drawShadedShapes(float alpha, float brightness,
            IShadedShape... shapes) throws VizException {
        IShadedShape[] actualShapes = new IShadedShape[shapes.length];
        DrawShadedShapeEvent[] shapeEvents = new DrawShadedShapeEvent[shapes.length];
        DrawShadedShapesEvent event = RemoteGraphicsEventFactory.createEvent(
                DrawShadedShapesEvent.class, this);
        event.setAlpha(alpha);
        event.setBrightness(brightness);
        for (int i = 0; i < shapes.length; ++i) {
            DispatchingShadedShape shape = (DispatchingShadedShape) shapes[i];
            shape.flushState();
            shapeEvents[i] = RemoteGraphicsEventFactory.createEvent(
                    DrawShadedShapeEvent.class, shape);
            actualShapes[i] = shape.getWrappedObject();
        }
        event.setObjects(shapeEvents);
        dispatch(event);

        // Actual draw
        wrappedObject.drawShadedShapes(alpha, brightness, actualShapes);
    }

    /**
     * @param shape
     * @param color
     * @param lineWidth
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawWireframeShape(com.raytheon.uf.viz.core.drawables.IWireframeShape,
     *      org.eclipse.swt.graphics.RGB, float)
     */
    @Override
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
    @Override
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
    @Override
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
    @Override
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
    @Override
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
            DispatchingFont df = (DispatchingFont) font;
            df.flushState();
            event.setFontId(df.getObjectId());
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
    @Override
    public void drawRect(IExtent pe, RGB color, float lineWidth, double alpha)
            throws VizException {
        wrappedObject.drawRect(pe, color, lineWidth, alpha);
        drawRect(pe, color, (float) alpha, 1.0f, false, null);
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
    @Override
    public void drawShadedRect(IExtent pe, RGB color, double alpha,
            byte[] pattern) throws VizException {
        wrappedObject.drawShadedRect(pe, color, alpha, pattern);
        drawRect(pe, color, (float) alpha, 1.0f, true, pattern);
    }

    private void drawRect(IExtent pe, RGB color, float alpha, float lineWidth,
            boolean filled, byte[] pattern) {
        DrawRectEvent event = RemoteGraphicsEventFactory.createEvent(
                DrawRectEvent.class, this);
        event.setExtent(pe);
        event.setColor(color);
        event.setAlpha(alpha);
        event.setLineWidth(lineWidth);
        event.setFilled(filled);
        event.setFillPattern(pattern);
        dispatch(event);
    }

    /**
     * @param circles
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawCircle(com.raytheon.uf.viz.core.DrawableCircle[])
     */
    @Override
    public void drawCircle(DrawableCircle... circles) throws VizException {
        wrappedObject.drawCircle(circles);
        DrawCirclesEvent event = RemoteGraphicsEventFactory.createEvent(
                DrawCirclesEvent.class, this);
        DrawCircleEvent[] events = new DrawCircleEvent[circles.length];
        for (int i = 0; i < circles.length; ++i) {
            events[i] = RemoteGraphicsEventFactory.createEvent(
                    DrawCircleEvent.class, this);
            events[i].setDrawableCircle(circles[i]);
        }
        event.setObjects(events);
        dispatch(event);
    }

    /**
     * @param lines
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawLine(com.raytheon.uf.viz.core.DrawableLine[])
     */
    @Override
    public void drawLine(DrawableLine... lines) throws VizException {
        wrappedObject.drawLine(lines);
        DrawLinesEvent event = RemoteGraphicsEventFactory.createEvent(
                DrawLinesEvent.class, this);
        DrawLineEvent[] events = new DrawLineEvent[lines.length];
        for (int i = 0; i < lines.length; ++i) {
            events[i] = RemoteGraphicsEventFactory.createEvent(
                    DrawLineEvent.class, this);
            events[i].setDrawableLine(lines[i]);
        }
        event.setObjects(events);
        dispatch(event);
    }

    /**
     * @deprecated: Use {@link #drawCircle(DrawableCircle...)}
     */
    @Deprecated
    public void drawArc(double x1, double y1, double z1, double radius,
            RGB color, float width, int startAzimuth, int endAzimuth,
            LineStyle lineStyle, boolean includeSides) throws VizException {
        DrawableCircle dc = new DrawableCircle();
        dc.setCoordinates(x1, y1, z1);
        dc.basics.color = color;
        dc.lineStyle = lineStyle;
        dc.startAzimuth = startAzimuth;
        dc.endAzimuth = endAzimuth;
        if (startAzimuth > endAzimuth) {
            endAzimuth += 360;
        }
        dc.numberOfPoints = endAzimuth - startAzimuth;
        dc.includeSides = includeSides;
        dc.lineWidth = width;
        dc.radius = radius;
        drawCircle(dc);
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
    @Override
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
    @Override
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
    @Override
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
    @Override
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
    @Override
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
    @Override
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
     * @deprecated: Use
     *              {@link #createShadedShape(boolean, GeneralGridGeometry, boolean)}
     *              instead
     */
    @Deprecated
    public IShadedShape createShadedShape(boolean mutable,
            IDescriptor descriptor, boolean tesselate) {
        return createShadedShape(mutable, descriptor.getGridGeometry(),
                tesselate);
    }

    /**
     * @param mutable
     * @param descriptor
     * @param tesselate
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#createShadedShape(boolean,
     *      GeneralGridGeometry, boolean)
     */
    @Override
    public IShadedShape createShadedShape(boolean mutable,
            GeneralGridGeometry targetGeometry, boolean tesselate) {
        DispatchingShadedShape shape = new DispatchingShadedShape(
                wrappedObject.createShadedShape(mutable, targetGeometry,
                        tesselate), getDispatcher());
        // Send creation event
        CreateShadedShapeEvent event = RemoteGraphicsEventFactory.createEvent(
                CreateShadedShapeEvent.class, shape);
        event.setTargetGeometry(targetGeometry);
        event.setMutable(mutable);
        event.setTesselate(tesselate);
        dispatch(event);
        return shape;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#init()
     */
    @Override
    public void init() {
        wrappedObject.init();
        defaultFont = new DispatchingFont(wrappedObject.getDefaultFont(),
                getDispatcher());
    }

    /**
     * @param display
     * @param isClearBackground
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#beginFrame(com.raytheon.uf.viz.core.IView,
     *      boolean)
     */
    @Override
    public void beginFrame(IView view, boolean isClearBackground) {
        wrappedObject.beginFrame(view, isClearBackground);
        BeginFrameEvent beginFrame = RemoteGraphicsEventFactory.createEvent(
                BeginFrameEvent.class, this);
        beginFrame.setExtent(view.getExtent().clone());
        beginFrame.setColor(backgroundColor);
        beginFrame.setBounds(bounds);
        dispatch(beginFrame);
    }

    /**
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#endFrame()
     */
    @Override
    public void endFrame() {
        wrappedObject.endFrame();
        dispatch(RemoteGraphicsEventFactory.createEvent(EndFrameEvent.class,
                this));
    }

    /**
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#resize()
     */
    @Override
    public void resize() {
        wrappedObject.resize();
    }

    /**
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#dispose()
     */
    @Override
    public void dispose() {
        wrappedObject.dispose();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#screenshot()
     */
    @Override
    public BufferedImage screenshot() {
        return wrappedObject.screenshot();
    }

    /**
     * @param extent
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#setupClippingPlane(com.raytheon.uf.viz.core.IExtent)
     */
    @Override
    public void setupClippingPlane(IExtent extent) {
        wrappedObject.setupClippingPlane(extent);
        SetupClippingPane event = RemoteGraphicsEventFactory.createEvent(
                SetupClippingPane.class, this);
        event.setExtent(extent);
        dispatch(event);
    }

    /**
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#clearClippingPlane()
     */
    @Override
    public void clearClippingPlane() {
        wrappedObject.clearClippingPlane();
        dispatch(RemoteGraphicsEventFactory.createEvent(
                ClearClippingPane.class, this));
    }

    /**
     * @param needsRefresh
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#setNeedsRefresh(boolean)
     */
    @Override
    public void setNeedsRefresh(boolean needsRefresh) {
        wrappedObject.setNeedsRefresh(needsRefresh);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#isNeedsRefresh()
     */
    @Override
    public boolean isNeedsRefresh() {
        return wrappedObject.isNeedsRefresh();
    }

    /**
     * @param backgroundColor
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#setBackgroundColor(org.eclipse.swt.graphics.RGB)
     */
    @Override
    public void setBackgroundColor(RGB backgroundColor) {
        this.backgroundColor = backgroundColor;
        wrappedObject.setBackgroundColor(backgroundColor);
    }

    /**
     * @deprecated: This method has no effect. IGraphicsTargets are not
     *              responsible to drawing a colorbar. Use method drawColorRamp
     *              to draw a color ramp
     */
    @Deprecated
    public void setUseBuiltinColorbar(boolean isColorbarDisplayed) {

    }

    /**
     * @param colorMap
     * @throws VizException
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawColorRamp(com.raytheon.uf.viz.core.DrawableColorMap)
     */
    @Override
    public void drawColorRamp(DrawableColorMap colorMap) throws VizException {
        wrappedObject.drawColorRamp(colorMap);
        DrawColorRampEvent event = RemoteGraphicsEventFactory.createEvent(
                DrawColorRampEvent.class, this);
        event.setDrawableColorMap(colorMap,
                DispatchingColorMapManager.getInstance(getDispatcher())
                        .getColorMapId(colorMap.getColorMapParams()));
        dispatch(event);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#getDefaultFont()
     */
    @Override
    public IFont getDefaultFont() {
        return defaultFont;
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
     * @deprecated Should not be used for anything
     */
    @Deprecated
    public String getViewType() {
        return VizConstants.VIEW_2D;
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
    @Override
    public void drawPoint(double x, double y, double z, RGB color,
            PointStyle pointStyle) throws VizException {
        wrappedObject.drawPoint(x, y, z, color, pointStyle);
        sendDrawPointsEvent(Arrays.asList(new double[] { x, y, z }), color,
                pointStyle, 1.0f);
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
    @Override
    public void drawPoints(Collection<double[]> locations, RGB color,
            PointStyle pointStyle, float magnification) throws VizException {
        wrappedObject.drawPoints(locations, color, pointStyle, magnification);
        sendDrawPointsEvent(locations, color, pointStyle, magnification);
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
    @Override
    public void drawPoint(double x, double y, double z, RGB color,
            PointStyle pointStyle, float magnification) throws VizException {
        wrappedObject.drawPoint(x, y, z, color, pointStyle, magnification);
        sendDrawPointsEvent(Arrays.asList(new double[] { x, y, z }), color,
                pointStyle, magnification);
    }

    private void sendDrawPointsEvent(Collection<double[]> points, RGB color,
            PointStyle pointStyle, float magnification) {
        DrawPointsEvent event = RemoteGraphicsEventFactory.createEvent(
                DrawPointsEvent.class, this);
        event.addPoints(points);
        event.setColor(color);
        event.setStyle(pointStyle);
        event.setMagnification(magnification);
        dispatch(event);
    }

    /**
     * @deprecated Use {@link #drawStrings(DrawableString...)}
     */
    @Deprecated
    public void drawString(IFont font, String text, double x, double y,
            double z, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment, Double rotation)
            throws VizException {
        DrawableString string = new DrawableString(text, color);
        string.setCoordinates(x, y, z);
        string.font = font;
        string.addTextStyle(textStyle);
        string.horizontalAlignment = horizontalAlignment;
        string.verticallAlignment = verticalAlignment;
        string.rotation = rotation;
        drawStrings(string);
    }

    /**
     * @deprecated Use {@link #drawStrings(DrawableString...)}
     */
    @Deprecated
    public void drawString(IFont font, String text, double x, double y,
            double z, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment, Double rotation)
            throws VizException {
        DrawableString string = new DrawableString(text, color);
        string.setCoordinates(x, y, z);
        string.font = font;
        string.addTextStyle(textStyle);
        string.horizontalAlignment = horizontalAlignment;
        string.rotation = rotation;
        drawStrings(string);
    }

    /**
     * @deprecated Use {@link #drawStrings(DrawableString...)}
     */
    @Deprecated
    public void drawStrings(IFont font, String[] text, double x, double y,
            double z, TextStyle textStyle, RGB[] colors,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment) throws VizException {
        DrawableString string = new DrawableString(text, colors);
        string.setCoordinates(x, y, z);
        string.font = font;
        string.addTextStyle(textStyle);
        string.horizontalAlignment = horizontalAlignment;
        string.verticallAlignment = verticalAlignment;
        drawStrings(string);
    }

    /**
     * @deprecated Use {@link #getStringsBounds(DrawableString)}
     */
    @Deprecated
    public Rectangle2D getStringBounds(IFont font, String text) {
        DrawableString params = new DrawableString(text, null);
        params.font = font;
        return getStringsBounds(params, text);
    }

    /**
     * @deprecated Use {@link #getStringsBounds(DrawableString)}
     */
    @Deprecated
    public Rectangle2D getStringBounds(IFont font, String[] text,
            TextStyle style) {
        DrawableString params = new DrawableString(text, (RGB[]) null);
        params.font = font;
        params.addTextStyle(style);
        return getStringsBounds(params);
    }

    /**
     * @deprecated Use {@link #drawCircle(DrawableCircle...)}
     */
    @Deprecated
    public void drawCircle(double x1, double y1, double z1, double radius,
            RGB color, float width) throws VizException {
        DrawableCircle circle = new DrawableCircle();
        circle.setCoordinates(x1, y1, z1);
        circle.lineWidth = width;
        circle.basics.color = color;
        circle.radius = new Double(radius);
        drawCircle(circle);
    }

    /**
     * @deprecated Use {@link #drawCircle(DrawableCircle...)}
     */
    @Deprecated
    public void drawFilledCircle(double x, double y, double z, double radius,
            RGB color) throws VizException {
        DrawableCircle circle = new DrawableCircle();
        circle.setCoordinates(x, y, z);
        circle.basics.color = color;
        circle.radius = new Double(radius);
        circle.filled = true;
        drawCircle(circle);
    }

    /**
     * @deprecated Use {@link ColorMapLoader#loadColorMap(String)}
     */
    @Deprecated
    public IColorMap buildColorMap(String name) throws VizException {
        return ColorMapLoader.loadColorMap(name);
    }

    /**
     * @deprecated Use {@link #drawLine(DrawableLine...)}
     */
    @Deprecated
    public void drawLine(double x1, double y1, double z1, double x2, double y2,
            double z2, RGB color, float width, LineStyle lineStyle)
            throws VizException {
        DrawableLine dl = new DrawableLine();
        dl.addPoint(x1, y1, z1);
        dl.addPoint(x2, y2, z2);
        dl.basics.color = color;
        dl.width = width;
        dl.lineStyle = lineStyle;
        drawLine(dl);
    }

    /**
     * @deprecated Use {@link #drawLine(DrawableLine...)}
     */
    @Deprecated
    public void drawLine(double x1, double y1, double z1, double x2, double y2,
            double z2, RGB color, float width) throws VizException {
        DrawableLine line = new DrawableLine();
        line.setCoordinates(x1, y1, z1);
        line.addPoint(x2, y2, z2);
        line.basics.color = color;
        line.width = width;
        line.lineStyle = LineStyle.DEFAULT;
        drawLine(line);
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
