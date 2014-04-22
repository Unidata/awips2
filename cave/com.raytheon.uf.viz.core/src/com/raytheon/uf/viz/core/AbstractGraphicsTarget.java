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
package com.raytheon.uf.viz.core;

import java.awt.geom.Rectangle2D;
import java.awt.image.RenderedImage;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
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

/**
 * A base target that handles many of the generic things a target needs to do,
 * such as calling alternative forms of overloaded functions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 25, 2012           bsteffen    Initial creation
 * Jul 18, 2013  2189     mschenke    Added ability to specify font type
 * Apr 04, 2014  2920     bsteffen    Allow strings to use mulitple styles.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractGraphicsTarget implements IGraphicsTarget {

    /** Does a refresh need to be performed? */
    protected boolean needsRefresh = true;

    protected GraphicsExtensionManager extensionManager;

    protected RGB backgroundColor = new RGB(0, 0, 0);

    public AbstractGraphicsTarget() {
        extensionManager = new GraphicsExtensionManager(this);
    }

    @Override
    public IFont initializeFont(File fontFile, float size, Style[] styles) {
        return initializeFont(fontFile, FontType.TRUETYPE, size, styles);
    }

    @Override
    public boolean drawRasters(PaintProperties paintProps,
            DrawableImage... images) throws VizException {
        return ImagingSupport.drawRasters(this, paintProps, images);
    }

    @Override
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
                throw new IllegalStateException(e);
            }
        }
        return rval;
    }

    @Override
    public boolean drawRaster(IImage image, PixelCoverage extent,
            PaintProperties paintProps) throws VizException {
        return this.drawRaster(image, extent, paintProps,
                RasterMode.SYNCHRONOUS);
    }

    @Override
    public boolean drawRaster(IImage image, PixelCoverage extent,
            PaintProperties paintProps, RasterMode mode) throws VizException {
        DrawableImage di = new DrawableImage(image, extent);
        di.setMode(mode);
        return drawRasters(paintProps, di);
    }

    @Override
    public void drawStrings(DrawableString... parameters) throws VizException {
        drawStrings(Arrays.asList(parameters));
    }

    @Override
    public Rectangle2D getStringsBounds(DrawableString parameters) {
        // TODO: Handle box already set? and OUTLINE style
        String[] text = parameters.getText();
        double maxWidth = 0;
        double totalHeight = 0;

        Rectangle2D bounds = null;

        for (String string : text) {
            Rectangle2D txtBounds = getStringsBounds(parameters, string);
            if (txtBounds.getWidth() > maxWidth) {
                maxWidth = txtBounds.getWidth();
            }
            totalHeight += txtBounds.getHeight();
            if (bounds == null) {
                bounds = txtBounds;
            }
        }

        if (bounds != null) {
            if (parameters.getTextStyles().contains(TextStyle.BOXED)
                    || parameters.getTextStyles().contains(TextStyle.BLANKED)) {
                maxWidth += 1.0f;
            }
            bounds.setRect(0, 0, maxWidth, totalHeight);
        }
        return bounds;
    }

    @Override
    public void drawShadedShape(IShadedShape shape, float alpha)
            throws VizException {
        drawShadedShape(shape, alpha, 1.0f);
    }

    @Override
    public void drawShadedShape(IShadedShape shape, float alpha,
            float brightness) throws VizException {
        drawShadedShapes(alpha, brightness, shape);
    }

    @Override
    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth) throws VizException {
        drawWireframeShape(shape, color, lineWidth, LineStyle.SOLID);
    }

    @Override
    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle) throws VizException {
        drawWireframeShape(shape, color, lineWidth, lineStyle, 1.0f);
    }

    @Override
    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, IFont font)
            throws VizException {
        drawWireframeShape(shape, color, lineWidth, lineStyle, font, 1.0f);
    }

    @Override
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
            dc.numberOfPoints = (endAzimuth + 360) - startAzimuth;
        } else {
            dc.numberOfPoints = endAzimuth - startAzimuth;
        }
        dc.includeSides = includeSides;
        dc.lineWidth = width;
        dc.radius = radius;
        drawCircle(dc);
    }

    @Override
    public double[] getPointOnCircle(double x1, double y1, double z1,
            double radius, double angle) throws VizException {
        double pointOnCircle[] = new double[3];

        pointOnCircle[0] = x1 + radius * Math.cos(Math.toRadians(angle));
        pointOnCircle[1] = y1 + radius * Math.sin(Math.toRadians(angle));
        // on plane z1
        pointOnCircle[2] = z1;
        return pointOnCircle;
    }

    @Override
    public IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor) {
        return createWireframeShape(mutable, descriptor, 0.0f);
    }

    @Override
    public IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor, float simplificationLevel) {
        return createWireframeShape(mutable, descriptor, simplificationLevel,
                false, null);

    }

    @Override
    public IWireframeShape createWireframeShape(boolean mutable,
            GeneralGridGeometry geom, float simplificationLevel) {
        return createWireframeShape(mutable, geom, simplificationLevel, false,
                null);
    }

    @Override
    public IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor, float simplificationLevel,
            boolean spatialChopFlag, IExtent extent) {
        return createWireframeShape(mutable, descriptor.getGridGeometry(),
                simplificationLevel, spatialChopFlag, extent);
    }

    @Override
    public IWireframeShape createWireframeShape(boolean mutableFlag,
            GeneralGridGeometry geom) {
        return createWireframeShape(mutableFlag, geom, 0.0f);
    }

    @Override
    public IShadedShape createShadedShape(boolean mutable,
            IDescriptor descriptor, boolean tesselate) {
        return createShadedShape(mutable, descriptor.getGridGeometry(),
                tesselate);
    }

    @Override
    public void setNeedsRefresh(boolean needsRefresh) {
        this.needsRefresh = needsRefresh;
    }

    @Override
    public boolean isNeedsRefresh() {
        return needsRefresh;
    }

    @Override
    public void setBackgroundColor(RGB backgroundColor) {
        this.backgroundColor = backgroundColor;
    }

    @Override
    @Deprecated
    public void setUseBuiltinColorbar(boolean isColorbarDisplayed) {
        // Most targets do not support a BuiltinColorbar and this functionality
        // is deprecated, so the default behavior is to completely ignore this.
    }

    @Override
    @Deprecated
    public String getViewType() {
        return VizConstants.VIEW_2D;
    }

    @Override
    public void drawPoint(double x, double y, double z, RGB color,
            PointStyle pointStyle) throws VizException {
        drawPoint(x, y, z, color, pointStyle, 1.0f);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#drawPoints(java.util.Collection,
     * org.eclipse.swt.graphics.RGB,
     * com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle, float)
     */
    @Override
    public void drawPoints(Collection<double[]> locations, RGB color,
            PointStyle pointStyle, float magnification) throws VizException {
        List<DrawableString> dstrings = new ArrayList<DrawableString>();
        for (double[] location : locations) {
            String text = null;
            switch (pointStyle) {
            case SQUARE:
                text = "■";
                break;
            case CIRCLE:
                text = "○";
                break;
            case CROSS:
                text = "+";
                break;
            case DASH:
                text = "-";
                break;
            case DISC:
                text = "●";
                break;

            case POINT:
                text = "•";
                break;
            case BOX:
                text = "□";
                break;
            case STAR:
                text = "*";
                break;
            case X:
                text = "x";
                break;
            case NONE:
            default:
                return;
            }
            DrawableString dstring = new DrawableString(text, color);
            dstring.setCoordinates(location[0], location[1]);
            dstring.magnification = magnification;
            dstrings.add(dstring);
        }
        drawStrings(dstrings);
    }

    @Override
    public void drawPoint(double x, double y, double z, RGB color,
            PointStyle pointStyle, float magnification) throws VizException {
        drawPoints(Arrays.asList(new double[] { x, y, z }), color, pointStyle,
                magnification);
    }

    @Override
    public void drawString(IFont font, String text, double x, double y,
            double z, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment, Double rotation)
            throws VizException {
        DrawableString params = new DrawableString(text, color);
        params.font = font;
        params.setCoordinates(x, y, z);
        params.addTextStyle(textStyle);
        params.horizontalAlignment = horizontalAlignment;
        params.verticallAlignment = verticalAlignment;
        params.rotation = rotation != null ? rotation : 0.0;
        drawStrings(params);
    }

    @Override
    public void drawString(IFont font, String text, double x, double y,
            double z, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment, Double rotation)
            throws VizException {
        drawString(font, text, x, y, z, textStyle, color, horizontalAlignment,
                VerticalAlignment.BOTTOM, rotation);
    }

    @Override
    public void drawStrings(IFont font, String[] text, double x, double y,
            double z, TextStyle textStyle, RGB[] colors,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment) throws VizException {
        DrawableString params = new DrawableString(text, colors);
        params.font = font;
        params.setCoordinates(x, y, z);
        params.addTextStyle(textStyle);
        params.horizontalAlignment = horizontalAlignment;
        params.verticallAlignment = verticalAlignment;
        drawStrings(params);
    }

    @Override
    public Rectangle2D getStringBounds(IFont font, String text) {
        if (font == null) {
            font = getDefaultFont();
        }
        DrawableString params = new DrawableString(text, null);
        params.font = font;
        return getStringsBounds(params, text);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#getStringBounds(com.raytheon
     * .uf.viz.core.drawables.IFont, java.lang.String[],
     * com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle)
     */
    @Override
    public Rectangle2D getStringBounds(IFont font, String[] text,
            TextStyle style) {
        DrawableString params = new DrawableString(text, (RGB[]) null);
        params.font = font;
        params.addTextStyle(style);
        return getStringsBounds(params);
    }

    @Override
    public void drawCircle(double x1, double y1, double z1, double radius,
            RGB color, float width) throws VizException {
        DrawableCircle circle = new DrawableCircle();
        circle.setCoordinates(x1, y1, z1);
        circle.lineWidth = width;
        circle.basics.color = color;
        circle.radius = new Double(radius);
        drawCircle(circle);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawFilledCircle(double,
     * double, double, double, org.eclipse.swt.graphics.RGB)
     */
    @Override
    public void drawFilledCircle(double x, double y, double z, double radius,
            RGB color) throws VizException {
        DrawableCircle circle = new DrawableCircle();
        circle.setCoordinates(x, y, z);
        circle.basics.color = color;
        circle.radius = new Double(radius);
        circle.filled = true;
        drawCircle(circle);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#buildColorMap(java.lang.String)
     */
    @Override
    public IColorMap buildColorMap(String name) throws VizException {
        return ColorMapLoader.loadColorMap(name);

    }

    @Override
    public void drawLine(double x1, double y1, double z1, double x2, double y2,
            double z2, RGB color, float width, LineStyle lineStyle)
            throws VizException {
        DrawableLine line = new DrawableLine();
        line.addPoint(x1, y1, z1);
        line.addPoint(x2, y2, z2);
        line.width = width;
        line.lineStyle = lineStyle;
        line.basics.color = color;
        drawLine(line);
    }

    @Override
    public void drawLine(double x1, double y1, double z1, double x2, double y2,
            double z2, RGB color, float width) throws VizException {
        drawLine(x1, y1, z1, x2, y2, z2, color, width, LineStyle.SOLID);
    }

    @Override
    public <T extends IGraphicsExtensionInterface> T getExtension(
            Class<T> extensionClass) throws VizException {
        return extensionManager.getExtension(extensionClass);
    }

}
