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
package com.raytheon.uf.viz.radar.gl;

import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.Collection;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IDataPreparer;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.geom.PixelCoordinate;
import com.raytheon.viz.core.gl.IGLTarget;
import com.vividsolutions.jts.geom.LinearRing;

/**
 * IGraphicsTarget for mosaicing radar data, delegates all calls to an IGLTarget
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2010            mschenke     Initial creation
 * Jul 19, 2010 #5952      bkowal       Defined the 'updateExtent' method.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MosaicGLTarget implements IGraphicsTarget {

    private IGLTarget delegate;

    public MosaicGLTarget(IGLTarget delegateTarget) {
        this.delegate = delegateTarget;
    }

    /* Important functions!!! */
    public boolean drawRaster(IImage image, PixelCoverage extent,
            PaintProperties paintProps, RasterMode mode) throws VizException {
        return delegate.drawRaster(image, extent, paintProps, mode,
                "mosaicMaxVal");
    }

    public boolean drawRaster(IImage image, PixelCoverage extent,
            PaintProperties paintProps) throws VizException {
        return delegate.drawRaster(image, extent, paintProps, "mosaicMaxVal");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#drawRasters(com.raytheon.uf.
     * viz.core.drawables.PaintProperties,
     * com.raytheon.uf.viz.core.DrawableImage[])
     */
    @Override
    public boolean drawRasters(PaintProperties paintProps,
            DrawableImage... images) throws VizException {
        return delegate.drawRasters("mosaicMaxVal", paintProps, images);
    }

    /* Standard delegate functions */
    public void beginFrame(IRenderableDisplay display, boolean isClearBackground) {
        delegate.beginFrame(display, isClearBackground);
    }

    public IColorMap buildColorMap(String name) throws VizException {
        return delegate.buildColorMap(name);
    }

    public void clearClippingPlane() {
        delegate.clearClippingPlane();
    }

    public IShadedShape createShadedShape(boolean mutable,
            IDescriptor descriptor, boolean tesselate) {
        return delegate.createShadedShape(mutable, descriptor, tesselate);
    }

    public IWireframeShape createWireframeShape(boolean mutable,
            GeneralGridGeometry geom, float simplificationLevel,
            boolean spatialChopFlag, IExtent extent) {
        return delegate.createWireframeShape(mutable, geom,
                simplificationLevel, spatialChopFlag, extent);
    }

    public IWireframeShape createWireframeShape(boolean mutableFlag,
            GeneralGridGeometry geom) {
        return delegate.createWireframeShape(mutableFlag, geom);
    }

    public IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor, float simplificationLevel,
            boolean spatialChopFlag, IExtent extent) {
        return delegate.createWireframeShape(mutable, descriptor,
                simplificationLevel, spatialChopFlag, extent);
    }

    public IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor, float simplificationLevel) {
        return delegate.createWireframeShape(mutable, descriptor,
                simplificationLevel);
    }

    public IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor) {
        return delegate.createWireframeShape(mutable, descriptor);
    }

    public void dispose() {
        delegate.dispose();
    }

    public void drawArc(double x1, double y1, double z1, double radius,
            RGB color, float width, int startAzimuth, int arcWidth,
            LineStyle lineStyle, boolean includeSides) throws VizException {
        delegate.drawArc(x1, y1, z1, radius, color, width, startAzimuth,
                arcWidth, lineStyle, includeSides);
    }

    public void drawCircle(double x1, double y1, double z1, double radius,
            RGB color, float width) throws VizException {
        delegate.drawCircle(x1, y1, z1, radius, color, width);
    }

    public void drawColorRamp(IColorMap colorMap, IExtent pixelExtent,
            float blendAlpha) throws VizException {
        delegate.drawColorRamp(colorMap, pixelExtent, blendAlpha);
    }

    public void drawCylinder(PixelCoordinate coord, RGB color, float alpha,
            double height, double baseRadius, double topRadius, int sideCount,
            int sliceCount, double rotation, double lean) {
        delegate.drawCylinder(coord, color, alpha, height, baseRadius,
                topRadius, sideCount, sliceCount, rotation, lean);
    }

    public void drawFilledCircle(double x, double y, double z, double radius,
            RGB color) throws VizException {
        delegate.drawFilledCircle(x, y, z, radius, color);
    }

    public void drawLine(double x1, double y1, double z1, double x2, double y2,
            double z2, RGB color, float width, LineStyle lineStyle)
            throws VizException {
        delegate.drawLine(x1, y1, z1, x2, y2, z2, color, width, lineStyle);
    }

    public void drawLine(double x1, double y1, double z1, double x2, double y2,
            double z2, RGB color, float width) throws VizException {
        delegate.drawLine(x1, y1, z1, x2, y2, z2, color, width);
    }

    public void drawPoint(double x, double y, double z, RGB color,
            PointStyle pointStyle) throws VizException {
        delegate.drawPoint(x, y, z, color, pointStyle);
    }

    public void drawRect(IExtent pe, RGB color, float lineWidth, double alpha)
            throws VizException {
        delegate.drawRect(pe, color, lineWidth, alpha);
    }

    public void drawShadedPolygon(LinearRing poly, RGB color, double alpha,
            byte[] pattern) throws VizException {
        delegate.drawShadedPolygon(poly, color, alpha, pattern);
    }

    public void drawShadedRect(IExtent pe, RGB color, double alpha,
            byte[] pattern) throws VizException {
        delegate.drawShadedRect(pe, color, alpha, pattern);
    }

    public void drawShadedShape(IShadedShape shape, float alpha)
            throws VizException {
        delegate.drawShadedShape(shape, alpha);
    }

    public void drawString(IFont font, String text, double x, double y,
            double z, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment, Double rotation)
            throws VizException {
        delegate.drawString(font, text, x, y, z, textStyle, color,
                horizontalAlignment, rotation);
    }

    public void drawString(IFont font, String text, double x, double y,
            double z, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment, Double rotation)
            throws VizException {
        delegate.drawString(font, text, x, y, z, textStyle, color,
                horizontalAlignment, verticalAlignment, rotation);
    }

    public void drawStrings(IFont font, String[] text, double x, double y,
            double z, TextStyle textStyle, RGB[] colors,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment) throws VizException {
        delegate.drawStrings(font, text, x, y, z, textStyle, colors,
                horizontalAlignment, verticalAlignment);
    }

    public void drawString(IFont font, String string, double xPos, double yPos,
            double zPos, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment, Double rotation, float alpha,
            double magnification) throws VizException {
        // not implemented
    }

    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, IFont font)
            throws VizException {
        delegate.drawWireframeShape(shape, color, lineWidth, lineStyle, font);
    }

    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, IFont font, float alpha)
            throws VizException {
        delegate.drawWireframeShape(shape, color, lineWidth, lineStyle, font,
                alpha);
    }

    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle) throws VizException {
        delegate.drawWireframeShape(shape, color, lineWidth, lineStyle);
    }

    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, float alpha)
            throws VizException {
        delegate.drawWireframeShape(shape, color, lineWidth, lineStyle, alpha);
    }

    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth) throws VizException {
        delegate.drawWireframeShape(shape, color, lineWidth);
    }

    public void endFrame() {
        delegate.endFrame();
    }

    public IFont getDefaultFont() {
        return delegate.getDefaultFont();
    }

    public double[] getPointOnCircle(double x1, double y1, double z1,
            double radius, double angle) throws VizException {
        return delegate.getPointOnCircle(x1, y1, z1, radius, angle);
    }

    public Rectangle2D getStringBounds(IFont font, String text) {
        return delegate.getStringBounds(font, text);
    }

    public String getViewType() {
        return delegate.getViewType();
    }

    public void init() {
        delegate.init();
    }

    public IFont initializeFont(File fontFile, float size, Style[] styles) {
        return delegate.initializeFont(fontFile, size, styles);
    }

    public IFont initializeFont(String fontName, float size, Style[] styles) {
        return delegate.initializeFont(fontName, size, styles);
    }

    public IFont initializeFont(String font) {
        return delegate.initializeFont(font);
    }

    @Deprecated
    public IImage initializeRaster(IDataPreparer preparer,
            ColorMapParameters optionalParams) {
        return delegate.initializeRaster(preparer, optionalParams);
    }

    public IImage initializeRaster(IRenderedImageCallback imageCallback) {
        return delegate.initializeRaster(imageCallback);
    }

    public boolean isNeedsRefresh() {
        return delegate.isNeedsRefresh();
    }

    public void resize() {
        delegate.resize();
    }

    public BufferedImage screenshot() {
        return delegate.screenshot();
    }

    public void setBackgroundColor(RGB backgroundColor) {
        delegate.setBackgroundColor(backgroundColor);
    }

    public void setNeedsRefresh(boolean needsRefresh) {
        delegate.setNeedsRefresh(needsRefresh);
    }

    public void setupClippingPlane(IExtent extent) {
        delegate.setupClippingPlane(extent);
    }

    public void setUseBuiltinColorbar(boolean isColorbarDisplayed) {
        delegate.setUseBuiltinColorbar(isColorbarDisplayed);
    }

    public void stage(IImage image) throws VizException {
        delegate.stage(image);
    }

    public void updateExtent(IExtent updatedExtent) {
        /* Do Nothing */
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
        return delegate.getStringBounds(font, text, style);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#drawColorRamp(com.raytheon.uf
     * .common.colormap.IColorMap, com.raytheon.uf.viz.core.IExtent, float,
     * float, float)
     */
    @Override
    public void drawColorRamp(IColorMap colorMap, IExtent pixelExtent,
            float blendAlpha, float brightness, float contrast)
            throws VizException {
        delegate.drawColorRamp(colorMap, pixelExtent, blendAlpha, brightness,
                contrast);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#drawShadedShape(com.raytheon
     * .uf.viz.core.drawables.IShadedShape, float, float)
     */
    @Override
    public void drawShadedShape(IShadedShape shape, float alpha,
            float brightness) throws VizException {
        delegate.drawShadedShape(shape, alpha, brightness);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#drawColorRamp(com.raytheon.uf
     * .viz.core.drawables.ColorMapParameters, com.raytheon.uf.viz.core.IExtent,
     * float)
     */
    @Override
    public void drawColorRamp(ColorMapParameters colorMapParams,
            IExtent pixelExtent, float blendAlpha) throws VizException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#drawColorRamp(com.raytheon.uf
     * .viz.core.drawables.ColorMapParameters, com.raytheon.uf.viz.core.IExtent,
     * float, float, float)
     */
    @Override
    public void drawColorRamp(ColorMapParameters colorMapParams,
            IExtent pixelExtent, float blendAlpha, float brightness,
            float contrast) throws VizException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IGraphicsTarget#drawPoint(double, double,
     * double, org.eclipse.swt.graphics.RGB,
     * com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle, float)
     */
    @Override
    public void drawPoint(double x, double y, double z, RGB color,
            PointStyle pointStyle, float magnification) throws VizException {

    }

    @Override
    public void renderOffscreen(IImage offscreenImage) throws VizException {
        delegate.renderOffscreen(offscreenImage);

    }

    @Override
    public void renderOnscreen() throws VizException {
        delegate.renderOnscreen();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#getStringsBounds(com.raytheon
     * .uf.viz.core.DrawStringsParameters)
     */
    @Override
    public Rectangle2D getStringsBounds(DrawableString parameters) {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#getStringsBounds(com.raytheon
     * .uf.viz.core.DrawStringsParameters, java.lang.String)
     */
    @Override
    public Rectangle2D getStringsBounds(DrawableString parameters, String string) {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#drawStrings(com.raytheon.uf.
     * viz.core.DrawStringsParameters)
     */
    @Override
    public void drawStrings(DrawableString... parameters) throws VizException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#drawColorRamp(com.raytheon.uf
     * .viz.core.DrawableColorMap)
     */
    @Override
    public void drawColorRamp(DrawableColorMap colorMap) throws VizException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#drawCircle(com.raytheon.uf.viz
     * .core.DrawableCircle)
     */
    @Override
    public void drawCircle(DrawableCircle... circle) throws VizException {

    }

    @Override
    public void drawStrings(Collection<DrawableString> parameters)
            throws VizException {

    }

    @Override
    public void drawPoints(Collection<double[]> locations, RGB color,
            PointStyle pointStyle, float magnification) throws VizException {

    }

    @Override
    public void drawShadedShapes(float alpha, float brightness,
            IShadedShape... shapes) throws VizException {
        delegate.drawShadedShapes(alpha, brightness, shapes);
    }

    @Override
    public <T> T getExtension(Class<T> extensionClass) throws VizException {
        return delegate.getExtension(extensionClass);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IGraphicsTarget#drawLine(com.raytheon.uf.viz
     * .core.DrawableLine[])
     */
    @Override
    public void drawLine(DrawableLine... lines) throws VizException {
        delegate.drawLine(lines);
    }

}
