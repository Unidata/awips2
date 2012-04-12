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
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.Collection;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.viz.core.data.IDataPreparer;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.geom.PixelCoordinate;
import com.vividsolutions.jts.geom.LinearRing;

/**
 * 
 * Base for any graphics target (GL, Swing, AWT, Postscript, etc.)
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date          Ticket#     Engineer    Description
 *     ------------ ----------  ----------- --------------------------
 *     7/1/06                    chammack    Initial Creation.
 *     7/19/10      #5952        bkowal      Created a new member and method that could
 *                                           track any needed updates to the Target extents.
 *                                           This functionality is primarily used by the
 *                                           Feature Following Zoom Tool at this time.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public interface IGraphicsTarget extends IImagingExtension {

    /** Defines alignment characteristics */
    public static enum HorizontalAlignment {
        CENTER, LEFT, RIGHT
    };

    public static enum VerticalAlignment {
        TOP, MIDDLE, BOTTOM
    }

    /** Defines text characteristics */
    public static enum TextStyle {
        NORMAL, BLANKED, BOXED, WORD_WRAP, DROP_SHADOW, OUTLINE
    };

    /**
     * DEFAULT should be treated as SOLID by the target, but Objects drawing on
     * the target may want to switch default to a different style before drawing
     */
    public static enum LineStyle {
        DEFAULT, SOLID, DASHED, DASHED_LARGE, DOTTED, DASH_DOTTED,
        // Task 69 : Added for NMAP
        SHORT_DASHED, // GEMPAK line type 2
        MEDIUM_DASHED, // GEMPAK line type 3
        LONG_DASH_SHORT_DASH, // GEMPAK line type 4
        LONG_DASHED, // GEMPAK line type 5
        LONG_DASH_THREE_SHORT_DASHES, // GEMPAK line type 6
        LONG_DASH_DOT, // GEMPAK line type 7
        LONG_DASH_THREE_DOTS, // GEMPAK line type 8
        MEDIUM_DASH_DOT, // GEMPAK line type 9
        DOTS
        // GEMPAK line type 10
    }

    public static enum PointStyle {
        NONE, POINT, CROSS, X, STAR, CIRCLE, DISC, BOX, SQUARE, DASH
    }

    /** Defines the raster mode */
    public static enum RasterMode {
        ASYNCHRONOUS, SYNCHRONOUS
    }

    /**
     * DEPRECATED: For general IImage construction, use
     * initializeRaster(RenderedImage). Other image construction methods should
     * be done through extensions
     * 
     * @param requester
     *            the object to use for requesting data for the image
     * @param optionalParams
     *            color map parameters for colormapped images
     * @return an image
     */
    @Deprecated
    public abstract IImage initializeRaster(IDataPreparer preparer,
            ColorMapParameters optionalParams);

    /**
     * This method will create an IImage object from a RenderedImage callback.
     * The callback is used to construct the RenderableImage when it is needed.
     * All targets need to support IImage creation for RenderedImage. Other
     * IImage construction methods should be done through extensions
     * 
     * @param imageCallback
     * @return
     */
    public abstract IImage initializeRaster(IRenderedImageCallback imageCallback);

    /**
     * Given the font, construct it with default values for the font
     * 
     * @param font
     * @return
     */
    public abstract IFont initializeFont(String font);

    /**
     * Create a font object
     * 
     * @param fontName
     *            the font name
     * @param size
     *            the size in points
     * @param styles
     *            the font styles
     * @return a prepared font reference
     */
    public abstract IFont initializeFont(String fontName, float size,
            IFont.Style[] styles);

    /**
     * Create a font object from a truetype font
     * 
     * 
     * @param fontFile
     *            the truetype font
     * @param size
     *            the size in points
     * @param styles
     *            the font styles
     * @return a prepared font reference
     */
    public abstract IFont initializeFont(File fontFile, float size,
            IFont.Style[] styles);

    /**
     * Draw a raster to a target, given an extent and an alpha (transparency)
     * value. Assumes synchronous operation.
     * 
     * This operation will block on unavailable data.
     * 
     * @param image
     *            the image reference object to draw
     * @param extent
     *            the extent of the drawable area
     * @param paintProps
     *            the paint properties
     * @return status whether the raster was able to be drawn
     * @throws VizException
     */
    public abstract boolean drawRaster(IImage image, PixelCoverage extent,
            PaintProperties paintProps) throws VizException;

    /**
     * Draw a raster to a target, given an extent and an alpha (transparency)
     * value
     * 
     * Depending on the raster mode, this operation may or may not block on
     * unavailable data.
     * 
     * In synchronous mode, it will attempt to do everything possible to draw
     * the imagery even if it means delaying the thread.
     * 
     * If in asychronous mode, it should return false back to notify that a
     * redraw should be rescheduled. Ideally, by the time the refresh occurs,
     * the raster should be available.
     * 
     * @param image
     *            the image reference object to draw
     * @param extent
     *            the extent of the drawable area
     * @param paintProps
     *            the paint properties
     * @param mode
     *            the drawing mode (synchronous, asynchronous)
     * @return status whether the raster was able to be drawn
     * @throws VizException
     */
    public abstract boolean drawRaster(IImage image, PixelCoverage extent,
            PaintProperties paintProps, RasterMode mode) throws VizException;

    /**
     * Draw the DrawableString object to the screen
     * 
     * @param parameters
     * @throws VizException
     */
    public abstract void drawStrings(DrawableString... parameters)
            throws VizException;

    /**
     * Draw multiple DrawableString objects to the screen, an implementor of
     * this interface may choose to optimize drawing of multiple strings using
     * this method to be faster than individual calls with a single
     * DrawableString.
     * 
     * @param parameters
     * @throws VizException
     */
    public abstract void drawStrings(Collection<DrawableString> parameters)
            throws VizException;

    /**
     * Get the string bounds for the parameters
     * 
     * @param parameters
     * @return
     */
    public abstract Rectangle2D getStringsBounds(DrawableString parameters);

    /**
     * Get the string bounds for the specified string using parameters as the
     * information about the string, this function ignores the String[] in
     * parameters
     * 
     * @param parameters
     * @param string
     * @return
     */
    public abstract Rectangle2D getStringsBounds(DrawableString parameters,
            String string);

    /**
     * Draw a (set of) filled shape(s)
     * 
     * HIGH PERFORMANCE OPERATION
     * 
     * @param shape
     *            the shaded shape object
     * @param alpha
     *            the alpha blending coefficient
     * @throws VizException
     */
    public abstract void drawShadedShape(IShadedShape shape, float alpha)
            throws VizException;

    /**
     * Draw a (set of) filled shape(s)
     * 
     * HIGH PERFORMANCE OPERATION
     * 
     * @param shape
     *            the shaded shape object
     * @param alpha
     *            the alpha blending coefficient
     * @param brightness
     *            the brightness blending coefficient
     * @throws VizException
     */
    public abstract void drawShadedShape(IShadedShape shape, float alpha,
            float brightness) throws VizException;

    /**
     * Draw a (set of) filled shape(s)
     * 
     * HIGH PERFORMANCE OPERATION
     * 
     * @param alpha
     *            the alpha blending coefficient
     * @param brightness
     *            the brightness blending coefficient
     * @param shapes
     *            the shaded shapes object
     * @throws VizException
     */
    public abstract void drawShadedShapes(float alpha, float brightness,
            IShadedShape... shapes) throws VizException;

    /**
     * Draw a (set of) wireframe shape(s)
     * 
     * HIGH PERFORMANCE OPERATION
     * 
     * @param shape
     *            the wireframe shape object
     * @param color
     *            the color of the shape
     * @param lineWidth
     *            the line width
     * @throws VizException
     */
    public abstract void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth) throws VizException;

    /**
     * Draw a (set of) wireframe shape(s)
     * 
     * HIGH PERFORMANCE OPERATION
     * 
     * @param shape
     *            the wireframe shape object
     * @param color
     *            the color of the shape
     * @param lineWidth
     *            the line width
     * @param lineStyle
     *            style of the line
     * @throws VizException
     */
    public abstract void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle) throws VizException;

    /**
     * Draw a (set of) wireframe shape(s)
     * 
     * HIGH PERFORMANCE OPERATION
     * 
     * @param shape
     *            the wireframe shape object
     * @param color
     *            the color of the shape
     * @param lineWidth
     *            the line width
     * @param lineStyle
     *            style of the line
     * @param alpha
     *            the alpha of the object being drawn
     * @throws VizException
     */
    public abstract void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, float alpha)
            throws VizException;

    /**
     * Draw a (set of) wireframe shape(s)
     * 
     * HIGH PERFORMANCE OPERATION
     * 
     * @param shape
     *            the wireframe shape object
     * @param color
     *            the color of the shape
     * @param lineWidth
     *            the line width
     * @param lineStyle
     *            style of the line
     * @param font
     *            the font to be used for labeling
     * @throws VizException
     */
    public abstract void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, IFont font)
            throws VizException;

    /**
     * Draw a (set of) wireframe shape(s)
     * 
     * HIGH PERFORMANCE OPERATION
     * 
     * @param shape
     *            the wireframe shape object
     * @param color
     *            the color of the shape
     * @param lineWidth
     *            the line width
     * @param lineStyle
     *            style of the line
     * @param font
     *            the font to be used for labeling
     * @param alpha
     *            the alpha of the object being drawn
     * @throws VizException
     */
    public abstract void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, IFont font, float alpha)
            throws VizException;

    /**
     * Draw a rectangle (This is not a high performance operation. Use
     * drawShadedShape or drawWireframeShape.)
     * 
     * @param rect
     *            the rectangle to draw
     * @throws VizException
     */
    public abstract void drawRect(IExtent pe, RGB color, float lineWidth,
            double alpha) throws VizException;

    /**
     * Draw a shaded rectangle
     * 
     * @param pe
     *            the area of the rectangle
     * @param color
     *            the color of the rectangle
     * @param alpha
     *            the alpha blending factor
     * @param pattern
     *            the fill pattern or null for solid fill
     * @throws VizException
     */
    public abstract void drawShadedRect(IExtent pe, RGB color, double alpha,
            byte[] pattern) throws VizException;

    /**
     * Draw a shaded polygon, not high performance, for singularly used and
     * rapidly changing shapes
     * 
     * @param poly
     *            the polygon to be drawn in world coordinates
     * @param color
     *            the color to fill with
     * @param alpha
     *            the alpha blending factor
     * @param pattern
     *            the fill pattern or null for solid fill
     */
    public abstract void drawShadedPolygon(LinearRing poly, RGB color,
            double alpha, byte[] pattern) throws VizException;

    /**
     * Draws a cicle with parameters about the circle
     * 
     * @param circle
     */
    public abstract void drawCircle(DrawableCircle... circles)
            throws VizException;

    /**
     * Draws a line with the parameters for the line
     * 
     * @param lines
     * @throws VizException
     */
    public abstract void drawLine(DrawableLine... lines) throws VizException;

    /**
     * 
     * @param x1
     *            x location of arc start.
     * @param y1
     *            y location of arc start.
     * @param z1
     *            z location of arc start
     * @param radius
     *            radius (if was a complete circle.
     * @param color
     *            color of arc
     * @param width
     *            width of arc
     * @param startAzimuth
     *            angle at where to draw the arc.
     * @param endAzimuth
     *            angle at where to end arc.
     * @throws VizException
     */
    public void drawArc(double x1, double y1, double z1, double radius,
            RGB color, float width, int startAzimuth, int arcWidth,
            LineStyle lineStyle, boolean includeSides) throws VizException;

    /**
     * Returns a point on a circle given the origin (x1,y1) radius and angle
     * (polar coordinates )
     * 
     * @param x1
     * @param y1
     * @param z1
     * @param radius
     * @param angle
     * @return double[x1,y1]
     * @throws VizException
     */
    public abstract double[] getPointOnCircle(double x1, double y1, double z1,
            double radius, double angle) throws VizException;

    /**
     * Create a wireframe shape object
     * 
     * @param mutable
     *            whether the shape changes after creation
     * @param descriptor
     *            the map descriptor
     * @return a wireframe shape object
     */
    public abstract IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor);

    /**
     * Create a wireframe shape object
     * 
     * @param mutable
     *            whether the shape changes after creation
     * @param descriptor
     *            the map descriptor
     * @param simplificationLevel
     *            the simplification level
     * @return a wireframe shape object
     */
    public abstract IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor, float simplificationLevel);

    /**
     * Create a wireframe shape object with options
     * 
     * @param mutable
     *            whether the shape changes after creation
     * @param descriptor
     *            the map descriptor
     * @param simplificationLevel
     *            the simplification level
     * @param spatialChopFlag
     *            whether to enable spatial chopping
     * @param extent
     *            the extent over which the features exist (enables spatial
     *            chopping)
     * @return
     */
    public abstract IWireframeShape createWireframeShape(boolean mutable,
            IDescriptor descriptor, float simplificationLevel,
            boolean spatialChopFlag, IExtent extent);

    public abstract IWireframeShape createWireframeShape(boolean mutableFlag,
            GeneralGridGeometry geom);

    public abstract IWireframeShape createWireframeShape(boolean mutable,
            GeneralGridGeometry geom, float simplificationLevel,
            boolean spatialChopFlag, IExtent extent);

    /**
     * Create a shaded shape object
     * 
     * @param mutable
     *            whether the shape changes after creation
     * @param descriptor
     *            the map descriptor
     * @param tesselate
     *            whether a shape requires tesselation to be convex
     * @return a shaded shape object
     */
    public abstract IShadedShape createShadedShape(boolean mutable,
            IDescriptor descriptor, boolean tesselate);

    /**
     * Initialization
     * 
     */
    public abstract void init();

    /**
     * Start a frame with a given extent. Must call endFrame after drawing is
     * complete.
     * 
     * @param display
     *            the display area that the frame covers
     * @param isClearBackground
     *            whether background should be cleared prior to drawing
     */
    public abstract void beginFrame(IRenderableDisplay display,
            boolean isClearBackground);

    /**
     * End a frame
     * 
     */
    public abstract void endFrame();

    /**
     * Trigger a resize (using the canvas)
     * 
     */
    public abstract void resize();

    /**
     * Dispose of allocated resources. Must call init() after calling dispose()
     * to begin using target again.
     * 
     */
    public abstract void dispose();

    /**
     * Take a screenshot of the current extent
     * 
     * @return a buffered image containing the current extent
     */
    public abstract BufferedImage screenshot();

    /**
     * Set up a clipping plane
     * 
     * This should be done before drawing.
     * 
     * @param extent
     *            clipping plane bounds
     */
    public abstract void setupClippingPlane(IExtent extent);

    /**
     * Clears the clipping plane
     * 
     * 
     */
    public abstract void clearClippingPlane();

    /**
     * Notifies the target that a refresh is needed
     * 
     * @param needsRefresh
     *            true if a refresh is needed
     */
    public abstract void setNeedsRefresh(boolean needsRefresh);

    /**
     * Return true if a refresh is currently needed
     * 
     * @return true if a refresh is needed
     */
    public abstract boolean isNeedsRefresh();

    /**
     * Sets the background color of the panes.
     * 
     * @param backgroundColor
     *            the background color to set
     */
    public abstract void setBackgroundColor(RGB backgroundColor);

    /**
     * DEPRECATED: This method has no effect. IGraphicsTargets are not
     * responsible to drawing a colorbar. Use method drawColorRamp to draw a
     * color ramp
     * 
     * Sets whether to display a builtin colorbar when displaying colormapped
     * images (Defaults to true)
     * 
     * @param isColorbarDisplayed
     *            boolean flag indicating whether to display the built in
     *            colorbar
     */
    @Deprecated
    public abstract void setUseBuiltinColorbar(boolean isColorbarDisplayed);

    /**
     * Draw the drawable colormap to the screen
     * 
     * @param colorMap
     *            the colorMap to draw
     */
    public abstract void drawColorRamp(DrawableColorMap colorMap)
            throws VizException;

    /**
     * Return the default font object
     * 
     * @return the default font object
     */
    public abstract IFont getDefaultFont();

    /**
     * Draw a cylinder.
     * 
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
     */
    public void drawCylinder(PixelCoordinate coord, RGB color, float alpha,
            double height, double baseRadius, double topRadius, int sideCount,
            int sliceCount, double rotation, double lean);

    /**
     * DEPRECATED: Should not be used for anything
     * 
     * @return
     */
    @Deprecated
    public String getViewType();

    /**
     * Draw a point
     * 
     * @param x
     * @param y
     * @param z
     * @param color
     * @param pointStyle
     * @throws VizException
     */
    public void drawPoint(double x, double y, double z, RGB color,
            PointStyle pointStyle) throws VizException;

    /**
     * Draw multiple points to the screen, an implementor of this interface may
     * choose to optimize drawing of multiple points using this method to be
     * faster calling drawPoint multiple times
     * 
     * @param locations
     *            each element in location should be either a double[2] {x,y} or
     *            a double[3] {x,y,z}
     * @param color
     * @param pointStyle
     * @param magnification
     * @throws VizException
     */
    public void drawPoints(Collection<double[]> locations, RGB color,
            PointStyle pointStyle, float magnification) throws VizException;

    /**
     * Draw a point
     * 
     * @param x
     * @param y
     * @param z
     * @param color
     * @param pointStyle
     * @param magnification
     * @throws VizException
     */
    public void drawPoint(double x, double y, double z, RGB color,
            PointStyle pointStyle, float magnification) throws VizException;

    /**
     * Gets the targets current view
     * 
     * @return
     */
    public IView getView();

    /**
     * Notify the Graphics Target that there are updated extents that need to be
     * set.
     * 
     * @param updatedExtent
     */
    public void updateExtent(IExtent updatedExtent);

    /**
     * Use getExtension(IOffscreenRenderingExtension.class).renderOffscreen(
     * offscreenImage) instead;
     */
    @Deprecated
    public void renderOffscreen(IImage offscreenImage) throws VizException;

    /**
     * Use getExtension(IOffscreenRenderingExtension.class).renderOnscreen()
     * instead;
     */
    @Deprecated
    public void renderOnscreen() throws VizException;

    /**
     * Use drawStrings(DrawableString parameters)
     */
    @Deprecated
    public abstract void drawString(IFont font, String text, double x,
            double y, double z, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment, Double rotation)
            throws VizException;

    /**
     * Use drawStrings(DrawableString parameters)
     */
    @Deprecated
    public abstract void drawString(IFont font, String text, double x,
            double y, double z, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment, Double rotation)
            throws VizException;

    /**
     * Use drawStrings(DrawableString parameters)
     */
    @Deprecated
    public abstract void drawStrings(IFont font, String[] text, double x,
            double y, double z, TextStyle textStyle, RGB[] colors,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment) throws VizException;

    /**
     * Use drawStrings(DrawableString parameters)
     */
    @Deprecated
    public void drawString(IFont font, String string, double xPos, double yPos,
            double zPos, TextStyle textStyle, RGB color,
            HorizontalAlignment horizontalAlignment,
            VerticalAlignment verticalAlignment, Double rotation, float alpha,
            double magnification) throws VizException;

    /**
     * Use getStringsBounds(DrawableStrings...) functions
     * 
     * Determines the bounds of a string in pixels
     * 
     * @param font
     *            the font to use (optional: can be null, will use default Font)
     * @param text
     *            the text to measure
     * @return the bounds of the text string in pixels
     */
    @Deprecated
    public abstract Rectangle2D getStringBounds(IFont font, String text);

    /**
     * Use getStringsBounds(DrawableStrings...) functions
     * 
     * Determines the bounds of a string array in pixels
     * 
     * @param font
     *            the font to use (optional: can be null, will use default Font)
     * @param text
     *            the text array to measure
     * @return the bounds of the text strings in pixels
     */
    @Deprecated
    public abstract Rectangle2D getStringBounds(IFont font, String[] text,
            TextStyle style);

    /**
     * Draw a ramp of colors using a colormap over a specified region
     * 
     * @param colorMap
     *            the colormap to apply
     * @param pixelExtent
     *            the pixelextent
     * @param blendAlpha
     *            the alpha to multiply the alpha values by
     */
    @Deprecated
    public abstract void drawColorRamp(IColorMap colorMap, IExtent pixelExtent,
            float blendAlpha) throws VizException;

    /**
     * Draw a ramp of colors using a colormap over a specified region
     * 
     * @param colorMap
     *            the colormap to apply
     * @param pixelExtent
     *            the pixelextent
     * @param blendAlpha
     *            the alpha to multiply the alpha values by
     * @param brightness
     *            the brightness to multiply the color values by
     * @param contrast
     *            the contrast to apply to the colorbar
     */
    @Deprecated
    public abstract void drawColorRamp(IColorMap colorMap, IExtent pixelExtent,
            float blendAlpha, float brightness, float contrast)
            throws VizException;

    /**
     * Draw a ramp of colors using a colormap over a specified region
     * 
     * @param colorMapParams
     *            the colormap to apply
     * @param pixelExtent
     *            the pixelextent
     * @param blendAlpha
     *            the alpha to multiply the alpha values by
     */
    @Deprecated
    public abstract void drawColorRamp(ColorMapParameters colorMapParams,
            IExtent pixelExtent, float blendAlpha) throws VizException;

    /**
     * Draw a ramp of colors using a colormap over a specified region
     * 
     * @param colorMap
     *            the colormap to apply
     * @param pixelExtent
     *            the pixelextent
     * @param blendAlpha
     *            the alpha to multiply the alpha values by
     * @param brightness
     *            the brightness to multiply the color values by
     * @param contrast
     *            the contrast to apply to the colorbar
     */
    @Deprecated
    public abstract void drawColorRamp(ColorMapParameters colorMapParams,
            IExtent pixelExtent, float blendAlpha, float brightness,
            float contrast) throws VizException;

    /**
     * DEPRECATED: call drawCircle(DrawableCircle...)
     * 
     * @param x1
     *            origin X
     * @param y1
     *            origin Y
     * @param z1
     *            origin z
     * @param radius
     *            radius of circle (in miles)
     * @param color
     *            color of line
     * @param width
     *            width of line
     * @throws VizException
     */
    @Deprecated
    public abstract void drawCircle(double x1, double y1, double z1,
            double radius, RGB color, float width) throws VizException;

    /**
     * DEPRECATED: call drawCircle(DrawableCircle...)
     * 
     * Draws a filled circle.
     * 
     * @param x
     * @param y
     * @param z
     * @param radius
     * @param color
     */
    @Deprecated
    public abstract void drawFilledCircle(double x, double y, double z,
            double radius, RGB color) throws VizException;

    /**
     * DEPRECATED: Call ColorMapLoader.loadColorMap(name) instead
     * 
     * Construct a colormap that can be applied to a colormapped image.
     * 
     * @param name
     *            name of the colormap to load
     * @return the colormap object
     * @throws VizException
     */
    @Deprecated
    public abstract IColorMap buildColorMap(String name) throws VizException;

    /**
     * DEPRECATED: Use drawLines(DrawableLine...)
     * 
     * Draw a line (This is not a high performance operation. Use drawWireframe
     * to draw sets of lines.)
     * 
     * @param x1
     *            first x coordinate
     * @param y1
     *            first y coordinate
     * @param z1
     *            first z coordinate
     * @param x2
     *            second x coordinate
     * @param y2
     *            second y coordinate
     * @param z2
     *            second z coordinate
     * @param color
     *            color of the line
     * @param width
     *            width of the line
     * @param lineStyle
     *            style of the line
     * @throws VizException
     */
    @Deprecated
    public abstract void drawLine(double x1, double y1, double z1, double x2,
            double y2, double z2, RGB color, float width, LineStyle lineStyle)
            throws VizException;

    /**
     * DEPRECATED: Use drawLines(DrawableLine...)
     * 
     * Draw a line (This is not a high performance operation. Use drawWireframe
     * to draw sets of lines.)
     * 
     * @param x1
     *            first x coordinate
     * @param y1
     *            first y coordinate
     * @param z1
     *            first z coordinate
     * @param x2
     *            second x coordinate
     * @param y2
     *            second y coordinate
     * @param z2
     *            second z coordinate
     * @param color
     *            color of the line
     * @param width
     *            width of the line
     * @throws VizException
     */
    @Deprecated
    public abstract void drawLine(double x1, double y1, double z1, double x2,
            double y2, double z2, RGB color, float width) throws VizException;

    /**
     * Can be used to request any extension for this target. Will throw
     * VizException if no extension available
     * 
     * @param <T>
     * @param extensionClass
     * @return
     */
    public abstract <T extends IGraphicsExtensionInterface> T getExtension(
            Class<T> extensionClass) throws VizException;

}
