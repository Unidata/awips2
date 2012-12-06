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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.viz.core.data.IDataPreparer;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;

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
        DEFAULT, //
        SOLID, //
        DASHED(4, (short) 0xAAAA), //
        DASHED_LARGE(4, (short) 0xEEEE), //
        DOTTED(1, (short) 0xAAAA), //
        DASH_DOTTED(1, (short) 0xE4E4),
        // Task 69 : Added for NMAP
        SHORT_DASHED(8, (short) 0xAAAA), // GEMPAK line type 2
        MEDIUM_DASHED(3, (short) 0xF8F8), // GEMPAK line type 3
        LONG_DASH_SHORT_DASH(3, (short) 0xFC38), // GEMPAK line type 4
        LONG_DASHED(3, (short) 0xFCFC), // GEMPAK line type 5
        LONG_DASH_THREE_SHORT_DASHES(3, (short) 0xFDB6), // GEMPAK line type 6
        LONG_DASH_DOT(2, (short) 0xFFE4), // GEMPAK line type 7
        LONG_DASH_THREE_DOTS(2, (short) 0xFC92), // GEMPAK line type 8
        MEDIUM_DASH_DOT(2, (short) 0xFF88), // GEMPAK line type 9
        DOTS(1, (short) 0x8888); // GEMPAK line type 10

        private final int factor;

        private final short pattern;

        LineStyle() {
            this.factor = 0;
            this.pattern = 0;
        }

        LineStyle(int factor, short pattern) {
            if (factor < 1) {
                this.factor = 1;
            } else if (factor > 255) {
                this.factor = 255;
            } else {
                this.factor = factor;
            }
            this.pattern = pattern;
        }

        public int getFactor() {
            return factor;
        }

        public short getPattern() {
            return pattern;
        }

        public int[] getSWTLineStyle() {
            if (this.factor == 0 || this.pattern == 0) {
                return null;
            } else {
                List<Integer> dashPattern = new ArrayList<Integer>();

                int p = this.pattern & 0xFFFF;

                // strip trailing 0s
                int prevLsb = p & 1;
                while (prevLsb == 0) {
                    p >>= 1;
                    prevLsb = p & 1;
                }

                int count = 0;
                int sum = 0;
                while (p != 0) {
                    int lsb = p & 1;
                    if (lsb != prevLsb) {
                        dashPattern.add(count * factor);
                        sum += count;
                        count = 0;
                        prevLsb = lsb;
                    }
                    count++;
                    p >>= 1;
                }
                if (prevLsb == 1 & count > 0) {
                    dashPattern.add(count * factor);
                    sum += count;
                }
                if (sum < 16) {
                    dashPattern.add((16 - sum) * factor);
                }

                int[] array = new int[dashPattern.size()];
                for (int i = 0; i < dashPattern.size(); i++) {
                    array[i] = dashPattern.get(i);
                }
                return array;
            }
        }
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
     * DEPRECATED: Use {@link #drawCircle(DrawableCircle...)}
     * 
     */
    @Deprecated
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
     * Create a wireframe shape object
     * 
     * @param mutable
     *            whether the shape changes after creation
     * @param descriptor
     *            the geometry for the shape
     * @param simplificationLevel
     *            the simplification level
     * @return a wireframe shape object
     */
    public abstract IWireframeShape createWireframeShape(boolean mutable,
            GeneralGridGeometry geom, float simplificationLevel);

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
     * DEPRECATED: Use
     * {@link #createShadedShape(boolean, GeneralGridGeometry, boolean)} instead
     * 
     */
    @Deprecated
    public abstract IShadedShape createShadedShape(boolean mutable,
            IDescriptor descriptor, boolean tesselate);

    /**
     * Create a shaded shape object
     * 
     * @param mutable
     *            whether the shape changes after creation
     * @param targetGeometry
     *            the geometry the shape is made for
     * @param tesselate
     *            whether a shape requires tesselation to be convex
     * @return a shaded shape object
     */
    public abstract IShadedShape createShadedShape(boolean mutable,
            GeneralGridGeometry targetGeometry, boolean tesselate);

    /**
     * Initialization
     * 
     */
    public abstract void init();

    /**
     * Start a frame with a given extent. Must call endFrame after drawing is
     * complete.
     * 
     * @param view
     *            viewable area of the frame
     * @param isClearBackground
     *            whether background should be cleared prior to drawing
     */
    public abstract void beginFrame(IView view, boolean isClearBackground);

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
     * /** Use drawStrings(DrawableString parameters)
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
