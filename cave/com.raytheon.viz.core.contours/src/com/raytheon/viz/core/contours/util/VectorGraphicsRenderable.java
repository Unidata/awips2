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
package com.raytheon.viz.core.contours.util;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormapShadedShapeExtension.IColormapShadedShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.impl.PackedCoordinateSequence;

/**
 * 
 * Class for bulk rendering vectors efficiently using {@link IWireframeShape}
 * and {@link IShadedShape}. All dimensions of rendered objects can be
 * controlled using a {@link VectorGraphicsConfig}. Several additional style
 * attributes can be set within this class to control the rendering in the
 * {@link #paint(IGraphicsTarget)} method.
 * 
 * Data is rendered by using one or more of the three paint vector methods:
 * {@link #paintArrow(Coordinate, double, double)},
 * {@link #paintBarb(Coordinate, double, double)}, and
 * {@link #paintDualArrow(Coordinate, double, double)}. These methods do not
 * actually draw the data to the screen, the add the vector to the internal
 * structure that can be painted with {@link #paint(IGraphicsTarget)}.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 27, 2011           bsteffen    Initial creation
 * Aug 27, 2013  2287     randerso    Refactored to allow subclassing
 * Sep 23, 2013  2363     bsteffen    Add more configuration options.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @see VectorGraphicsConfig
 */
public class VectorGraphicsRenderable {

    protected final IWireframeShape lineShape;

    protected final IColormapShadedShape filledShape;

    protected RGB color;

    protected float lineWidth = 1.0f;

    protected LineStyle lineStyle = LineStyle.DEFAULT;

    protected RGB blankColor = null;

    protected float blankWidthRatio = 10.0f;

    protected VectorGraphicsConfig config;

    public VectorGraphicsRenderable(IDescriptor descriptor,
            IGraphicsTarget target) throws VizException {
        this(descriptor, target, new VectorGraphicsConfig());
    }

    public VectorGraphicsRenderable(IDescriptor descriptor,
            IGraphicsTarget target, VectorGraphicsConfig config)
            throws VizException {
        this.lineShape = target.createWireframeShape(true, descriptor);
        this.filledShape = target.getExtension(
                IColormapShadedShapeExtension.class).createColormapShadedShape(
                descriptor.getGridGeometry(), false);
        this.config = config;
    }

    /**
     * Set the color of the rendered vectors. This setting takes effect when
     * {@link #paint(IGraphicsTarget)} is called and applies to all vectors.
     * 
     * @param color
     *            the color.
     */
    public void setColor(RGB color) {
        this.color = color;
    }

    /**
     * Set the line width of the rendered vectors. This setting takes effect
     * when {@link #paint(IGraphicsTarget)} is called and applies to all
     * vectors.
     * 
     * @param lineWidth
     *            the line width
     */
    public void setLineWidth(float lineWidth) {
        this.lineWidth = lineWidth;
    }

    /**
     * Set the line style of the rendered vectors. This setting takes effect
     * when {@link #paint(IGraphicsTarget)} is called and applies to all
     * vectors.
     * 
     * @param lineStyle
     *            the line style
     */
    public void setLineStyle(LineStyle lineStyle) {
        this.lineStyle = lineStyle;
    }

    /**
     * Enable blanking with a reasonable width ratio of 10.
     * 
     * @param color
     *            the blanking color
     * @see #enableBlanking(RGB, float)
     */
    public void enableBlanking(RGB color) {
        enableBlanking(color, 10);
    }

    /**
     * Enable blanking. Blanking will render an area of the specified color
     * behind the barb to make it easier to see on cluttered dispaly. The size
     * of the blanked area is controlled with blankLineWidthRatio.
     * 
     * @param color
     *            the blanking color.
     * @param blankLineWidthRatio
     *            the width of the line, the actual width is this value
     *            multiplied by lineWidth so wider vectors will have wider
     *            blanked areas.
     */
    public void enableBlanking(RGB color,
            float blankLineWidthRatio) {
        this.blankColor = color;
        this.blankWidthRatio = blankLineWidthRatio;
    }

    /**
     * Turn off all blanking
     * 
     * @see #enableBlanking(RGB, float)
     */
    public void disableBlanking() {
        this.blankColor = null;
    }

    /**
     * Set the configuration for rendering vectors. The
     * {@link VectorGraphicsConfig} controls all the dimensions of the lines of
     * the rendered vectors. The configuration takes affect when
     * {@link #paintArrow(Coordinate, double, double)},
     * {@link #paintBarb(Coordinate, double, double)}, or
     * {@link #paintDualArrow(Coordinate, double, double)} is called.
     * 
     * @param config
     *            the configuration.
     */
    public void setConfig(VectorGraphicsConfig config) {
        this.config = config;
    }

    /**
     * @return the current vector configuration
     * @see #setConfig(VectorGraphicsConfig)
     */
    public VectorGraphicsConfig getConfig() {
        return config;
    }

    /**
     * Add a wind barb to be painted. The direction is indicated by the
     * orientation of the shaft and barbs are drawn to indicate speed. Wind
     * speeds are always rounded to the nearest unit of 5, for example a
     * magnitude of 18 will be rounded up to 20 and a magnitude of 17 will be
     * rounded down to 15.
     * 
     * @param plotLoc
     *            the location on the screen where the barb originates in
     *            descriptor pixel space.
     * @param magnitude
     *            the magnitude of the barb, this is translated into barbs.
     * @param direction
     *            the direction of the vector, this is used to orient the shaft.
     * @see VectorGraphicsConfig
     */
    public void paintBarb(Coordinate plotLoc, double magnitude, double direction) {
        if (magnitude < config.getCalmCircleMaximumMagnitude()) {
            paintPoint(plotLoc);
        }
        if (magnitude < config.getMinimumMagnitude()) {
            return;
        }

        /* adding 2.5 will round barbs to nearest 5 */
        int speed = (int) (magnitude + 2.5);
        double size = config.getScaledSize();
        double barb = size * config.getBarbLengthRatio();
        double add = size * config.getBarbSpacingRatio();
        // DIRECTIONS
        double uudd = -magnitude * Math.sin(direction);
        double vvff = -magnitude * Math.cos(direction);
        double dix = -uudd / magnitude;
        double djy = -vvff / magnitude;
        double barbRotationRadians = config.getBarbRotationRadians();
        double dix1 = Math.cos(barbRotationRadians) * dix
                + Math.sin(barbRotationRadians) * djy;
        double djy1 = (-1) * Math.sin(barbRotationRadians) * dix
                + Math.cos(barbRotationRadians) * djy;


        // SPEED AND COUNTERS:
        int n50 = speed / 50;
        int calcSpd = speed - 50 * n50;
        int n10 = calcSpd / 10;
        calcSpd = calcSpd - 10 * n10;
        int n5 = calcSpd / 5;
        double sx = ((n50 + n50 + n10 + n5 + 2)) * add;
        double staff = Math.max(size, sx);

        // DRAW STAFF
        double ix2 = plotLoc.x + dix * size * config.getOffsetRatio();
        double jy2 = plotLoc.y - djy * size * config.getOffsetRatio();
        double ix1 = ix2 + dix * staff;
        double jy1 = jy2 - djy * staff;
        lineShape.addLineSegment(new double[][] { { ix2, jy2 }, { ix1, jy1 } });

        // PLOT LONE HALF-BARB, IF NECESSARY
        if (n50 == 0 && n10 == 0) {
            ix2 = ix1 - dix * add;
            jy2 = jy1 + djy * add;
            ix1 = ix2 + dix1 * barb / 2.0;
            jy1 = jy2 - djy1 * barb / 2.0;
            lineShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
            return;
        }

        // PLOT FLAGS, IF NECESSARY
        for (int i = 0; i < n50; i++) {
            ix2 = ix1 + dix1 * barb;
            jy2 = jy1 - djy1 * barb;
            double ix3 = ix1 - dix * add * 2;
            double jy3 = jy1 + djy * add * 2;
            lineShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
            lineShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix3, jy3 } });
            if (config.isBarbFillFiftyTriangle()) {
                double[] triangleRaw = { ix1, jy1, ix2, jy2, ix3, jy3, ix1, jy1 };
                CoordinateSequence triangleSeq = new PackedCoordinateSequence.Double(
                        triangleRaw, 2);
                LineString triangleLS = new GeometryFactory()
                        .createLineString(triangleSeq);
                filledShape.addPolygonPixelSpace(
                        new LineString[] { triangleLS }, this);
            }
            ix1 = ix1 - dix * add * 2;
            jy1 = jy1 + djy * add * 2;
        }

        // PLOT BARB, IF NECESSARY
        for (int i = 0; i < n10; i++) {
            ix2 = ix1 + dix1 * barb;
            jy2 = jy1 - djy1 * barb;
            lineShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
            ix1 = ix1 - dix * add;
            jy1 = jy1 + djy * add;
        }

        // PLOT HALF-BARB, IF NECESSARY
        if (n5 != 0) {
            ix2 = ix1 + dix1 * barb / 2.0;
            jy2 = jy1 - djy1 * barb / 2.0;
            lineShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
        }
    }

    protected void paintPoint(Coordinate plotLoc) {

        double[][] line = new double[9][2];

        double aa = config.getScaledSize() * config.getCalmCircleSizeRatio();
        double saa = aa * 0.7071067811865475;

        line[8][0] = line[0][0] = plotLoc.x + aa;
        line[8][1] = line[0][1] = plotLoc.y;
        line[1][0] = plotLoc.x + saa;
        line[1][1] = plotLoc.y + saa;

        line[2][0] = plotLoc.x;
        line[2][1] = plotLoc.y + aa;

        line[3][0] = plotLoc.x - saa;
        line[3][1] = plotLoc.y + saa;

        line[4][0] = plotLoc.x - aa;
        line[4][1] = plotLoc.y;

        line[5][0] = plotLoc.x - saa;
        line[5][1] = plotLoc.y - saa;

        line[6][0] = plotLoc.x;
        line[6][1] = plotLoc.y - aa;

        line[7][0] = plotLoc.x + saa;
        line[7][1] = plotLoc.y - saa;

        lineShape.addLineSegment(line);
    }

    /**
     * Add a dual arrow to be painted. This is an arrow that has half a head on
     * one end and the other half on the other end. It is used for deformation.
     * 
     * @param plotLoc
     *            the location on the screen where the arrow originates in
     *            descriptor pixel space.
     * @param magnitude
     *            the magnitude of the arrow, this is translated into the length
     *            of the arrow
     * @param direction
     *            the direction of the vector, this is used to orient the staff.
     * @see VectorGraphicsConfig
     */
    public void paintDualArrow(Coordinate plotLoc, double magnitude,
            double direction) {
        if (magnitude < config.getMinimumMagnitude()) {
            return;
        }
        double size = config.getScaledSize();
        double staff = config.getArrowScaler().scale(magnitude)
                * config.getSizeScaler();
        double barb = size * config.getArrowHeadSizeRatio()
                + staff
                * config.getArrowHeadStaffRatio();

        // DIRECTIONS
        double uudd = -magnitude * Math.sin(direction);
        double vvff = -magnitude * Math.cos(direction);
        double dix = uudd / magnitude;
        double djy = vvff / magnitude;
        double dix1 = -dix - djy;
        double djy1 = dix - djy;

        // DRAW BODY OF ARROW
        double ix2 = plotLoc.x + dix * size * config.getOffsetRatio();
        double jy2 = plotLoc.y - djy * size * config.getOffsetRatio();
        double ix1 = ix2 + dix * staff;
        double jy1 = jy2 - djy * staff;

        double ix3 = ix1 + dix1 * barb;
        double jy3 = jy1 - djy1 * barb;
        double ix4 = ix2 - dix1 * barb;
        double jy4 = jy2 + djy1 * barb;
        lineShape.addLineSegment(new double[][] { { ix4, jy4 }, { ix2, jy2 },
                { ix1, jy1 }, { ix3, jy3 } });

    }

    /**
     * Add an arrow to be painted.
     * 
     * @param plotLoc
     *            the location on the screen where the arrow originates in
     *            descriptor pixel space.
     * @param magnitude
     *            the magnitude of the arrow, this is translated into the length
     *            of the arrow
     * @param direction
     *            the direction of the vector, this is used to orient the staff.
     * @see VectorGraphicsConfig
     */
    public void paintArrow(Coordinate plotLoc, double magnitude,
            double direction) {
        if (magnitude < config.getCalmCircleMaximumMagnitude()) {
            paintPoint(plotLoc);
        }
        if (magnitude < config.getMinimumMagnitude()) {
            return;
        }
        double size = config.getScaledSize();
        double staff = config.getArrowScaler().scale(magnitude)
                * config.getSizeScaler();
        double barb = size * config.getArrowHeadSizeRatio() + staff
                * config.getArrowHeadStaffRatio();

        // DIRECTIONS
        double uudd = -magnitude * Math.sin(direction);
        double vvff = -magnitude * Math.cos(direction);
        double dix = uudd / magnitude;
        double djy = vvff / magnitude;
        double dix1 = -dix - djy;
        double djy1 = dix - djy;
        double dix2 = -dix + djy;
        double djy2 = -dix - djy;

        // DRAW BODY OF ARROW
        double ix2 = plotLoc.x;
        double jy2 = plotLoc.y;
        double ix1 = ix2 + dix * staff;
        double jy1 = jy2 - djy * staff;
        lineShape.addLineSegment(new double[][] { { ix2, jy2 }, { ix1, jy1 } });
        // DRAW HEAD OF ARROW.
        ix2 = ix1 + dix1 * barb;
        jy2 = jy1 - djy1 * barb;
        double ix3 = ix1 + dix2 * barb;
        double jy3 = jy1 - djy2 * barb;
        lineShape.addLineSegment(new double[][] { { ix2, jy2 }, { ix1, jy1 },
                { ix3, jy3 } });
    }

    /**
     * Draw all the vectors to the screen.
     * 
     * @param target
     *            a graphics target, this must be the same type of target that
     *            was available during construction.
     * 
     * @throws VizException
     *             occurs when the target fails to render some shapes.
     */
    public void paint(IGraphicsTarget target) throws VizException {
        if (blankColor != null) {
            target.drawWireframeShape(lineShape, blankColor, lineWidth
                    * blankWidthRatio,
                    LineStyle.SOLID);
        }
        target.drawWireframeShape(lineShape, color, lineWidth, lineStyle);
        if (filledShape.isDrawable()) {
            Map<Object, RGB> colorMap = new HashMap<Object, RGB>();
            colorMap.put(this, color);
            target.getExtension(IColormapShadedShapeExtension.class)
                    .drawColormapShadedShape(filledShape, colorMap, 1.0f, 1.0f);
        }
    }

    /**
     * Clean up internal graphics resources. This must be called to avoid
     * leaking memory and/or graohics resources.
     */
    public void dispose() {
        lineShape.dispose();
        filledShape.dispose();
    }

}
