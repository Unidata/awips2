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
package fsl.tools.glyph;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.TexturePaint;
import java.awt.geom.AffineTransform;
import java.awt.geom.FlatteningPathIterator;
import java.awt.geom.GeneralPath;
import java.awt.geom.Line2D;
import java.awt.geom.PathIterator;
import java.util.ArrayList;
import java.util.Vector;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.viz.adapter.CoordConverter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Abstract class representing a drawable path object. The path is made up of an
 * arbitrary number of points joined by straight line segments. The path may be
 * of an arbitrary thickness, and its style may be either dashed or solid. It
 * may also be closed, and thus form a polygon.
 * 
 * @author Christopher Golden
 */
public abstract class Path extends MultiPointGlyph implements Fillable {

    // Protected Static Constants

    /**
     * Mode specifier for <code>findNearestPoint()</code> that indicates that
     * only nodes are to be checked, and the closest node is to be returned, if
     * any.
     */
    protected static final int NODES_ONLY = 0;

    /**
     * Mode specifier for <code>findNearestPoint()</code> that indicates that
     * both nodes and line segments between nodes are to be checked, and the
     * closest node is to be returned, if any.
     */
    protected static final int NODES_AND_SEGMENTS = 1;

    /**
     * Mode specifier for <code>findNearestPoint()</code> that indicates that
     * only line segments between nodes are to be checked, and the closest
     * segment is to be returned, if any.
     */
    protected static final int SEGMENTS_ONLY = 2;

    // Protected Variables

    /**
     * Flag indicating whether or not the border should be drawn if this path is
     * closed; otherwise, it is ignored.
     */
    protected boolean border = true;

    /**
     * Flag indicating whether or not the path should be closed, and thus drawn
     * as a polygon.
     */
    protected boolean closed = false;

    /**
     * Fill pattern; must be <code>SPARSE_FILL</code>,
     * <code>MEDIUM_FILL</code>, <code>
     * DENSE_FILL</code>, or
     * <code>SOLID</code>.
     */
    protected int fillPattern = SOLID_FILL;

    /**
     * Fill color. This is used to fill the polygon if the path is closed,
     * otherwise, it is ignored.
     */
    protected Color fillColor = Color.black;

    /**
     * Array of distances along the path, each holding the length along the path
     * from the beginning in in display units at which the corresponding node is
     * found. If the path is closed, an additional index will be found at the
     * end of the array that holds the distance from the beginning, all the way
     * around the path, back to the beginning. This is display-context
     * sensitive, and so is transient.
     */
    protected transient double[] distances = null;

    /**
     * Bounding rectangle of the path; this is display-context sensitive, and so
     * is transient.
     */
    protected transient Rectangle boundingRect = null;

    /**
     * Basic path shape, to be used to create both the drawing and the bounding
     * shapes. This is display-context sensitive, and so is transient.
     */
    protected transient Shape pathShape = null;

    /**
     * Bounding shape of the path, to be used for hit tests; this is
     * display-context sensitive, and so is transient.
     */
    protected transient Shape boundingShape = null;

    /**
     * Shape of the path to be used for drawing; this is display-context
     * sensitive, and therefore is transient.
     */
    protected transient Shape drawingShape = null;

    /**
     * Shape of the path to be used for filling if the path is filled and not to
     * be drawn; it includes the outline of any attached shape adornments. This
     * is display-context sensitive, and therefore is transient.
     */
    protected transient Shape fillShape = null;

    /**
     * Texture paint object to be used for filling if the path is closed and a
     * non-solid fill pattern is being used. This is display-context sensitive,
     * and so is transient.
     */
    protected transient TexturePaint patternPaint = null;

    /**
     * Last point used in an edit operation; this is only needed during edits,
     * and so is transient.
     */
    protected transient Point lastPoint = null;

    // Public Constructors

    /**
     * Create an instance with a starting color of black and standard thickness.
     * The path has zero points to start with and is open.
     */
    public Path() {
    }

    /**
     * Create an instance with the specified color, style, thickness, and is
     * open and has no points to start.
     * 
     * @param frame
     *            Frame in which this glyph exists; if <code>ALL_FRAMES</code>,
     *            the glyph exists in all frames.
     * @param color
     *            Color of the path.
     * @param style
     *            Style of the path.
     * @param thickness
     *            Width of the path.
     */
    public Path(int frame, Color color, int style, int thickness) {
        super(frame, color, style, thickness, new Vector<Coordinate>(),
                new Vector<Adornment>());
    }

    /**
     * Create an instance with the specified color, style, thickness, and
     * closure, with no points to start.
     * 
     * @param frame
     *            Frame in which this glyph exists; if <code>ALL_FRAMES</code>,
     *            the glyph exists in all frames.
     * @param border
     *            Flag indicating whether or not the border of the path should
     *            be drawn if the path is closed; if it is open, this value is
     *            ignored.
     * @param color
     *            Color of the path; this value is ignored if the path is closed
     *            and does not have a visible border.
     * @param style
     *            Style of the path; this value is ignored if the path is closed
     *            and does not have a visible border.
     * @param thickness
     *            Width of the path; this value is ignored if the path is closed
     *            and does not have a visible border.
     * @param closed
     *            Flag indicating whether or not the path is to be closed, and
     *            thus form a polygon.
     * @param fillColor
     *            Color of fill of the polygon, if the path is closed. If the
     *            path is open, this is ignored.
     * @param fillPattern
     *            Fill pattern of the polygon, if the path is closed; must be
     *            <code>
     *                    SPARSE_FILL</code>, <code>
     *                    MEDIUM_FILL</code>,
     *            <code>
     *                    DENSE_FILL</code>, or </code> SOLID_FILL</code>. If
     *            the path is open, this is ignored.
     */
    public Path(int frame, boolean border, Color color, int style,
            int thickness, boolean closed, Color fillColor, int fillPattern) {
        super(frame, color, style, thickness, new Vector<Coordinate>(),
                new Vector<Adornment>());
        this.border = border;
        this.closed = closed;
        this.fillColor = fillColor;
        this.fillPattern = fillPattern;
    }

    /**
     * Create an instance with the specified color, style, thickness, and
     * closure, and with the specified points.
     * 
     * @param frame
     *            Frame in which this glyph exists; if <code>ALL_FRAMES</code>,
     *            the glyph exists in all frames.
     * @param border
     *            Flag indicating whether or not the border of the path should
     *            be drawn if the path is closed; if it is open, this value is
     *            ignored.
     * @param color
     *            Color of the path; this value is ignored if the path is closed
     *            and does not have a visible border.
     * @param style
     *            Style of the path; this value is ignored if the path is closed
     *            and does not have a visible border.
     * @param thickness
     *            Width of the path; this value is ignored if the path is closed
     *            and does not have a visible border.
     * @param closed
     *            Flag indicating whether or not the path is to be closed, and
     *            thus form a polygon.
     * @param fillColor
     *            Color of fill of the polygon, if the path is closed. If the
     *            path is open, this is ignored.
     * @param fillPattern
     *            Fill pattern of the polygon, if the path is closed; must be
     *            <code>
     *                    SPARSE_FILL</code>, <code>
     *                    MEDIUM_FILL</code>,
     *            <code>
     *                    DENSE_FILL</code>, or </code> SOLID_FILL</code>. If
     *            the path is open, this is ignored.
     * @param points
     *            Points making up the path.
     */
    public Path(int frame, boolean border, Color color, int style,
            int thickness, boolean closed, Color fillColor, int fillPattern,
            Vector<Coordinate> points) {
        super(frame, color, style, thickness, points, new Vector<Adornment>());
        this.border = border;
        this.closed = closed;
        this.fillColor = fillColor;
        this.fillPattern = fillPattern;
    }

    // Public Methods

    /**
     * Determines whether another object is equal to this object.
     * 
     * @param obj
     *            The object to which this object is to be compared.
     * @return True if the objects are the same, false otherwise.
     */
    public boolean equals(Object obj) {

        // These are equal only if the superclass says so,
        // they are both paths, and their closure flags,
        // border flags, and fill colors and patterns are
        // all the same.
        return (super.equals(obj)
                && (obj instanceof Path)
                && (border == ((Path) obj).border)
                && (closed == ((Path) obj).closed)
                && (((fillColor == null) && (((Path) obj).fillColor == null)) || ((fillColor != null)
                        && (((Path) obj).fillColor != null) && fillColor
                        .equals(((Path) obj).fillColor))) && (fillPattern == ((Path) obj).fillPattern));
    }

    /**
     * Get the hash code of this object.
     * 
     * @return Hash code of this object.
     */
    public int hashCode() {

        // Combine the hash codes of the superclass, the
        // closure flag, border flag, and the fill color
        // and pattern.
        return (int) ((((long) super.hashCode())
                + ((long) new Boolean(border).hashCode())
                + ((long) new Boolean(closed).hashCode())
                + (fillColor != null ? (long) fillColor.hashCode() : 0L) + (long) fillPattern) % (long) Integer.MAX_VALUE);
    }

    /**
     * Create a clone of this glyph.
     */
    public Object clone() {
        Path glyph = null;
        try {
            glyph = (Path) super.clone();
            glyph.border = border;
            glyph.closed = closed;
            glyph.fillColor = fillColor;
            glyph.fillPattern = fillPattern;
            glyph.distances = null;
            glyph.pathShape = null;
            glyph.boundingRect = null;
            glyph.boundingShape = null;
            glyph.drawingShape = null;
            glyph.fillShape = null;
            glyph.lastPoint = null;
        } catch (Exception e) {
            // Logger.logBug("Clone error.", e);
            e.printStackTrace();
        }
        return glyph;
    }

    // /**
    // * Paint the glyph.
    // *
    // * @param gc Graphics context in which the
    // * path is to be drawn.
    // * @param transformer Coordinate transformer to be used
    // * to convert from the display
    // * coordinates to latitude-longitude
    // * format.
    // * @param properties Display properties to be used.
    // * @param useColor Color to be used when painting the
    // * glyph instead of whatever color(s)
    // * would usually be used; if <code>
    // * null</code>, standard painting
    // * will be done.
    // */
    // public void paint(Graphics gc, CoordConverter transformer,
    // DisplayProperties properties, Color useColor) {
    //		
    // // Make sure that the display elements have already
    // // been calculated.
    // mapToDisplayCoordinates(gc, transformer);
    //
    // // If the path is of 0 length, do nothing.
    // if (pixelPoints.size() < 2)
    // return;
    //
    // // If points are being added, draw the path as a
    // // simple thin path; otherwise, use the shapes to
    // // draw it.
    // if (editType == FINISH_CREATION) {
    //
    // // Copy the pixel coordinates into arrays.
    // int[][] coords = new int[2][pixelPoints.size()];
    // for (int i = 0; i < pixelPoints.size(); i++) {
    // Point point = (Point) pixelPoints.elementAt(i);
    // coords[0][i] = point.x;
    // coords[1][i] = point.y;
    // }
    //
    // // Draw the path as a polyline.
    // gc.setColor(properties.transform(useColor != null ? useColor : color));
    // gc.drawPolyline(coords[0], coords[1], pixelPoints.size());
    // } else {
    //
    // // Draw the shape representing this path or poly-
    // // gon, stroked appropriately to give it the
    // // correct thickness, style, etc., if the glyph
    // // should be painted before any of its adornments.
    // if (getPaintingMode() == PAINT_UNDER) {
    // if ((closed == false) || (closed && border)) {
    // gc.setColor(properties.transform(useColor != null ? useColor :
    // color));
    // ((Graphics2D) gc).draw(drawingShape);
    // ((Graphics2D) gc).fill(drawingShape);
    // }
    // if (closed && (fillColor != null) && (useColor == null)) {
    //
    // // If the fill pattern is solid, just
    // // fill it; otherwise, use the pattern
    // // paint object when filling it.
    // if (fillPattern == SOLID_FILL) {
    // /*
    // if (useColor != null)
    // gc.setColor(properties.
    // transform(new Color(useColor.getRed(),
    // useColor.getGreen(),
    // useColor.getBlue(),
    // fillColor.getAlpha())));
    // else
    // gc.setColor(properties.transform(fillColor));
    // */
    // gc.setColor(properties.transform(fillColor));
    // ((Graphics2D) gc).fill(pathShape);
    // } else {
    // Paint oldPaint = ((Graphics2D) gc).getPaint();
    // /*
    // if (useColor != null) {
    // TexturePaint usePatternPaint =
    // createPatternFillPaintObject(properties, useColor);
    // ((Graphics2D) gc).setPaint(usePatternPaint);
    // } else {
    // if (patternPaint == null)
    // createPatternFillPaintObject(properties);
    // ((Graphics2D) gc).setPaint(patternPaint);
    // }
    // */
    // createPatternFillPaintObject(properties);
    // ((Graphics2D) gc).setPaint(patternPaint);
    // ((Graphics2D) gc).fill(pathShape);
    // ((Graphics2D) gc).setPaint(oldPaint);
    // }
    // }
    // }
    //
    // // If no painting of this glyph is to be done, but
    // // it is filled, draw the fill now.
    // if ((fillShape != null) && (fillColor != null) && (useColor == null)) {
    //
    // // If the fill pattern is solid, just fill it;
    // // otherwise, use the pattern paint object
    // // when filling it.
    // if (fillPattern == SOLID_FILL) {
    // /*
    // if (useColor != null)
    // gc.setColor(properties.
    // transform(new Color(useColor.getRed(),
    // useColor.getGreen(),
    // useColor.getBlue(),
    // fillColor.getAlpha())));
    // else
    // gc.setColor(properties.transform(fillColor));
    // */
    // gc.setColor(properties.transform(fillColor));
    // ((Graphics2D) gc).fill(fillShape);
    // } else {
    // Paint oldPaint = ((Graphics2D) gc).getPaint();
    // /*
    // if (useColor != null) {
    // TexturePaint usePatternPaint =
    // createPatternFillPaintObject(properties, useColor);
    // ((Graphics2D) gc).setPaint(usePatternPaint);
    // } else {
    // if (patternPaint == null)
    // createPatternFillPaintObject(properties);
    // ((Graphics2D) gc).setPaint(patternPaint);
    // }
    // */
    // createPatternFillPaintObject(properties);
    // ((Graphics2D) gc).setPaint(patternPaint);
    // ((Graphics2D) gc).fill(fillShape);
    // ((Graphics2D) gc).setPaint(oldPaint);
    // }
    // }
    //
    // // Draw and fill the shapes representing any
    // // adornments associated with this path, if the
    // // border is to be drawn.
    // if ((closed == false) || border)
    // paintAdornments(gc, transformer, properties, useColor);
    //
    // // Draw the shape representing this path or poly-
    // // gon, stroked appropriately to give it the
    // // correct thickness, style, etc., if the glyph
    // // should be painted after its adornments.
    // if (getPaintingMode() == PAINT_OVER) {
    // if ((closed == false) || (closed && border)) {
    // gc.setColor(properties.transform(useColor != null ? useColor :
    // color));
    // ((Graphics2D) gc).draw(drawingShape);
    // ((Graphics2D) gc).fill(drawingShape);
    // }
    // if (closed && (fillColor != null) && (useColor == null)) {
    //
    // // If the fill pattern is solid, just
    // // fill it; otherwise, use the pattern
    // // paint object when filling it.
    // if (fillPattern == SOLID_FILL) {
    // /*
    // if (useColor != null)
    // gc.setColor(properties.
    // transform(new Color(useColor.getRed(),
    // useColor.getGreen(),
    // useColor.getBlue(),
    // fillColor.getAlpha())));
    // else
    // gc.setColor(properties.transform(fillColor));
    // */
    // gc.setColor(properties.transform(fillColor));
    // ((Graphics2D) gc).fill(pathShape);
    // } else {
    // Paint oldPaint = ((Graphics2D) gc).getPaint();
    // /*
    // if (useColor != null) {
    // TexturePaint usePatternPaint =
    // createPatternFillPaintObject(properties, useColor);
    // ((Graphics2D) gc).setPaint(usePatternPaint);
    // } else {
    // if (patternPaint == null)
    // createPatternFillPaintObject(properties);
    // ((Graphics2D) gc).setPaint(patternPaint);
    // }
    // */
    // createPatternFillPaintObject(properties);
    // ((Graphics2D) gc).setPaint(patternPaint);
    // ((Graphics2D) gc).fill(pathShape);
    // ((Graphics2D) gc).setPaint(oldPaint);
    // }
    // }
    // }
    // }
    // }
    //
    // /**
    // * Convert the path to DGM format and place the result
    // * in the supplied byte array.
    // *
    // * @param dgm DGM array in which to place the
    // * translated path.
    // * @param gc Graphics context in which this
    // * glyph is drawn.
    // * @param transformer Coordinate transformer to be
    // * used to convert from the display
    // * coordinates to latitude-longitude
    // * format.
    // * @param cartesian Flag indicating whether or not
    // * the conversion should utilize
    // * Cartesian instead of lat-long
    // * coordinates. A 1024 by 1024
    // * coordinate space is assumed if
    // * this flag is true.
    // */
    // public void toDGM(DGMArray dgm, Graphics gc, CoordConverter transformer,
    // boolean cartesian) {
    //
    //
    // // FUTURE ENHANCEMENT: It may be desirable to
    // // approximate the path's color. For now, this
    // // is not being done.
    //
    //
    // // FUTURE ENHANCEMENT: It may be desirable to
    // // approximate the path's thickness. For now,
    // // this is not being done.
    //
    //
    // // Do nothing with the basic path itself if it is
    // // not to be drawn.
    // if (getPaintingMode() != NO_PAINTING) {
    //
    // // Set the line style.
    // dgm.setLineTexture((short) (style == DASHED_STYLE ? 0x00FF : 0xFFFF),
    // (short) (style == DASHED_STYLE ? 16 : 0));
    //			
    // // Translate the display coordinates to DGM
    // // coordinates. Add an extra segment at the
    // // end that joins the last and first nodes
    // // if the path is closed.
    // short[] xCoords = new short[points.size() + (closed ? 1 : 0)];
    // short[] yCoords = new short[points.size() + (closed ? 1 : 0)];
    // for (int i = 0; i < points.size(); i++) {
    // LatLong latLong = (LatLong) points.elementAt(i);
    // if (cartesian) {
    // Point point = transformer.convert(latLong, false);
    // xCoords[i] = (short) point.x;
    // yCoords[i] = (short) point.y;
    // } else {
    // xCoords[i] = (short) ((((double) latLong.lon) * 60.0) + 0.5);
    // yCoords[i] = (short) ((((double) latLong.lat) * -60.0) + 0.5);
    // }
    // }
    // if (closed) {
    // LatLong latLong = (LatLong) points.elementAt(0);
    // if (cartesian) {
    // Point point = transformer.convert(latLong, false);
    // xCoords[points.size()] = (short) point.x;
    // yCoords[points.size()] = (short) point.y;
    // } else {
    // xCoords[points.size()] =
    // (short) ((((double) latLong.lon) * 60.0) + 0.5);
    // yCoords[points.size()] =
    // (short) ((((double) latLong.lat) * -60.0) + 0.5);
    // }
    // }
    //			
    // // Add the path as a set of linked vectors in
    // // the DGM file.
    // dgm.addLinkedVectors((short) xCoords.length, xCoords, yCoords);
    // }
    //
    // // Add any adornments to the DGM array.
    // adornmentsToDGM(dgm, gc, transformer, cartesian);
    // }
    //
    // /**
    // * Convert the path to Extended DGM format and place
    // * the result in the supplied byte array.
    // *
    // * @param dgm Extended DGM array in which to
    // * place the translated path.
    // * @param gc Graphics context in which this
    // * glyph is drawn.
    // * @param transformer Coordinate transformer to be
    // * used to convert from the display
    // * coordinates to latitude-longitude
    // * format.
    // * @param cartesian Flag indicating whether or not
    // * the conversion should utilize
    // * Cartesian instead of lat-long
    // * coordinates. A 1024 by 1024
    // * coordinate space is assumed if
    // * this flag is true.
    // */
    // public void toExtendedDGM(DGMArray dgm, Graphics gc, CoordConverter
    // transformer,
    // boolean cartesian) {
    //
    // // Add the adornments if the path is to be drawn
    // // over them or not at all.
    // if (((closed == false) || border) && (getPaintingMode() == PAINT_OVER))
    // adornmentsToExtendedDGM(dgm, gc, transformer, cartesian);
    //
    // // Paint the path itself if it is to be drawn, but
    // // if not, see if it should be filled anyway.
    // if (getPaintingMode() != NO_PAINTING) {
    //
    // // Set the drawing color.
    // dgm.setColorEx(border ? color : new Color(0, 0, 0, 0));
    // if (closed)
    // dgm.setFillColorEx(fillColor == null ? new Color(0, 0, 0, 0) :
    // fillColor);
    //
    //
    // // FUTURE ENHANCEMENT: It may be desirable to
    // // approximate the path's thickness. For now,
    // // this is not being done.
    //
    //
    // // Set the line style.
    // dgm.setLineTexture((short) (style == DASHED_STYLE ? 0x00FF : 0xFFFF),
    // (short) (style == DASHED_STYLE ? 16 : 0));
    //			
    // // Translate the display coordinates to DGM
    // // coordinates.
    // short[] xCoords = new short[points.size()];
    // short[] yCoords = new short[points.size()];
    // for (int i = 0; i < points.size(); i++) {
    // LatLong latLong = (LatLong) points.elementAt(i);
    // if (cartesian) {
    // Point point = transformer.convert(latLong, false);
    // xCoords[i] = (short) point.x;
    // yCoords[i] = (short) point.y;
    // } else {
    // xCoords[i] = (short) ((((double) latLong.lon) * 60.0) + 0.5);
    // yCoords[i] = (short) ((((double) latLong.lat) * -60.0) + 0.5);
    // }
    // }
    //			
    // // Add the path as either a set of linked vec-
    // // tors or a polygon, depending upon whether
    // // or not the path is closed, in the DGM file.
    // if (closed)
    // dgm.addPolygonEx((short) xCoords.length, xCoords, yCoords);
    // else
    // dgm.addLinkedVectors((short) xCoords.length, xCoords, yCoords);
    // } else if ((fillShape != null) && (fillColor != null)) {
    //
    // // Set the fill color.
    // dgm.setFillColorEx(fillColor);
    //			
    // // Get a flattened path describing the shape,
    // // and convert each point along the path to
    // // lat-longs.
    // FlatteningPathIterator iterator =
    // new FlatteningPathIterator(fillShape.getPathIterator(null), 2.0, 4);
    // LatLong currentPoint = null;
    // Point thisPoint = null;
    // float[] coords = new float[6];
    // ArrayList points = new ArrayList();
    // while (iterator.isDone() == false) {
    // switch (iterator.currentSegment(coords)) {
    // case PathIterator.SEG_MOVETO:
    // case PathIterator.SEG_LINETO:
    // thisPoint = new Point((int) (coords[0] + 0.5f),
    // (int) (coords[1] + 0.5f));
    // currentPoint = transformer.convert(thisPoint);
    // points.add(currentPoint);
    // break;
    // case PathIterator.SEG_CLOSE:
    // break;
    // }
    // iterator.next();
    // }
    //
    // // Translate the lat-long coordinates to DGM
    // // coordinates.
    // short[] xCoords = new short[points.size()];
    // short[] yCoords = new short[points.size()];
    // for (int i = 0; i < points.size(); i++) {
    // LatLong latLong = (LatLong) points.get(i);
    // if (cartesian) {
    // Point point = transformer.convert(latLong, false);
    // xCoords[i] = (short) point.x;
    // yCoords[i] = (short) point.y;
    // } else {
    // xCoords[i] = (short) ((((double) latLong.lon) * 60.0) + 0.5);
    // yCoords[i] = (short) ((((double) latLong.lat) * -60.0) + 0.5);
    // }
    // }
    //			
    // // Add the coordinates as a polygon in the
    // // DGM file.
    // dgm.addPolygonEx((short) xCoords.length, xCoords, yCoords);
    // }
    //
    // // Add the adornments if the path is to be drawn
    // // under them.
    // if (((closed == false) || border) &&
    // (getPaintingMode() != PAINT_OVER))
    // adornmentsToExtendedDGM(dgm, gc, transformer, cartesian);
    // }

    /*
     * (non-Javadoc)
     * 
     * @see fsl.tools.glyph.GlyphImpl#prepareShape(com.raytheon.viz.core.drawables.IWireframeShape,
     *      com.raytheon.viz.core.drawables.IShadedShape,
     *      com.raytheon.viz.adapter.CoordConverter)
     */
    @Override
    public void prepareShape(IWireframeShape ws, IShadedShape ss,
            CoordConverter transformer) {
        // Add the adornments if the path is to be drawn
        // over them or not at all.
        if (((closed == false) || border) && (getPaintingMode() == PAINT_OVER))
            adornmentsToShape(ws, ss, transformer);

        // Paint the path itself if it is to be drawn, but
        // if not, see if it should be filled anyway.
        if (getPaintingMode() != NO_PAINTING) {

            // Set the drawing color.
            // dgm.setColorEx(border ? color : new Color(0, 0, 0, 0));
            // if (closed)
            // dgm.setFillColorEx(fillColor == null ? new Color(0, 0, 0, 0) :
            // fillColor);

            // FUTURE ENHANCEMENT: It may be desirable to
            // approximate the path's thickness. For now,
            // this is not being done.

            // Set the line style.
            // dgm.setLineTexture((short) (style == DASHED_STYLE ? 0x00FF :
            // 0xFFFF),
            // (short) (style == DASHED_STYLE ? 16 : 0));

            // Translate the display coordinates to DGM
            // coordinates.
            Coordinate[] coords = new Coordinate[points.size()];
            for (int i = 0; i < points.size(); i++) {
                Coordinate c = points.elementAt(i);
                coords[i] = new Coordinate(c.x, c.y);
            }

            // Add the path as either a set of linked vec-
            // tors or a polygon, depending upon whether
            // or not the path is closed, in the DGM file.
            if (closed) {
                GeometryFactory gf = new GeometryFactory();
                LineString ls = gf.createLineString(coords);
                ss.addPolygon(new LineString[] { ls }, new RGB(color.getRed(),
                        color.getGreen(), color.getBlue()));
            } else
                ws.addLineSegment(coords);
        } else if ((fillShape != null) && (fillColor != null)) {

            // Set the fill color.
            // dgm.setFillColorEx(fillColor);

            // Get a flattened path describing the shape,
            // and convert each point along the path to
            // lat-longs.
            FlatteningPathIterator iterator = new FlatteningPathIterator(
                    fillShape.getPathIterator(null), 2.0, 4);
            Coordinate currentPoint = null;
            Point thisPoint = null;
            float[] coords = new float[6];
            ArrayList<Coordinate> points = new ArrayList<Coordinate>();
            while (iterator.isDone() == false) {
                switch (iterator.currentSegment(coords)) {
                case PathIterator.SEG_MOVETO:
                case PathIterator.SEG_LINETO:
                    thisPoint = new Point((int) (coords[0] + 0.5f),
                            (int) (coords[1] + 0.5f));
                    currentPoint = transformer.convert(thisPoint);
                    points.add(currentPoint);
                    break;
                case PathIterator.SEG_CLOSE:
                    break;
                }
                iterator.next();
            }

            // Translate the lat-long coordinates to DGM
            // coordinates.
            Coordinate[] coordArr = new Coordinate[points.size()];
            for (int i = 0; i < points.size(); i++) {
                coordArr[i] = points.get(i);

            }
            GeometryFactory gf = new GeometryFactory();
            LineString ls = gf.createLineString(coordArr);
            // Add the coordinates as a polygon in the
            // DGM file.
            ss.addPolygon(new LineString[] { ls }, new RGB(color.getRed(),
                    color.getBlue(), color.getGreen()));
        }

        // Add the adornments if the path is to be drawn
        // under them.
        if (((closed == false) || border) && (getPaintingMode() != PAINT_OVER))
            adornmentsToShape(ws, ss, transformer);

    }

    /**
     * Flush display coordinates, since they are no longer valid. This method is
     * called when something about the glyph (position, etc.) or the display
     * context in which the glyph is painted has changed.
     */
    public void flushDisplayCoordinates() {
        super.flushDisplayCoordinates();
        distances = null;
        pathShape = null;
        boundingRect = null;
        boundingShape = null;
        drawingShape = null;
        fillShape = null;
    }

    /**
     * Indicate whether or not the path contains the specified point.
     * 
     * @param point
     *            Point to check to see if the path contains it.
     * @param gc
     *            Graphics context in which this path is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return True if the path contains the point, otherwise false.
     */
    public boolean contains(Point point, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Make sure that the bounding shape has already
        // been calculated.
        mapToDisplayCoordinates(gc, transformer);

        // If the bounding shape or the adornment shapes
        // contain the point, return true; otherwise,
        // return false.
        return (boundingShape.contains(point.x, point.y) ||
        // adornmentsContain(point, gc, transformer) ||
                (closed && (fillColor != null) && (fillColor.getAlpha() != 0) && pathShape
                        .contains(point.x, point.y)) || ((fillShape != null) && fillShape
                .contains(point.x, point.y)));
    }

    /**
     * Indicate whether or not the glyph intersects with the specified
     * rectangle.
     * 
     * @param rectangle
     *            Rectangle to check to see if the glyph intersects with it.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return True if the glyph intersects with the rectangle, otherwise false.
     */
    public boolean intersects(Rectangle rectangle, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Make sure that the bounding shape has already
        // been calculated.
        mapToDisplayCoordinates(gc, transformer);

        // If the bounding shape or the adornments inter-
        // sect with the rectangle, return true; other-
        // wise, return false.
        return (boundingShape.intersects(rectangle) ||
        // adornmentsIntersect(rectangle, gc, transformer) ||
                (closed && (fillColor != null) && (fillColor.getAlpha() != 0) && pathShape
                        .intersects(rectangle)) || ((fillShape != null) && fillShape
                .intersects(rectangle)));
    }

    /**
     * Get the bounding rectangle of the path.
     * 
     * @param gc
     *            Graphics context in which this path is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Bounding rectangle of the path.
     */
    public Rectangle getBoundingRect(IGraphicsTarget gc,
            CoordConverter transformer) {

        // Make sure that the bounding rectangle has
        // already been calculated.
        mapToDisplayCoordinates(gc, transformer);

        // Inflate the bounding rectangle to adjust it
        // for path thickness.
        Rectangle adjustedForThickness = new Rectangle(boundingRect);
        if (thickness > 1)
            adjustedForThickness.grow(thickness - 1, thickness - 1);
        adjustedForThickness.grow(2, 2);

        // Return the adjusted bounding rectangle.
        return adjustedForThickness;
    }

    /**
     * Begin editing the path. The types of editing supported are
     * <code>FINISH_CREATION</code>, <code>MOVE</code>, and
     * <code>MOVE_ONLY</code>.
     * 
     * @param type
     *            Type of edit to be performed.
     * @param event
     *            Event that triggered this edit.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return True if an edit operation has been started, false otherwise.
     */
    public boolean startEdit(int type, Event event, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Do nothing unless the left mouse button was
        // released.
        // if (event.type != SWT.MouseMove && event.type != SWT.MouseUp)
        // return false;

        // The only editing operations that this class
        // supports are move and adding points to the
        // end.
        if ((type == MOVE) || (type == MOVE_ONLY)) {

            // Remember the starting point of the edit,
            // and let the superclass do the rest.
            lastPoint = new Point(event.x, event.y);
            return super.startEdit(type, event, gc, transformer);
        } else if (type == FINISH_CREATION)
            return super.startEdit(type, event, gc, transformer);
        else
            return false;
    }

    /**
     * Find out whether or not the border is visible. This flag is ignored if
     * the path is not closed.
     * 
     * @return Flag indicating whether or not the border is visible.
     */
    public boolean isBorderVisible() {
        return border;
    }

    /**
     * Set the state of the border's visibility. Note that this flag is ignored
     * if the path is not closed.
     * 
     * @param visible
     *            Flag indicating whether or not the border is to be visible.
     */
    public void setBorderVisible(boolean visible) {
        this.border = visible;
        flushDisplayCoordinates();
    }

    /**
     * Find out whether or not the path is closed.
     * 
     * @return True if the path is closed and thus forms a polygon, false
     *         otherwise.
     */
    public boolean isClosed() {
        return closed;
    }

    /**
     * Set the value of the closure flag.
     * 
     * @param closed
     *            Flag indicating whether or not the path is to be closed, and
     *            thus form a polygon.
     */
    public void setClosed(boolean closed) {
        this.closed = closed;
        flushDisplayCoordinates();
    }

    /**
     * Get the fill color.
     * 
     * @return Fill color.
     */
    public Color getFillColor() {
        return fillColor;
    }

    /**
     * Set the fill color to the specified color.
     * 
     * @param fillColor
     *            New fill color of the glyph.
     */
    public void setFillColor(Color fillColor) {
        this.fillColor = fillColor;
        patternPaint = null;
    }

    /**
     * Get the fill pattern.
     * 
     * @return Fill pattern; one of <code>
     *         SPARSE_FILL</code>, <code>
     *         MEDIUM_FILL</code>,
     *         <code>
     *         DENSE_FILL</code>, or </code> SOLID_FILL</code>.
     */
    public int getFillPattern() {
        return fillPattern;
    }

    /**
     * Set the fill pattern to the specified pattern.
     * 
     * @param fillPattern
     *            New fill pattern; one of <code>SPARSE_FILL</code>, <code>
     *         MEDIUM_FILL</code>,
     *            <code>
     *         DENSE_FILL</code>, or </code> SOLID_FILL</code>.
     */
    public void setFillPattern(int fillPattern) {
        this.fillPattern = fillPattern;
        patternPaint = null;
    }

    /**
     * Get the number of points making up the path.
     * 
     * @return Number of points making up the path.
     */
    public int getNumPoints() {
        return points.size();
    }

    /**
     * Get the length of the glyph in display units.
     * 
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Length of the glyph in display units.
     */
    public double getLength(IGraphicsTarget gc, CoordConverter transformer) {

        // Make sure that the display coordinates are
        // current, and return the length.
        mapToDisplayCoordinates(gc, transformer);
        return (distances != null ? distances[distances.length - 1] : 0.0);
    }

    /**
     * Get the point that is the specified distance along the length of the
     * glyph.
     * 
     * @param distance
     *            Distance along the length of the glyph at which the point
     *            lies.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Point that lies at the specified distance along the length of the
     *         glyph.
     */
    public Point getPointAtDistanceAlong(double distance, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Make sure that the display coordinates are current.
        mapToDisplayCoordinates(gc, transformer);

        // Do nothing if the distance specified is longer than
        // the length of the path.
        if ((distances == null) || (distances[distances.length - 1] < distance))
            return null;

        // See if the distance given is at the beginning or
        // the end of the line; if so, just return the appro-
        // priate point.
        if (distance == 0.0)
            return new Point((Point) pixelPoints.elementAt(0));
        else if (distance == distances[distances.length - 1])
            return new Point((Point) pixelPoints
                    .elementAt(pixelPoints.size() - 1));

        // Do a binary search of the distances in order to
        // find which node is at the distance specified, or if
        // no such node exists, between which two nodes the
        // point that is the specified distance along the
        // length of the path is found.
        boolean betweenNodes = false;
        int lower = 0, upper = distances.length, index = distances.length / 2;
        while (distance != distances[index]) {
            if (distance > distances[index]) {
                if (distance < distances[index + 1]) {
                    betweenNodes = true;
                    break;
                } else
                    lower = index;
            } else
                upper = index;
            index = ((upper - lower) / 2) + lower;
        }

        // If the point is between the indexed node and the
        // next one, find it exactly; otherwise, return the
        // node itself.
        if (betweenNodes) {
            double fraction = (distance - distances[index])
                    / (distances[index + 1] - distances[index]);
            Point point1 = (Point) pixelPoints.elementAt(index);
            Point point2 = (Point) pixelPoints.elementAt(index + 1);
            return new Point(((int) (fraction * (point2.x - point1.x)))
                    + point1.x, ((int) (fraction * (point2.y - point1.y)))
                    + point1.y);
        } else
            return new Point((Point) pixelPoints.elementAt(index));
    }

    /**
     * Get the point that lies along the glyph's length at the specified
     * fraction of the glyph's total length.
     * 
     * @param fraction
     *            Fraction of the total length at which the point lies; must be
     *            between 0.0 and 1.0 inclusive.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Point that lies at the specified fraction of the total length
     *         along the glyph.
     */
    public Point getPointAtFractionAlong(double fraction, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Make sure that the display coordinates are
        // current.
        mapToDisplayCoordinates(gc, transformer);

        // Do nothing if the fraction specified is not within
        // the allowable bounds, or if the path contains no
        // nodes.
        if ((distances == null) || (fraction < 0.0) || (fraction > 1.0))
            return null;

        // Convert the fraction to a distance and find the
        // point using the distance method.
        return getPointAtDistanceAlong(distances[distances.length - 1]
                * fraction, gc, transformer);
    }

    /**
     * Get the distance along the glyph at which the specified point lies.
     * 
     * @param point
     *            Point that lies along the length of the glyph.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Distance along the length of the glyph at which the point lies.
     */
    public double getDistanceAtPointAlong(Point point, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Make sure that the display coordinates are current.
        mapToDisplayCoordinates(gc, transformer);

        // Do nothing if the distances array was not calculated.
        if (distances == null)
            return 0.0;

        // Get the point along the path itself, and the segment
        // within the path where that point occurs. If the
        // point is not close enough to the path, do no more.
        Point pointOnPath = new Point();
        int segmentIndex = findNearestPoint(point, SEGMENTS_ONLY, 2.0,
                pointOnPath);
        if (segmentIndex == -1)
            return 0.0;

        // Determine how far the point that is on the path is
        // from the first point of the segment, and add this to
        // the distance from the beginning of the path to that
        // first segment point; this yields the total distance.
        Point segmentPoint = (Point) pixelPoints.elementAt(segmentIndex);
        double totalDistance = distances[segmentIndex]
                + Math
                        .sqrt(((pointOnPath.x - segmentPoint.x) * (pointOnPath.x - segmentPoint.x))
                                + ((pointOnPath.y - segmentPoint.y) * (pointOnPath.y - segmentPoint.y)));
        return (totalDistance > distances[distances.length - 1] ? distances[distances.length] - 1
                : totalDistance);
    }

    /**
     * Get the fraction of the total length of the glyph that indicates where
     * along the glyph the specified point lies.
     * 
     * @param point
     *            Point that lies along the length of the glyph.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Fraction of the total length of the glyph that indicates where
     *         along the glyph the point lies.
     */
    public double getFractionAtPointAlong(Point point, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Get the distance along the path at the specified
        // point.
        double distance = getDistanceAtPointAlong(point, gc, transformer);
        if (distances == null)
            return 0.0;
        else
            return distance / distances[distances.length - 1];
    }

    /**
     * Get the section that is between the specified distances along the length
     * of the glyph.
     * 
     * @param startDistance
     *            Distance along the length of the glyph at which the starting
     *            point of the range lies.
     * @param endDistance
     *            Distance along the length of the glyph at which the endng
     *            point of the range lies; must be greater than
     *            <code>startDistance</code>.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Section of the glyph between the points at the specified
     *         distances along the length of the glyph.
     */
    public Point[] getSectionAtDistanceAlong(double startDistance,
            double endDistance, IGraphicsTarget gc, CoordConverter transformer) {

        // Make sure that the display coordinates are current.
        mapToDisplayCoordinates(gc, transformer);

        // Do nothing if the distances specified are longer
        // than the length of the path, or if the starting
        // distance is greater than or equal to the ending
        // distance.
        if ((distances == null)
                || (distances[distances.length - 1] < startDistance)
                || (distances[distances.length - 1] < endDistance)
                || (startDistance >= endDistance))
            return null;

        // See if the distances given are at the endpoints of
        // the path; if so, just use the entire path.
        if ((startDistance == 0.0)
                && (endDistance == distances[distances.length - 1])) {
            Point[] section = new Point[pixelPoints.size()];
            for (int j = 0; j < section.length; j++)
                section[j] = new Point((Point) pixelPoints.elementAt(j));
            return section;
        }

        // Do a binary search of the distances in order to
        // find which nodes are at the respective distances
        // specified, or if no such node exists for one of
        // the distances, then between which two nodes the
        // point that is the specified distance along the
        // length of the path is found.
        boolean[] betweenNodes = { false, false };
        int[] index = new int[2];
        for (int j = 0; j < 2; j++) {
            int lower = (j == 1 ? index[0] : 0), upper = distances.length;
            index[j] = ((upper - lower) / 2) + lower;
            double distance = (j == 0 ? startDistance : endDistance);
            while (distance != distances[index[j]]) {
                if (distance > distances[index[j]]) {
                    if (distance < distances[index[j] + 1]) {
                        betweenNodes[j] = true;
                        break;
                    } else
                        lower = index[j];
                } else
                    upper = index[j];
                index[j] = ((upper - lower) / 2) + lower;
            }
        }

        // Create an array of points and fill it with the
        // points appropriate to the section asked for by
        // the caller. Begin with the starting point; if it
        // is between the indexed node and the next one,
        // find it exactly; otherwise, just use the node.
        Point[] section = new Point[1 + (betweenNodes[1] ? 1 : 0) + index[1]
                - index[0]];
        if (betweenNodes[0]) {
            double fraction = (startDistance - distances[index[0]])
                    / (distances[index[0] + 1] - distances[index[0]]);
            Point point1 = (Point) pixelPoints.elementAt(index[0]);
            Point point2 = (Point) pixelPoints.elementAt(index[0] + 1);
            section[0] = new Point(((int) (fraction * (point2.x - point1.x)))
                    + point1.x, ((int) (fraction * (point2.y - point1.y)))
                    + point1.y);
        } else
            section[0] = new Point((Point) pixelPoints.elementAt(index[0]));

        // Iterate through the nodes between the start and
        // the end, adding each to the section.
        int j;
        for (j = 1, index[0]++; index[0] <= index[1]; j++, index[0]++)
            section[j] = new Point((Point) pixelPoints.elementAt(index[0]));

        // If the last point is between the indexed node
        // and the next one, find it exactly and add it to
        // the section path; otherwise, nothing needs to
        // be done, since the last node has already been
        // added above.
        if (betweenNodes[1]) {
            double fraction = (endDistance - distances[index[1]])
                    / (distances[index[1] + 1] - distances[index[1]]);
            Point point1 = (Point) pixelPoints.elementAt(index[1]);
            Point point2 = (Point) pixelPoints.elementAt(index[1] + 1);
            section[j] = new Point(((int) (fraction * (point2.x - point1.x)))
                    + point1.x, ((int) (fraction * (point2.y - point1.y)))
                    + point1.y);
        }

        // Return the section that was compiled.
        return section;
    }

    /**
     * Get the section that is between the points that lie along the glyph's
     * length at the specified fractions of the glyph's total length.
     * 
     * @param startFraction
     *            Fraction of the total length at which the starting point lies;
     *            must be between 0.0 and 1.0 inclusive.
     * @param endFraction
     *            Fraction of the total length at which the starting point lies;
     *            must be between 0.0 and 1.0 inclusive, and must be greater
     *            than <code>startFraction</code>.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Section of the glyph between the points at the specified
     *         fractions of the glyph's total length along the length of the
     *         glyph.
     */
    public Point[] getSectionAtFractionAlong(double startFraction,
            double endFraction, IGraphicsTarget gc, CoordConverter transformer) {

        // Make sure that the display coordinates are
        // current.
        mapToDisplayCoordinates(gc, transformer);

        // Do nothing if the fractions specified are not
        // within the allowable bounds, or if the path
        // contains no nodes.
        if ((distances == null) || (startFraction < 0.0)
                || (startFraction > 1.0) || (endFraction < 0.0)
                || (endFraction > 1.0))
            return null;

        // Convert the fractions to distances and find the
        // section using the distance method.
        return getSectionAtDistanceAlong(distances[distances.length - 1]
                * startFraction, distances[distances.length - 1] * endFraction,
                gc, transformer);
    }

    /**
     * Find the angle of the path at the point specified.
     * 
     * @param point
     *            A single point that lies along the path.
     * @param section
     *            Length of the section of the glyph, in pixels, to be used to
     *            calculate the angle. The section will have half its length on
     *            each side of the point if possible.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Angle in degrees.
     */
    public double getAngleAtPointAlong(Point point, int section,
            IGraphicsTarget gc, CoordConverter transformer) {

        // Make sure that the display coordinates are
        // current.
        mapToDisplayCoordinates(gc, transformer);

        // Get the distance that this point is along the
        // path; then get the points that are each half of
        // section length away from the point to either
        // side along the path. These points will be used
        // to calculate the angle between them.
        double startDistance, endDistance;
        double halfLength = ((double) section) / 2.0;
        double distance = getDistanceAtPointAlong(point, gc, transformer);
        if (distances[distances.length - 1] <= (double) section) {
            startDistance = 0.0;
            endDistance = distances[distances.length - 1];
        } else if (distance - halfLength <= 0.0) {
            startDistance = 0.0;
            endDistance = (double) section;
        } else if (distance + halfLength >= distances[distances.length - 1]) {
            endDistance = distances[distances.length - 1];
            startDistance = endDistance - (double) section;
        } else {
            startDistance = distance - halfLength;
            endDistance = distance + halfLength;
        }
        Point[] endPoints = getSectionAtDistanceAlong(startDistance,
                endDistance, gc, transformer);
        if (endPoints == null)
            return 0.0;
        Point point1 = endPoints[0], point2 = endPoints[endPoints.length - 1];

        // Calculate the angle between the two points.
        if (point2.x == point1.x) {
            if (point2.y == point1.y)
                return 0.0;
            else
                return (point2.y > point1.y ? 90.0 : 270.0);
        } else {
            double slope = ((double) (point2.y - point1.y))
                    / (double) (point2.x - point1.x);
            double angle = Math.toDegrees(Math.atan(slope));
            if (angle < 0.0)
                angle += 360.0;
            if ((point2.y <= point1.y) && (point2.x < point1.x))
                angle += 180.0;
            else if ((point2.y > point1.y) && (point2.x < point1.x))
                angle += 180.0;
            return angle % 360.0;
        }

        /*
         * // First check the nodes to see if the point is one // of them, or
         * very close to one of them; if so, // mark the segment before the
         * point as the one to // be used to determine the angle. int
         * segmentIndex = -1; for (int j = 0; j < pixelPoints.size() - (closed ?
         * 1 : 0); j++) { Point thisPoint = (Point) pixelPoints.elementAt(j);
         * double distance = Math.sqrt((point.y - thisPoint.y) * (point.y -
         * thisPoint.y) + (point.x - thisPoint.x) * (point.x - thisPoint.x)); if
         * (distance <= 1.0) { if (closed) segmentIndex = (j == 0 ?
         * pixelPoints.size() - 1 : j - 1); else segmentIndex = (j == 0 ? 0 : j -
         * 1); break; } } // If no segment has been marked for use yet, iter- //
         * ate through the segments between the points, // looking for one on
         * which the specified point // lies, and mark the one found as the
         * segment to // use to calculate the angle. if (segmentIndex == -1) {
         * for (int j = 1; j < pixelPoints.size(); j++) { // First, check to see
         * if the point is within // the bounding rectangle of the two adjacent //
         * points. Point point1 = (Point) pixelPoints.elementAt(j - 1); Point
         * point2 = (Point) pixelPoints.elementAt(j); if ((point.x + 2 <
         * (point1.x < point2.x ? point1.x : point2.x)) || (point.x - 2 >
         * (point1.x > point2.x ? point1.x : point2.x)) || (point.y + 2 <
         * (point1.y < point2.y ? point1.y : point2.y)) || (point.y - 2 >
         * (point1.y > point2.y ? point1.y : point2.y))) continue; // If the
         * distance between the point and this // path segment is within a
         * pixel, then use // that segment; otherwise, keep looking. if
         * (Line2D.ptSegDist(point1.x, point1.y, point2.x, point2.y, point.x,
         * point.y) <= 1.0) { segmentIndex = j - 1; break; } } } // If a segment
         * was found, calculate the angle for // that segment; otherwise return
         * 0. if (segmentIndex != -1) { Point point1 = (Point)
         * pixelPoints.elementAt(segmentIndex); Point point2 = (Point)
         * pixelPoints.elementAt(segmentIndex + 1 == pixelPoints.size() ? 0 :
         * segmentIndex + 1); if (point2.x == point1.x) { if (point2.y ==
         * point1.y) return 0.0; else return (point2.y > point1.y ? 90.0 :
         * 270.0); } else { double slope = ((double) (point2.y - point1.y)) /
         * (double) (point2.x - point1.x); double angle =
         * Math.toDegrees(Math.atan(slope)); if (angle < 0.0) angle += 360.0; if
         * ((point2.y <= point1.y) && (point2.x < point1.x)) angle += 180.0;
         * else if ((point2.y > point1.y) && (point2.x < point1.x)) angle +=
         * 180.0; return angle % 360.0; } } else return 0.0;
         * 
         */
    }

    // Protected Methods

    /**
     * Get the mode in which the glyph should be painted with respect to any
     * adornments attached to it: either <code>NO_PAINTING</code>, in which
     * case the glyph is not painted, just its adornments;
     * <code>PAINT_UNDER</code>, in which case the glyph is painted before
     * its adornments; or <code>PAINT_OVER</code>, in which case the glyph is
     * painted after its adornments.
     * 
     * @return Mode in which the glyph should be painted with respect to any
     *         adornments attached to it.
     */
    protected int getPaintingMode() {
        return PAINT_OVER;
    }

    /**
     * Calculate the pixel location of the glyph on the display based on the
     * lat-long information.
     * 
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     */
    protected void mapToDisplayCoordinates(IGraphicsTarget gc,
            CoordConverter transformer) {

        // Calculate the display coordinates if the bounding
        // rectangle does not exist (meaning that either the
        // glyph has never been mapped to display coordinates
        // before, or it has changed since the last mapping).
        if (boundingRect == null) {

            // Create the bounding rectangle and pixel points
            // vector.
            boundingRect = new Rectangle();
            pixelPoints = new Vector<Point>();

            // Iterate through the lat-long pairs, transforming
            // each in turn into a set of pixel coordinates.
            // Record each as a pixel point.
            Point point;
            Coordinate latLong;
            for (int j = 0; j < points.size(); j++) {
                latLong = (Coordinate) points.elementAt(j);
                point = transformer.convert(latLong);
                pixelPoints.addElement(point);

                // Add each point to the rectangle. The first
                // point has the rectangle adjusted to match
                // it, but after that the points are merely
                // added to the rectangle to cause it to grow
                // appropriately.
                if (j == 0) {
                    boundingRect.x = point.x;
                    boundingRect.y = point.y;
                } else
                    boundingRect.add(point);
            }

            // If the path is closed, add the first node at
            // the end again.
            if (closed && (points.size() > 0)) {
                latLong = (Coordinate) points.elementAt(0);
                point = transformer.convert(latLong);
                pixelPoints.addElement(point);
            }

            // Create the basic and bounding shapes for the
            // path.
            createPathShape();
            createBoundingShape();

            // Iterate through the path, getting the distance
            // along the length of the path of each node from
            // the starting point.
            if (pixelPoints.size() > 0) {
                distances = new double[pixelPoints.size()];
                Point point1 = null, point2 = null;
                for (int j = 0; j < pixelPoints.size(); j++) {
                    point1 = point2;
                    point2 = (Point) pixelPoints.elementAt(j);
                    if (point1 != null)
                        distances[j] += distances[j - 1]
                                + Math
                                        .sqrt(((point1.x - point2.x) * (point1.x - point2.x))
                                                + ((point1.y - point2.y) * (point1.y - point2.y)));
                    else
                        distances[j] = 0.0;
                }
            } else
                distances = null;

            // Create the drawing shape for the path.
            createVisuals(gc, transformer);

            // // Add the bounding rectangles of any adornments.
            // if (editType != FINISH_CREATION) {
            // Rectangle adornmentRect =
            // getAdornmentBoundingRectangle(gc, transformer);
            // if (adornmentRect != null)
            // boundingRect = boundingRect.union(adornmentRect);
            // }

            // Inflate the rectangle a bit to be on the safe
            // side.
            boundingRect.grow(1, 1);
        }
    }

    /**
     * Calculate the lat-long location of the glyph based on its pixel location
     * on the display.
     * 
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between display
     *            coordinates and lat-long pairs.
     */
    protected void mapToLatLongCoordinates(IGraphicsTarget gc,
            CoordConverter transformer) {

        // Convert the display coordinate to lat-long pairs
        // and create a new list of the latter.
        Coordinate latLong;
        points.clear();
        for (int j = 0; j < pixelPoints.size() - (closed ? 1 : 0); j++) {
            latLong = transformer.convert((Point) pixelPoints.elementAt(j));
            points.addElement(latLong);
        }
    }

    /**
     * Translate the display coordinates to the specified point.
     * 
     * @param point
     *            Point to which to translate.
     */
    protected void translateDisplayCoordinates(Point point) {

        // Figure out the deltas between the last point
        // and this one.
        int xDelta = point.x - lastPoint.x;
        int yDelta = point.y - lastPoint.y;

        // Remember this point in the edit.
        lastPoint = point;

        // Translate the bounding rectangle as well. A
        // new bounding rectangle must be created in case
        // the old one is being used by some other object
        // (after the other object queried this one for
        // its bounding rectangle).
        boundingRect = new Rectangle(boundingRect);
        boundingRect.translate(xDelta, yDelta);

        // Iterate through the pixel coordinates, adding
        // the specified delta to each in turn.
        for (int j = 0; j < pixelPoints.size(); j++) {
            Point thisPoint = (Point) pixelPoints.elementAt(j);
            thisPoint.x += xDelta;
            thisPoint.y += yDelta;
        }

        // Transform the various shapes by the specified
        // delta.
        AffineTransform at = new AffineTransform();
        at.translate(xDelta, yDelta);
        pathShape = at.createTransformedShape(pathShape);
        boundingShape = at.createTransformedShape(boundingShape);
        drawingShape = at.createTransformedShape(drawingShape);
        if (fillShape != null)
            fillShape = at.createTransformedShape(fillShape);

        // Translate the display coordinates of any
        // adornments.
        translateAdornments(xDelta, yDelta);
    }

    /**
     * Create the path that is used as the basis of bounding and drawing shapes.
     */
    protected void createPathShape() {

        // Create a path shape from the path and return it.
        Point element = null;
        pathShape = new GeneralPath(GeneralPath.WIND_NON_ZERO, pixelPoints
                .size());
        for (int j = 0; j < pixelPoints.size() - (closed ? 1 : 0); j++) {
            element = (Point) pixelPoints.elementAt(j);
            if (j == 0)
                ((GeneralPath) pathShape).moveTo(element.x, element.y);
            else
                ((GeneralPath) pathShape).lineTo(element.x, element.y);
        }
        if (closed && (pixelPoints.size() > 0))
            ((GeneralPath) pathShape).closePath();
    }

    /**
     * Create the bounding and drawing shapes for this path.
     */
    protected void createBoundingShape() {

        // Create the bounding shape for this path by stroking
        // the path with a solid-style pen. For thin paths,
        // the pen is of sufficient thickness to allow it to
        // contain points that are in actuality outside the
        // path, in order to make it easier for the user to
        // click on the path.
        BasicStroke stroke = new BasicStroke(thickness == 0 ? 7 : thickness + 6);
        boundingShape = stroke.createStrokedShape(pathShape);
    }

    /**
     * Create the visual elements of this glyph.
     * 
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     */
    protected void createVisuals(IGraphicsTarget gc, CoordConverter transformer) {

        // Create the drawing shape for this path by stroking
        // the path with the correct pen thickness and style.
        float[] dashArray = { (float) 6.0, (float) 6.0 };
        float dashOffset = (float) (style == DASHED_STYLE ? 9.0 : 0.0);
        BasicStroke stroke = new BasicStroke(thickness, BasicStroke.CAP_BUTT,
                BasicStroke.JOIN_ROUND, (float) 10.0,
                (style == DASHED_STYLE ? dashArray : null), dashOffset);
        drawingShape = stroke.createStrokedShape(pathShape);

        // If the glyph is closed but not to be drawn, it
        // needs a fill shape built up out of its shape adorn-
        // ments. Iterate through its adornments, adding the
        // shape of each one that has a shape to the fill
        // shape.
        if (closed && (editType == NONE) && (getPaintingMode() == NO_PAINTING)
                && (adornments.size() != 0)) {
            fillShape = new GeneralPath(GeneralPath.WIND_NON_ZERO);
            for (int j = 0; j < adornments.size(); j++) {
                Adornment adornment = (Adornment) adornments.elementAt(j);
                if (adornment instanceof ShapeAdornment)
                    ((GeneralPath) fillShape).append(
                            ((ShapeAdornment) adornment).getRawShape(gc,
                                    transformer), true);
            }
            ((GeneralPath) fillShape).closePath();
        }
    }

    // /**
    // * Create the texture paint object used if pattern
    // * filling the polygon.
    // *
    // * @param properties Display properties to be used.
    // */
    // protected void createPatternFillPaintObject(DisplayProperties properties)
    // {
    //
    // // If the texture paint object already exists,
    // // do nothing.
    // if (patternPaint != null)
    // return;
    //
    // // Create the image to be used as the pattern
    // // fill.
    // int patternSize = (fillPattern == SPARSE_FILL ? 12 :
    // (fillPattern == MEDIUM_FILL ? 6 : 3));
    // BufferedImage image = new BufferedImage(patternSize, patternSize,
    // BufferedImage.TYPE_INT_ARGB);
    //
    // // Draw the pattern on the image.
    // Graphics imageGraphics = image.getGraphics();
    // imageGraphics.setColor(new Color(0, 0, 0, 0));
    // imageGraphics.fillRect(0, 0, patternSize, patternSize);
    // imageGraphics.setColor(properties.transform(fillColor));
    // for (int j = 0; j < patternSize; j++)
    // imageGraphics.drawLine(j, patternSize - (1 + j), j, patternSize - (1 +
    // j));
    // imageGraphics.dispose();
    //
    // // Create the textured paint object.
    // patternPaint = new TexturePaint(image,
    // new Rectangle(0, 0, patternSize, patternSize));
    // }

    // /**
    // * Create the texture paint object used if pattern
    // * filling the polygon with a specified color.
    // *
    // * @param properties Display properties to be used.
    // * @param useColor Color to be used.
    // * @return Texture paint object created.
    // */
    // protected TexturePaint createPatternFillPaintObject(DisplayProperties
    // properties,
    // Color useColor) {
    //
    // // Create the image to be used as the pattern
    // // fill.
    // int patternSize = (fillPattern == SPARSE_FILL ? 12 :
    // (fillPattern == MEDIUM_FILL ? 6 : 3));
    // BufferedImage image = new BufferedImage(patternSize, patternSize,
    // BufferedImage.TYPE_INT_ARGB);
    //
    // // Draw the pattern on the image.
    // Graphics imageGraphics = image.getGraphics();
    // imageGraphics.setColor(new Color(0, 0, 0, 0));
    // imageGraphics.fillRect(0, 0, patternSize, patternSize);
    // imageGraphics.setColor(properties.transform(useColor));
    // for (int j = 0; j < patternSize; j++)
    // imageGraphics.drawLine(j, patternSize - (1 + j), j, patternSize - (1 +
    // j));
    // imageGraphics.dispose();
    //
    // // Create the textured paint object and return it.
    // return new TexturePaint(image, new Rectangle(0, 0, patternSize,
    // patternSize));
    // }

    /**
     * Find the point on the path that is closest to the specified point, within
     * the specified slop distance.
     * 
     * @param point
     *            A single point that may or may not lie on the path.
     * @param mode
     *            Mode in which to operate when looking for the nearest point.
     *            Must be either <code>NODES_ONLY</code>, <code>
     *                 NODES_AND_SEGMENTS</code>,
     *            or <code>
     *                 SEGMENTS_ONLY</code>.
     * @param slop
     *            Slop allowed when determining whether <code>point</code> is
     *            close enough to a path node or segment to make that node or
     *            segment the nearest one.
     * @param nearest
     *            Point in which the coordinates of the point along the path is
     *            stored by this method if the <code>SEGMENTS_ONLY
     *                 </code> mode
     *            is specified; otherwise, this parameter is not referenced or
     *            altered, as for other modes a node index is returned whose
     *            coordinates may be determined elsewhere.
     * @return If <code>SEGMENTS_ONLY</code> was specified, the nearest
     *         segment or -1 if no segment is close enough; otherwise, the
     *         nearest node or -1 if no node is close enough.
     */
    protected int findNearestPoint(Point point, int mode, double slop,
            Point nearest) {

        // If the mode calls for looking through nodes,
        // iterate through them; if one is within the slop
        // distance in pixels from this point, return its
        // index.
        if (mode != SEGMENTS_ONLY) {
            for (int j = 0; j < pixelPoints.size() - (closed ? 1 : 0); j++) {
                Point thisPoint = (Point) pixelPoints.elementAt(j);
                double distance = Math.sqrt((point.y - thisPoint.y)
                        * (point.y - thisPoint.y) + (point.x - thisPoint.x)
                        * (point.x - thisPoint.x));
                if (distance < slop)
                    return j;
            }
        }

        // If the mode calls for looking through segments
        // between adjacent points, iterate through them; if
        // the point is within the slop distance in pixels
        // from one of the segments, return either the index
        // of the segment (if the SEGMENTS_ONLY mode is
        // operative) or the index of the node closest to the
        // segment.
        if (mode != NODES_ONLY) {
            for (int j = 1; j < pixelPoints.size(); j++) {

                // If the distance between the point and this
                // path segment is within the slop distance in
                // pixels, then if the mode is SEGMENTS_ONLY,
                // return the point where the line comes clo-
                // sest to the segment and the segment's index,
                // or if the mode is something else, determine
                // which of the adjacent nodes is closer and
                // return that node's index; otherwise, keep
                // looking.
                Point point1 = (Point) pixelPoints.elementAt(j - 1);
                Point point2 = (Point) pixelPoints.elementAt(j);
                if (Line2D.ptSegDist(point1.x, point1.y, point2.x, point2.y,
                        point.x, point.y) <= slop) {
                    if (mode == SEGMENTS_ONLY) {

                        // Figure out what point to return
                        // as coordinates of the closest point
                        // to the segment. If the segment is
                        // vertical or horizontal, these are
                        // simple to calcluate; otherwise, the
                        // coordinates require the calculation
                        // first of the segment's slope and
                        // offset (m and b from y = mx + b),
                        // then the slope and offset of the
                        // line running through the original
                        // point perpendicularly to the seg-
                        // ment, and finally the finding of
                        // an intersection between these two
                        // lines.
                        if (point2.x == point1.x) {
                            nearest.x = point1.x;
                            nearest.y = point.y;

                            // Make sure that the y coordinate
                            // is within the bounds of the
                            // segment's y coordinates.
                            if (nearest.y < (point1.y < point2.y ? point1.y
                                    : point2.y))
                                nearest.y = (point1.y < point2.y ? point1.y
                                        : point2.y);
                            if (nearest.y > (point1.y > point2.y ? point1.y
                                    : point2.y))
                                nearest.y = (point1.y > point2.y ? point1.y
                                        : point2.y);
                        } else if (point2.y == point1.y) {
                            nearest.x = point.x;
                            nearest.y = point1.y;

                            // Make sure that the x coordinate
                            // is within the bounds of the
                            // segment's x coordinates.
                            if (nearest.x < (point1.x < point2.x ? point1.x
                                    : point2.x))
                                nearest.x = (point1.x < point2.x ? point1.x
                                        : point2.x);
                            if (nearest.x > (point1.x > point2.x ? point1.x
                                    : point2.x))
                                nearest.x = (point1.x > point2.x ? point1.x
                                        : point2.x);
                        } else {

                            // Get the slopes and offsets for
                            // the line equations describing
                            // the segment's line and the line
                            // running perpendicularly from
                            // the point to the segment.
                            double segmentM = ((double) (point2.y - point1.y))
                                    / (double) (point2.x - point1.x);
                            double segmentB = ((double) point1.y)
                                    - (segmentM * (double) point1.x);
                            double perpM = -1.0 / segmentM;
                            double perpB = ((double) point.y)
                                    - (perpM * (double) point.x);

                            // Calculate the point of inter-
                            // section between the two line
                            // equations.
                            double nearestX = (perpB - segmentB)
                                    / (segmentM - perpM);
                            nearest.x = (int) Math.round(nearestX);
                            nearest.y = (int) Math.round((perpM * nearestX)
                                    + perpB);

                            // Make sure that the point cal-
                            // culated lies on the segment;
                            // if not, adjust it so that it
                            // is whichever segment endpoint
                            // is closest to it.
                            if (nearest.x < (point1.x < point2.x ? point1.x
                                    : point2.x)) {
                                nearest.x = (point1.x < point2.x ? point1.x
                                        : point2.x);
                                nearest.y = (point1.x < point2.x ? point1.y
                                        : point2.y);
                            } else if (nearest.y < (point1.y < point2.y ? point1.y
                                    : point2.y)) {
                                nearest.y = (point1.y < point2.y ? point1.y
                                        : point2.y);
                                nearest.x = (point1.y < point2.y ? point1.x
                                        : point2.x);
                            } else if (nearest.x > (point1.x > point2.x ? point1.x
                                    : point2.x)) {
                                nearest.x = (point1.x > point2.x ? point1.x
                                        : point2.x);
                                nearest.y = (point1.x > point2.x ? point1.y
                                        : point2.y);
                            } else if (nearest.y > (point1.y > point2.y ? point1.y
                                    : point2.y)) {
                                nearest.y = (point1.y > point2.y ? point1.y
                                        : point2.y);
                                nearest.x = (point1.y > point2.y ? point1.x
                                        : point2.x);
                            }
                        }

                        // Return the index of the segment.
                        return j - 1;
                    } else {

                        // Determine which node is closest
                        // and return its index.
                        if (Math.sqrt((point.y - point1.y)
                                * (point.y - point1.y) + (point.x - point1.x)
                                * (point.x - point1.x)) > Math
                                .sqrt((point.y - point2.y)
                                        * (point.y - point2.y)
                                        + (point.x - point2.x)
                                        * (point.x - point2.x)))
                            return (j == pixelPoints.size() - 1 ? 0 : j);
                        else
                            return j - 1;
                    }
                }
            }
        }

        // Since no nearest point was found, return -1.
        return -1;
    }

    /**
     * Find the point on the path that is closest to the specified point. This
     * method, unlike the other <code>
     * findNearestPoint()</code>, always
     * returns a point and segment that is closest, simply because it does not
     * care how close the point is to the path; it just looks for the closest
     * point on the path, regardless of the distance.
     * 
     * @param point
     *            A single point that may or may not lie on the path.
     * @param nearest
     *            Point in which the coordinates of the point along the path is
     *            stored (if something other than -1 is returned).
     * @return The nearest segment index, or -1 if the path has less than two
     *         nodes.
     */
    protected int findNearestPoint(Point point, Point nearest) {

        // If the mode calls for looking through segments
        // between adjacent points, iterate through them; if
        // the point is within the slop distance in pixels
        // from one of the segments, return either the index
        // of the segment (if the SEGMENTS_ONLY mode is
        // operative) or the index of the node closest to the
        // segment.
        double closestDistance = 1000000.0;
        int closestSegment = -1;
        for (int j = 1; j < pixelPoints.size(); j++) {

            // If the distance between the point and this
            // path segment is the closest so far, record it
            // and the segment's index.
            Point point1 = (Point) pixelPoints.elementAt(j - 1);
            Point point2 = (Point) pixelPoints.elementAt(j);
            double dist = Line2D.ptSegDist(point1.x, point1.y, point2.x,
                    point2.y, point.x, point.y);
            if (dist < closestDistance) {
                closestDistance = dist;
                closestSegment = j - 1;
            }
        }

        // Find the point along the closest segment that is
        // closest to the specified point.
        if (closestSegment != -1) {
            Point point1 = (Point) pixelPoints.elementAt(closestSegment);
            Point point2 = (Point) pixelPoints.elementAt(closestSegment + 1);

            // If the segment is vertical or horizontal,
            // the point is simple to calcluate; otherwise,
            // the point's coordinates require the calcu-
            // lation first of the segment's slope and off-
            // set (m and b from y = mx + b), then the slope
            // and offset of the line running through the
            // original point perpendicularly to the seg-
            // ment, and finally the finding of an inter-
            // section between these two lines.
            if (point2.x == point1.x) {
                nearest.x = point1.x;
                nearest.y = point.y;

                // Make sure that the y coordinate is within
                // the bounds of the segment's y coordinates.
                if (nearest.y < (point1.y < point2.y ? point1.y : point2.y))
                    nearest.y = (point1.y < point2.y ? point1.y : point2.y);
                if (nearest.y > (point1.y > point2.y ? point1.y : point2.y))
                    nearest.y = (point1.y > point2.y ? point1.y : point2.y);
            } else if (point2.y == point1.y) {
                nearest.x = point.x;
                nearest.y = point1.y;

                // Make sure that the x coordinate is within
                // the bounds of the segment's x coordinates.
                if (nearest.x < (point1.x < point2.x ? point1.x : point2.x))
                    nearest.x = (point1.x < point2.x ? point1.x : point2.x);
                if (nearest.x > (point1.x > point2.x ? point1.x : point2.x))
                    nearest.x = (point1.x > point2.x ? point1.x : point2.x);
            } else {

                // Get the slopes and offsets for the line
                // equations describing the segment's line
                // and the line running perpendicularly from
                // the point to the segment.
                double segmentM = ((double) (point2.y - point1.y))
                        / (double) (point2.x - point1.x);
                double segmentB = ((double) point1.y)
                        - (segmentM * (double) point1.x);
                double perpM = -1.0 / segmentM;
                double perpB = ((double) point.y) - (perpM * (double) point.x);

                // Calculate the point of intersection be-
                // tween the two line equations.
                double nearestX = (perpB - segmentB) / (segmentM - perpM);
                nearest.x = (int) Math.round(nearestX);
                nearest.y = (int) Math.round((perpM * nearestX) + perpB);

                // Make sure that the point calculated lies
                // on the segment; if not, adjust it so that
                // it is whichever segment endpoint is clo-
                // sest to it.
                if (nearest.x < (point1.x < point2.x ? point1.x : point2.x)) {
                    nearest.x = (point1.x < point2.x ? point1.x : point2.x);
                    nearest.y = (point1.x < point2.x ? point1.y : point2.y);
                } else if (nearest.y < (point1.y < point2.y ? point1.y
                        : point2.y)) {
                    nearest.y = (point1.y < point2.y ? point1.y : point2.y);
                    nearest.x = (point1.y < point2.y ? point1.x : point2.x);
                } else if (nearest.x > (point1.x > point2.x ? point1.x
                        : point2.x)) {
                    nearest.x = (point1.x > point2.x ? point1.x : point2.x);
                    nearest.y = (point1.x > point2.x ? point1.y : point2.y);
                } else if (nearest.y > (point1.y > point2.y ? point1.y
                        : point2.y)) {
                    nearest.y = (point1.y > point2.y ? point1.y : point2.y);
                    nearest.x = (point1.y > point2.y ? point1.x : point2.x);
                }
            }
        }

        // Return the closest segment's index.
        return closestSegment;
    }

}
