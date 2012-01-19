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
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.util.Vector;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.viz.adapter.CoordConverter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The drawable representation for a line object. The line is made up of two
 * points joined by a straight segment. The line may be of an arbitrary
 * thickness, and its style may be either dashed or solid.
 * 
 * @author Christopher Golden
 */
public class Line extends MultiPointGlyph {

    // Protected Static Constants

    /**
     * Value specifying the move-location-1 edit type, indicating that the first
     * endpoint is being moved.
     */
    protected static final int MOVE_LOC_1 = -1;

    /**
     * Value specifying the move-location-2 edit type, indicating that the
     * second endpoint is being moved.
     */
    protected static final int MOVE_LOC_2 = -2;

    // Protected Variables

    /**
     * Bounding rectangle of the line; this is display-context sensitive, and so
     * is transient.
     */
    protected transient Rectangle boundingRect = null;

    /**
     * Bounding shape of the line, to be used for hit tests; this is
     * display-context sensitive, and so is transient.
     */
    protected transient Shape boundingShape = null;

    /**
     * Shape of the line to be used for drawing; this is display-context
     * sensitive, and therefore is transient.
     */
    protected transient Shape drawingShape = null;

    /**
     * Last point used in an edit operation; this is only needed during edits,
     * and so is transient.
     */
    protected transient Point lastPoint = null;

    // Public Constructors

    /**
     * Create an instance with a starting color of black and standard thickness
     * and style with no starting or ending points to begin with.
     */
    public Line() {
    }

    /**
     * Create an instance with the specified color, style, thickness, and
     * starting and ending points.
     * 
     * @param frame
     *            Frame in which this glyph exists; if <code>ALL_FRAMES</code>,
     *            the glyph exists in all frames.
     * @param color
     *            Color of the line.
     * @param style
     *            Style of the line.
     * @param thickness
     *            Width of the line.
     * @param startPoint
     *            Starting point of the line; may be <code>null</code>.
     * @param endPoint
     *            Ending point of the line; may be <code>null</code>.
     */
    public Line(int frame, Color color, int style, int thickness,
            Coordinate startPoint, Coordinate endPoint) {
        super(frame, color, style, thickness, new Vector<Coordinate>(),
                new Vector<Adornment>());
        if (startPoint != null)
            points.addElement(startPoint);
        if (endPoint != null)
            points.addElement(endPoint);
    }

    /**
     * Create an instance with the specified color, style, thickness, and
     * starting and ending points.
     * 
     * @param frame
     *            Frame in which this glyph exists; if <code>ALL_FRAMES</code>,
     *            the glyph exists in all frames.
     * @param color
     *            Color of the line.
     * @param style
     *            Style of the line.
     * @param thickness
     *            Width of the line.
     * @param startPoint
     *            Starting point of the line; may be <code>null</code>.
     * @param endPoint
     *            Ending point of the line; may be <code>null</code>.
     * @param transformer
     *            Coordinate converter used to translate between X,Y and
     *            latitude-longitude coordinates.
     */
    public Line(int frame, Color color, int style, int thickness,
            Point startPoint, Point endPoint, CoordConverter transformer) {
        super(frame, color, style, thickness, new Vector<Coordinate>(),
                new Vector<Adornment>());
        if (startPoint != null)
            points.addElement(transformer.convert(startPoint));
        if (endPoint != null)
            points.addElement(transformer.convert(endPoint));
    }

    // Public Methods

    /**
     * Create a clone of this glyph.
     */
    public Object clone() {
        Line glyph = null;
        try {
            glyph = (Line) super.clone();
            glyph.boundingRect = null;
            glyph.boundingShape = null;
            glyph.drawingShape = null;
            glyph.lastPoint = null;
        } catch (Exception e) {
            // Logger.logBug("Clone error.", e);
            e.printStackTrace();
        }
        return glyph;
    }

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
        // Add the adornments if the line is to be drawn
        // over them or not at all.
        if ((getPaintingMode() == PAINT_OVER)
                || (getPaintingMode() == NO_PAINTING))
            adornmentsToShape(ws, ss, transformer);

        // Do nothing with the basic line itself if it is
        // not to be drawn.
        if (getPaintingMode() != NO_PAINTING) {

            // Set the drawing color.
            // dgm.setColorEx(color);

            // FUTURE ENHANCEMENT: It may be desirable to
            // approximate the line's thickness. For now,
            // this is not being done.

            // Set the line style.
            // dgm.setLineTexture((short) (style == DASHED_STYLE ? 0x00FF :
            // 0xFFFF),
            // (short) (style == DASHED_STYLE ? 16 : 0));

            // Translate the display coordinates to DGM
            // coordinates.
            Coordinate[] coords = new Coordinate[2];
            for (int j = 0; j < 2; j++) {
                Coordinate old = points.elementAt(j);
                coords[j] = new Coordinate(old.x, old.y);
                System.out.println("Adding pt: " + coords[j]);
            }

            ws.addLineSegment(coords);
        }

        // Add the adornments if the path is to be drawn
        // under them.
        if (getPaintingMode() == PAINT_UNDER)
            adornmentsToShape(ws, ss, transformer);
    }

    // /**
    // * Paint the glyph.
    // *
    // * @param gc Graphics context in which the line
    // * is to be drawn.
    // * @param transformer Coordinate transformer to be used to
    // * convert from the display coordinates
    // * to latitude-longitude format.
    // * @param properties Display properties to be used.
    // * @param useColor Color to be used when painting the
    // * glyph instead of whatever color(s)
    // * would usually be used; if <code>
    // * null</code>, standard painting
    // * will be done.
    // */
    // public void paint(IGraphicsTarget gc, CoordConverter transformer,
    // Color useColor) {
    //		
    // // Translate the coordinates by adding the line's
    // // location coordinates to each point and converting
    // // to pixel coordinates.
    // mapToDisplayCoordinates(gc, transformer);
    //
    // // If the line is not fully created, do nothing.
    // if (pixelPoints.size() < 2)
    // return;
    //
    // // If points are being added, draw the line as a
    // // simple thin line; otherwise, use the shapes to
    // // draw it.
    // if (editType == FINISH_CREATION) {
    // Point point1 = (Point) pixelPoints.elementAt(0);
    // Point point2 = (Point) pixelPoints.elementAt(1);
    // gc.setColor(properties.transform(useColor != null ? useColor : color));
    // gc.drawLine(point1.x, point1.y, point2.x, point2.y);
    // } else {
    //
    // // Draw the shape representing this path,
    // // stroked appropriately to give it the correct
    // // thickness, style, etc., if the glyph should
    // // be painted before any of its adornments.
    // if (getPaintingMode() == PAINT_UNDER) {
    // gc.setColor(properties.transform(useColor != null ? useColor : color));
    // ((Graphics2D) gc).draw(drawingShape);
    // ((Graphics2D) gc).fill(drawingShape);
    // }
    //
    // // Draw and fill the shapes representing any
    // // adornments associated with this line if no
    // // endpoint-moving is going on.
    // if ((editType != MOVE_LOC_1) && (editType != MOVE_LOC_2))
    // paintAdornments(gc, transformer, properties, useColor);
    //
    // // Draw the shape representing this path,
    // // stroked appropriately to give it the correct
    // // thickness, style, etc., if the glyph should
    // // be painted after all of its adornments.
    // if (getPaintingMode() == PAINT_OVER) {
    // gc.setColor(properties.transform(useColor != null ? useColor : color));
    // ((Graphics2D) gc).draw(drawingShape);
    // ((Graphics2D) gc).fill(drawingShape);
    // }
    // }
    // }

    // /**
    // * Convert this object to DGM format and place the result
    // * in the supplied byte array.
    // *
    // * @param dgm DGM array in which to place the
    // * translated object.
    // * @param gc Graphics context in which this
    // * glyph is drawn.
    // * @param transformer Coordinate converter to be used
    // * to convert from the display
    // * coordinates to latitude-longitude
    // * pairs.
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
    // // approximate the line's color. For now, this
    // // is not being done.
    //
    //
    // // FUTURE ENHANCEMENT: It may be desirable to
    // // approximate the line's thickness. For now,
    // // this is not being done.
    //
    //
    // // Set the line style.
    // dgm.setLineTexture((short) (style == DASHED_STYLE ? 0x00FF : 0xFFFF),
    // (short) (style == DASHED_STYLE ? 16 : 0));
    //
    // // Translate the display coordinates to DGM
    // // coordinates.
    // short[] xCoords = new short[2];
    // short[] yCoords = new short[2];
    // for (int j = 0; j < 2; j++) {
    // LatLong latLong = (LatLong) points.elementAt(j);
    // if (cartesian) {
    // Point point = transformer.convert(latLong, false);
    // xCoords[j] = (short) point.x;
    // yCoords[j] = (short) point.y;
    // } else {
    // xCoords[j] = (short) ((((double) latLong.lon) * 60.0) + 0.5);
    // yCoords[j] = (short) ((((double) latLong.lat) * -60.0) + 0.5);
    // }
    // }
    //
    // // Add the line as a set of linked vectors in
    // // the DGM file.
    // dgm.addLinkedVectors((short) xCoords.length, xCoords, yCoords);
    //
    // // Add any adornments to the DGM array.
    // adornmentsToDGM(dgm, gc, transformer, cartesian);
    // }
    //
    // /**
    // * Convert this object to Extended DGM format and place
    // * the result in the supplied byte array.
    // *
    // * @param dgm Extended DGM array in which to
    // * place the translated object.
    // * @param gc Graphics context in which this
    // * glyph is drawn.
    // * @param transformer Coordinate converter to be used
    // * to convert from the display
    // * coordinates to latitude-longitude
    // * pairs.
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
    // // Add the adornments if the line is to be drawn
    // // over them or not at all.
    // if ((getPaintingMode() == PAINT_OVER) || (getPaintingMode() ==
    // NO_PAINTING))
    // adornmentsToExtendedDGM(dgm, gc, transformer, cartesian);
    //
    // // Do nothing with the basic line itself if it is
    // // not to be drawn.
    // if (getPaintingMode() != NO_PAINTING) {
    //
    // // Set the drawing color.
    // dgm.setColorEx(color);
    //
    //
    // // FUTURE ENHANCEMENT: It may be desirable to
    // // approximate the line's thickness. For now,
    // // this is not being done.
    //
    //
    // // Set the line style.
    // dgm.setLineTexture((short) (style == DASHED_STYLE ? 0x00FF : 0xFFFF),
    // (short) (style == DASHED_STYLE ? 16 : 0));
    //
    // // Translate the display coordinates to DGM
    // // coordinates.
    // short[] xCoords = new short[2];
    // short[] yCoords = new short[2];
    // for (int j = 0; j < 2; j++) {
    // LatLong latLong = (LatLong) points.elementAt(j);
    // if (cartesian) {
    // Point point = transformer.convert(latLong, false);
    // xCoords[j] = (short) point.x;
    // yCoords[j] = (short) point.y;
    // } else {
    // xCoords[j] = (short) ((((double) latLong.lon) * 60.0) + 0.5);
    // yCoords[j] = (short) ((((double) latLong.lat) * -60.0) + 0.5);
    // }
    // }
    //
    // // Add the line as a set of linked vectors in
    // // the DGM file.
    // dgm.addLinkedVectors((short) xCoords.length, xCoords, yCoords);
    // }
    //
    // // Add the adornments if the path is to be drawn
    // // under them.
    // if (getPaintingMode() == PAINT_UNDER)
    // adornmentsToExtendedDGM(dgm, gc, transformer, cartesian);
    // }

    /**
     * Flush display coordinates, since they are no longer valid. This method is
     * called when something about the glyph (position, etc.) or the display
     * context in which the glyph is painted has changed.
     */
    public void flushDisplayCoordinates() {
        super.flushDisplayCoordinates();
        boundingRect = null;
        boundingShape = null;
        drawingShape = null;
    }

    /**
     * Indicate whether or not the line contains the specified point.
     * 
     * @param point
     *            Point to check to see if the line contains it.
     * @param gc
     *            Graphics context in which this line is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return True if the line contains the point, otherwise false.
     */
    public boolean contains(Point point, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Make sure that the bounding shape has already
        // been calculated.
        mapToDisplayCoordinates(gc, transformer);

        // If the bounding shape contains the point, return
        // true; otherwise, return false.
        return (boundingShape.contains(point.x, point.y) || adornmentsContain(
                point, gc, transformer));
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
        return (boundingShape.intersects(rectangle) || adornmentsIntersect(
                rectangle, gc, transformer));
    }

    /**
     * Get the bounding rectangle of the line.
     * 
     * @param gc
     *            Graphics context in which this line is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Bounding rectangle of the line.
     */
    public Rectangle getBoundingRect(IGraphicsTarget gc,
            CoordConverter transformer) {

        // Make sure that the bounding rectangle has
        // already been calculated.
        mapToDisplayCoordinates(gc, transformer);

        // Inflate the bounding rectangle to adjust it
        // for line thickness.
        Rectangle adjustedForThickness = new Rectangle(boundingRect);
        if (thickness > 1)
            adjustedForThickness.grow(thickness - 1, thickness - 1);
        adjustedForThickness.grow(2, 2);

        // Return the adjusted bounding rectangle.
        return adjustedForThickness;
    }

    /**
     * Begin editing the line. The types of editing supported are
     * <code>FINISH_CREATION</code>, <code>MOVE_ONLY</code>, and
     * <code>MOVE</code>. The latter actually performsb differently depending
     * upon whether the user begins the edit at one of the endpoints or
     * elsewhere on the line. Starting the move at an endpoint drags just that
     * endpoint to a new location, whereas starting elsewhere moves the entire
     * line.
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
        if (event.type != SWT.MouseUp)
            return false;
        Point point = new Point(event.x, event.y);

        // The only editing operations that this class
        // supports are move, adding another point to fin-
        // ish creation, and replacing a section.
        if (type == MOVE) {

            // Remember the starting point of the edit.
            lastPoint = point;

            // Make sure that the display coordinates are
            // current.
            mapToDisplayCoordinates(gc, transformer);

            // If the point at which the edit started is
            // close to the first endpoint, drag just that
            // endpoint; if close to the second, drag that
            // one; and if elsewhere, drag the entire
            // line.
            Point point1 = (Point) pixelPoints.elementAt(0);
            Point point2 = (Point) pixelPoints.elementAt(1);
            if ((point.x > point1.x - 5) && (point.x < point1.x + 5)
                    && (point.y > point1.y - 5) && (point.y < point1.y + 5))
                editType = MOVE_LOC_1;
            else if ((point.x > point2.x - 5) && (point.x < point2.x + 5)
                    && (point.y > point2.y - 5) && (point.y < point2.y + 5))
                editType = MOVE_LOC_2;
            else
                editType = MOVE;
            return true;
        } else if (type == MOVE_ONLY) {
            editType = MOVE;
            lastPoint = point;
            return true;
        } else
            return super.startEdit(type, event, gc, transformer);
    }

    /**
     * Edit the glyph based upon the specified event.
     * 
     * @param event
     *            Input event that triggered this call.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return True if the edit should continue, false if the glyph has ended
     *         the edit.
     */
    public boolean edit(Event event, IGraphicsTarget gc,
            CoordConverter transformer) {

        // If the first or second endpoint is being moved,
        // translate just that endpoint; otherwise, let
        // the superclass handle it.
        if ((editType == MOVE_LOC_1) || (editType == MOVE_LOC_2)) {

            // Ignore the event unless it is a mouse move or
            // left mouse button release.
            if (event.type != SWT.MouseMove && event.type != SWT.MouseUp)
                return true;

            // Get the point the mouse is over and use it
            // to translate the endpoint.
            Point point = new Point(event.x, event.y);
            int xDelta = point.x - lastPoint.x;
            int yDelta = point.y - lastPoint.y;
            Point endPoint = (Point) pixelPoints
                    .elementAt(editType == MOVE_LOC_1 ? 0 : 1);
            endPoint.x += xDelta;
            endPoint.y += yDelta;

            // Recreate the bounding rectangle.
            boundingRect = new Rectangle();
            for (int j = 0; j < 2; j++) {
                endPoint = (Point) pixelPoints.elementAt(j);
                if (j == 0) {
                    boundingRect.x = endPoint.x;
                    boundingRect.y = endPoint.y;
                } else
                    boundingRect.add(endPoint);
            }
            boundingRect.grow(1, 1);

            // Remember the this point in the edit.
            lastPoint = point;

            // Recreate the drawing shape.
            createVisuals(gc, transformer);

            // If the mouse was released, end the
            // edit; otherwise, let it continue.
            if (event.type == SWT.MouseUp) {
                finishEdit(gc, transformer);
                return false;
            } else
                return true;
        } else
            return super.edit(event, gc, transformer);
    }

    /**
     * Finish up editing the glyph.
     * 
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate transformer to be used to convert from the display
     *            coordinates to latitude-longitude format.
     */
    public void finishEdit(IGraphicsTarget gc, CoordConverter transformer) {

        // Handle the edit finishing differently depending
        // upon what sort of editing was being done.
        if ((editType == MOVE_LOC_1) || (editType == MOVE_LOC_2)) {

            // Set the flag indicating that the glyph is no
            // longer being edited.
            editType = NONE;

            // Calculate the lat-long coordinates based upon
            // the display coordinates.
            mapToLatLongCoordinates(gc, transformer);
            flushDisplayCoordinates();
        } else
            super.finishEdit(gc, transformer);
    }

    /**
     * Get the length of the line in display units.
     * 
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Length of the line in display units.
     */
    public double getLength(IGraphicsTarget gc, CoordConverter transformer) {

        // Make sure that the display coordinates are
        // current.
        mapToDisplayCoordinates(gc, transformer);

        // Do nothing if there are not two endpoints.
        if (pixelPoints.size() < 2)
            return 0.0;

        // Return the length.
        return getLength();
    }

    /**
     * Get the point that is the specified distance along the length of the
     * line.
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
     *         line, or <code>null</code> if the line is not valid (i.e. it
     *         does not have two endpoints) or if the distance specified is
     *         greater than the total length of the line.
     */
    public Point getPointAtDistanceAlong(double distance, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Make sure that the display coordinates are
        // current.
        mapToDisplayCoordinates(gc, transformer);

        // Do nothing if there are not two endpoints, or if
        // the distance specified is longer than the length
        // of the line.
        double length;
        if ((pixelPoints.size() < 2) || ((length = getLength()) < distance))
            return null;

        // Convert the distance into a fraction and figure
        // out the point.
        return getPointAtFractionAlong(distance / length, gc, transformer);
    }

    /**
     * Get the point that lies along the line's length at the specified fraction
     * of the line's total length.
     * 
     * @param fraction
     *            Fraction of the total length at which the point lies; must be
     *            between 0.0 and 1.0 inclusive.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Point that lies at the specified fraction of the length along the
     *         line, or <code>null</code> if the line is not valid (i.e. it
     *         does not have two endpoints) or if the fraction specified is
     *         outside the allowable range.
     */
    public Point getPointAtFractionAlong(double fraction, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Make sure that the display coordinates are
        // current.
        mapToDisplayCoordinates(gc, transformer);

        // Do nothing if there are not two endpoints, or if
        // the fraction specified is not within the allowable
        // bounds.
        if ((pixelPoints.size() < 2) || (fraction < 0.0) || (fraction > 1.0))
            return null;

        // Find the point that is this fraction of the total
        // distance along the line.
        Point point1 = (Point) pixelPoints.elementAt(0);
        Point point2 = (Point) pixelPoints.elementAt(1);
        return new Point(((int) (fraction * (point2.x - point1.x))) + point1.x,
                ((int) (fraction * (point2.y - point1.y))) + point1.y);
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

        // Do nothing if there are not two endpoints.
        if (pixelPoints.size() < 2)
            return 0.0;

        // Get the point along the line itself. If the point
        // is not close enough to the line, do no more.
        Point pointOnPath = findNearestPoint(point, 1.0);
        if (pointOnPath == null)
            return 0.0;

        // Determine how far the point that is on the line is
        // from the first endpoint; this yields the total
        // distance.
        Point endPoint = (Point) pixelPoints.elementAt(0);
        return Math
                .sqrt(((pointOnPath.x - endPoint.x) * (pointOnPath.x - endPoint.x))
                        + ((pointOnPath.y - endPoint.y) * (pointOnPath.y - endPoint.y)));
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
        return distance / getLength();
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

        // Make sure that the display coordinates are
        // current.
        mapToDisplayCoordinates(gc, transformer);

        // Do nothing if there are not two endpoints, or if
        // the distances specified are longer than the length
        // of the line.
        double length;
        if ((pixelPoints.size() < 2)
                || ((length = getLength()) < startDistance)
                || (length < endDistance))
            return null;

        // Convert the distances into fractions and figure
        // out the point.
        return getSectionAtFractionAlong(startDistance / length, endDistance
                / length, gc, transformer);
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

        // If the starting fraction is not less than the
        // ending fraction, return nothing.
        if (startFraction >= endFraction)
            return null;

        // Translate the two fractions into points, and if
        // both translate successfully, simply return the
        // two points; otherwise, return nothing.
        Point[] section = new Point[2];
        section[0] = getPointAtFractionAlong(startFraction, gc, transformer);
        section[1] = getPointAtFractionAlong(endFraction, gc, transformer);
        if ((section[0] != null) && (section[1] != null))
            return section;
        else
            return null;
    }

    /**
     * Find the angle of the path at the point specified.
     * 
     * @param point
     *            A single point that lies along the glyph.
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

        // Do nothing if there are not two endpoints.
        if (pixelPoints.size() < 2)
            return 0.0;

        // Find the angle between the two endpoints.
        Point point1 = (Point) pixelPoints.elementAt(0);
        Point point2 = (Point) pixelPoints.elementAt(1);
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

            // Iterate through the lat-long pairs, transform-
            // ing each in turn into a set of pixel coordi-
            // nates. Record each as a pixel point.
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

            // Add the bounding rectangles of any adornments.
            if (editType != FINISH_CREATION) {
                Rectangle adornmentRect = getAdornmentBoundingRectangle(gc,
                        transformer);
                if (adornmentRect != null)
                    boundingRect = boundingRect.union(adornmentRect);
            }

            // Inflate the rectangle a bit to be on the safe
            // side.
            boundingRect.grow(1, 1);

            // Create the bounding and drawing shapes.
            createBoundingShape();
            createVisuals(gc, transformer);
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
        for (int j = 0; j < pixelPoints.size(); j++) {
            latLong = transformer.convert((Point) pixelPoints.elementAt(j));
            points.setElementAt(latLong, j);
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
        boundingShape = at.createTransformedShape(boundingShape);
        drawingShape = at.createTransformedShape(drawingShape);

        // Translate the display coordinates of any
        // adornments.
        translateAdornments(xDelta, yDelta);
    }

    /**
     * Create the bounding shape for this line.
     */
    protected void createBoundingShape() {

        // If the display coordinates have not been calcula-
        // ted, do nothing.
        if (pixelPoints.size() < 2)
            return;

        // Create the bounding shape for this line by
        // stroking the path with a solid-style pen. For
        // thin lines, the pen is of sufficient thickness
        // to allow it to contain points that are in
        // actuality outside the line, in order to make
        // it easier for the user to click on the line.
        BasicStroke stroke = new BasicStroke(thickness == 0 ? 7 : thickness + 6);
        Point point1 = (Point) pixelPoints.elementAt(0);
        Point point2 = (Point) pixelPoints.elementAt(1);
        boundingShape = stroke.createStrokedShape(new Line2D.Float(point1.x,
                point1.y, point2.x, point2.y));
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

        // If the display coordinates have not been calcula-
        // ted, do nothing.
        if (pixelPoints.size() < 2)
            return;

        // Create the drawing shape for this line by stroking
        // the line with the correct pen thickness and style.
        float[] dashArray = { (float) 6.0, (float) 6.0 };
        float dashOffset = (float) (style == DASHED_STYLE ? 9.0 : 0.0);
        BasicStroke stroke = new BasicStroke(thickness, BasicStroke.CAP_BUTT,
                BasicStroke.JOIN_ROUND, (float) 10.0,
                (style == DASHED_STYLE ? dashArray : null), dashOffset);
        Point point1 = (Point) pixelPoints.elementAt(0);
        Point point2 = (Point) pixelPoints.elementAt(1);
        drawingShape = stroke.createStrokedShape(new Line2D.Float(point1.x,
                point1.y, point2.x, point2.y));
    }

    /**
     * Get the length of the line in display units.
     * 
     * @return Length of the line in display units.
     */
    protected double getLength() {

        // Calculate the distance between the two end-
        // points.
        Point point1 = (Point) pixelPoints.elementAt(0);
        Point point2 = (Point) pixelPoints.elementAt(1);
        return Math.sqrt(((point1.x - point2.x) * (point1.x - point2.x))
                + ((point1.y - point2.y) * (point1.y - point2.y)));
    }

    /**
     * Process the specified event during glyph creation.
     * 
     * @param event
     *            Event that is to be processed.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate transformer to be used to convert from the display
     *            coordinates to latitude-longitude format.
     * @return True if the creation is to continue, otherwise false.
     */
    protected boolean processEventDuringCreation(Event event,
            IGraphicsTarget gc, CoordConverter transformer) {

        // Ignore the event unless it is a mouse move or
        // left mouse button release.
        if (event.type != SWT.MouseMove || event.type != SWT.MouseUp)
            return true;

        // Recalculate the bounding rectangle to ensure
        // that it contains the new point.
        mapToDisplayCoordinates(gc, transformer);

        // Convert the specified point into a lat-long pair.
        Point point = new Point(event.x, event.y);
        Coordinate latLong = transformer.convert(new Point(point));

        // Use this point as the first or second endpoint,
        // the former if no endpoints have been chosen, the
        // latter if one or both have already been supplied
        // already.
        if (points.size() < 2) {
            points.addElement(latLong);
            pixelPoints.addElement(new Point(point));
        } else {
            points.setElementAt(latLong, 1);
            pixelPoints.setElementAt(new Point(point), 1);
        }

        // If no points have been added until now, then the
        // bounding rectangle is, in essence, the new point;
        // otherwise, the rectangle is recreated.
        Point firstPoint = (Point) pixelPoints.elementAt(0);
        boundingRect.x = firstPoint.x;
        boundingRect.y = firstPoint.y;
        boundingRect.width = boundingRect.height = 0;
        if (pixelPoints.size() == 2)
            boundingRect.add(point);

        // If the mouse was released, the creation of this
        // glyph is complete; otherwise, it should continue.
        return (event.type != SWT.MouseUp);
    }

    /**
     * Find the point on the line that is closest to the specified point.
     * 
     * @param point
     *            A single point that may or may not lie on the line.
     * @param slop
     *            Slop allowed when determining whether <code>point</code> is
     *            close enough to the line to calculate a nearest point.
     * @return The point that lies on the line closest to the specified point,
     *         or <code>null</code> if the line is too far from the point.
     */
    protected Point findNearestPoint(Point point, double slop) {

        // If the distance between the point and the line
        // is within 5 pixels, then return the point where
        // the the specified point is closest to the line.
        // Otherwise return nothing.
        Point nearest = new Point();
        Point point1 = (Point) pixelPoints.elementAt(0);
        Point point2 = (Point) pixelPoints.elementAt(1);
        if (Line2D.ptSegDist(point1.x, point1.y, point2.x, point2.y, point.x,
                point.y) <= slop) {

            // Figure out what point to return as coordin-
            // ates of the closest point to the line. If
            // the line is vertical or horizontal, it is
            // simple to calcluate; otherwise, the coord-
            // inates require the calculation first of the
            // line's slope and offset (m and b from
            // y = mx + b), then the slope and offset of
            // the line running through the original point
            // perpendicularly to this line, and finally
            // the finding of an intersection between
            // these two lines.
            if (point2.x == point1.x) {
                nearest.x = point1.x;
                nearest.y = point.y;
            } else if (point2.y == point1.y) {
                nearest.x = point.x;
                nearest.y = point1.y;
            } else {
                double segmentM = (double) (point2.y - point1.y)
                        / (double) (point2.x - point1.x);
                double segmentB = ((double) point1.y)
                        - (segmentM * (double) point1.x);
                double perpM = -1.0 / segmentM;
                double perpB = ((double) point.y) - (perpM * (double) point.x);
                nearest.x = (int) Math.round((perpB - segmentB)
                        / (segmentM - perpM));
                nearest.y = (int) Math.round((perpM * (double) nearest.x)
                        + perpB);
            }
            return nearest;
        } else
            return null;
    }
}
