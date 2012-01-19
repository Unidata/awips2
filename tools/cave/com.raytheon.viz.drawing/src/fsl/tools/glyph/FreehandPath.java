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

import java.awt.Color;
import java.awt.Point;
import java.util.Vector;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.viz.adapter.CoordConverter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The drawable representation for a freehand path object. This path is sketched
 * out and edited using freehand drawing techniques. It may be open or closed;
 * in the latter case, it forms a polygon.
 * 
 * @author Christopher Golden
 */
public class FreehandPath extends Path {

    // Public Static Constants

    /**
     * No smoothing algorithm.
     */
    public static final int SMOOTHING_NONE = 0;

    /**
     * Averaging smoothing algorithm.
     */
    public static final int SMOOTHING_AVERAGE = 1;

    /**
     * Bicubic smoothing algorithm.
     */
    public static final int SMOOTHING_BICUBIC = 2;

    // Protected Variables

    /**
     * Smoothing algorithm to be used; must be one of the
     * <code>SMOOTHING_xxx</code> constants defined by this class.
     */
    protected int smoothingType = SMOOTHING_AVERAGE;

    /**
     * Smoothness of path.
     */
    protected int smoothness;

    /**
     * Display context coordinates of points making up a new section of the path
     * being added during an edit operation.
     */
    protected transient Vector<Point> newSectionPoints = null;

    // Public Constructors

    /**
     * Create an instance with a starting color of black and standard thickness.
     * The path has zero points to start with and is open.
     */
    public FreehandPath() {
        smoothness = 2;
    }

    /**
     * Create an instance with the specified color, style, and thickness, with
     * average smoothing and that is open and has no points to start.
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
    public FreehandPath(int frame, Color color, int style, int thickness) {
        super(frame, color, style, thickness);
        smoothness = 2;
    }

    /**
     * Create an instance with the specified color, style, thickness, and
     * smoothing type, and that is open and has no points to start.
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
     * @param smoothing
     *            Smoothing type; must be one of the various
     *            <code>SMOOTHING_xxx</code> constants.
     */
    public FreehandPath(int frame, Color color, int style, int thickness,
            int smoothing) {
        super(frame, color, style, thickness);
        smoothingType = smoothing;
        smoothness = 2;
    }

    /**
     * Create an instance with the specified border, color, style, thickness,
     * closure, fill color, and fill pattern, with average smoothing and with no
     * points to start.
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
    public FreehandPath(int frame, boolean border, Color color, int style,
            int thickness, boolean closed, Color fillColor, int fillPattern) {
        super(frame, border, color, style, thickness, closed, fillColor,
                fillPattern);
        smoothness = 2;
    }

    /**
     * Create an instance with the specified border, color, style, thickness,
     * smoothing, closure, fill color, and fill pattern, with no points to
     * start.
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
     * @param smoothing
     *            Smoothing type; must be one of the various
     *            <code>SMOOTHING_xxx</code> constants.
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
    public FreehandPath(int frame, boolean border, Color color, int style,
            int thickness, int smoothing, boolean closed, Color fillColor,
            int fillPattern) {
        super(frame, border, color, style, thickness, closed, fillColor,
                fillPattern);
        smoothingType = smoothing;
        smoothness = 2;
    }

    /**
     * Create an instance with the specified border, color, style, thickness,
     * smoothing, closure, fill color, and fill pattern, and with the specified
     * points.
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
     * @param smoothing
     *            Smoothing type; must be one of the various
     *            <code>SMOOTHING_xxx</code> constants.
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
    public FreehandPath(int frame, boolean border, Color color, int style,
            int thickness, int smoothing, boolean closed, Color fillColor,
            int fillPattern, Vector<Coordinate> points) {
        super(frame, border, color, style, thickness, closed, fillColor,
                fillPattern, points);
        smoothingType = smoothing;
        smoothness = 2;
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
        // they are both paths, and they have the same
        // smoothness.
        return (super.equals(obj) && (obj instanceof FreehandPath) && (smoothness == ((FreehandPath) obj).smoothness));
    }

    /**
     * Get the hash code of this object.
     * 
     * @return Hash code of this object.
     */
    public int hashCode() {

        // Combine the hash codes of the superclass and
        // the smoothness.
        return (int) ((((long) super.hashCode()) + ((long) smoothness)) % (long) Integer.MAX_VALUE);
    }

    /**
     * Create a clone of this glyph.
     */
    public Object clone() {
        FreehandPath glyph = null;
        try {
            glyph = (FreehandPath) super.clone();
            glyph.smoothness = smoothness;
            glyph.newSectionPoints = null;
        } catch (Exception e) {
            // Logger.logBug("Clone error.", e);
            e.printStackTrace();
        }
        return glyph;
    }

    /**
     * Paint the glyph.
     * 
     * @param gc
     *            Graphics context in which the path is to be drawn.
     * @param transformer
     *            Coordinate transformer to be used to convert from the display
     *            coordinates to latitude-longitude format.
     * @param properties
     *            Display properties to be used.
     * @param useColor
     *            Color to be used when painting the glyph instead of whatever
     *            color(s) would usually be used; if <code>
     *                    null</code>,
     *            standard painting will be done.
     */
    // public void paint(Graphics gc, CoordConverter transformer,
    // DisplayProperties properties, Color useColor) {
    //
    // // Let the superclass do most of the work.
    // super.paint(gc, transformer, properties, useColor);
    //
    // // If a new section is being added, draw it as well.
    // if ((editType == CHANGE_NODES) && (newSectionPoints.size() > 1) &&
    // (useColor == null)) {
    // gc.setColor(properties.transform(color));
    // int[][] coords = new int[2][newSectionPoints.size()];
    // for (int i = 0; i < newSectionPoints.size(); i++) {
    // Point point = (Point) newSectionPoints.elementAt(i);
    // coords[0][i] = point.x;
    // coords[1][i] = point.y;
    // }
    // gc.drawPolyline(coords[0], coords[1], newSectionPoints.size());
    // }
    // }
    /**
     * Begin editing the path. The types of editing supported are
     * <code>FINISH_CREATION</code>, <code>CHANGE_NODES</code>,
     * <code>MOVE</code>, and <code>MOVE_ONLY</code>.
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
        // if (event.type != SWT.MouseUp)
        // return false;

        // The only editing operations that this class
        // supports are move, adding points to the end,
        // and changing nodes.
        if (type == CHANGE_NODES) {

            // If no nearest point is found, do not start
            // editing.
            Point point = new Point(event.x, event.y);
            mapToDisplayCoordinates(gc, transformer);
            if (findNearestPoint(point, NODES_AND_SEGMENTS, 5.0, null) == -1)
                return false;

            // Create a list to hold the new section's
            // points and add the first point.
            editType = type;
            newSectionPoints = new Vector<Point>();
            processPointForNewSection(point, gc, transformer);
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

        // Add the point to the new section if doing
        // node changes; otherwise, let the superclass
        // handle the edit.
        if (editType == CHANGE_NODES) {

            // Do nothing unless the mouse was moved
            // or its button released.
            if (event.type != SWT.MouseMove && event.type != SWT.MouseUp)
                return true;

            // Process the new point.
            processPointForNewSection(new Point(event.x, event.y), gc,
                    transformer);

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

        // Handle the completion of the edit if section re-
        // placement is being performed; otherwise, let the
        // superclass handle it.
        if (editType == CHANGE_NODES) {

            // If a new section was created, replace the
            // old section with the new one.
            if (newSectionPoints.size() > 1) {

                // Determine the starting and ending in-
                // dices of the old section to be replaced.
                // If the ending index is not found, then
                // the whole path from one end to the star-
                // ting index is to be replaced.
                int startIndex = findNearestPoint((Point) newSectionPoints
                        .elementAt(0), NODES_AND_SEGMENTS, 5.0, null);
                int endIndex = findNearestPoint((Point) newSectionPoints
                        .elementAt(newSectionPoints.size() - 1),
                        NODES_AND_SEGMENTS, 5.0, null);
                if (endIndex == -1)
                    replaceOpenSegment(startIndex, gc, transformer);
                else
                    replaceClosedSegment(startIndex, endIndex, gc, transformer);

                // The display coordinates must be recal-
                // culated.
                flushDisplayCoordinates();
            }
            newSectionPoints = null;
            editType = NONE;
        } else
            super.finishEdit(gc, transformer);
    }

    /**
     * Get the glyph's smoothing type.
     * 
     * @return Smoothing type; will be one of the <code>
     *         SMOOTHING_XXX</code>
     *         constants.
     */
    public int getSmoothType() {
        return smoothingType;
    }

    /**
     * Set the glyph's smoothing type.
     * 
     * @param smoothType
     *            Type of smoothing the glyph should do when edited; must be one
     *            of the <code>SMOOTHING_XXX</code> values.
     */
    public void setSmoothType(int smoothType) {
        this.smoothingType = smoothType;
    }

    /**
     * Smooth out the path.
     * 
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter to be used for smoothing as necessary.
     */
    public void smooth(IGraphicsTarget gc, CoordConverter transformer) {
        smooth(1, points.size() - 1, gc, transformer);
    }

    // Protected Methods

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
        if (event.type != SWT.MouseMove && event.type != SWT.MouseUp)
            return true;

        // Recalculate the bounding rectangle to ensure that
        // it contains the new point.
        mapToDisplayCoordinates(gc, transformer);

        // See if the point should be tacked onto the end
        // of the path.
        Point point = new Point(event.x, event.y);
        if ((event.type != SWT.MouseUp)
                || (pixelPoints.size() == 0)
                || (pixelPoints.elementAt(pixelPoints.size() - 1).equals(point) == false)) {

            // Convert the specified point into a lat-long
            // pair.
            Coordinate latLong = transformer.convert(new Point(point));

            if (latLong == null)
                return false;

            // Add the point.
            points.addElement(latLong);
            pixelPoints.addElement(point);

            // If no points have been added until now, then
            // the bounding rectangle is, in essence, the
            // new point; otherwise, the new point is merely
            // added to the rectangle to cause the latter to
            // expand in an appropriate manner.
            if (points.size() == 1) {
                boundingRect.x = point.x;
                boundingRect.y = point.y;
            } else
                boundingRect.add(point);
        }

        // If the mouse was released, the creation of this
        // glyph is complete; otherwise, it should continue.
        return (event.type != SWT.MouseUp);
    }

    /**
     * Process the specified point during section replacement.
     * 
     * @param point
     *            Point in display coordinates to be processed.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate transformer to be used to convert from the display
     *            coordinates to latitude-longitude format.
     */
    protected void processPointForNewSection(Point point, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Add the point to the new section if section replace-
        // ment is being performed.
        if (editType == CHANGE_NODES) {

            // Recalculate the bounding rectangle to ensure
            // that it contains the new point.
            mapToDisplayCoordinates(gc, transformer);
            boundingRect.add(point);

            // Add the point.
            newSectionPoints.addElement(point);
        }
    }

    /**
     * Find out whether the sign of the specified integer is negative or not.
     * 
     * @param value
     *            Value to check.
     * @return True if the integer is non-negative, false if negative.
     */
    protected boolean signOf(int value) {
        return (value >= 0);
    }

    /**
     * Determine whether or not the replacement segment should run from the
     * given node index upwards or downwards.
     * <p>
     * In order to determine which segment to to replace, two points are placed
     * equidistant from the point of intersection. The distances from a point on
     * the new segment to these points is computed. The point that is closer to
     * the new segment will be deleted. Since there is no indication of the
     * direction in which the new segment was drawn a check is made to see if
     * the added point is in the same or the opposite direction in which points
     * are stored (i.e. using their indices).
     * 
     * @param startIndex
     *            Index of the first point of the path to be replaced.
     * @return True if the replacement segment should run from the given node
     *         index upwards or downwards.
     */
    protected boolean shouldReplaceUpperSegment(int startIndex) {

        // If there are only two points on the path, always
        // replace the upper segment no matter what. Other-
        // wise, if the start index is the first or last in-
        // dex, then always replace the lower or upper seg-
        // ment respectively. Otherwise, choose other points
        // on the path as reference points and continue.
        Point pointA = null, pointB = null, pointC = null;
        if (pixelPoints.size() < 3)
            return true;
        else if (startIndex == 0)
            return false;
        else if (startIndex == pixelPoints.size() - 1)
            return true;
        else if ((startIndex == 1) || (startIndex == pixelPoints.size() - 2)) {
            pointA = (Point) pixelPoints.elementAt(startIndex - 1);
            pointB = (Point) pixelPoints.elementAt(startIndex);
            pointC = (Point) pixelPoints.elementAt(startIndex + 1);
        } else {
            pointA = (Point) pixelPoints.elementAt(startIndex - 2);
            pointB = (Point) pixelPoints.elementAt(startIndex);
            pointC = (Point) pixelPoints.elementAt(startIndex + 2);
        }

        // If the points are at the same X position, just
        // return true. This is done to avoid dividing by 0
        // below.
        if (pointA.x == pointC.x)
            return true;

        // Create two points equidistant from the starting
        // point.
        Point nextPoint = (Point) newSectionPoints.elementAt(newSectionPoints
                .size() / 4);
        float dd = 9;
        Point point1 = new Point(0, 0);
        Point point2 = new Point(0, 0);
        float slop = (float) (pointA.y - pointC.y)
                / (float) (pointA.x - pointC.x);
        float cons = dd / (float) Math.sqrt((slop * slop) + 1);
        point1.x = pointB.x + (int) cons;
        point2.x = pointB.x - (int) cons;
        point1.y = (int) (slop * (point1.x - pointB.x)) + pointB.y;
        point2.y = (int) (slop * (point2.x - pointB.x)) + pointB.y;

        // Compute distances from another point on the new
        // line segment to these points.
        int del1x = (nextPoint.x - point1.x) * (nextPoint.x - point1.x);
        int del1y = (nextPoint.y - point1.y) * (nextPoint.y - point1.y);
        int del2x = (nextPoint.x - point2.x) * (nextPoint.x - point2.x);
        int del2y = (nextPoint.y - point2.y) * (nextPoint.y - point2.y);
        double dist1 = Math.sqrt((double) (del1x + del1y));
        double dist2 = Math.sqrt((double) (del2x + del2y));

        // Determine in which direction to replace points.
        if (dist1 < dist2)
            return (signOf(point1.x - pointB.x) != signOf(pointA.x - pointB.x));
        else
            return (signOf(point1.x - pointB.x) == signOf(pointA.x - pointB.x));
    }

    /**
     * Remove the range of the specified list from the given starting index to
     * the given ending index, including the start but not the end.
     * 
     * @param list
     *            List from which range is to be removed.
     * @param startIndex
     *            Starting index.
     * @param endIndex
     *            Ending index.
     */
    protected void removeRange(Vector<Coordinate> list, int startIndex,
            int endIndex) {
        for (int j = startIndex; j < endIndex; j++)
            list.removeElementAt(startIndex);
    }

    /**
     * Replace the nodes of the path between the specified starting and ending
     * indices with the new section.
     * 
     * @param startIndex
     *            Index of the first point of the path to be replaced.
     * @param endIndex
     *            Index of the last point of the path to be replaced.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate transformer to be used to convert from the display
     *            coordinates to latitude-longitude format.
     */
    protected void replaceClosedSegment(int startIndex, int endIndex,
            IGraphicsTarget gc, CoordConverter transformer) {

        // See if the segment that is to be replaced is the
        // upper segment, depending upon the direction in
        // which the replacement segment goes after leaving
        // its starting point.
        boolean upper = shouldReplaceUpperSegment(startIndex);

        // If the segment to be replaced is a a single seg-
        // ment within the path, replace just that segment.
        // Otherwise, what needs to be replaced is two sep-
        // erate segments, one at each end of the path. In
        // essence, what will be done is that the new seg-
        // ment will replace one of the segments, and the
        // other one will be deleted.
        if ((upper && (startIndex < endIndex))
                || ((upper == false) && (startIndex > endIndex))) {

            // Delete the old segment.
            removeRange(points,
                    (startIndex < endIndex ? startIndex : endIndex),
                    (startIndex < endIndex ? endIndex : startIndex) + 1);

            // Add the points for the new segment, converting each
            // to lat-long pairs before adding it. Go through the
            // points from first to last if the start index is
            // before the end index; otherwise, go from last to
            // first.
            for (int j = 0; j < newSectionPoints.size(); j++) {
                Coordinate latLong = transformer
                        .convert((Point) newSectionPoints
                                .elementAt(startIndex < endIndex ? j
                                        : newSectionPoints.size() - j - 1));
                points.insertElementAt(latLong,
                        (startIndex < endIndex ? startIndex : endIndex) + j);
            }

            // Do any smoothing of the new segment required.
            if (startIndex > endIndex) {
                int index = startIndex;
                startIndex = endIndex;
                endIndex = index;
            }
            smooth((startIndex > 0 ? startIndex : 1), (endIndex + 1 < points
                    .size() - 1 ? endIndex + 1 : points.size() - 1), gc,
                    transformer);
        } else {

            // Remember how many nodes from the end the ending
            // index is before replacing one end.
            int howManyNodesToEndIndex = 0;
            if (startIndex < endIndex)
                howManyNodesToEndIndex = points.size() - endIndex;

            // Replace the segment at the end to which the start
            // index is closer first.
            replaceOpenSegment(startIndex, gc, transformer);

            // Delete the other segment.
            if (startIndex < endIndex)
                removeRange(points, points.size() - howManyNodesToEndIndex,
                        points.size());
            else
                removeRange(points, 0, endIndex);

            // Ensure that the path looks like it is closed,
            // i.e. that it starts and ends with the same
            // point.
            if (points.elementAt(0).equals(points.elementAt(points.size() - 1)) == false)
                points.addElement((Coordinate) (points.elementAt(0)).clone());
        }
    }

    /**
     * Replace all points of the path from specified index to whichever end of
     * the path makes the most sense with the new section.
     * 
     * @param startIndex
     *            Index of the first point of the path to be replaced.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate transformer to be used to convert from the display
     *            coordinates to latitude-longitude format.
     */
    protected void replaceOpenSegment(int startIndex, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Replace whichever segment should be replaced.
        if (shouldReplaceUpperSegment(startIndex)) {

            // Remove the old points.
            removeRange(points, startIndex, points.size());

            // Iterate through the new section, appending each
            // point to the path after converting it to a lat-
            // long pair.
            for (int j = 0; j < newSectionPoints.size(); j++) {
                Coordinate latLong = transformer
                        .convert((Point) newSectionPoints.elementAt(j));
                points.addElement(latLong);
            }

            // Do any smoothing of the new segment required.
            smooth((startIndex > 0 ? startIndex : 1), points.size() - 1, gc,
                    transformer);
        } else {

            // Remove the old points.
            removeRange(points, 0, startIndex);

            // Iterate through the new section in reverse,
            // inserting each point into the path after con-
            // verting it to a lat-long pair.
            for (int j = 0; j < newSectionPoints.size(); j++) {
                Coordinate latLong = transformer
                        .convert((Point) newSectionPoints
                                .elementAt(newSectionPoints.size() - j - 1));
                points.insertElementAt(latLong, j);
            }

            // Do any smoothing of the new segment required.
            smooth(1, (startIndex + 1 < points.size() - 1 ? startIndex + 1
                    : points.size() - 1), gc, transformer);
        }
    }

    /**
     * Smooth the specified section of the path using the appropriate smoothing
     * algorithm.
     * 
     * @param startIndex
     *            Starting index from which to smooth.
     * @param endIndex
     *            Ending index up to which to smooth.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter to be used.
     */
    protected void smooth(int startIndex, int endIndex, IGraphicsTarget gc,
            CoordConverter transformer) {
        if (smoothingType == SMOOTHING_NONE)
            return;
        else if (smoothingType == SMOOTHING_AVERAGE)
            smoothAverage(startIndex, endIndex);
        else
            smoothBicubic(startIndex, endIndex, gc, transformer);
    }

    /**
     * Smooth out the path using the averaging algorithm between the two
     * specified node indices, including the starting index but excluding the
     * ending index.
     * 
     * @param startIndex
     *            Starting index from which to smooth.
     * @param endIndex
     *            Ending index up to which to smooth.
     */
    protected void smoothAverage(int startIndex, int endIndex) {

        // If no smoothing is to be done, return.
        if (smoothness == 0)
            return;

        // The display coordinates will need to be
        // recalculated later.
        flushDisplayCoordinates();

        // Copy the list of the path's defining
        // points. These will be used as the base
        // from which each point is smoothed.
        int numPoints = points.size();
        Coordinate[] oldPoints = new Coordinate[numPoints];
        points.copyInto(oldPoints);

        // Smoothing is performed by averaging each
        // point with the points to either side of
        // it. The number of points referenced to
        // each side is specified by the passed-in
        // parameter.
        for (int i = startIndex; i < endIndex; i++) {
            Coordinate point = new Coordinate();

            // Start with the original value of the
            // point being smoothed.
            int count = 1;
            point.x = oldPoints[i].x;
            point.y = oldPoints[i].y;

            // Include points before this point in
            // the calculation.
            for (int j = 1; (j <= smoothness) && (i - j >= 0); j++) {
                point.x += oldPoints[i - j].x;
                point.y += oldPoints[i - j].y;
                count++;
            }

            // Include points following this point
            // in the calculation.
            for (int j = 1; (j <= smoothness) && (i + j < numPoints); j++) {
                point.x += oldPoints[i + j].x;
                point.y += oldPoints[i + j].y;
                count++;
            }

            // Take the average of all the points,
            // yielding the smoothed value of this
            // point.
            point.x /= count;
            point.y /= count;
            points.setElementAt(point, i);
        }
    }

    /**
     * Smooth the specified section of the path using a bicubic algorithm,
     * including the starting index but excluding the ending index. This should
     * create a smooth line that roughly follows the original path.
     * 
     * @param startIndex
     *            Starting index from which to smooth.
     * @param endIndex
     *            Ending index up to which to smooth.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter to be used.
     */
    protected void smoothBicubic(int startIndex, int endIndex,
            IGraphicsTarget gc, CoordConverter transformer) {

        // Flush old display coordinates and remap them, as
        // these will be used for smoothing.
        flushDisplayCoordinates();
        mapToDisplayCoordinates(gc, transformer);

        // Extend the starting index and ending index from
        // which to smooth.
        int EXTRA = 4;
        if (startIndex - EXTRA < 1)
            startIndex = 1;
        else
            startIndex -= EXTRA;
        if (endIndex + EXTRA >= points.size() - 1)
            endIndex = points.size() - 1;
        else
            endIndex += EXTRA;

        // Set the sampling interval.
        final int INTERVAL = 5;

        // Save the length of the input array.
        int total = pixelPoints.size();
        int buffSize = pixelPoints.size() * 4;

        // Create a final array for the entire line.
        int[] xFinal = new int[buffSize];
        int[] yFinal = new int[buffSize];

        // Create an interim array for the smoothed line.
        int[] xInterim = new int[buffSize];
        int[] yInterim = new int[buffSize];

        // Copy selected points into a new array; this
        // thins out the array to allow better smoothing.
        Point point = (Point) pixelPoints.elementAt(startIndex);
        xInterim[0] = point.x;
        yInterim[0] = point.y;
        int num = 1;
        for (int j = startIndex; j < endIndex; j++) {

            // If the point is outside the boundaries of
            // the points to be smoothed, just copy it
            // into the arrays. Otherwise, if it is far
            // enough from the previous point in either
            // the X or Y direction, copy it into the
            // array. If not copying it to the array,
            // then decrement the end index because there
            // is now one less point.
            point = (Point) pixelPoints.elementAt(j);
            if ((Math.abs(point.x - xInterim[num - 1]) > INTERVAL)
                    || (Math.abs(point.y - yInterim[num - 1]) > INTERVAL)) {
                xInterim[num] = point.x;
                yInterim[num] = point.y;
                num++;
            }
        }

        // Check if there are enough points to perform
        // smoothing; if not, do nothing more.
        if (num < 4)
            return;

        // Copy the points before the area to be smoothed
        // into the final array.
        for (int j = 0; j < startIndex; j++) {
            point = (Point) pixelPoints.elementAt(j);
            xFinal[j] = point.x;
            yFinal[j] = point.y;
        }

        // Smooth the entire line, using bicubic algorithm.
        int start = 0;
        int pts = 8;
        for (int spl = 0; spl < num - 3; spl = spl + 4) {

            // Create smooth curve for this section.
            float x1 = xInterim[spl + 3] - (3 * xInterim[spl + 2])
                    + (3 * xInterim[spl + 1]) - xInterim[spl];
            float x2 = (3 * xInterim[spl + 2]) - (6 * xInterim[spl + 1])
                    + (3 * xInterim[spl]);
            float x3 = (3 * xInterim[spl + 1]) - (3 * xInterim[spl]);
            float x4 = xInterim[spl];
            float y1 = yInterim[spl + 3] - (3 * yInterim[spl + 2])
                    + (3 * yInterim[spl + 1]) - yInterim[spl];
            float y2 = (3 * yInterim[spl + 2]) - (6 * yInterim[spl + 1])
                    + (3 * yInterim[spl]);
            float y3 = (3 * yInterim[spl + 1]) - (3 * yInterim[spl]);
            float y4 = yInterim[spl];
            for (int i = 0; i < pts; i++) {
                float t = ((float) i) / (float) pts;
                float x0 = (x1 * t * t * t) + (x2 * t * t) + (x3 * t) + x4;
                float y0 = (y1 * t * t * t) + (y2 * t * t) + (y3 * t) + y4;

                // Find the connecting point.
                if (i == 0) {
                    for (int m = start; m < start + pts; m++) {
                        if (((int) (x0) == (int) xFinal[startIndex + m])
                                && ((int) (y0) == (int) yFinal[startIndex + m])) {
                            start = m;
                            break;
                        }
                    }
                }
                xFinal[startIndex + start + i - 1] = (int) (x0);
                yFinal[startIndex + start + i - 1] = (int) (y0);

            }

            // Process each section and add to final
            // array.
            float offset1 = (((float) pts) * 0.6666666f) + 0.5f;
            float offset2 = (((float) pts) * 0.25f) + 0.5f;
            xInterim[spl + 3] = xFinal[startIndex + start + (int) offset1];
            yInterim[spl + 3] = yFinal[startIndex + start + (int) offset1];
            xInterim[spl + 2] = xFinal[startIndex + start + (int) offset2];
            yInterim[spl + 2] = yFinal[startIndex + start + (int) offset2];
            spl = spl - 2;

        }

        // Ensure final point is included in the smoothed
        // array.
        int size = start + pts - 1;
        xFinal[startIndex + size] = ((Point) pixelPoints.elementAt(endIndex)).x;
        yFinal[startIndex + size] = ((Point) pixelPoints.elementAt(endIndex)).y;
        size++;

        // Copy the points after the area to be smoothed
        // into the final array.
        for (int j = endIndex; j < total; j++) {
            point = (Point) pixelPoints.elementAt(j);
            xFinal[startIndex + size + (j - endIndex)] = point.x;
            yFinal[startIndex + size + (j - endIndex)] = point.y;
        }

        // Create a new vector of lat-longs to replace the
        // old ones.
        pixelPoints.clear();
        for (int j = 0; j < startIndex + size + (total - endIndex - 1); j++)
            pixelPoints.addElement(new Point(xFinal[j], yFinal[j]));

        // for (int i =0; i<pixelPoints.size(); i++)
        // System.err.println(i+" "+pixelPoints.elementAt(i));

        // Remap to lat-long coordinates.
        mapToLatLongCoordinates(gc, transformer);
        flushDisplayCoordinates();
    }
}
