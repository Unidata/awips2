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
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.FlatteningPathIterator;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.util.ArrayList;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.viz.adapter.CoordConverter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * The arc edge adornment, providing an adornment that decorates a glyph with
 * arc projections along its length.
 * 
 * @author Christopher Golden
 */
public class ArcEdgeAdornment extends EdgeAdornment {

    // Protected Static Constants

    private static final long serialVersionUID = 1L;

    /**
     * X coordinates of points at 30 degree intervals on the boundary of a
     * circle of radius 1 from 0 to 90 degrees. These are used to create
     * pseudo-arcs when translating arc decorations to DGM format. Y coordinates
     * of these points are derived by using these coordinates in reverse order.
     */
    protected static final double[] CIRCLE_COORDS = { 1.0, 0.866025403784, 0.5,
            0.0 };

    // Protected Classes

    /**
     * Arc decoration that is placed at intervals along the length of the glyph
     * being adorned.
     */
    protected class Arc implements Decoration {

        // Protected Variables

        private static final long serialVersionUID = 1L;

        /**
         * Shape of the arc.
         */
        protected Shape shape;

        /**
         * Array of points defining the arc.
         */
        protected short[][] coords = new short[2][8];

        // Public Constructors

        /**
         * Construct an instance that runs along the specified section of a
         * glyph.
         * 
         * @param glyphPath
         *            Section of the glyph along which the arc should run.
         */
        public Arc(Point[] glyphPath) {

            // Get the starting and ending points of the
            // arc.
            Point startPoint = glyphPath[0];
            Point endPoint = glyphPath[glyphPath.length - 1];

            // If the slope is vertical, calculate the
            // bearing angle manually to avoid divide by
            // 0 problems; otherwise, use atan().
            double bearing;
            if (startPoint.x == endPoint.x)
                bearing = (startPoint.y > endPoint.y ? Math.PI * 1.5
                        : Math.PI * 0.5);
            else {
                bearing = Math.atan(((double) endPoint.y - startPoint.y)
                        / ((double) endPoint.x - startPoint.x));
                if (endPoint.x - startPoint.x < 0)
                    bearing += Math.PI;
            }

            // Calculate the starting angle and the
            // swept angle from the bearing.
            double startAngle = bearing * 180.0 / Math.PI;
            double sweepAngle = (rightSide ? -180.0 : 180.0);

            // Because the coordinate system for draw-
            // ing purposes defines greater values of
            // y as lower onscreen than lesser values
            // (i.e. the opposite of the standard
            // Cartesian coordinate system), the start-
            // ing angle must be adjusted.
            if (startAngle < 180.0)
                startAngle += (180.0 - startAngle) * 2.0;
            else if (startAngle > 180.0)
                startAngle += (360.0 - startAngle) * 2.0;
            startAngle %= 360.0;

            // Get the midpoint between the two points
            // to serve as the center of the arc, and
            // the radius of the arc as well.
            Point2D.Double center = new Point2D.Double(
                    (endPoint.x + startPoint.x) / 2.0,
                    (endPoint.y + startPoint.y) / 2.0);
            double radius = Math
                    .sqrt(((endPoint.x - center.x) * (endPoint.x - center.x))
                            + ((endPoint.y - center.y) * (endPoint.y - center.y)));

            // Now the DGM coordinates must be derived.
            // Since the DGM format does not allow for
            // arcs, an arc must be translated by conver-
            // ting it to an approximation of an arc
            // formed of straight line segments. The
            // segments join up points calculated to be
            // lying along the true arc at 30 degree
            // intervals. Note that if the arc is to be
            // filled, it will also use these coordinates
            // instead of an actual Arc object. Start
            // with the the end point instead of the
            // start point if the swept angle of the arc
            // is not negative.
            coords[0][0] = (short) (sweepAngle < 0.0 ? startPoint.x
                    : endPoint.x);
            coords[1][0] = (short) (sweepAngle < 0.0 ? startPoint.y
                    : endPoint.y);

            // Figure out what angle with which to start.
            // Calculate this based upon the start angle
            // being either the given start angle, or
            // the opposite of the given start angle if
            // the swept angle is -180 degrees. The
            // calculation must yield the first angle at
            // 30 degree intervals that is greater than
            // the start angle. Also determine the last
            // angle of the arc.
            int firstAngle = (((short) (startAngle + 0.5)) + (sweepAngle < 0.0 ? 180
                    : 0)) % 360;
            int angle = ((firstAngle / 30) + 1) * 30;
            int stopAngle = firstAngle + 180;

            // Iterate through the angles in between at
            // 30 degree intervals, calculating a point
            // along the arc at each interval.
            int j;
            for (j = 1; angle < stopAngle; j++, angle += 30) {

                // Determine what multiplier to apply to
                // the radius when adding the latter to
                // the X and Y coordinates.
                int adjAngle = angle % 180;
                int index = (adjAngle <= 90 ? adjAngle : 180 - adjAngle) / 30;
                adjAngle = angle % 360;

                // Calculate the point on the circle by
                // adding the center to the radius, ad-
                // justing the latter by multiplying it
                // by the appropriate multiplier.
                coords[0][j] = (short) (center.x + 0.5);
                coords[0][j] += (short) ((radius * CIRCLE_COORDS[index] * ((adjAngle > 90)
                        && (adjAngle < 270) ? -1.0 : 1.0)) + 0.5);
                coords[1][j] = (short) (center.y + 0.5);
                coords[1][j] += (short) ((radius * CIRCLE_COORDS[3 - index] * (adjAngle > 180 ? 1.0
                        : -1.0)) + 0.5);
            }

            // Extract and translate the last point.
            // Use the start point instead of the end
            // point if the swept angle of the arc is
            // not negative.
            coords[0][j] = (short) (sweepAngle < 0.0 ? endPoint.x
                    : startPoint.x);
            coords[1][j] = (short) (sweepAngle < 0.0 ? endPoint.y
                    : startPoint.y);

            // Copy the coordinates to a new array so
            // that only the coordinates calculated will
            // be used.
            short[][] oldCoords = coords;
            coords = new short[2][j + 1];
            for (j = 0; j < coords.length; j++)
                System.arraycopy(oldCoords[j], 0, coords[j], 0,
                        coords[j].length);

            // Create the arc shape if the arcs are not
            // meant to be filled; otherwise, create a
            // pseudo-arc using the above-calculated
            // coordinates.
            if (decorationsAreSolid()) {
                GeneralPath shape = new GeneralPath(GeneralPath.WIND_NON_ZERO);
                shape.moveTo(coords[0][0], coords[1][0]);
                for (j = 1; j < coords[0].length; j++)
                    shape.lineTo(coords[0][j], coords[1][j]);

                // Add in the section of the glyph; if
                // the arc started at the starting
                // point, add it backwards; otherwise,
                // add it frontwards.
                if (sweepAngle < 0.0)
                    for (j = glyphPath.length - 2; j > 0; j--)
                        shape.lineTo(glyphPath[j].x, glyphPath[j].y);
                else
                    for (j = 1; j < glyphPath.length - 1; j++)
                        shape.lineTo(glyphPath[j].x, glyphPath[j].y);

                // Close the path.
                shape.closePath();
                this.shape = shape;
            } else {
                GeneralPath shape = new GeneralPath(GeneralPath.WIND_NON_ZERO);
                if (rightSide)
                    shape.moveTo(coords[0][0], coords[1][0]);
                else
                    shape.moveTo(coords[0][coords[0].length - 1],
                            coords[1][coords[1].length - 1]);
                for (j = (rightSide ? 1 : coords[0].length - 2); (rightSide && (j < coords[0].length))
                        || ((rightSide == false) && j >= 0); j += (rightSide ? 1
                        : -1))
                    shape.lineTo(coords[0][j], coords[1][j]);
                this.shape = shape;
                /*
                 * THIS CODE WORKED WELL EXCEPT FOR MULTIPOINTGLYPHS THAT ARE
                 * NOT DRAWN BUT ARE FILLED BY THEIR ADORN- MENTS' SHAPES. shape =
                 * new Arc2D.Double(center.x - radius, center.y - radius, radius *
                 * 2, radius * 2, startAngle, sweepAngle, Arc2D.OPEN);
                 */
            }
        }

        // Public Methods

        /**
         * Get the shape that describes this decoration.
         */
        public Shape getShape(CoordConverter transformer) {
            return shape;
        }

        /**
         * Get the apex point of this decoration.
         * 
         * @param transformer
         *            Coordinate converter to be used to convert to the display
         *            coordinates from lat-long pairs.
         * @return Apex point of this decoration.
         */
        public Point getApex(CoordConverter transformer) {
            return new Point(
                    (int) (((coords[0][3] + coords[0][4]) / 2.0) + 0.5),
                    (int) (((coords[1][3] + coords[1][4]) / 2.0) + 0.5));
        }

        public void prepareShape(IWireframeShape wshape, IShadedShape sshape,
                CoordConverter transformer) {
            // Draw the arc as a set of linked vectors or
            // a polygon, depending upon whether the arc
            // is filled or not.
            if (decorationsAreSolid()) {
                // Get a flattened path describing the
                // shape, and convert each point along
                // the path to lat-longs.
                FlatteningPathIterator iterator = new FlatteningPathIterator(
                        shape.getPathIterator(null), 2.0, 4);
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

                // Translate the lat-long coordinates to
                // DGM coordinates.
                Coordinate[] coordArr = new Coordinate[points.size()];

                for (int i = 0; i < points.size(); i++) {
                    coordArr[i] = points.get(i);
                }
                // Add the points as a polygon.
                GeometryFactory gf = new GeometryFactory();
                LineString ls = gf.createLineString(coordArr);
                sshape.addPolygon(new LineString[] { ls }, new RGB(color
                        .getRed(), color.getGreen(), color.getBlue()));

            } else {
                Coordinate[] coordArr = new Coordinate[coords[0].length];
                for (int j = 0; j < coords[0].length; j++) {
                    coordArr[j] = transformer.convert(new Point(coords[0][j],
                            coords[1][j]));

                }
                wshape.addLineSegment(coordArr);
            }
        }

        /**
         * Convert this decoration to DGM format and place the result in the
         * supplied byte array.
         * 
         * @param dgm
         *            DGM array in which to place the translated decoration.
         * @param transformer
         *            Coordinate converter to be used to convert from the
         *            display coordinates to lat-long pairs.
         * @param cartesian
         *            Flag indicating whether or not the conversion should
         *            utilize Cartesian instead of lat-long coordinates. A 1024
         *            by 1024 coordinate space is assumed if this flag is true.
         */
        // public void toDGM(DGMArray dgm, CoordConverter transformer, boolean
        // cartesian) {
        //
        // // Draw the arc as a set of linked vectors.
        // short[] xCoords = new short[coords[0].length];
        // short[] yCoords = new short[coords[1].length];
        // for (int j = 0; j < coords[0].length; j++) {
        // Coordinate latLong = transformer.convert(new Point(coords[0][j],
        // coords[1][j]));
        // if (cartesian) {
        // Point point = transformer.convert(latLong, false);
        // xCoords[j] = (short) point.x;
        // yCoords[j] = (short) point.y;
        // } else {
        // xCoords[j] = (short) ((((double) latLong.x) * 60.0) + 0.5);
        // yCoords[j] = (short) ((((double) latLong.y) * -60.0) + 0.5);
        // }
        // }
        // dgm.addLinkedVectors((short) xCoords.length, xCoords, yCoords);
        // }
        //
        // /**
        // * Convert this decoration to Extended DGM format
        // * and place the result in the supplied byte array.
        // *
        // * @param dgm Extended DGM array in which
        // * to place the translated
        // * decoration.
        // * @param transformer Coordinate converter to be
        // * used to convert from the
        // * display coordinates to
        // * lat-long pairs.
        // * @param cartesian Flag indicating whether or
        // * not the conversion should
        // * utilize Cartesian instead of
        // * lat-long coordinates. A 1024
        // * by 1024 coordinate space is
        // * assumed if this flag is
        // * true.
        // */
        // public void toExtendedDGM(DGMArray dgm, CoordConverter transformer,
        // boolean cartesian) {
        //
        // // Draw the arc as a set of linked vectors or
        // // a polygon, depending upon whether the arc
        // // is filled or not.
        // if (decorationsAreSolid()) {
        //
        // // Set the fill color.
        // dgm.setFillColorEx(color);
        //			
        // // Get a flattened path describing the
        // // shape, and convert each point along
        // // the path to lat-longs.
        // FlatteningPathIterator iterator =
        // new FlatteningPathIterator(shape.getPathIterator(null), 2.0, 4);
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
        // // Translate the lat-long coordinates to
        // // DGM coordinates.
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
        // // Add the points as a polygon.
        // dgm.addPolygonEx((short) xCoords.length, xCoords, yCoords);
        // } else {
        // short[] xCoords = new short[coords[0].length];
        // short[] yCoords = new short[coords[1].length];
        // for (int j = 0; j < coords[0].length; j++) {
        // LatLong latLong = transformer.convert(new Point(coords[0][j],
        // coords[1][j]));
        // if (cartesian) {
        // Point point = transformer.convert(latLong, false);
        // xCoords[j] = (short) point.x;
        // yCoords[j] = (short) point.y;
        // } else {
        // xCoords[j] = (short) ((((double) latLong.lon) * 60.0) + 0.5);
        // yCoords[j] = (short) ((((double) latLong.lat) * -60.0) + 0.5);
        // }
        // }
        // dgm.addLinkedVectors((short) xCoords.length, xCoords, yCoords);
        // }
        // }
        /**
         * Translate the decoration by the specified deltas.
         * 
         * @param xDelta
         *            X delta by which to translate the decoration.
         * @param yDelta
         *            Y delta by which to translate the decoration.
         */
        public void translateBy(int xDelta, int yDelta) {

            // Translate the coordinates, then the shape.
            for (int j = 0; j < coords.length; j++)
                for (int k = 0; k < coords[j].length; k++)
                    coords[j][k] += (j == 0 ? xDelta : yDelta);
            AffineTransform at = new AffineTransform();
            at.translate(xDelta, yDelta);
            shape = at.createTransformedShape(shape);
        }

    }

    // Protected Variables

    /**
     * Flag indicating whether or not the arcs should be drawn as solid.
     */
    protected boolean solidArcs = true;

    // Public Constructors

    /**
     * Create an instance associated with the specified glyph with a color of
     * black, arcs on the left, 10 display units between each arc, 10 units
     * between each arc's starting and ending points, 0 offset of the first arc,
     * and hollow arcs.
     * 
     * @param glyph
     *            Glyph decorated by this adornment.
     */
    public ArcEdgeAdornment(MultiPointGlyph glyph) {
        super(glyph);
    }

    /**
     * Create an instance associated with the specified glyph with the specified
     * color, orientation of decorations, space between each decoration, space
     * within each decoration, offset, and fill.
     * 
     * @param glyph
     *            Glyph decorated by this adornment.
     * @param color
     *            Color to be used when painting this adornment.
     * @param rightSide
     *            Flag indicating whether the decorations are to be on the left
     *            or the right side.
     * @param intervalBetween
     *            Distance along the glyph between one decoration and the next.
     * @param intervalWithin
     *            Distance along the glyph between the connection points of a
     *            decoration.
     * @param offsetForFirst
     *            Offset into the glyph's length at which to start the first
     *            arc.
     * @param solidArcs
     *            Flag indicating whether or not the arcs should be solid.
     */
    public ArcEdgeAdornment(MultiPointGlyph glyph, Color color,
            boolean rightSide, double intervalBetween, double intervalWithin,
            double offsetForFirst, boolean solidArcs) {
        super(glyph, color, rightSide, intervalBetween, intervalWithin,
                offsetForFirst);
        this.solidArcs = solidArcs;
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

        // These are equal only if the superclass says
        // so, they are both of the same class, and the
        // solid flags are the same.
        return (super.equals(obj) && (obj instanceof ArcEdgeAdornment) && (solidArcs == ((ArcEdgeAdornment) obj).solidArcs));
    }

    // Protected Methods

    /**
     * Determine whether or not the decorations along the edge should be filled
     * or hollow. If hollow, they will be styled with the parent glyph's
     * thickness and line style.
     * 
     * @return True if the decorations should be filled, false otherwise.
     */
    protected boolean decorationsAreSolid() {
        return solidArcs;
    }

    /**
     * Get a decoration to be attached to the adornment at along the specified
     * path.
     * 
     * @param glyphPath
     *            Section of the glyph along which the decoration should run.
     */
    protected Decoration getDecorationFor(Point[] glyphPath) {
        return new Arc(glyphPath);
    }

}
