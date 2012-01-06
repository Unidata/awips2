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
import java.util.ArrayList;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.viz.adapter.CoordConverter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * The triangle edge adornment, providing an adornment that decorates a glyph
 * with triangular projections along its length.
 * 
 * @author Christopher Golden
 */
public class TriangleEdgeAdornment extends EdgeAdornment {

    // Protected Classes

    private static final long serialVersionUID = 1L;

    /**
     * Triangle decoration that is placed at intervals along the length of the
     * glyph being adorned.
     */
    protected class Triangle implements Decoration {

        // Protected Variables

        private static final long serialVersionUID = 1L;

        /**
         * Shape of the triangle.
         */
        protected Shape shape;

        /**
         * Array of coordinates defining the triangle, the first subarray being
         * X coordinates, the second being Y coordinates.
         */
        protected short[][] coords = new short[2][3];

        // Public Constructors

        /**
         * Construct an instance that runs along the specified section of a
         * glyph.
         * 
         * @param glyphPath
         *            Section of the glyph along which the triangle should run.
         */
        public Triangle(Point[] glyphPath) {

            // Get the starting and ending points of the
            // triangle.
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

            // Get the distance between the two points,
            // and the midpoint between them as well.
            double distance = Math
                    .sqrt(((endPoint.x - startPoint.x) * (endPoint.x - startPoint.x))
                            + ((endPoint.y - startPoint.y) * (endPoint.y - startPoint.y))) * 2.0 / 3.0;
            Point midPoint = new Point((endPoint.x + startPoint.x) / 2,
                    (endPoint.y + startPoint.y) / 2);

            // Find the apex point of the triangle by
            // using a perpendicular angle to the bear-
            // ing angle already calculated. Record
            // the three points defining the triangle.
            double perp = bearing + (Math.PI / (rightSide ? 2.0 : -2.0));
            coords[0][0] = (short) startPoint.x;
            coords[1][0] = (short) startPoint.y;
            coords[0][1] = (short) (((int) (distance * Math.cos(perp))) + midPoint.x);
            coords[1][1] = (short) (((int) (distance * Math.sin(perp))) + midPoint.y);
            coords[0][2] = (short) endPoint.x;
            coords[1][2] = (short) endPoint.y;

            // Create a path and add the points just
            // calculated above to it. This yields
            // the visual representation of the tri-
            // angle. Close the triangle if it is
            // meant to be solid.
            GeneralPath shape = new GeneralPath(GeneralPath.WIND_NON_ZERO);
            shape.moveTo(coords[0][0], coords[1][0]);
            for (int j = 1; j < 3; j++)
                shape.lineTo(coords[0][j], coords[1][j]);
            if (decorationsAreSolid()) {
                for (int j = glyphPath.length - 2; j > 0; j--)
                    shape.lineTo(glyphPath[j].x, glyphPath[j].y);
                shape.closePath();
            }
            this.shape = shape;
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
            return new Point(coords[0][1], coords[1][1]);
        }

        // /**
        // * Convert this decoration to DGM format and place
        // * the result in the supplied byte array.
        // *
        // * @param dgm DGM array in which to place
        // * the translated decoration.
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
        // public void toDGM(DGMArray dgm, CoordConverter transformer, boolean
        // cartesian) {
        //
        // // Draw the triangle as a set of linked vec-
        // // tors.
        // short[] xCoords = new short[3];
        // short[] yCoords = new short[3];
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
        // dgm.addLinkedVectors((short) 3, xCoords, yCoords);
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
        // // Draw the triangle as a set of linked vectors
        // // or a polygon, depending upon whether it is
        // // filled or not.
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
        // Coordinate currentPoint = null;
        // Point thisPoint = null;
        // float[] coords = new float[6];
        // ArrayList<Coordinate> points = new ArrayList<Coordinate>();
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
        // Coordinate latLong = points.get(i);
        // if (cartesian) {
        // Point point = transformer.convert(latLong, false);
        // xCoords[i] = (short) point.x;
        // yCoords[i] = (short) point.y;
        // } else {
        // xCoords[i] = (short) ((((double) latLong.x) * 60.0) + 0.5);
        // yCoords[i] = (short) ((((double) latLong.y) * -60.0) + 0.5);
        // }
        // }
        //
        // // Add the points as a polygon.
        // dgm.addPolygonEx((short) xCoords.length, xCoords, yCoords);
        // } else {
        // short[] xCoords = new short[3];
        // short[] yCoords = new short[3];
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
        // dgm.addLinkedVectors((short) 3, xCoords, yCoords);
        // }
        // }

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
     * Flag indicating whether or not the triangles should be drawn as solid.
     */
    protected boolean solidTriangles = false;

    // Public Constructors

    /**
     * Create an instance associated with the specified glyph with a color of
     * black, triangles on the left, 10 display units between each triangle, 10
     * units between each triangle's starting and ending points, 0 offset, and
     * hollow triangles.
     * 
     * @param glyph
     *            Glyph decorated by this adornment.
     */
    public TriangleEdgeAdornment(MultiPointGlyph glyph) {
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
     *            triangle.
     * @param solidTriangles
     *            Flag indicating whether or not the triangles should be painted
     *            solid.
     */
    public TriangleEdgeAdornment(MultiPointGlyph glyph, Color color,
            boolean rightSide, double intervalBetween, double intervalWithin,
            double offsetForFirst, boolean solidTriangles) {
        super(glyph, color, rightSide, intervalBetween, intervalWithin,
                offsetForFirst);
        this.solidTriangles = solidTriangles;
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
        // so, they are both of the same class, and they
        // have the same fill flag.
        return (super.equals(obj) && (obj instanceof TriangleEdgeAdornment) && (solidTriangles == ((TriangleEdgeAdornment) obj).solidTriangles));
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
        return solidTriangles;
    }

    /**
     * Get a decoration to be attached to the adornment at along the specified
     * path.
     * 
     * @param glyphPath
     *            Section of the glyph along which the decoration should run.
     */
    protected Decoration getDecorationFor(Point[] glyphPath) {
        return new Triangle(glyphPath);
    }
}
