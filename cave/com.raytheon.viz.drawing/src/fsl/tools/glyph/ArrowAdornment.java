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
import java.awt.geom.GeneralPath;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.viz.adapter.CoordConverter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The arrow adornment class, providing an adornment that looks like an arrow at
 * one end of a glyph.
 * 
 * @author Christopher Golden
 */
public class ArrowAdornment extends StyledAdornment {

    // Protected Variables

    private static final long serialVersionUID = 1L;

    /**
     * Flag indicating whether the arrow should be at the beginning or the end
     * of the glyph.
     */
    protected boolean atEnd;

    /**
     * Angle at the apex of the arrow.
     */
    protected double apex;

    /**
     * Length of the arrow's two arms in display units.
     */
    protected double length;

    /**
     * Three points defining the arrow head; these are display-context
     * sensitive, and thus are transient.
     */
    protected transient Point[] points = null;

    // Public Constructors

    /**
     * Create an instance with a 60 degree apex angle, and arms each about 12
     * display units in length, and position it at the end of the specified
     * glyph.
     * 
     * @param glyph
     *            Glyph decorated by this adornment.
     */
    public ArrowAdornment(MultiPointGlyph glyph) {
        super(glyph);
        atEnd = true;
        apex = Math.PI / 3.0;
        length = 12.0;
    }

    /**
     * Create an instance with the specified apex angle and length for its arms,
     * and position it at the specified end of the specified glyph.
     * 
     * @param glyph
     *            Glyph decorated by this adornment.
     * @param color
     *            Color to be used when painting this adornment.
     * @param atEnd
     *            Flag indicating whether the arrow should be at the start or
     *            the end of the glyph it decorates.
     * @param apex
     *            Apex angle, specified in radians.
     * @param length
     *            Length in display units of the arms.
     */
    public ArrowAdornment(MultiPointGlyph glyph, Color color, boolean atEnd,
            double apex, double length) {
        super(glyph, color);
        this.atEnd = atEnd;
        this.apex = apex;
        this.length = length;
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

        // These are equal only if the superclass says so
        // and the end flags, apex angles, and lengths
        // are the same.
        return (super.equals(obj) && (obj instanceof ArrowAdornment)
                && (atEnd == ((ArrowAdornment) obj).atEnd)
                && (apex == ((ArrowAdornment) obj).apex) && (length == ((ArrowAdornment) obj).length));
    }

    /**
     * Get the hash code of this object.
     * 
     * @return Hash code of this object.
     */
    public int hashCode() {

        // Use the hash code of the superclass combined
        // with those of the end flag, apex angle, and
        // length.
        return (int) ((((long) super.hashCode())
                + ((long) (new Boolean(atEnd)).hashCode())
                + ((long) (new Double(apex)).hashCode()) + ((long) (new Double(
                length)).hashCode())) % (long) Integer.MAX_VALUE);
    }

    /**
     * Get the length of the arrow.
     * 
     * @return Length of the arrow.
     */
    public double getLength() {
        return length;
    }

    /**
     * Translate the adornment by the specified deltas.
     * 
     * @param xDelta
     *            X delta by which to translate the adornment.
     * @param yDelta
     *            Y delta by which to translate the adornment.
     */
    public void translateBy(int xDelta, int yDelta) {
        super.translateBy(xDelta, yDelta);
        for (int j = 0; j < points.length; j++)
            points[j].translate(xDelta, yDelta);
    }

    // Protected Methods

    /**
     * Create a clone of this adornment.
     * 
     * @return The new clone of this adornment.
     */
    protected Object clone() {
        ArrowAdornment adornment = null;
        try {
            adornment = (ArrowAdornment) super.clone();
            adornment.atEnd = atEnd;
            adornment.apex = apex;
            adornment.length = length;
            adornment.points = null;
        } catch (Exception e) {
            // Logger.logBug("Clone error.", e);
            e.printStackTrace();
        }
        return adornment;
    }

    /**
     * Create the shape for the adornment.
     * 
     * @param gc
     *            Graphics context in which this adornment's glyph is drawn.
     * @param transformer
     *            Coordinate converter to be used to convert from to display
     *            coordinates from lat-long pairs.
     */
    protected void createShape(IGraphicsTarget gc, CoordConverter transformer) {

        // Determine the point at whichever end of the
        // glyph is appropriate where the arrow will
        // connect with the glyph, and then get the point
        // that is the same distance away from the first
        // point as the length of the arms. If this is
        // longer than the glyph's total length, just get
        // the other endpoint. From the two points, the
        // bearing angle may be calculated, which will
        // determine which way the arrow should be
        // pointing.
        double glyphLength = glyph.getLength(gc, transformer);
        points = new Point[3];
        points[1] = glyph.getPointAtDistanceAlong((atEnd ? glyphLength : 0.0),
                gc, transformer);
        Point otherPoint = null;
        if (glyph.getLength(gc, transformer) < length)
            otherPoint = glyph.getPointAtDistanceAlong((atEnd ? 0.0
                    : glyphLength), gc, transformer);
        else
            otherPoint = glyph.getPointAtDistanceAlong((atEnd ? glyphLength
                    - length : length), gc, transformer);

        // If the slope is vertical, calculate the
        // bearing angle manually to avoid divide by 0
        // problems; otherwise, use atan().
        double bearing;
        if (points[1].x == otherPoint.x)
            bearing = (points[1].y > otherPoint.y ? Math.PI * 1.5
                    : Math.PI * 0.5);
        else {
            bearing = Math.atan(((double) otherPoint.y - points[1].y)
                    / ((double) otherPoint.x - points[1].x));
            if (otherPoint.x - points[1].x < 0)
                bearing += Math.PI;
        }

        // Calculate the bearing angles from the apex of
        // the arrowhead to its other two points.
        points[0] = new Point((int) (length * Math.cos(bearing - (apex / 2.0)))
                + points[1].x,
                (int) (length * Math.sin(bearing - (apex / 2.0))) + points[1].y);

        points[2] = new Point((int) (length * Math.cos(bearing + (apex / 2.0)))
                + points[1].x,
                (int) (length * Math.sin(bearing + (apex / 2.0))) + points[1].y);

        // Create a path and add the points calculated
        // above to it.
        GeneralPath path = new GeneralPath(GeneralPath.WIND_NON_ZERO);
        path.moveTo(points[0].x, points[0].y);
        for (int j = 1; j < 3; j++)
            path.lineTo(points[j].x, points[j].y);
        shape = path;
        rawShape = path;

        // Create the final styled shape from the path.
        styleShape(BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER);
    }

    /*
     * (non-Javadoc)
     * 
     * @see fsl.tools.glyph.Adornment#prepareShape(com.raytheon.viz.core.drawables.IWireframeShape,
     *      com.raytheon.viz.core.drawables.IShadedShape,
     *      com.raytheon.viz.adapter.CoordConverter)
     */
    @Override
    public void prepareShape(IWireframeShape wshape, IShadedShape sshape,
            CoordConverter transformer) {
        Coordinate[] c = new Coordinate[3];
        for (int j = 0; j < points.length; j++) {
            c[j] = transformer.convert(points[j]);

        }

        wshape.addLineSegment(c);

    }

    // /**
    // * Add the adornment to the specified DGM array. It
    // * is safe to assume that <code>createShape()</code>
    // * has been executed prior to the calling of this
    // * method.
    // *
    // * @param dgm DGM array in which to place the
    // * translated adornment.
    // * @param transformer Coordinate converter to be used
    // * to convert from the display
    // * coordinates to lat-long pairs.
    // * @param cartesian Flag indicating whether or not
    // * the conversion should utilize
    // * Cartesian instead of lat-long
    // * coordinates. A 1024 by 1024
    // * coordinate space is assumed if
    // * this flag is true.
    // */
    // protected void addToDGM(DGMArray dgm, CoordConverter transformer,
    // boolean cartesian) {
    //
    // // Let the superclass set up the line style.
    // super.addToDGM(dgm, transformer, cartesian);
    //
    // // Draw the arrow as a set of linked vectors.
    // short[] xCoords = new short[3];
    // short[] yCoords = new short[3];
    // for (int j = 0; j < points.length; j++) {
    // LatLong latLong = transformer.convert(points[j]);
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
    // // Add the path as a set of linked vectors in the
    // // DGM file.
    // dgm.addLinkedVectors((short) 3, xCoords, yCoords);
    // }
    //
    // /**
    // * Add the adornment to the specified Extended DGM
    // * array. It is safe to assume that <code>
    // * createShape()</code> has been executed prior to
    // * the calling of this method.
    // *
    // * @param dgm Extended DGM array in which to
    // * place the translated adornment.
    // * @param transformer Coordinate converter to be used
    // * to convert from the display
    // * coordinates to lat-long pairs.
    // * @param cartesian Flag indicating whether or not
    // * the conversion should utilize
    // * Cartesian instead of lat-long
    // * coordinates. A 1024 by 1024
    // * coordinate space is assumed if
    // * this flag is true.
    // */
    // protected void addToExtendedDGM(DGMArray dgm, CoordConverter transformer,
    // boolean cartesian) {
    // addToDGM(dgm, transformer, cartesian);
    // }
}
