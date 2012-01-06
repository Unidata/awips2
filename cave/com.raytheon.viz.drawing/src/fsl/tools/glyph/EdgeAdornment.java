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
import java.awt.Shape;
import java.awt.geom.GeneralPath;
import java.io.Serializable;
import java.util.Vector;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.viz.adapter.CoordConverter;

/**
 * The edge adornment, an abstract base class from which adornments that consist
 * of decorations running along the length of a glyph may be derived.
 * 
 * @author Christopher Golden
 */
public abstract class EdgeAdornment extends StyledAdornment {

    // Protected Classes

    /**
     * Interface describing the methods that must be implemented by a class that
     * is to encapsulate the decorations that run along an edge adornment.
     */
    protected interface Decoration extends Serializable {

        // Public Methods

        /**
         * Get the shape that describes this decoration.
         * 
         * @param transformer
         *            Coordinate converter to be used to convert to the display
         *            coordinates from lat-long pairs.
         * @return Shape of this decoration.
         */
        public Shape getShape(CoordConverter transformer);

        /**
         * Get the apex point of this decoration.
         * 
         * @param transformer
         *            Coordinate converter to be used to convert to the display
         *            coordinates from lat-long pairs.
         * @return Apex point of this decoration.
         */
        public Point getApex(CoordConverter transformer);

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
        // cartesian);
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
        // boolean cartesian);

        /**
         * Translate the decoration by the specified deltas.
         * 
         * @param xDelta
         *            X delta by which to translate the decoration.
         * @param yDelta
         *            Y delta by which to translate the decoration.
         */
        public void translateBy(int xDelta, int yDelta);

        public void prepareShape(IWireframeShape wshape, IShadedShape sshape,
                CoordConverter converter);

    }

    // Protected Variables

    /**
     * Flag indicating whether the decorations should be on the left or the
     * right side of the line. Which side is which is determined by following
     * the glyph along its length from its starting point to its ending point.
     */
    protected boolean rightSide;

    /**
     * Interval between decorations, measuring how much of the glyph's length
     * should exist between the ending point of one decoration and the starting
     * point of the next.
     */
    protected double intervalBetween;

    /**
     * Interval between the starting and ending points of each decoration,
     * measuring how much of the glyph's length should exist between each
     * decoration's starting and ending points.
     */
    protected double intervalWithin;

    /**
     * Offset into glyph's length at which to start the decorations. If this is
     * 0, the adornment draws the first decoration starting at the point at
     * <code>intervalBetween / 2</code> into the glyph's length. A negative
     * offset causes the first decoration to begin closer to the beginning of
     * the glyph's length, and conversely a positive offset causes the first
     * decoration to start farther along the glyph's length.
     */
    protected double offsetForFirst;

    /**
     * Decorations that are attached to the glyph. This is display-context
     * sensitive, and so is transient.
     */
    protected transient Vector<Decoration> decorations = null;

    // Public Constructors

    /**
     * Create an instance associated with the specified glyph with a color of
     * black, decorations on the right, 10 display units between each
     * decoration, 10 units between each decoration's starting and and ending
     * points, and a 0 offset.
     * 
     * @param glyph
     *            Glyph decorated by this adornment.
     */
    public EdgeAdornment(MultiPointGlyph glyph) {
        super(glyph);
        rightSide = true;
        intervalBetween = 10.0;
        intervalWithin = 10.0;
        offsetForFirst = 0.0;
    }

    /**
     * Create an instance associated with the specified glyph with the specified
     * color, orientation of decorations, space between each decoration, space
     * within each decoration, and offset of the first decoration.
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
     *            decoration.
     */
    public EdgeAdornment(MultiPointGlyph glyph, Color color, boolean rightSide,
            double intervalBetween, double intervalWithin, double offsetForFirst) {
        super(glyph, color);
        this.rightSide = rightSide;
        this.intervalBetween = intervalBetween;
        this.intervalWithin = intervalWithin;
        this.offsetForFirst = offsetForFirst;
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

        // these are equal only if the superclass says
        // so, and the side flags, intervals between
        // and within the decorations, and offset of the
        // first decorations are the same.
        return (super.equals(obj) && (obj instanceof EdgeAdornment)
                && (rightSide == ((EdgeAdornment) obj).rightSide)
                && (intervalBetween == ((EdgeAdornment) obj).intervalBetween)
                && (intervalWithin == ((EdgeAdornment) obj).intervalWithin) && (offsetForFirst == ((EdgeAdornment) obj).offsetForFirst));
    }

    /**
     * Get the hash code of this object.
     * 
     * @return Hash code of this object.
     */
    public int hashCode() {

        // Use the hash code of the superclass combined
        // with those of the side flag, interval between,
        // interval within, and offset.
        return (int) ((((long) super.hashCode())
                + ((long) (new Boolean(rightSide)).hashCode())
                + ((long) (new Double(intervalBetween)).hashCode())
                + ((long) (new Double(intervalWithin)).hashCode()) + ((long) (new Double(
                offsetForFirst)).hashCode())) % (long) Integer.MAX_VALUE);
    }

    /**
     * Find out whether or not the decorations of this adornment are on the left
     * or right side of the glyph.
     * 
     * @return True if the decorations are on the right side of the glyph, false
     *         otherwise.
     */
    public boolean isOnRightSide() {
        return rightSide;
    }

    /**
     * Get the interval between the decorations as measured along the length of
     * the glyph.
     * 
     * @return Interval between the decorations.
     */
    public double getIntervalBetween() {
        return intervalBetween;
    }

    /**
     * Get the actual interval between the decorations as measured along the
     * length of the glyph. This value depends upon how much is supposed to be
     * in between each decoration, as well as the length of the parent glyph,
     * etc.
     * 
     * @param gc
     *            Graphics context in which this adornment's glyph is drawn.
     * @param transformer
     *            Coordinate converter to be used to convert from to display
     *            coordinates from lat-long pairs.
     * @return Actual interval between the decorations.
     */
    public double getActualIntervalBetween(IGraphicsTarget gc,
            CoordConverter transformer) {
        double glyphLength = glyph.getLength(gc, transformer);
        double count = Math.floor(glyphLength
                / (intervalBetween + intervalWithin));
        double remainder = glyphLength
                - (count * (intervalBetween + intervalWithin));
        return intervalBetween + (remainder / count);
    }

    /**
     * Get the interval between the starting and ending points of each
     * decoration as measured along the length of the glyph.
     * 
     * @return Interval between the starting and ending points of each
     *         decoration.
     */
    public double getIntervalWithin() {
        return intervalWithin;
    }

    /**
     * Get the offset at which to start the decorations. If this is 0, the
     * adornment draws the first decoration starting at the point that is
     * <code>intervalBetween / 2</code> into the glyph's length. A negative
     * offset causes the first decoration to begin closer to the beginning of
     * the glyph's length, and conversely a positive offset causes the first
     * decoration to start farther along the glyph's length.
     * 
     * @return Offset at which to start the decorations.
     */
    public double getOffsetForFirst() {
        return offsetForFirst;
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
        for (int j = 0; j < decorations.size(); j++)
            ((Decoration) decorations.elementAt(j)).translateBy(xDelta, yDelta);
    }

    /**
     * Get the apexes of the projections of this adornment.
     * 
     * @param gc
     *            Graphics context in which this adornment's glyph is drawn.
     * @param transformer
     *            Coordinate converter to be used to convert to display
     *            coordinates from lat-long pairs.
     * @return List of points that are the apexes of this adornment.
     */
    public Vector<Point> getProjectionApexes(IGraphicsTarget gc,
            CoordConverter transformer) {
        if (shape == null)
            createShape(gc, transformer);
        Vector<Point> apexes = new Vector<Point>();
        for (int j = 0; j < decorations.size(); j++)
            apexes.add(((Decoration) decorations.elementAt(j))
                    .getApex(transformer));
        return apexes;
    }

    // Protected Methods

    /**
     * Create a clone of this adornment.
     * 
     * @return The new clone of this adornment.
     */
    protected Object clone() {
        EdgeAdornment adornment = null;
        try {
            adornment = (EdgeAdornment) super.clone();
            adornment.rightSide = rightSide;
            adornment.intervalBetween = intervalBetween;
            adornment.intervalWithin = intervalWithin;
            adornment.offsetForFirst = offsetForFirst;
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
     *            Coordinate converter to be used to convert to display
     *            coordinates from lat-long pairs.
     */
    protected void createShape(IGraphicsTarget gc, CoordConverter transformer) {

        // Get the length of the glyph, and take any
        // remainder left over when the length is divided
        // by the sum of the two intervals and spread it
        // evenly amongst the intervals between the de-
        // corations. This will ensure that the latter
        // are evenly spaced along the glyph's edge.
        double glyphLength = glyph.getLength(gc, transformer);
        double count = Math.floor(glyphLength
                / (intervalBetween + intervalWithin));
        double remainder = glyphLength
                - (count * (intervalBetween + intervalWithin));
        double between = intervalBetween + (remainder / count);

        // Iterate along the line, creating the shapes
        // for each decoration along the way and adding
        // all of these shapes together.
        decorations = new Vector<Decoration>();
        GeneralPath path = new GeneralPath(GeneralPath.WIND_NON_ZERO);
        GeneralPath rawPath = new GeneralPath(GeneralPath.WIND_NON_ZERO);
        for (double soFar = (between / 2.0) + offsetForFirst; soFar < glyphLength; soFar += between
                + intervalWithin) {

            // Create the decoration and add its shape
            // to the total shape.
            Decoration decoration = getDecorationFor(glyph
                    .getSectionAtDistanceAlong(soFar, soFar + intervalWithin,
                            gc, transformer));
            decorations.addElement(decoration);
            path.append(decoration.getShape(transformer), false);
            rawPath.append(decoration.getShape(transformer), true);
        }
        shape = path;
        rawShape = rawPath;

        // Create the final styled shape from the path, if
        // the decorations are not meant to be solid.
        if (decorationsAreSolid() == false)
            styleShape(BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER);
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
    // // Tell each of the decorations to add themselves
    // // to the DGM array.
    // for (int j = 0; j < decorations.size(); j++)
    // ((Decoration) decorations.elementAt(j)).toDGM(dgm, transformer,
    // cartesian);
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
    //
    // // Let the superclass set up the line style.
    // super.addToExtendedDGM(dgm, transformer, cartesian);
    //
    // // Tell each of the decorations to add themselves
    // // to the DGM array.
    // for (int j = 0; j < decorations.size(); j++)
    // ((Decoration) decorations.elementAt(j)).
    // toExtendedDGM(dgm, transformer, cartesian);
    // }

    @Override
    public void prepareShape(IWireframeShape wshape, IShadedShape sshape,
            CoordConverter converter) {
        for (int j = 0; j < decorations.size(); j++)
            ((Decoration) decorations.elementAt(j)).prepareShape(wshape,
                    sshape, converter);
    }

    /**
     * Determine whether or not the decorations along the edge should be filled
     * or hollow. If hollow, they will be styled with the parent glyph's
     * thickness and line style.
     * 
     * @return True if the decorations should be filled, false otherwise.
     */
    protected abstract boolean decorationsAreSolid();

    /**
     * Get a decoration to be attached to the adornment at along the specified
     * path.
     * 
     * @param glyphPath
     *            Section of the glyph along which the decoration should run.
     */
    protected abstract Decoration getDecorationFor(Point[] glyphPath);
}
