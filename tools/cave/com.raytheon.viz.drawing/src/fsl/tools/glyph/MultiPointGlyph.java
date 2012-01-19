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
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Vector;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.viz.adapter.CoordConverter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The abstract base class for drawable multi-point glyphs. Such objects have
 * multiple lat-long locations associated with each instance, and may have
 * different styles (dashed or solid) and thicknesses. Examples of some possible
 * subclasses include straight line segments, multi-node paths, and splines.
 * <p>
 * Subclasses may be augmented visually by attaching objects that implement
 * <code>Adornment</code> to themselves.
 * 
 * @author Christopher Golden
 * @see fsl.tools.glyph.Adornment
 */
public abstract class MultiPointGlyph extends GlyphImpl implements Linear {

    // Protected Static Constants

    /**
     * Value indicating that this glyph should not be painted at all, though its
     * adornments may be painted.
     */
    protected static final int NO_PAINTING = 0;

    /**
     * Value indicating that this glyph should be painted before its adornments
     * are painted.
     */
    protected static final int PAINT_UNDER = 1;

    /**
     * Value indicating that this glyph should be painted after its adornments
     * are painted.
     */
    protected static final int PAINT_OVER = 2;

    // Protected Variables

    /**
     * Glyph line style.
     */
    protected int style;

    /**
     * Glyph line thickness.
     */
    protected int thickness;

    /**
     * List of lat-long points making up the glyph.
     */
    protected Vector<Coordinate> points;

    /**
     * List of adornments decorating this glyph.
     */
    protected Vector<Adornment> adornments;

    /**
     * Display context coordinates of points making up the glyph. As this is
     * display-context sensitive, it is transient.
     */
    protected transient Vector<Point> pixelPoints = null;

    // Public Constructors

    /**
     * Create an instance with a starting color of black, solid style, and
     * standard thickness. The glyph has zero points to start with.
     */
    public MultiPointGlyph() {
        style = SOLID_STYLE;
        thickness = 0;
        points = new Vector<Coordinate>();
        adornments = new Vector<Adornment>();
    }

    /**
     * Create an instance with the specified color, style, thickness, and
     * points.
     * 
     * @param frame
     *            Frame in which this glyph exists; if <code>ALL_FRAMES</code>,
     *            the glyph exists in all frames.
     * @param color
     *            Color of the glyph.
     * @param style
     *            Line style of the glyph.
     * @param thickness
     *            Line thickness of the glyph.
     * @param points
     *            List of lat-long points making up the glyph.
     * @param adornments
     *            List of adornments to decorate the glyph.
     */
    public MultiPointGlyph(int frame, Color color, int style, int thickness,
            Vector<Coordinate> points, Vector<Adornment> adornments) {
        super(frame, color);
        this.style = style;
        this.thickness = thickness;
        this.points = points;
        this.adornments = adornments;
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
        // and the line styles, line thicknesses, points,
        // and adornments are equal.
        return (super.equals(obj) && (obj instanceof MultiPointGlyph)
                && (style == ((MultiPointGlyph) obj).style)
                && (thickness == ((MultiPointGlyph) obj).thickness)
                && points.equals(((MultiPointGlyph) obj).points) && adornments
                .equals(((MultiPointGlyph) obj).adornments));
    }

    /**
     * Get the hash code of this object.
     * 
     * @return Hash code of this object.
     */
    public int hashCode() {

        // Combine the hash codes of the superclass and
        // the line style, line thickness, points, and
        // adornments.
        return (int) ((((long) super.hashCode()) + ((long) style)
                + ((long) thickness) + ((long) points.hashCode()) + ((long) adornments
                .hashCode())) % (long) Integer.MAX_VALUE);
    }

    /**
     * Create a clone of this glyph.
     */
    @SuppressWarnings("unchecked")
    public Object clone() {
        MultiPointGlyph glyph = null;
        try {
            glyph = (MultiPointGlyph) super.clone();
            glyph.style = style;
            glyph.thickness = thickness;
            glyph.points = (Vector<Coordinate>) points.clone();
            glyph.adornments = new Vector<Adornment>();
            for (int j = 0; j < adornments.size(); j++)
                glyph.adornments.addElement(((Adornment) adornments
                        .elementAt(j)).createCopyFor(glyph));
            glyph.pixelPoints = null;
        } catch (Exception e) {
            // Logger.logBug("Clone error.", e);
            e.printStackTrace();
        }
        return glyph;
    }

    /**
     * Flush display coordinates, since they are no longer valid. This method is
     * called when something about the glyph (position, etc.) or the display
     * context in which the glyph is painted has changed.
     */
    public void flushDisplayCoordinates() {
        pixelPoints = null;
        flushAdornmentDisplayCoordinates();
    }

    /**
     * Get the lat-long point at the specified index.
     * 
     * @param index
     *            Index of the lat-long point desired.
     * @return Lat-long point.
     */
    public Coordinate getPoint(int index) {
        return (Coordinate) points.elementAt(index);
    }

    /**
     * Get the lat-long points making up the glyph.
     * 
     * @return Lat-long points making up the glyph. This should be considered
     *         constant, i.e. it is not to be modified by the caller.
     */
    public Vector<Coordinate> getPoints() {
        return points;
    }

    /**
     * Begin editing the glyph. The types of editing supported are
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
        // if(event.type != SWT.MouseUp)
        // return false;

        // The only editing operations that this superclass
        // supports are move and creation-finishing, the
        // latter to allow the user to add additional points
        // to the object after it is instantiated as part
        // of its creation.
        if ((type == MOVE) || (type == MOVE_ONLY)) {

            // Make sure that the display coordinates are
            // current, and set the flag indicating that the
            // glyph is being edited.
            mapToDisplayCoordinates(gc, transformer);
            editType = MOVE;
            return true;
        } else if (type == FINISH_CREATION) {

            // Process this point as a creation point, and
            // set the flag indicating that the glyph is
            // being edited.
            editType = FINISH_CREATION;
            processEventDuringCreation(event, gc, transformer);
            return true;
        } else
            return false;
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
        // Handle move or creation-finishing edit events
        // only.
        if (editType == MOVE) {

            // Ignore the event unless it is a mouse move
            // or left mouse button release.
            if (event.type != SWT.MouseUp && event.type != SWT.MouseMove)
                return false;

            // Move the glyph and, if the mouse was re-
            // leased, finish the edit.
            Point point = new Point(event.x, event.y);
            translateDisplayCoordinates(point);
            if (event.type == SWT.MouseUp) {
                finishEdit(gc, transformer);
                return false;
            } else
                return true;
        } else if (editType == FINISH_CREATION) {

            // Provide the point so that it may be
            // added or otherwise processed; if this
            // ends the edit, finish it.
            if (processEventDuringCreation(event, gc, transformer))
                return true;
            else {
                finishEdit(gc, transformer);
                return false;
            }
        } else
            return false;
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
        if (editType == MOVE) {

            // Set the flag indicating that the glyph is no
            // longer being edited.
            editType = NONE;

            // Calculate the lat-long coordinates based upon
            // the display coordinates.
            mapToLatLongCoordinates(gc, transformer);
        } else if (editType == FINISH_CREATION) {

            // Set the flag indicating that the glyph is no
            // longer being edited, and flush the old dis-
            // play coordinates.
            editType = NONE;
            flushDisplayCoordinates();
        }
    }

    /**
     * Set the color to the specified color.
     * 
     * @param color
     *            New color of the glyph.
     */
    public void setColor(Color color) {

        // Let the superclass do its thing, and then set
        // the color of the adornment.
        super.setColor(color);
        for (int j = 0; j < adornments.size(); j++)
            ((Adornment) adornments.elementAt(j)).setColor(color);
    }

    /**
     * Get the line style of the glyph.
     * 
     * @return Line style of the glyph.
     */
    public int getStyle() {
        return style;
    }

    /**
     * Set the line style of the glyph.
     * 
     * @param style
     *            New line style of the glyph.
     */
    public void setStyle(int style) {
        this.style = style;
        flushDisplayCoordinates();
    }

    /**
     * Get the line thickness of the glyph.
     * 
     * @return Line thickness of the glyph.
     */
    public int getThickness() {
        return thickness;
    }

    /**
     * Set the line thickness of the glyph.
     * 
     * @param thickness
     *            New line thickness of the glyph.
     */
    public void setThickness(int thickness) {
        this.thickness = thickness;
        flushDisplayCoordinates();
    }

    /**
     * Add an adornment to the glyph.
     * 
     * @param adornment
     *            Adornment to be added.
     */
    public void addAdornment(Adornment adornment) {
        adornments.addElement(adornment);
    }

    /**
     * Get all the adornments decorating this glyph.
     * 
     * @return All adornments decorating this glyph, or <code>null</code> if
     *         no adornments are associated with this glyph.
     */
    public Adornment[] getAdornments() {
        if (adornments.size() == 0)
            return null;
        Adornment[] adornmentArray = new Adornment[adornments.size()];
        adornments.copyInto(adornmentArray);
        return adornmentArray;
    }

    /**
     * Remove any adornments currently associated with the glyph.
     */
    public void removeAllAdornments() {
        adornments.removeAllElements();
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
    public abstract double getLength(IGraphicsTarget gc,
            CoordConverter transformer);

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
    public abstract Point getPointAtDistanceAlong(double distance,
            IGraphicsTarget gc, CoordConverter transformer);

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
    public abstract Point getPointAtFractionAlong(double fraction,
            IGraphicsTarget gc, CoordConverter transformer);

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
    public abstract double getDistanceAtPointAlong(Point point,
            IGraphicsTarget gc, CoordConverter transformer);

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
    public abstract double getFractionAtPointAlong(Point point,
            IGraphicsTarget gc, CoordConverter transformer);

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
    public abstract Point[] getSectionAtDistanceAlong(double startDistance,
            double endDistance, IGraphicsTarget gc, CoordConverter transformer);

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
    public abstract Point[] getSectionAtFractionAlong(double startFraction,
            double endFraction, IGraphicsTarget gc, CoordConverter transformer);

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
    public abstract double getAngleAtPointAlong(Point point, int section,
            IGraphicsTarget gc, CoordConverter transformer);

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
    protected abstract int getPaintingMode();

    /**
     * Process the specified event during glyph creation.
     * 
     * @param event
     *            Event that is to be processed.
     * @param transformer
     *            Coordinate transformer to be used to convert from the display
     *            coordinates to latitude-longitude format.
     * @return True if the creation is to continue, otherwise false.
     */
    protected abstract boolean processEventDuringCreation(Event event,
            IGraphicsTarget gc, CoordConverter transformer);

    /**
     * Create the visual elements of this glyph.
     * 
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     */
    protected abstract void createVisuals(IGraphicsTarget gc,
            CoordConverter transformer);

    /**
     * Flush the display coordinates of any adornments.
     */
    protected void flushAdornmentDisplayCoordinates() {
        for (int j = 0; j < adornments.size(); j++)
            ((Adornment) adornments.elementAt(j)).flushDisplayCoordinates();
    }

    /**
     * Paint any adornments.
     * 
     * @param gc
     *            Graphics context in which the adornments are to be drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @param properties
     *            Display properties to be used.
     * @param useColor
     *            Color to be used when painting the adornments instead of
     *            whatever color(s) would usually be used; if <code>null</code>,
     *            standard painting will be done.
     * 
     */
    // protected void paintAdornments(Graphics gc, CoordConverter transformer,
    // DisplayProperties properties, Color useColor) {
    //
    // // Iterate through all adornments, painting each
    // // in turn.
    // for (int j = 0; j < adornments.size(); j++) {
    // Adornment adornment = (Adornment) adornments.elementAt(j);
    // gc.setColor(properties.transform(useColor != null ? useColor :
    // adornment.getColor()));
    // if (adornment instanceof ShapeAdornment) {
    // Shape shape = ((ShapeAdornment) adornment).getShape(gc, transformer);
    // ((Graphics2D) gc).draw(shape);
    // ((Graphics2D) gc).fill(shape);
    // } else
    // ((RenderedAdornment) adornment).paint(gc, transformer, properties,
    // useColor);
    // }
    // }
    public void createShape(IGraphicsTarget gc, CoordConverter transformer) {
        for (int j = 0; j < adornments.size(); j++) {
            Adornment adornment = (Adornment) adornments.elementAt(j);
            if (adornment instanceof ShapeAdornment) {
                ((ShapeAdornment) adornment).getShape(gc, transformer);
            }
        }
    }

    /**
     * Add the adornments to the specified DGM array.
     * 
     * @param dgm
     *            DGM array in which to place the translated adornments.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate transformer to be used to convert from the display
     *            coordinates to latitude-longitude format.
     * @param cartesian
     *            Flag indicating whether or not the conversion should utilize
     *            Cartesian instead of lat-long coordinates. A 1024 by 1024
     *            coordinate space is assumed if this flag is true.
     */
    // protected void adornmentsToDGM(DGMArray dgm, Graphics gc,
    // CoordConverter transformer, boolean cartesian) {
    // for (int j = 0; j < adornments.size(); j++)
    // ((Adornment) adornments.elementAt(j)).toDGM(dgm, gc, transformer,
    // cartesian);
    // }
    /**
     * Add the adornments to the specified Extended DGM array.
     * 
     * @param dgm
     *            Extended DGM array in which to place the translated
     *            adornments.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate transformer to be used to convert from the display
     *            coordinates to latitude-longitude format.
     * @param cartesian
     *            Flag indicating whether or not the conversion should utilize
     *            Cartesian instead of lat-long coordinates. A 1024 by 1024
     *            coordinate space is assumed if this flag is true.
     */
    // protected void adornmentsToExtendedDGM(DGMArray dgm, Graphics gc,
    // CoordConverter transformer,
    // boolean cartesian) {
    // for (int j = 0; j < adornments.size(); j++)
    // ((Adornment) adornments.elementAt(j)).
    // toExtendedDGM(dgm, gc, transformer, cartesian);
    // }
    protected void adornmentsToShape(IWireframeShape ws, IShadedShape ss,
            CoordConverter transformer) {
        for (int j = 0; j < adornments.size(); j++)
            ((Adornment) adornments.elementAt(j)).prepareShape(ws, ss,
                    transformer);
    }

    /**
     * Create a bounding rectangle that encompasses all the adornments
     * decorating this glyph.
     * 
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return Bounding rectangle that encompasses all the adornments decorating
     *         this glyph.
     */
    protected Rectangle getAdornmentBoundingRectangle(IGraphicsTarget gc,
            CoordConverter transformer) {

        // Iterate through all adornments, creating a union
        // of their bounding rectangles.
        Rectangle rectangle = null;
        for (int j = 0; j < adornments.size(); j++) {
            Shape shape;
            Adornment adornment = (Adornment) adornments.elementAt(j);
            if (adornment instanceof ShapeAdornment)
                shape = ((ShapeAdornment) adornment).getShape(gc, transformer);
            else
                shape = ((RenderedAdornment) adornment).getBoundingShape(gc,
                        transformer);
            if (rectangle == null)
                rectangle = shape.getBounds();
            else
                rectangle = rectangle.union(shape.getBounds());
        }
        return rectangle;
    }

    /**
     * Determine whether or not one or more of the adornments of this glyph
     * contain the specified point.
     * 
     * @param point
     *            Point to be checked to see if it is contained by any
     *            adornments.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return True if any of the adornments contain the point, false otherwise.
     */
    protected boolean adornmentsContain(Point point, IGraphicsTarget gc,
            CoordConverter transformer) {

        // Iterate through the adornments, checking each in
        // turn to see if it contains the point. If none
        // of them contain the point, return false.
        for (int j = 0; j < adornments.size(); j++) {
            Adornment adornment = (Adornment) adornments.elementAt(j);
            Shape shape;
            if (adornment instanceof ShapeAdornment)
                shape = ((ShapeAdornment) adornment).getShape(gc, transformer);
            else
                shape = ((RenderedAdornment) adornment).getBoundingShape(gc,
                        transformer);
            if (shape.contains(point.x, point.y))
                return true;
        }
        return false;
    }

    /**
     * Determine whether or not one or more of the adornments of this glyph
     * intersects with the specified rectangle.
     * 
     * @param rectangle
     *            Rectangle to be checked to see if it intersects with any
     *            adornments.
     * @param gc
     *            Graphics context in which this glyph is drawn.
     * @param transformer
     *            Coordinate converter used to translate between lat-long pairs
     *            and display coordinates.
     * @return True if any of the adornments intersect with the rectangle, false
     *         otherwise.
     */
    protected boolean adornmentsIntersect(Rectangle rectangle,
            IGraphicsTarget gc, CoordConverter transformer) {

        // Iterate through the adornments, checking each in
        // turn to see if it intersects with the rectangle.
        // If none of them intersect with it, return false.
        for (int j = 0; j < adornments.size(); j++) {
            Adornment adornment = (Adornment) adornments.elementAt(j);
            Shape shape;
            if (adornment instanceof ShapeAdornment)
                shape = ((ShapeAdornment) adornment).getShape(gc, transformer);
            else
                shape = ((RenderedAdornment) adornment).getBoundingShape(gc,
                        transformer);
            if (shape.intersects(rectangle))
                return true;
        }
        return false;
    }

    /**
     * Translate any adornments by the specified deltas.
     * 
     * @param xDelta
     *            X delta by which to translate the adornments.
     * @param yDelta
     *            X delta by which to translate the adornments.
     */
    protected void translateAdornments(int xDelta, int yDelta) {
        for (int j = 0; j < adornments.size(); j++)
            ((Adornment) adornments.elementAt(j)).translateBy(xDelta, yDelta);
    }
}
