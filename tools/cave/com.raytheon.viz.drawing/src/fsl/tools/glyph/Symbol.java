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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.viz.adapter.CoordConverter;
import com.vividsolutions.jts.geom.Coordinate;



/**
 * The abstract base class for drawable objects that are
 * not freehand, modifiable glyphs (such as lines or shapes)
 * but rather entities based upon static symbols which have
 * a single lat-long location associated with each instance.
 * They may have different orientations.  Examples of some
 * possible subclasses include text annotations, wind barbs,
 * and miscellaneous meteorological symbols.
 *
 * @author Christopher Golden
 */
public abstract class Symbol extends GlyphImpl {


	// Public Static Constants

	/**
	 * Oriented so that it is angled along the latitude
	 * line.
	 */
	public static final double ORIENTED_ALONG_LATITUDE = -10000.0;


	// Protected Variables

	/**
	 * Location of glyph.
	 */
	protected Coordinate location;

	/**
	 * Orientation of symbol in degrees; if <code>
	 * ORIENTED_ALONG_LATITUDE</code>, the symbol is
	 * oriented along the line of its current latitude.
	 * Note that if subclasses wish to have their
	 * instances oriented along the latitude, they
	 * should use <code>getActualOrientation()</code>
	 * to get the orientation to be used, instead of
	 * accessing this variable directly.
	 */
	protected double orientation;

	/**
	 * Display pixel location; this is display-context
	 * sensitive, and so is transient.
	 */
	protected transient Point pixelLoc = null;

	/**
	 * Last point used in an edit operation; this is only
	 * needed during edits, and so is transient.
	 */
	protected transient Point lastPoint = null;


	// Public Constructors

	/**
	 * Create an instance with a starting location of 0.0,
	 * a color of black, and the standard orientation.
	 */
	public Symbol() {
		location = new Coordinate();
		orientation = 0.0;
	}

	/**
	 * Create an instance with the specified location,
	 * color, and orientation.
	 *
	 * @param frame       Frame in which this glyph exists;
	 *                    if <code>ALL_FRAMES</code>, the
	 *                    glyph exists in all frames.
	 * @param location    Location of the symbol in
	 *                    lat-long coordinates.
	 * @param color       Color of the symbol.
	 * @param orientation Orientation of the symbol; if
	 *                    <code>ORIENTED_ALONG_LATITUDE</code>,
	 *                    the symbol is oriented along its
	 *                    latitude.
	 */
	public Symbol(int frame, Coordinate location, Color color, double orientation) {
		super(frame, color);
		this.location = location;
		this.orientation = orientation;
	}

	/**
	 * Create an instance with the specified location,
	 * color, and orientation.
	 *
	 * @param frame       Frame in which this glyph exists;
	 *                    if <code>ALL_FRAMES</code>, the
	 *                    glyph exists in all frames.
	 * @param transformer Coordinate converter used to
	 *                    translate between X,Y and
	 *                    latitude-longitude coordinates.
	 * @param location    Location of the symbol.
	 * @param color       Color of the symbol.
	 * @param orientation Orientation of the symbol; if
	 *                    <code>ORIENTED_ALONG_LATITUDE</code>,
	 *                    the symbol is oriented along its
	 *                    latitude.
	 */
	public Symbol(int frame, CoordConverter transformer, Point location,
				  Color color, double orientation) {
		super(frame, color);
		this.location = transformer.convert(location);
		this.orientation = orientation;
	}


	// Public Methods

    /**
     * Determines whether another object is equal to this
	 * object.
	 *
     * @param  obj The object to which this object is to
	 *             be compared.
     * @return True if the objects are the same, false
	 *         otherwise.
     */
    public boolean equals(Object obj) {

		// These are equal if the superclass says so and if the
		// locations and orientations are equal.
		return (super.equals(obj) && (obj instanceof Symbol) &&
				location.equals(((Symbol) obj).location) &&
				(orientation == ((Symbol) obj).orientation));
    }

    /**
     * Get the hash code of this object.
	 *
     * @return Hash code of this object.
     */
	public int hashCode() {

		// Combine the hash codes of the superclass, the location,
		// and the orientation.
		return (int) ((((long) super.hashCode()) +
					   ((long) location.hashCode()) +
					   ((long) (new Double(orientation)).hashCode())) %
					  (long) Integer.MAX_VALUE);
	}

	/**
	 * Create a clone of this glyph.
	 */
	public Object clone() {
		Symbol glyph = null;
		try {
			glyph = (Symbol) super.clone();
			glyph.location = new Coordinate(location);
			glyph.orientation = orientation;
			glyph.pixelLoc = null;
			glyph.lastPoint = null;
		} catch (Exception e) {
			//Logger.logBug("Clone error.", e);
			e.printStackTrace();
		}
		return glyph;
	}

	/**
	 * Flush display coordinates, since they are no longer
	 * valid.  This method is called when something about
	 * the glyph (position, etc.) or the display context
	 * in which the glyph is painted has changed.
	 */
	public void flushDisplayCoordinates() {
		pixelLoc = null;
	}

	/**
	 * Begin editing the glyph.  The only edits supported
	 * by the base class are <code>MOVE</code> and
	 * <code>MOVE_ONLY</code>; other editing must be
	 * supported by overriding this method in subclasses.
	 *
	 * @param  type        Type of edit to be performed.
	 * @param  event       Event that triggered this edit.
	 * @param  gc          Graphics context in which this
	 *                     glyph is drawn.
	 * @param  transformer Coordinate converter used to
	 *                     translate between lat-long
	 *                     pairs and display coordinates.
	 * @return True if an edit operation has been started,
	 *         false otherwise.
	 */
	public boolean startEdit(int type, Event event, IGraphicsTarget gc,
							 CoordConverter transformer) {

		// Do nothing unless the left mouse button was
		// released.
		if ( event.type != SWT.MouseUp)
			return false;

		// The only editing operation that this class supports
		// is move.
		if ((type == MOVE) || (type == MOVE_ONLY)) {

			// Remember the starting point of the edit.
			lastPoint = new Point(event.x, event.y);

			// Make sure that the display coordinates are
			// current, and set the flag indicating that the
			// glyph is being edited.
			mapToDisplayCoordinates(gc, transformer);
			editType = MOVE;
			return true;
		} else
			return false;
	}

	/**
	 * Edit the glyph based upon the specified event.
	 *
	 * @param  event       Input event that triggered
	 *                     this call.
	 * @param  gc          Graphics context in which this
	 *                     glyph is drawn.
	 * @param  transformer Coordinate converter used to
	 *                     translate between lat-long
	 *                     pairs and display coordinates.
	 * @return True if the edit should continue, false
	 *         if the glyph has ended the edit.
	 */
	public boolean edit(Event event, IGraphicsTarget gc, CoordConverter transformer) {

		// Only edit if the glyph is currently in move
		// If so, translate display coordinates in a
		// subclass-specific manner if a mouse move was
		// received.  If the left mouse button was
		// released, end the edit.
		if (editType == MOVE) {

			// Ignore the event unless it is a mouse move
			// or left mouse button release.
			if (event.type != SWT.MouseMove && event.type != SWT.MouseUp)
				return true;

			// Move the glyph and, if the mouse was re-
			// leased, finish the edit.
			Point point = new Point(event.x, event.y);
			translateDisplayCoordinates(point);
			if (event.type == SWT.MouseUp) {
				finishEdit(gc, transformer);
				return false;
			} else
				return true;
		} else
			return false;
	}

	/**
	 * Finish up editing the glyph.
	 *
	 * @param gc          Graphics context in which this
	 *                    glyph is drawn.
	 * @param transformer Coordinate transformer to be used to
	 *                    convert from the display coordinates
	 *                    to latitude-longitude format.
	 */
	public void finishEdit(IGraphicsTarget gc, CoordConverter transformer) {
		if (editType != NONE) {
			
			// Set the flag indicating that the glyph is no
			// longer being edited.
			editType = NONE;
			
			// Calculate the lat-long coordinates based upon
			// the display coordinates.
			mapToLatLongCoordinates(gc, transformer);
		}
	}

	/**
	 * Get the location of the glyph in X,Y format.
	 *
	 * @param transformer Coordinate converter to be used
	 *                    to convert from the display
	 *                    coordinates to latitude-longitude
	 *                    pairs.
	 * @return Location of the glyph.
	 */
	public Point getLocation(CoordConverter transformer) {
		if (pixelLoc != null)
			return pixelLoc;
		else
			return transformer.convert(location);
	}

	/**
	 * Get the location of the glyph in lat-long format.
	 *
	 * @return Location of the glyph.
	 */
	public Coordinate getLocation() {
		return location;
	}

	/**
	 * Set the location to the specified X,Y point.
	 *
	 * @param transformer Coordinate converter to be used
	 *                    to convert from the display
	 *                    coordinates to latitude-longitude
	 *                    pairs.
	 * @param location    New location of the glyph.
	 */
	public void setLocation(CoordConverter transformer, Point location) {
		this.location = transformer.convert(location);
		flushDisplayCoordinates();
	}

	/**
	 * Set the location to the specified lat-long
	 * coordinate.
	 *
	 * @param location New location of the glyph.
	 */
	public void setLocation(Coordinate location) {
		this.location = location;
		flushDisplayCoordinates();
	}

	/**
	 * Get the orientation of the symbol.
	 *
	 * @return Orientation of the symbol in degrees, or
	 * 	       <code>ORIENTED_ALONG_LATITUDE</code> if
	 *         oriented along the latitude line.
	 */
	public double getOrientation() {
		return orientation;
	}

	/**
	 * Set the orientation of the symbol.
	 *
	 * @param orientation New orientation of the symbol
	 *                    in degrees, or <code>
	 *                    ORIENTED_ALONG_LATITUDE</code>
	 *                    if it is to be oriented along
	 *                    the latitude line.
	 */
	public void setOrientation(double orientation) {
		this.orientation = orientation;
		flushDisplayCoordinates();
	}


	// Protected Methods

	/**
	 * Calculate the pixel location of the glyph on the
	 * display based on the lat-long information.
	 *
	 * @param gc          Graphics context in which this
	 *                    glyph is drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long
	 *                    pairs and display coordinates.
	 */
	protected void mapToDisplayCoordinates(IGraphicsTarget gc, CoordConverter transformer) {
		if (pixelLoc == null)
			pixelLoc = transformer.convert(location);
	}

	/**
	 * Calculate the lat-long location of the glyph based
	 * on its pixel location on the display.
	 *
	 * @param gc          Graphics context in which this
	 *                    glyph is drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between display
	 *                    coordinates and lat-long pairs.
	 */
	protected void mapToLatLongCoordinates(IGraphicsTarget gc, CoordConverter transformer) {
		location = transformer.convert(pixelLoc);
	}

	/**
	 * Translate the display coordinates to the specified
	 * point.
	 *
	 * @param point Point to which to translate.
	 */
	protected void translateDisplayCoordinates(Point point) {
		pixelLoc.translate(point.x - lastPoint.x, point.y - lastPoint.y);
		lastPoint = point;
	}

	/**
	 * Get the actual orientation to be used. This
	 * method returns the orientation given by the
	 * <code>orientation</code> member variable, unless
	 * it is equal to <code>ORIENTED_ALONG_LATITUDE</code>,
	 * in which case it computes the orientation to be
	 * used and returns that.
	 *
	 * @param  transformer Coordinate converter used to
	 *                     translate between lat-long pairs
	 *                     and display coordinates.
	 * @return Orientation to be used.
	 */
	protected double getActualOrientation(CoordConverter transformer) {

		// If the symbol is to be oriented along the cur-
		// rent latitude line, calculate the orientation;
		// otherwise just use the given orientation.
		if (orientation == ORIENTED_ALONG_LATITUDE) {

			// Get the current lat-long location,
			// taking it from the pixel location if
			// that exists so that the orientation
			// will be calculated based upon the cur-
			// rent screen point if the annotation is
			// being moved.
			Coordinate location = null;
			if (pixelLoc != null)
				location = transformer.convert(pixelLoc);
			else
				location = this.location;

			// Find two points who have a horizontal
			// and/or a vertical distance from one ano-
			// ther that is greater than 10 pixels;
			// this is done to ensure that the angles
			// calculated are fine-grained enough at
			// all scales.
			int dx = 0, dy = 0;
			for (float offset = 0.5f; (Math.abs(dx) < 10.0) && (Math.abs(dy) < 10.0);
				 offset *= 2.0f) {
				Coordinate loc = new Coordinate(location.y, location.x - offset);
				Point point1 = transformer.convert(loc);
				loc.x += (2.0f * offset);
				Point point2 = transformer.convert(loc);
				dx = point2.x - point1.x;
				dy = point2.y - point1.y;
			}

			// Calculate the angle between the two
			// points.
			if (dy == 0)
				return 0.0;
			else if (dx == 0)
				return (dy > 0 ? 270.0 : 90.0);
			else {
				double result = Math.toDegrees(Math.atan(((double) dy) / (double) dx));
				result *= -1.0;
				if (result < 0.0)
					result += 360.0;
				return result;
			}
		} else
			return orientation;
	}
}
