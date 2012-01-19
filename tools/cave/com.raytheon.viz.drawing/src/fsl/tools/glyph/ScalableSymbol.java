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

import com.raytheon.viz.adapter.CoordConverter;
import com.vividsolutions.jts.geom.Coordinate;


/**
 * The abstract base class for drawable objects that are
 * not freehand, modifiable glyphs (such as lines or shapes)
 * but rather entities based upon static symbols.  They may
 * have different orientations, like their base class, but
 * additionally they may also be scaled.  Examples of possible
 * subclasses include meteorological symbols and wind barbs.
 *
 * @author Christopher Golden
 */
public abstract class ScalableSymbol extends Symbol {


	// Protected Variables

	/**
	 * Size of symbol.
	 */
	protected double size;


	// Public Constructors

	/**
	 * Create an instance with a starting location
	 * of 0,0, a color of black, and the standard
	 * orientation and size.
	 */
	public ScalableSymbol() {
		size = 1.0;
	}

	/**
	 * Create an instance with the specified location,
	 * color, orientation, and size.
	 *
	 * @param frame       Frame in which this glyph exists;
	 *                    if <code>ALL_FRAMES</code>, the
	 *                    glyph exists in all frames.
	 * @param location    Location of the symbol in
	 *                    lat-long coordinates.
	 * @param color       Color of the symbol.
	 * @param orientation Orientation of the symbol.
	 * @param size        Size of the symbol.
	 */
	public ScalableSymbol(int frame, Coordinate location, Color color,
						  double orientation, double size) {
		super(frame, location, color, orientation);
		this.size = size;
	}

	/**
	 * Create an instance with the specified location,
	 * color, orientation, and size.
	 *
	 * @param frame       Frame in which this glyph exists;
	 *                    if <code>ALL_FRAMES</code>, the
	 *                    glyph exists in all frames.
	 * @param transformer Coordinate converter used to
	 *                    translate between X,Y and
	 *                    latitude-longitude coordinates.
	 * @param location    Location of the symbol.
	 * @param color       Color of the symbol.
	 * @param orientation Orientation of the symbol.
	 * @param size        Size of the symbol.
	 */
	public ScalableSymbol(int frame, CoordConverter transformer, Point location,
						  Color color, double orientation, double size) {
		super(frame, transformer, location, color, orientation);
		this.size = size;
	}


	// Public Methods

    /**
     * Determines whether another object is equal to this object.
	 *
     * @param  obj The object to which this object is to be compared.
     * @return True if the objects are the same, false otherwise.
     */
    public boolean equals(Object obj) {

		// These are equal only if the superclass says so and if
		// the sizes are equal.
		return (super.equals(obj) && (obj instanceof ScalableSymbol) &&
				(size == ((ScalableSymbol) obj).size));
    }

    /**
     * Get the hash code of this object.
	 *
     * @return Hash code of this object.
     */
	public int hashCode() {

		// Combine the hash codes of the superclass and the size.
		return (int) ((((long) super.hashCode()) +
					   ((long) (new Double(size)).hashCode())) %
					  (long) Integer.MAX_VALUE);
	}

	/**
	 * Create a clone of this glyph.
	 */
	public Object clone() {
		ScalableSymbol glyph = null;
		try {
			glyph = (ScalableSymbol) super.clone();
			glyph.size = size;
		} catch (Exception e) {
			//Logger.logBug("Clone error.", e);
			e.printStackTrace();
		}
		return glyph;
	}

	/**
	 * Get the size of the symbol.
	 *
	 * @return Size of the symbol.
	 */
	public double getSize() {
		return size;
	}

	/**
	 * Set the size of the symbol.
	 *
	 * @param size New size of the symbol.
	 */
	public void setSize(double size) {
		this.size = size;
		flushDisplayCoordinates();
	}
}

