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
import java.io.Serializable;

import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.viz.adapter.CoordConverter;



/**
 * The adornment, an abstract class from which various
 * types of adornments may be derived.  Adornments are
 * used to decorate <code>MultiPointGlyph</code> objects;
 * they are visual augmentations for any glyph that has
 * multiple location points associated with it, such as a
 * line, path, or polygon.  Examples of adornments
 * include arrowheads at the ends of lines, and circular
 * projections along the length of a path.
 *
 * @author Christopher Golden
 * @see fsl.tools.glyph.MultiPointGlyph
 */
public abstract class Adornment implements Serializable, Cloneable {


	// Protected Variables

	/**
	 * Glyph with which this adornment is associated.
	 */
	protected MultiPointGlyph glyph;

	/**
	 * Color to be used when painting this adornment.
	 */
	protected Color color;


	// Public Constructors

	/**
	 * Create an instance associated with the specified
	 * glyph and a color of black.
	 *
	 * @param glyph Glyph decorated by this adornment.
	 */
	public Adornment(MultiPointGlyph glyph) {
		this.glyph = glyph;
		color = Color.black;
	}

	/**
	 * Create an instance associated with the specified
	 * glyph with the specified color.
	 *
	 * @param glyph Glyph decorated by this adornment.
	 * @param color Color to be used when painting this
	 *              adornment.
	 */
	public Adornment(MultiPointGlyph glyph, Color color) {
		this.glyph = glyph;
		this.color = color;
	}


	// Public Methods

    /**
     * Determines whether another object is equal to this
	 * object.
	 *
     * @param  obj The object to which this object is to
	 *         be compared.
     * @return True if the objects are the same, false
	 *         otherwise.
     */
    public boolean equals(Object obj) {

		// These are equal only if the colors are the
		// same.
		return ((obj instanceof Adornment) &&
				color.equals(((Adornment) obj).color));
    }

    /**
     * Get the hash code of this object.
	 *
     * @return Hash code of this object.
     */
	public int hashCode() {

		// Use the hash code of the color.
		return (int) (((long) color.hashCode()) % (long) Integer.MAX_VALUE);
	}

	/**
	 * Create a copy of this adornment to be associated
	 * with the specified glyph.
	 *
	 * @param  glyph Glyph with which the copy is to be
	 *               associated.
	 * @return The copy of this adornment.
	 */
	public Adornment createCopyFor(MultiPointGlyph glyph) {
		Adornment adornment = null;
		try {
			adornment = (Adornment) clone();
			adornment.glyph = glyph;
		} catch (Exception e) {
			//Logger.logBug("Clone error.", e);
			e.printStackTrace();
		}
		return adornment;
	}

	/**
	 * Get the color to be used when painting this
	 * adornment.
	 *
	 * @return Color to be used when painting this
	 *         adornment.
	 */
	public Color getColor() {
		return color;
	}

	/**
	 * Set the color to be used when painting this
	 * adornment.
	 *
	 * @param color New color to be used.
	 */
	public void setColor(Color color) {
		this.color = color;
	}

	/**
	 * Receive notification that the glyph that is being
	 * decorated by this adornment has changed in some
	 * way, or the display upon which it is drawn has
	 * changed.
	 */
	public abstract void flushDisplayCoordinates();

	/**
	 * Translate the adornment by the specified deltas.
	 *
	 * @param xDelta X delta by which to translate the
	 *               adornment.
	 * @param yDelta Y delta by which to translate the
	 *               adornment.
	 */
	public abstract void translateBy(int xDelta, int yDelta);

//	/**
//	 * Convert this adornment to DGM format and place the
//	 * result in the supplied byte array.
//	 *
//	 * @param dgm         DGM array in which to place the
//	 *                    translated adornment.
//	 * @param gc          Graphics context in which this
//	 *                    adornment's glyph is drawn.
//	 * @param transformer Coordinate converter to be used
//	 *                    to convert from the display
//	 *                    coordinates to lat-long pairs.
//	 * @param cartesian   Flag indicating whether or not
//	 *                    the conversion should utilize
//	 *                    Cartesian instead of lat-long
//	 *                    coordinates. A 1024 by 1024
//	 *                    coordinate space is assumed if
//	 *                    this flag is true.
//	 */
//	public abstract void toDGM(DGMArray dgm, Graphics gc, CoordConverter transformer,
//							   boolean cartesian);
//
//	/**
//	 * Convert this adornment to Extended DGM format and
//	 * place the result in the supplied byte array.
//	 *
//	 * @param dgm         Extended DGM array in which to
//	 *                    place the translated adornment.
//	 * @param gc          Graphics context in which this
//	 *                    adornment's glyph is drawn.
//	 * @param transformer Coordinate converter to be used
//	 *                    to convert from the display
//	 *                    coordinates to lat-long pairs.
//	 * @param cartesian   Flag indicating whether or not
//	 *                    the conversion should utilize
//	 *                    Cartesian instead of lat-long
//	 *                    coordinates. A 1024 by 1024
//	 *                    coordinate space is assumed if
//	 *                    this flag is true.
//	 */
//	public void toExtendedDGM(DGMArray dgm, Graphics gc, CoordConverter transformer,
//							  boolean cartesian) {
//		dgm.setColorEx(color);
//		toDGM(dgm, gc, transformer, cartesian);
//	}
	
	
	public abstract void prepareShape(IWireframeShape wshape, IShadedShape sshape, CoordConverter converter);
	
}
