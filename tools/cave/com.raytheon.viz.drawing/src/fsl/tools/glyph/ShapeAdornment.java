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
import java.awt.Shape;
import java.awt.geom.AffineTransform;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.viz.adapter.CoordConverter;



/**
 * The shape adornment, an abstract class from which
 * various types of adornments consisting of shapes may
 * be derived.  Examples of shape adornments include
 * arrowheads at the ends of lines, and circular
 * projections along the length of a path.  A shape
 * adornment must always be capable of being rendered as
 * a single <code>Shape</code> object in a single color.
 * If an adornment requires more than one color, it
 * should be instantiated as multiple adornments instead.
 *
 * @author Christopher Golden
 */
public abstract class ShapeAdornment extends Adornment {


	// Protected Variables

	/**
	 * Shape describing this adornment.  Since this is
	 * display-context sensitive, it is transient.
	 */
	protected transient Shape shape = null;

	/**
	 * Raw shape describing this adornment, used only
	 * if this adornment's <code>shape</code> has been
	 * stroked for drawing. Since this is
	 * display-context sensitive, it is transient.
	 */
	protected transient Shape rawShape = null;


	// Public Constructors

	/**
	 * Create an instance associated with the specified
	 * glyph and a color of black.
	 *
	 * @param glyph Glyph decorated by this adornment.
	 */
	public ShapeAdornment(MultiPointGlyph glyph) {
		super(glyph);
	}

	/**
	 * Create an instance associated with the specified
	 * glyph with the specified color.
	 *
	 * @param glyph Glyph decorated by this adornment.
	 * @param color Color to be used when painting this
	 *              adornment.
	 */
	public ShapeAdornment(MultiPointGlyph glyph, Color color) {
		super(glyph, color);
	}


	// Public Methods

	/**
	 * Get the shape that describes this adornment.
	 *
	 * @param  gc          Graphics context in which this
	 *                     adornment's glyph is drawn.
	 * @param  transformer Coordinate converter used to
	 *                     translate between lat-long
	 *                     pairs and display coordinates.
	 * @return Shape that describes this adornment.
	 */
	public Shape getShape(IGraphicsTarget gc, CoordConverter transformer) {

		// Create the shape if it does not exist.
		if (shape == null)
			createShape(gc, transformer);

		// Return the shape.
		return shape;
	}

	/**
	 * Get the raw shape that describes this adornment,
	 * that is, the shape that has not been stroked for
	 * painting.
	 *
	 * @param  gc          Graphics context in which this
	 *                     adornment's glyph is drawn.
	 * @param  transformer Coordinate converter used to
	 *                     translate between lat-long
	 *                     pairs and display coordinates.
	 * @return Raw shape that describes this adornment.
	 */
	public Shape getRawShape(IGraphicsTarget gc, CoordConverter transformer) {

		// Create the shape if it does not exist.
		if (rawShape == null)
			createShape(gc, transformer);

		// Return the shape.
		return rawShape;
	}

	/**
	 * Receive notification that the glyph that is being
	 * decorated by this adornment has changed in some
	 * way, or the display upon which it is drawn has
	 * changed.  This tells the adornment to be sure to
	 * recreate its <code>Shape</code> object before
	 * returning it the next time <code>getShape()</code>
	 * is called.
	 */
	public void flushDisplayCoordinates() {
		shape = null;
		rawShape = null;
	}

	/**
	 * Translate the adornment by the specified deltas.
	 *
	 * @param xDelta X delta by which to translate the
	 *               adornment.
	 * @param yDelta Y delta by which to translate the
	 *               adornment.
	 */
	public void translateBy(int xDelta, int yDelta) {
		if (shape != null) {
			AffineTransform at = new AffineTransform();
			at.translate(xDelta, yDelta);
			shape = at.createTransformedShape(shape);
			rawShape = at.createTransformedShape(rawShape);
		}
	}

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
//	public void toDGM(DGMArray dgm, Graphics gc, CoordConverter transformer,
//					  boolean cartesian) {
//
//		// Create the shape if it does not exist.
//		if (shape == null)
//			createShape(gc, transformer);
//
//		// Add the decoration to the DGM array.
//		addToDGM(dgm, transformer, cartesian);
//	}
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
//
//		// Create the shape if it does not exist.
//		if (shape == null)
//			createShape(gc, transformer);
//
//		// Set the color.
//		dgm.setColorEx(color);
//
//		// Add the decoration to the DGM array.
//		addToExtendedDGM(dgm, transformer, cartesian);
//	}


	// Protected Methods

	/**
	 * Create a clone of this adornment.
	 *
	 * @return Clone of this adornment.
	 */
	protected Object clone() {
		ShapeAdornment adornment = null;
		try {
			adornment = (ShapeAdornment) super.clone();
			adornment.shape = null;
			adornment.rawShape = null;
		} catch (Exception e) {
			//Logger.logBug("Clone error.", e);
			e.printStackTrace();
		}
		return adornment;
	}

	/**
	 * Create the shape for the adornment.
	 *
	 * @param gc          Graphics context in which this
	 *                    adornment's glyph is drawn.
	 * @param transformer Coordinate converter to be used
	 *                    to convert from to display
	 *                    coordinates from lat-long pairs.
	 */
	protected abstract void createShape(IGraphicsTarget gc, CoordConverter transformer);

//	/**
//	 * Add the adornment to the specified DGM array.  It
//	 * is safe to assume that <code>createShape()</code>
//	 * has been executed prior to the calling of this
//	 * method.
//	 *
//	 * @param dgm         DGM array in which to place the
//	 *                    translated adornment.
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
//	protected abstract void addToDGM(DGMArray dgm, CoordConverter transformer,
//									 boolean cartesian);
//
//	/**
//	 * Add the adornment to the specified Extended DGM
//	 * array.  It is safe to assume that <code>
//	 * createShape()</code> has been executed prior to
//	 * the calling of this method.
//	 *
//	 * @param dgm         Extended DGM array in which 
//	 *                    to place the translated
//	 *                    adornment.
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
//	protected abstract void addToExtendedDGM(DGMArray dgm, CoordConverter transformer,
//											 boolean cartesian);
}
