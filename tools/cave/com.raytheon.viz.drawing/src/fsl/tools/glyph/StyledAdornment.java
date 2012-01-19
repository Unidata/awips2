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


/**
 * The styled adornment, an abstract base class from which
 * shape adornments that have the same thickness and line
 * style as the glyphs they adorn may be derived.
 *
 * @author Christopher Golden
 */
public abstract class StyledAdornment extends ShapeAdornment {


	// Public Constructors

	/**
	 * Create an instance associated with the specified
	 * glyph and a color of black.
	 *
	 * @param glyph Glyph decorated by this adornment.
	 */
	public StyledAdornment(MultiPointGlyph glyph) {
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
	public StyledAdornment(MultiPointGlyph glyph, Color color) {
		super(glyph, color);
	}


	// Protected Methods

//	/**
//	 * Add the adornment to the specified DGM array.  It
//	 * is safe to assume that <code>createShape()</code>
//	 * has been executed prior to the calling of this
//	 * method.
//	 * <p>
//	 * Note that subclasses should override this method,
//	 * calling this one at the beginning of their own
//	 * in order to set up the DGM line texture according
//	 * to the parent glyph's line style.
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
//	protected void addToDGM(DGMArray dgm, CoordConverter transformer,
//							boolean cartesian) {
//
//		// Set the line texture to the appropriate style.
//		if (glyph.getStyle() == MultiPointGlyph.DASHED_STYLE)
//			dgm.setLineTexture((short) 0x00FF, (short) 16);
//		else
//			dgm.setLineTexture((short) 0xFFFF, (short) 0);
//	}
//
//	/**
//	 * Add the adornment to the specified Extended DGM
//	 * array.  It is safe to assume that <code>
//	 * createShape()</code> has been executed prior to
//	 * the calling of this method.
//	 * <p>
//	 * Note that subclasses should override this method,
//	 * calling this one at the beginning of their own
//	 * in order to set up the DGM line texture according
//	 * to the parent glyph's line style.
//	 *
//	 * @param dgm         Extended DGM array in which to
//	 *                    place the translated adornment.
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
//	protected void addToExtendedDGM(DGMArray dgm, CoordConverter transformer,
//									boolean cartesian) {
//
//		// Set the line texture to the appropriate style.
//		if (glyph.getStyle() == MultiPointGlyph.DASHED_STYLE)
//			dgm.setLineTexture((short) 0x00FF, (short) 16);
//		else
//			dgm.setLineTexture((short) 0xFFFF, (short) 0);
//	}

	/**
	 * Convert the existing shape to one that is styled
	 * appropriately.  This method should be called at the
	 * end of any subclass definition of createShape() in
	 * order to style the subclass-specific shape in an
	 * appropriate manner.
	 *
	 * @param cap  The line cap style.
	 * @param join The segment join style.
	 *
	 * @see java.awt.BasicStroke
	 */
	protected void styleShape(int cap, int join) {

		// Create the stroking object and then stroke the
		// existing shape.
		float[] dashArray = { 6.0f, 6.0f };
		int style = glyph.getStyle();
		float dashOffset = (style == MultiPointGlyph.DASHED_STYLE ? 9.0f : 0.0f);
		BasicStroke stroke =
			new BasicStroke(glyph.getThickness(), cap, join, 10.0f,
							(style == MultiPointGlyph.DASHED_STYLE ? dashArray :
							 null), dashOffset);
		shape = stroke.createStrokedShape(shape);
	}
}
