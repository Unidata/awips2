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

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.viz.adapter.CoordConverter;



/**
 * The rendered adornment, an abstract class from which
 * various types of adornments that render themselves may
 * be derived.  Examples of rendered adornments include
 * complex multicolored geometric shapes, or text labels.
 *
 * @author Christopher Golden
 */
public abstract class RenderedAdornment extends Adornment {


	// Public Constructors

	/**
	 * Create an instance associated with the specified
	 * glyph and a color of black.
	 *
	 * @param glyph Glyph decorated by this adornment.
	 */
	public RenderedAdornment(MultiPointGlyph glyph) {
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
	public RenderedAdornment(MultiPointGlyph glyph, Color color) {
		super(glyph, color);
	}


	// Public Methods

	/**
	 * Paint this adornment.
	 *
	 * @param gc          Graphics context in which this
	 *                    adornment is to be drawn.
	 * @param transformer Coordinate converter used to
	 *                    translate between lat-long pairs
	 *                    and display coordinates.
	 * @param properties  Display properties to be used.
	 * @param useColor    Color to be used when painting the
	 *                    glyph instead of whatever color(s)
	 *                    would usually be used; if <code>
	 *                    null</code>, standard painting
	 *                    will be done.
	 */
	public abstract void paint(IGraphicsTarget target, CoordConverter transformer,
							   Color useColor);

	/**
	 * Get the bounding shape for this adornment.
	 *
	 * @param  gc          Graphics context in which this
	 *                     adornment is drawn.
	 * @param  transformer Coordinate converter used to
	 *                     translate between lat-long pairs
	 *                     and display coordinates.
	 * @return Bounding shape for this adornment.
	 */
	public abstract Shape getBoundingShape(IGraphicsTarget gc, CoordConverter transformer);
}
