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
package fsl.tools.scribble;

import java.awt.Color;
import fsl.tools.glyph.TriangleEdgeAdornment;

/**
 * The drawable representation for a cold front, with
 * with semicircular protrusions lining one side of
 * the path.  The path may be created with an arbitrary
 * thickness and symbol size.
 *
 * @author Christopher Golden
 */
public class ColdFront extends Front {


	// Protected Variables

	/**
	 * Flag indicating whether or not the front is aloft.
	 */
	protected boolean aloft;


	// Public Constructors

	/**
	 * Create an instance with the specified thickness,
	 * symbol size, symbol interval, arrangement of
	 * symbols with respect to on which side of the
	 * path they are drawn, and altitude (aloft or on
	 * the ground).
	 *
	 * @param frame          Frame in which this glyph
	 *                       exists; if <code>
	 *                       ALL_FRAMES</code>, the
	 *                       glyph exists in all frames.
	 * @param thickness      Width of the path.
	 * @param smoothing      Smoothing type; must be one
	 *                       of the various <code>
	 *                       SMOOTHING_xxx</code>
	 *                       constants.
	 * @param symbolSize     Width of each cold front
	 *                       symbol.
	 * @param symbolInterval Interval along the path
	 *                       between each cold front
	 *                       symbol.
	 * @param rightSide      Flag indicating whether
	 *                       the symbols should be on
	 *                       the left or the right side
	 *                       of the path.
	 * @param aloft          Flag indicating whether
	 *                       the front is aloft or not.
	 */
	public ColdFront(int frame, int thickness, int smoothing, double symbolSize,
					 double symbolInterval, boolean rightSide, boolean aloft) {
		super(frame, Color.blue, SOLID_STYLE, thickness, smoothing,
			  symbolSize, symbolInterval, rightSide);
		this.aloft = aloft;
		createAdornments();
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

		// These are equal only if the superclass says so
		// and they are both cold fronts.
		return (super.equals(obj) && (obj instanceof ColdFront));
    }


	// Protected Methods

	/**
	 * Create the adornments.
	 */
	protected void createAdornments() {
		super.createAdornments();
//		addAdornment(new TriangleEdgeAdornment(this, Color.cyan, rightSide,
		addAdornment(new TriangleEdgeAdornment(this, Color.blue, rightSide,
											   intervalBetween, intervalWithin, 0,
											   !aloft));
	}
}
