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
import fsl.tools.glyph.FreehandPath;

/**
 * The front, a base class from which fronts may be derived.
 * Essentially, the front adds tracking for space between
 * decorations and within decorations to its superclass, as
 * well as tracking of which side the primary decorations
 * are on.
 *
 * @author Christopher Golden
 */
public class Front extends FreehandPath {


	// Protected Variables

	/**
	 * Space within decorations.
	 */
	protected double intervalWithin;

	/**
	 * Space between decorations.
	 */
	protected double intervalBetween;

	/**
	 * Flag indicating whether or not the primary decorations
	 * are on the right side of the path.
	 */
	protected boolean rightSide;


	// Public Constructors

	/**
	 * Construct an instance with the specified properties
	 * that is not closed.
	 *
	 * @param frame     Frame in which this glyph exists; if
	 *                  <code>ALL_FRAMES</code>, the glyph
	 *                  exists in all frames.
	 * @param color     Color of the path.
	 * @param style     Style of the path.
	 * @param thickness Thickness of the path.
	 * @param smoothing Smoothing type; must be one of the
	 *                  various <code>SMOOTHING_xxx</code>
	 *                  constants.
	 * @param within    Interval within each decoration.
	 * @param between   Interval between each decoration.
	 * @param rightSide Flag indicating whether or not the
	 *                  primary decorations are on the right
	 *                  side of the path.
	 */
	public Front(int frame, Color color, int style, int thickness, int smoothing,
				 double within, double between, boolean rightSide) {
		super(frame, color, style, thickness, smoothing);
		this.intervalWithin = within;
		this.intervalBetween = between;
		this.rightSide = rightSide;
	}

	/**
	 * Construct an instance with the specified properties.
	 *
	 * @param frame     Frame in which this glyph exists; if
	 *                  <code>ALL_FRAMES</code>, the glyph
	 *                  exists in all frames.
	 * @param border    Flag indicating whether or not the
	 *                  border of the path should be drawn
	 *                  if the path is closed; if it is
	 *                  open, this value is ignored.
	 * @param color     Color of the path.
	 * @param style     Style of the path.
	 * @param thickness Thickness of the path.
	 * @param smoothing Smoothing type; must be one of the
	 *                  various <code>SMOOTHING_xxx</code>
	 *                  constants.
	 * @param closed    Flag indicating whether or not the
	 *                  path is to be closed, and thus form
	 *                  a polygon.
	 * @param fillColor Color of fill of the polygon, if the
	 *                  path is closed. If the path is open,
	 *                  this is ignored.
	 * @param within    Interval within each decoration.
	 * @param between   Interval between each decoration.
	 * @param rightSide Flag indicating whether or not the
	 *                  primary decorations are on the right
	 *                  side of the path.
	 */
	public Front(int frame, boolean border, Color color, int style, int thickness,
				 int smoothing, boolean closed, Color fillColor, double within,
				 double between, boolean rightSide) {
		super(frame, border, color, style, thickness, smoothing,
			  closed, fillColor, SOLID_FILL);
		this.intervalWithin = within;
		this.intervalBetween = between;
		this.rightSide = rightSide;
	}

	/**
	 * Construct an instance with the specified properties.
	 *
	 * @param frame       Frame in which this glyph exists; if
	 *                    <code>ALL_FRAMES</code>, the glyph
	 *                    exists in all frames.
	 * @param border      Flag indicating whether or not the
	 *                    border of the path should be drawn
	 *                    if the path is closed; if it is
	 *                    open, this value is ignored.
	 * @param color       Color of the path.
	 * @param style       Style of the path.
	 * @param thickness   Thickness of the path.
	 * @param smoothing   Smoothing type; must be one of the
	 *                    various <code>SMOOTHING_xxx</code>
	 *                    constants.
	 * @param closed      Flag indicating whether or not the
	 *                    path is to be closed, and thus form
	 *                    a polygon.
	 * @param fillColor   Color of fill of the polygon, if the
	 *                    path is closed. If the path is open,
	 *                    this is ignored.
	 * @param fillPattern Fill pattern of the polygon, if
	 *                    the front is closed; must be <code>
	 *                    SPARSE_FILL</code>, <code>
	 *                    MEDIUM_FILL</code>, <code>
	 *                    DENSE_FILL</code>, or </code>
	 *                    SOLID_FILL</code>.  If the front
	 *                    is open, this is ignored.
	 * @param within      Interval within each decoration.
	 * @param between     Interval between each decoration.
	 * @param rightSide   Flag indicating whether or not the
	 *                    primary decorations are on the right
	 *                    side of the path.
	 */
	public Front(int frame, boolean border, Color color, int style, int thickness,
				 int smoothing, boolean closed, Color fillColor, int fillPattern,
				 double within, double between, boolean rightSide) {
		super(frame, border, color, style, thickness, smoothing,
			  closed, fillColor, fillPattern);
		this.intervalWithin = within;
		this.intervalBetween = between;
		this.rightSide = rightSide;
	}


	// Public Methods

	/**
	 * Get the interval within decorations.
	 *
	 * @return Interval within decorations.
	 */
	public double getIntervalWithin() {
		return intervalWithin;
	}

	/**
	 * Set the interval within decorations.
	 *
	 * @param within Interval within decorations.
	 */
	public void setIntervalWithin(double within) {
		intervalWithin = within;
		createAdornments();
		flushDisplayCoordinates();
	}

	/**
	 * Get the interval between decorations.
	 *
	 * @return Interval between decorations.
	 */
	public double getIntervalBetween() {
		return intervalBetween;
	}

	/**
	 * Set the interval between decorations.
	 *
	 * @param between Interval between decorations.
	 */
	public void setIntervalBetween(double between) {
		intervalBetween = between;
		createAdornments();
		flushDisplayCoordinates();
	}

	/**
	 * Find out whether or not the primary decorations
	 * are on the right side.
	 *
	 * @return True if the primary decorations are on
	 *         the right side, false otherwise.
	 */
	public boolean isOnRightSide() {
		return rightSide;
	}

	/**
	 * Set the flag indicating whether or not the primary
	 * decorations are on the right side.
	 *
	 * @param rightSide Flag indicating whether or not
	 *                  the primary decorations are on
	 *                  the right side.
	 */
	public void setRightSide(boolean rightSide) {
		this.rightSide = rightSide;
		createAdornments();
		flushDisplayCoordinates();
	}


	// Protected Methods

	/**
	 * Create the adornments.  This implementation of the
	 * method merely clears the old adornments away; this
	 * method should be overridden to add the required
	 * adornments.
	 */
	protected void createAdornments() {
		removeAllAdornments();
	}
}

