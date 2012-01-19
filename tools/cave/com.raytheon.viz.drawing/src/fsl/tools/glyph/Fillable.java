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

/**
 * Interface describing the methods that must be implemented
 * and the constants used by glyphs that may be filled with
 * a solid color or pattern fill.
 *
 * @author Christopher Golden
 */
public interface Fillable {


	// Public Static Constants

	/**
	 * Sparse fill pattern for closed and non-transparent
	 * glyphs.
	 */
	public static final int SPARSE_FILL = 0;

	/**
	 * Medium fill pattern for closed and non-transparent
	 * glyphs.
	 */
	public static final int MEDIUM_FILL = 1;

	/**
	 * Dense fill pattern for closed and non-transparent
	 * glyphs.
	 */
	public static final int DENSE_FILL = 2;

	/**
	 * Solid fill pattern for closed and non-transparent
	 * glyphs.
	 */
	public static final int SOLID_FILL = 3;


	// Public Methods

	/**
	 * Find out whether or not the border is visible.
	 *
	 * @return Flag indicating whether or not the border
	 *         is visible.
	 */
	public boolean isBorderVisible();

	/**
	 * Set the state of the border's visibility.
	 *
	 * @param visible Flag indicating whether or not the
	 *                border is to be visible.
	 */
	public void setBorderVisible(boolean visible);

	/**
	 * Get the fill color.
	 *
	 * @return Fill color.
	 */
	public Color getFillColor();

	/**
	 * Set the fill color to the specified color.
	 *
	 * @param fillColor New fill color of the glyph.
	 */
	public void setFillColor(Color fillColor);

	/**
	 * Get the fill pattern.
	 *
	 * @return Fill pattern; one of <code>
	 *         SPARSE_FILL</code>, <code>
	 *         MEDIUM_FILL</code>, <code>
	 *         DENSE_FILL</code>, or </code>
	 *         SOLID_FILL</code>.
	 */
	public int getFillPattern();

	/**
	 * Set the fill pattern to the specified pattern.
	 *
	 * @param fillPattern New fill pattern; one of
	 *         <code>SPARSE_FILL</code>, <code>
	 *         MEDIUM_FILL</code>, <code>
	 *         DENSE_FILL</code>, or </code>
	 *         SOLID_FILL</code>.
	 */
	public void setFillPattern(int fillPattern);
}
