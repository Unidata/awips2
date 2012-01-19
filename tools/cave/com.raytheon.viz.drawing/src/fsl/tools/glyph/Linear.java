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


/**
 * Interface describing the methods that must be implemented
 * and the constants used by glyphs that are in some fashion
 * linear, i.e. they are lines, paths, polygons, shapes, etc.
 * that may have different styles and thicknesses for their
 * paths or borders.
 *
 * @author Christopher Golden
 */
public interface Linear {


	// Public Static Constants

	/**
	 * Value indicating solid style.
	 */
	public static final int SOLID_STYLE = 0;

	/**
	 * Value indicating dashed style.
	 */
	public static final int DASHED_STYLE = 1;


	// Public Methods

	/**
	 * Get the line style of the glyph.
	 *
	 * @return Line style of the glyph.
	 */
	public int getStyle();

	/**
	 * Set the line style of the glyph.
	 *
	 * @param style New line style of the glyph.
	 */
	public void setStyle(int style);

	/**
	 * Get the line thickness of the glyph.
	 *
	 * @return Line thickness of the glyph.
	 */
	public int getThickness();

	/**
	 * Set the line thickness of the glyph.
	 *
	 * @param thickness New line thickness of the
	 *                  glyph.
	 */
	public void setThickness(int thickness);
}
