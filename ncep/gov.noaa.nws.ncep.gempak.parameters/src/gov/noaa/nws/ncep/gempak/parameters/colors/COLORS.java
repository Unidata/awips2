package gov.noaa.nws.ncep.gempak.parameters.colors;

import org.eclipse.swt.graphics.RGB;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;

/**
 *   COLORS specifies a list of color numbers which must be separated
 *    using semicolons:
 *
 *               color 1 ; color 2 ; ... ; color n
 *
 *    The last color in the list will be repeated, if necessary, to
 *    determine all colors required by the program.
 * 
 *
 * <p>
 *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * July 2010                M. Li       Initial Creation
 *                                         
 * </pre>
 * @author mli
 * @version 1
 * @see $GEMPAK/help/hlx/colors.hl2 
 */

public class COLORS {

	private RGB[] colors;
	
	public COLORS(String colorsAttributeString) {
		parseColorsAttributes(colorsAttributeString);
	}

	private void parseColorsAttributes(String colorsAttributeString) {
		if (colorsAttributeString == null || colorsAttributeString.trim().length() <= 0) return;
		
		String[] colorsStrings = colorsAttributeString.trim().split(";");
		colors = new RGB[colorsStrings.length];
		for(int i = 0; i < colorsStrings.length; i++) {
			colors[i] = GempakColor.convertToRGB(Integer.valueOf(colorsStrings[i]));
		}
	}

	public RGB getFirstColor() {
		return colors[0];
	}
	
	public RGB[] getColors() {
		return colors;
	}
}
