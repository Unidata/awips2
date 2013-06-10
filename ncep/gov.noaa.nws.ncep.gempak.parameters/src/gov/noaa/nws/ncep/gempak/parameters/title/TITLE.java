package gov.noaa.nws.ncep.gempak.parameters.title;

import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;

import org.eclipse.swt.graphics.RGB;


/**
 *   TITLE is the title color, title line, and title string separated by slashes:
 *
 *         title color / title line location / title string | short title
 *
 *    If the title color is 0, a title is not plotted.
 *
 *    The title line specifies the line on which the title will be written.
 *    The value of the title line has the following meanings:
 *    
 *    		0      bottom line
 *    		-n      n lines from bottom
 *    		+n      n lines from top
 *    
 *    If the line is not specified, the default is program dependent.
 *    
 *    The title string is the title to be written.  If no title string is
 *    specified, a default title will be determined by the program.
 *    
 *    In the grid display programs, special characters will be replaced
 *    as follows:
 *    		 ^            Forecast date/time
 *    		 ~            Valid date/time
 *    		 @            Vertical level
 *    		 _            Grid function
 *    		 $            Nonzero scaling factor
 *    		 #            Grid point location
 *     		 ?            Day of the week flag
 *    
 *    If the information for which a character stands is not applicable to
 *    the program, nothing is output in its place.  Zero values of the
 *    scaling factor are not displayed.
 *    
 *    If the "?" is included the abbreviated day of the week is added to the
 *    beginning of the date/time string.  The day of the week flag must always
 *    be used in combination with either special character for specifying the
 *    date/time string.  The result is the day of the week for the valid date/time.
 *    
 *    A short title may also be input by the user after a |. This is used
 *    to label the metafile frame in the NC device driver. If the short title
 *    is blank, a suitable label is generated for the frame.  The day of the
 *    week is not included in the short title.
 *    
 *
 * <p>
 *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13,2010   320          X. Guo       Initial Creation
 * Nov 22,2010                X. Guo       Check title old syntax with ^ and ~
 * Mar 06,2013	 683		  Xiaochuan	   Handle 2 digit number for the colors
 * 										   set in title. Using default color when color
 * 										   number over 32. 		
 *                                         
 * </pre>
 * @author xguo
 * @version 1
 * @see $GEMPAK/help/hlx/title.hl2 
 */

public class TITLE {
	//Title Color
	private RGB titleColor = new RGB(255, 0, 0);
	
	//Title Line Location
	private int titleLineLocation = 0;
	
	// Title String
	private String titleString = null;
	
	public TITLE (String titleString) {
		parseTitleString (titleString);
	}
	
	private void parseTitleString (String title) {
		int col = 0;
		String expression_1 = "[-+]?[0-9]";
		String expression_2 = "[-+]?[0-9]{2}";
		String tmp;
		int colors_size = GempakColor.values().length;
		
		if (title == null || title.trim().length() <= 0) return;
		
		/*
		 * check '/' character in the title string and handle the current format
		 */
		if ( title.indexOf('/') >= 0 ) {
			String[] titleStr = title.trim().split("/");
			if ( titleStr.length >= 1 && titleStr[0] != null &&
					titleStr[0].trim().length() > 0) {
				tmp = titleStr[0].trim();
				if ( tmp.matches(expression_1) || tmp.matches(expression_2)) {
					col = Integer.valueOf(tmp);
				}
			}
			if ( col > 0 && col <= colors_size) {
				titleColor = GempakColor.convertToRGB(col);
			}
			else {	// use white color when the number over size of GempakColor
				titleColor = GempakColor.convertToRGB(1);
			}
			
			if ( titleStr.length >= 2 && titleStr[1] != null &&
						titleStr[1].trim().length() > 0) {
				tmp = titleStr[1].trim();
				if ( tmp.matches(expression_1)) {
					titleLineLocation = Integer.valueOf(tmp);
				}
			}
			if ( titleStr.length >= 3 && titleStr[2] != null &&
					titleStr[2].trim().length() > 0) {
				titleString = checkOldTitleSyntax(titleStr[2].trim());
			}
			else {
				titleString = "~ @ _$";
			}
		}
		else {//use white color and whole string
			titleColor = GempakColor.convertToRGB(1);
			titleString = title.trim();
		}
	}

	/**
	 * Check for old syntax with ^ and ~
	 * @return String
	 */
	private String checkOldTitleSyntax ( String title ) {
		String ttlstr = title;
		int pos = ttlstr.indexOf('^');
		
		if ( (ttlstr.charAt(0) == '~') && (pos > 0)) {
			String tmpStr = ttlstr.substring(1,ttlstr.length());
			pos --;
			while ( pos > 0 ) {
				tmpStr = tmpStr.substring(0, pos-1) + "~" + tmpStr.substring(pos + 1, tmpStr.length());
				pos = tmpStr.indexOf('^');
			}
			ttlstr = tmpStr;
		}
		
		return ttlstr;
	}
	
	public RGB getTitleColor() {
		return titleColor;
	}

	public int getTitleLineLoaction() {
		return titleLineLocation;
	}

	public String getTitleString () {
		return titleString;
	}	
}
