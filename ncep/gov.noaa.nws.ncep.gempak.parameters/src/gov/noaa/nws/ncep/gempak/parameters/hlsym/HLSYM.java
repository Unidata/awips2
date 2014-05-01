package gov.noaa.nws.ncep.gempak.parameters.hlsym;


/**
 *  HLSYM
 *
 *   HLSYM defines the characteristics for the HILO symbols specified
 *   in HILO.  The text sizes, value position, fonts, text widths and
 *   hardware/software flags are specified for the symbols (s) and
 *   plotted values (v) as:
 *      sizes;sizev/position/fonts;fontv/widths;widthv/hwflgs;hwflgv
 *
 *   The size, font, width, and hw flag are the same as the TEXT
 *   variable.  If only one value is given, it is used for both the
 *   symbol and value.
 *
 *   The value plotting position may be 1, 2, or 3 where 2 is the
 *   default.  The number selects the position of the value string
 *   beneath the symbol string.  The three positions are shown below:
 *
 *                                 H
 *                              1  2  3
 *
 *   It is common for HILO symbols near the edge of the display to be
 *   hidden when hardware text font is used. Therefore, when using
 *   hardware text font, the number of HILO symbols displayed may be
 *   slightly less than what the user specifies.
 *   
 *   Examples:
 *   
 *      HLSYM = 2;1/3/2//HW  -- symbol text size = 2
 *                              value text size = 1
 *                              plot value in position 3
 *                              hardware text font 2 applies to both
 *                              
 *      HLSYM = 2/1/1;2/     -- symbol text size = 2
 *                              value text size = 2
 *                              plot value in position 1
 *                              symbol text font = 1
 *                              value text font = 2
 *   This class scans the user input for HLSYM.  The entry for  HLSYM is 
 *   in the above form.
 *   
 * <p>
 *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15,2010   350         X. Guo       Initial Creation
 * Dec 06,2010               X. Guo       Change all sizes to float value
 * Feb 07,2011               X. Guo       Add integer regular expression pattern
 *                                        to check location,font,width
 *                                            
 * @author xguo
 * @version 1
 * @see $GEMPAK/help/hlx/hlsym.hl2
 */

public class HLSYM {
	//Input Hi-Lo symbol parameter
	private String hlSymbolString;
	
	//Text output for marker string
	private String hlMarkerString;
	
	// Text output for value string
	private String hlValueString;
	
	//Label location
	private int hlLableLoc;
	
	//HL symbol parse flag
	private boolean isHLSymParsed;
	
	
	//expression
	private String hlExpre = "((-|\\+)?[0-9]+(\\.[0-9]+)?)+";
	private String intExpre = "[0-9]";
	/**
	 * The default constructor <code>HLSYM</code> generates a single
	 **/
	public HLSYM(){
		/*use the default values*/
		hlsymInit ();
		isHLSymParsed = true;
	}

	/**
	 * HLSYM constructor
	 */
	public HLSYM ( String hlsymString ) {
		
		isHLSymParsed = false;
		hlsymInit ();
		if ( hlsymString != null ) {
			setHLSymInputString ( hlsymString );
			String tmpStr = null;
			if ( hlsymString.length() > 0 ) {
				if(hlsymString.contains(" ")){
					String strWithoutBlank = removeBlanksWithinString (hlsymString);
					if ( strWithoutBlank.length() > 0 ) {
						tmpStr = strWithoutBlank;
					}
				}
				else {
					tmpStr = hlsymString;
				}
			}
			
			if ( tmpStr != null ) {
				parseHLSymbolString ( tmpStr );
			}
			isHLSymParsed = true;
		}
		
	}
	
	/**
	 * Set HL Symbol input string
	 */
	private void setHLSymInputString (String hlsymStr) {
		hlSymbolString = hlsymStr;
	}
	
	/**
	 * Initialize all attributes with the default values
	 */
	private void hlsymInit () {
		hlSymbolString = null;
		hlMarkerString = "1.286";
		hlValueString = "1.0";
		hlLableLoc = 2;
	}
	
	/**
	 * Parse HL symbol parameter
	 */
	private void parseHLSymbolString ( String hlsymString ) {
		int pos, start = 0, cnt = 0;
		String hlStr = hlsymString + "/";
		
		/*
		 * check and see if the input string with 6 strings separated by slashes
		 * otherwise use all default vaules 
		 */
		while (( pos = hlStr.indexOf("/", start)) >= 0 ) {
			
			String tmp = hlStr.substring(start, pos);
			if ( cnt == 0 ) { //parse symbol/value size
				parseHLSize (tmp);
			}
			else if ( cnt == 1 ) { //parse label location
				parseLabelLoc ( tmp);
			}
			else if ( cnt == 2 ) { //parse symbol/value font
				parseHLFont ( tmp );
			}
			else if ( cnt == 3 ) { // parse symbol/value width 
				parseHLWidth (tmp);
			}
			else if ( cnt == 4 ) { //parse symbol/value hardware flag
				parseHLHardwareFlag ( tmp );
			}
			else {
				break;
			}
			
			if ( cnt >= 4 ) break;
			start = pos + 1;
			cnt ++;
		}
	}
	
	/**
	 * Parse Symbol:Value size
	 */
	private void parseHLSize ( String sym ){
		
		float size;
		float sizes = 1.286f, sizev = 1.0f;		
		int start = 0, cnt = 0 ;
		String symStr = sym;
		if ( (symStr != null) && (symStr.length() > 0 ) ) {
			int pos = symStr.indexOf(';');
			if ( pos < 0 ) {
				symStr = sym + ";" + sym ;
			}
			symStr = symStr + ";";
			while ( ( pos = symStr.indexOf(";", start)) >= 0 ) {
				if ( pos != start ) {
					String tmp = symStr.substring(start, pos);
					if ( tmp.matches(hlExpre)) {
						size = Float.valueOf(tmp);
						if ( cnt == 0 ) {
							sizes = convertGempakSizeToCave (size);
						}
						else {
							sizev = convertGempakSizeToCave (size);
						}
					}
		
				}
				if ( cnt >= 1  ) break;
				start =pos + 1;
				cnt ++;
			}
		}
		
		setMarkerString (String.valueOf(sizes));
		setValueString (String.valueOf(sizev));
	}
	
	/**
	 * Parse Label location
	 * There are three position: 1    2     3
	 * The default is 2
	 */
	private void parseLabelLoc ( String loc ) {
		
		if ( loc.matches(intExpre)) {
			int value = Integer.valueOf(loc);
			if ( ( 1 <= value ) && ( value <= 3)) {
				setLabelLocation ( value );
			}
		}
	}
	
	/**
	 * Parse font
	 */
	private void parseHLFont ( String font ) {
		int size, fonts = 1, fontv = 1;
		int start = 0, cnt = 0 ;
		String fontStr = font;
		int pos = fontStr.indexOf(';');
		
		if ( pos < 0 ) {
			fontStr = font + ";" + font ;
		}
		fontStr = fontStr + ";";
		while ( ( pos = fontStr.indexOf(";", start)) >= 0 ) {
			if ( pos != start ) {
				String tmp = fontStr.substring(start, pos);
				if ( tmp.matches(intExpre)) {
					size = Integer.valueOf(tmp);
					if ( cnt == 0 ) {
						fonts = convertGempakFontSizeToCave (size);
					}
					else {
						fontv = convertGempakFontSizeToCave (size);
					}
				}
				
			}
			if ( cnt >= 1  ) break;
			start =pos + 1;
			cnt ++;
		}
		
		appendMarkerString ("/" + String.valueOf(fonts));
		appendValueString ( "/" + String.valueOf(fontv));
	}
	
	/**
	 * Parse symbol/value width
	 */
	private void parseHLWidth ( String width ) {
		int size, widths = 2, widthv = 2;
		int start = 0, cnt = 0 ;
		String widthStr = width;
		int pos = widthStr.indexOf(';');
		
		if ( pos < 0 ) {
			widthStr = width + ";" + width ;
		}
		widthStr = widthStr + ";";
		while ( ( pos = widthStr.indexOf(";", start)) >= 0 ) {
			if ( pos != start ) {
				String tmp = widthStr.substring(start, pos);
				if ( tmp.matches(intExpre)) {
					size = Integer.valueOf(tmp);
					if ( cnt == 0 ) {
						widths = convertGempakWidthSizeToCave (size);
					}
					else {
						widthv = convertGempakWidthSizeToCave (size);
					}
				}
				
			}
			if ( cnt >= 1  ) break;
			start =pos + 1;
			cnt ++;
		}
		
		appendMarkerString ("/" + String.valueOf(widths));
		appendValueString ( "/" + String.valueOf(widthv));
	}
	
	/**
	 * Parse hardware flag
	 *        HW -- Hardware fonts
	 *        SW -- Software fonts
	 *        Default is SW
	 */
	private void parseHLHardwareFlag ( String hwflag ) {
		String hwflg, hwflgs = null, hwflgv = null;
		int start = 0, cnt = 0 ;
		String hwflagStr = hwflag;
		int pos = hwflagStr.indexOf(';');
		
		if ( pos < 0 ) {
			hwflagStr = hwflag + ";" + hwflag ;
		}
		hwflagStr = hwflagStr + ";";
		while ( ( pos = hwflagStr.indexOf(";", start)) >= 0 ) {
			if ( pos != start ) {
				String tmp = hwflagStr.substring(start, pos);
				if ( tmp.toUpperCase().compareTo("HW") == 0) {
					hwflg = "HW";
				}
				else {
					hwflg = "SW";
				}
				if ( cnt == 0 ) {
					hwflgs = hwflg;
				}
				else {
					hwflgv = hwflg;
				}
				
			}
			if ( cnt >= 1  ) break;
			start =pos + 1;
			cnt ++;
		}
		
		
		if ( hwflgs != null ) {
			appendMarkerString ("/" + hwflgs);
		}
		if ( hwflgv != null ) {
			appendValueString ( "/" + hwflgv);
		}
	}
	
	/**
	 * Set Text output for marker string
	 */
	private void setMarkerString ( String value) {
		hlMarkerString = value;
	}
	
	/**
	 * Append Text output for marker string
	 */
	private void appendMarkerString ( String value) {
		hlMarkerString = hlMarkerString + value;
	}
	
	/**
	 * Set Text output for value string
	 */
	private void setValueString ( String value) {
		hlValueString = value;
	}
	
	/**
	 * Append Text output for value string
	 */
	private void appendValueString ( String value) {
		hlValueString = hlValueString + value;
	}
	
	/**
	 * Set label location
	 */
	private void setLabelLocation ( int loc ){
		hlLableLoc = loc;
	}
	
	/**
	 * Convert GEMPAK symbol/value size to CAVE
	 *  Name       GEMPAK Size        CAVE Size
	 *  Tiny          0.714               10
	 *  Small         0.857               12
	 *  Medium        1.000               14
	 *  Large         1.286               18
	 *  Huge          1.714               24
	 *  Giant         2.429               34      
	 */
	private float convertGempakSizeToCave ( float value ) {
		float size = 1.0f;
		float [] range = {0.714f, 0.857f, 1.000f, 1.286f, 1.714f, 2.429f };
		int len = range.length;

		if ( (range[0] <= value ) && ( value <= range[len-1]) ) {
			for ( int i = 0; i < len-1; i ++ ) {
				if ( ( range[i] <= value ) && ( value <= range[i+1]) ) {
					if ( ( value - range[i] ) <= ( range[i+1] - value ) ) {
						size = range[i];
					}
					else {
						size = range[i+1];
					}
					break;
				}
			}
		}
		else if ( value > range[len-1] ) {
			size = range[len-1];
		}
		
		return size;
	}
	
	/**
	 * Convert GEMPAK font size to CAVE
	 * In CAVE, there are three fonts:
	 *         Name             Value
	 *         Courier            1
	 *         Nimbus Sans L      2
	 *         Liberation Serif   3
	 */
	private int convertGempakFontSizeToCave ( int value) {
		int size = 1;
		
		if ( ( 1 <= value ) && ( value <= 3 )) {
			size = value;
		}
		return size;
	}
	
	/**
	 * Convert GEMPAK width size to CAVE
	 * In GEMPAK, the width range is 1 - 9. Default is 2
	 */
	private int convertGempakWidthSizeToCave ( int value) {
		int size = 2;
		
		if ( ( 1 <= value ) && ( value <= 9 )) {
			size = value;
		}
		return size;
	}
	
	/**
	 * The method <tt>removeBlanksWithinString</tt> splits the input string into
	 * tokens using a blank-space as a delimiter.
	 * Then it concatenates the tokens, as long as the tokens are non-empty.
	 * @param String inputString
	 * @return String outputString
	 */
	private String removeBlanksWithinString (String inputString){
		StringBuffer strBuff = new StringBuffer();
		String outputString = " ";
		String tokens[] = inputString.split(" ");
		for(String s : tokens){
			if(!(s.isEmpty())){
				strBuff.append(s);
			}
		}
		outputString = strBuff.toString();
		outputString.trim();
        return outputString;
	}
	/**
	 * Get HL Symbol input parameter
	 * @return String
	 */
	public String getHLSymbolInputString () {
		return hlSymbolString;
	}
	
	/**
	 * Get HL text out marker string
	 * @return String
	 */
	public String getMarkerString () {
		return hlMarkerString;
	}
	
	/**
	 * Get HL text output value string
	 * @return String
	 */
	public String getValueString () {
		return hlValueString;
	}
	
	/**
	 * Get HL Label location
	 * @return int
	 */
	public int getLabelLoc (){
		return hlLableLoc;
	}
	
	/**
	 * @return boolean
	 */
	public boolean isHLSymbolStringParsed () {
		return this.isHLSymParsed;
	}
}
