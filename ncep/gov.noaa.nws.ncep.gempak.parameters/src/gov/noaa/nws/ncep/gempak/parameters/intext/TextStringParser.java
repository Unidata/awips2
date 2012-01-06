package gov.noaa.nws.ncep.gempak.parameters.intext;


/**
 * TEXT
 *
 *
 *   TEXT is the size, font, text width and hardware/software flag for
 *   graphics text separated with slashes: 
 *   
 *                   text size / font / width / hw flag
 *                   
 *   The size may be a real number multiplier for the default text
 *   size.  If the size is zero or unspecified, the current size
 *        will be used.
 *        
 *   The size may also be a name, or the first character of a name.
 *   The name is converted to a real number multiplier.  For hardware 
 *   text, the named sizes correspond to discrete point sizes.  Sizes
 *   other than the named sizes will be rounded to the nearest point
 *   size.  Any size may be given for software text.  The standard
 *   names and size values are given in the table FONTSZ.TBL:
 *   
 *              Name            Size    HW Point
 *              ----            ----    --------
 *              Tiny            0.714      10
 *              Small           0.857      12
 *              Medium          1.000      14
 *              Large           1.286      18
 *              Huge            1.714      24
 *              Giant           2.429      34
 *              
 *   The text width is the integer line width to be used in generating
 *   software text.  If the text size, font or width is not specified,
 *   the current value is used.
 *   
 *   The text border is a border/blank fill flag.  If the input value is
 *   less than or equal to zero, the current value is used.  Border is a
 *   three digit number, ABC, where:
 *   
 *      A - Border      B - Blank Fill          C - Border Type 
 *      ----------      --------------          ---------------
 *      1 = No          1 = No                  1 = Regular Box
 *      2 = Yes         2 = Yes                 2 = Low Pressure Box
 *                      3 = Reverse video       3 = High Pressure Box
 *                                              4 = Freezing Level Box
 *                                              5 = Underline
 *                                              6 = Overline
 *                                              
 *   If reverse video is selected, the text color and background color
 *   are switched.  The result is similar to a photographic negative.
 *   Low and high border types refer to the low and high outlines for
 *   use on aviation forecast products.
 *   
 *   The text rotation is a character input that specifies whether the
 *   text is aligned with the screen (S) or with north (N) on a given
 *   image. If the choice is invalid or not specified, the current value
 *   will be used.
 *   
 *   The text justification is a character input that specifies whether
 *   the text is justified to the center (C), right (R), or left (L).
 *   If the choice is invalid or not specified, the current value will
 *   be used.
 *   
 *   The hardware/software selector must be HW or SW to change to
 *   hardware- or software-generated text.  This selector can appear
 *   anywhere in the string.
 *   
 *   The font number must be specified by using the HW selector and
 *   choosing a font number from the list below.
 *   
 *                   REGULAR    ITALIC    BOLD     ITALIC-BOLD
 *      Courier         1         11       21         31
 *      Helvetica       2         12       22         32
 *      Times           3         13       23         33
 *      
 *   There are also four software fonts.
 *   
 *                           REGULAR    ITALIC    BOLD     ITALIC-BOLD
 *      Mixed Case              1         --       21         --
 *      All Upper Case          2         --       22         --
 *      Helvetica Emulation     3         --       23         --
 *      Times Emulation         4         14       24         34
 *   If a font does not support a particular style, the next lower
 *   style is used instead. For example, font 1 does not support Italic,
 *   therefore Italic is converted to regular.
 *   
 *   If a particular device driver does not support hardware fonts,
 *   the requested font is emulated using the appropriate software font.
 *   
 *   Examples:
 *   
 *      TEXT = 1/21/SW          --      text size = 1; bold software font 1
 *      
 *      TEXT = 1/2/HW           --      text size = 1; hardware text font 2
 *      
 *      TEXT = 2.5              --      text size = 2.5; current text font
 *      
 *      TEXT = 1.24///221/s/c   --      text size = 1.24; current text font;
 *                                      current text width; Border = yes,
 *                                      Blank fill = yes, Border type = box;
 *                                      screen relative; center justified
 * <p>
 *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30,2010   363         X. Guo       Initial Creation
 * Dec 06,2010               X. Guo       Add symbol size
 *                                         
 * </pre>
 * @author xguo
 * @version 1
 * @see $GEMPAK/help/hlx/txt.hl2 */

public class TextStringParser {
	
	//Input Text String
	private String inputTxtStr;
	
	//Symbol/Marker size
	private float symbolSize;
	
	//Text size
	private int textSize;
	
	//Text font
	private int font;
	
	//Text width
	private int width;
	
	//Text HW flag
	private String hwflg;
	
	//Parse Flag
	private boolean isTextStringParsed;
	
	//expression
	private String textExpre = "((-|\\+)?[0-9]+(\\.[0-9]+)?)+";
	
	/**
	 * The default constructor <code>HILOStringParser</code> generates a single
	 **/
	public TextStringParser(){
		/*A single line with all default attributes set is created*/
		initDefaultValues ();
		isTextStringParsed = false;
	}

	//TEXT construct
	public TextStringParser ( String textStr ) {
		
		isTextStringParsed = false;
		String tmpStr = null;
		inputTxtStr = textStr;
		
		initDefaultValues ();
		if(!(textStr == null) && !(textStr.isEmpty())){
			if(textStr.contains(" ")){
				String strWithoutBlank = removeBlanksWithinString (textStr);
				if ( strWithoutBlank.length() > 0 ) {
					tmpStr = strWithoutBlank;
				}
			}
			else {
				tmpStr = textStr;
			}
		}
		
		if ( tmpStr != null ) {
			isTextStringParsed = parseTextString ( tmpStr );
		}
	}
	
	/*
	 * Parse TEXT input string
	 * This subroutine scans the user input for TEXT.  This consists of 
     * four input strings separated by slashes.  
     * size/font/width/hw flag  
	 */
	private boolean parseTextString ( String txtStr ) {
		int pos, start = 0, cnt = 0;
		String textStr = txtStr + "/";
		
		/*
		 * check and see if the input string with 4 strings separated by slashes
		 */
		while (( pos = textStr.indexOf("/", start)) >= 0 ) {
			if ( pos != start ) {
				String tmp = textStr.substring(start, pos);
				if ( cnt == 0 ) { //parse size
					if ( tmp.matches(textExpre) ) {
						convertGempakSizeToCaveNumber (Float.valueOf(tmp));
					}
					else {
						convertTextNameToCaveNumber(tmp);
					}
				}
				else if ( cnt == 1 ) { //parse font
					if ( tmp.matches(textExpre) ) {
						setTextFont (checkFontSize(Integer.valueOf(tmp)));
					}
					else {
						setTextFont (1);
					}
				}
				else if ( cnt == 2 ) { //parse width
					if ( tmp.matches(textExpre) ) {
						setTextWidth (Integer.valueOf(tmp));
					}
					else {
						setTextWidth (2);
					}
				}
				else if ( cnt == 3 ) { // parse hw flag
					setTextHWFlag (tmp);
				}
				else {
					break;
				}
			}
			start = pos + 1;
			cnt ++;
		}

		return true;
	}
	
	/**
	 * Initialize default values
	 */
	private void initDefaultValues () {
		symbolSize = 1.286f;
		textSize = 14;
		font = 1;
		width = 2;
		hwflg = "HW";
	}
	
	/**
	 * Set Symbol/Marker size
	 */
	private void setSymbolMarkerSize ( float size ) {
		this.symbolSize = size ;
	}
	
	/**
	 * Get Symbol/Marker size
	 */
	public float getSymbolMarkerSize () {
		return symbolSize;
	}
	
	/**
	 * Set Text size
	 */
	private void setTextSize ( int size ) {
		this.textSize = size;
	}
	
	/**
	 * Get Text size
	 */
	public int getTextSize () {
		return textSize ;
	}
	
	/**
	 * Set Text font
	 */
	private void setTextFont ( int font ) {
		this.font = font;
	}
	
	/**
	 * Get Text font
	 */
	public int getTextFont () {
		return font;
	}
	
	/**
	 * Set Text width
	 */
	private void setTextWidth ( int width ) {
		this.width = width;
	}
	
	/**
	 * Get Text width
	 */
	public int getTextWidth () {
		return width;
	}
	
	/**
	 * Set Text HW flag
	 */
	private void setTextHWFlag ( String hwflg ) {
		this.hwflg = hwflg ;
	}
	
	/**
	 * Get Text HW flag
	 */
	public String getTextHWFlag () {
		return hwflg;
	}
	/**
	 * Get parse status
	 */
	public boolean isTextParsed () {
		return isTextStringParsed;
	}
	
	/**
	 * Get Input Text String
	 */
	public String getInputTextString () {
		return inputTxtStr;
	}
	/**
	 * Convert size name to CAVE number
	 *  Name       GEMPAK Size        CAVE Size
	 *  Tiny          0.714               10
	 *  Small         0.857               12
	 *  Medium        1.000               14
	 *  Large         1.286               18
	 *  Huge          1.714               24
	 *  Giant         2.429               34      
	 */
	private void convertTextNameToCaveNumber ( String textName ) {
		
		float sizes = 1.0f;
		int sizev = 14;
		int [] fontsize = {10, 12, 14, 18, 24, 34 };
		float [] range = {0.714f, 0.857f, 1.000f, 1.286f, 1.714f, 2.429f };
		String [] name = {"TINY","SMALL","MEDIUM","LARGE","HUGE","GIANT"};
		
		
		for ( int i = 0; i < fontsize.length-1; i ++ ) {
			String txtName1 = textName.substring(0, 1);
			String name1 = name[i].substring(0, 1);
			if ( textName.toUpperCase().compareTo(name[i]) == 0 ||
					txtName1.toUpperCase().compareTo(name1) == 0) {
				sizev = fontsize[i] ;
				sizes = range[i] ;
				break;
			}	
		}
		setTextSize (sizev );
		setSymbolMarkerSize (sizes);
	}
	
	/**
	 * Convert size name to CAVE number
	 *  Name       GEMPAK Size        CAVE Size
	 *  Tiny          0.714               10
	 *  Small         0.857               12
	 *  Medium        1.000               14
	 *  Large         1.286               18
	 *  Huge          1.714               24
	 *  Giant         2.429               34      
	 */
	private void convertGempakSizeToCaveNumber ( float value ) {
		
		float sizes = 1.0f;
		int sizev = 14;
		int [] fontsize = {10, 12, 14, 18, 24, 34 };
		float [] range = {0.714f, 0.857f, 1.000f, 1.286f, 1.714f, 2.429f };	
		int len = range.length;
		
		if ( value <= range[len-1] ) {
			for ( int i = 0; i < len-1; i ++ ) {
				if ( ( range[i] <= value ) && ( value <= range[i+1]) ) {
					if ( ( value - range[i] ) <= ( range[i+1] - value ) ) {
						sizev = fontsize[i] ;
						sizes = range[i] ;
					}
					else {
						sizev = fontsize[i+1] ;
						sizes = range[i+1] ;
					}
					break;
				}
			}	
		}
		else {
			sizev = fontsize[len-1] ;
			sizes = range[len-1] ;
		}
		setTextSize (sizev );
		setSymbolMarkerSize (sizes);
	}
	
	/**
	 * Check font size
	 * In CAVE, there are three fonts:
	 *         Name             Value
	 *         Courier            1
	 *         Nimbus Sans L      2
	 *         Liberation Serif   3
	 */
	private int checkFontSize ( int value) {
		int size = 1;
		
		if ( ( 1 <= value ) && ( value <= 3 )) {
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
}
