package gov.noaa.nws.ncep.gempak.parameters.intext;


/**
 * TEXT
 *
 *
 *   TEXT is the size, font, text width and hardware/software flag for
 *   graphics text separated with slashes: 
 *   
 *                   text size / font / width/ border / rotation / justification /  / hw flag
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
 * Sep 09, 2013  1036        S. Gurung    Added border, justification, rotation and textStyle
 *                                         
 * </pre>
 * @author xguo
 * @version 1
 * @see $GEMPAK/help/hlx/text.hl2 */

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
		
	//Text border
	private int border;
	
	//Text justification
	private char justification;
	
	//Text rotation
	private char rotation;

	//Text style (normal/bold/italic)
	private int textStyle;
	
	//Parse Flag
	private boolean isTextStringParsed;
	
	//expression
	private String textExpre = "((-|\\+)?[0-9]+(\\.[0-9]+)?)+";
	
	//font expression
	private String fontExpre = "[1-3]+";
		
	//border expression
	private String borderExpre = "[1-2][1-3][1]";
	
	//rotation expression
	private String rotExpre = "s|n|S|N";
	
	//justification expression
	private String justExpre = "c|C|r|R|l|L";
		
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
		 * check and see if the input string with 4 or 7 strings separated by slashes
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
					if ( tmp.matches(fontExpre) ) {
						if (tmp.length()==1){
							setTextFont (checkFontSize(Integer.valueOf(tmp)));
						}
						else {
							setTextFont (checkFontSize(Integer.valueOf(tmp.substring(1,2))));
							setTextStyle(Integer.valueOf(tmp.substring(0,1)));
						}
					}
					else {
						setTextFont (1);
					}
				}
				else if ( cnt == 2 ) { //parse width
					if ( tmp.matches(textExpre) ) {
						setTextWidth (Math.round(Float.valueOf(tmp)));
					}
					else {
						setTextWidth (2);
					}
				}
				else if (cnt == 3 && (tmp.equalsIgnoreCase("HW") || tmp.equalsIgnoreCase("SW"))) { // parse hw flag for HLSYM
					setTextHWFlag (tmp);
				}
				else if ( cnt == 3 ) { // parse border
					if ( tmp.matches(borderExpre)  && tmp.length() == 3) { 
						setTextBorder (Integer.valueOf(tmp));
					}
				}				
				else if ( cnt == 4 ) { // parse rotation
					if ( tmp.matches(rotExpre)  && tmp.length() == 1) { 
						setTextRotation (tmp.toUpperCase().charAt(0));
					}
				}
				else if ( cnt == 5 ) { // parse justification
					if ( tmp.matches(justExpre) && tmp.length() == 1) { 
						setTextJustification (tmp.toUpperCase().charAt(0));
					}
				}
				else if ( cnt == 6 ) { // parse hw flag
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
		border = 000;
		rotation = 'S';
		justification = 'C';
		textStyle = 0;
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
	 * Set Text border
	 */
	private void setTextBorder ( int border ) {
		this.border = border;
	}
	
	/**
	 * Get Text border
	 */
	public int getTextBorder () {
		return border;
	}
	
	/**
	 * Set Text justification
	 */
	private void setTextJustification ( char justification ) {
		this.justification = justification;
	}
	
	/**
	 * Get Text justification
	 */
	public char getTextJustification () {
		return justification;
	}
	

	/**
	 * Set Text rotation
	 */
	private void setTextRotation ( char rotation ) {
		this.rotation = rotation;
	}
	
	/**
	 * Get Text rotation
	 */
	public char getTextRotation () {
		return rotation;
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
	 * Get textStyle
	 * 
	 * 0  Normal
	 * 1  Italic
	 * 2  Bold
	 * 3  Italic-Bold
	 */
	public int getTextStyle() {
		return textStyle;
	}

	/**
	 * Set textStyle
	 */
	public void setTextStyle(int textStyle) {
		this.textStyle = textStyle;
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
		
		for ( int i = 0; i <= fontsize.length-1; i ++ ) {
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
		
		for ( int i = 0; i < len-1; i ++ ) {
			
			if ( value <= range[len-1] ) {
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
			} else if (value == fontsize[i]) {	
					sizev = fontsize[i];
					sizes = range[i];					
					break;
			} else {	
				sizev = fontsize[len-1] ;
				sizes = range[len-1] ;
			}
		}	
		
		/*if ( value <= range[len-1] ) {
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
		}*/
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
