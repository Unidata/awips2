package gov.noaa.nws.ncep.gempak.parameters.intext;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * TEXT format: size/font/width/hwflgs (used by HLSYM) OR size/font/width/border/rotation/justification/hwflgs (used by TEXT)
 *
 */
public class TextStringParserTest {
	private static int testCaseNumber=1;
	
	@Test
	public void testTextInputStringParse1 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with blank string----------------");

	    
		TextStringParser txt = new TextStringParser("");
        
		if ( ! txt.isTextParsed()) {
			assertEquals (txt.isTextParsed(),false);
			
			System.out.println ( "--User Input Text parameter:" + txt.getInputTextString() );
			System.out.println ( "-----------------------------" );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse2 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with sizes----------------");

	    
		TextStringParser txt = new TextStringParser("1.286");
        
		if ( txt.isTextParsed() ) {
			assertEquals (txt.isTextParsed(),true);
			assertEquals (txt.getSymbolMarkerSize(), 1.286f);
			assertEquals (txt.getTextSize(), 18);
			assertEquals (txt.getTextFont(), 1);
			assertEquals (txt.getTextWidth(), 2);
			assertEquals (txt.getTextHWFlag(), "HW");
			
			System.out.println ( "--User Input Text parameter:" + txt.getInputTextString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Symbol/Marker size  	: " + txt.getSymbolMarkerSize() );
			System.out.println ( "    Text size  		  	: " + txt.getTextSize() );
			System.out.println ( "    Text font  			: " + txt.getTextFont() );
			System.out.println ( "    Text width 			: " + txt.getTextWidth() );
			System.out.println ( "    Text hwflg 			: " + txt.getTextHWFlag() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse3 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size name ----------------");

	    
		TextStringParser txt = new TextStringParser("tin");
        
		if ( txt.isTextParsed() ) {
			assertEquals (txt.isTextParsed(),true);
			assertEquals (txt.getSymbolMarkerSize(), 0.714f);
			assertEquals (txt.getTextSize(), 10);
			assertEquals (txt.getTextFont(), 1);
			assertEquals (txt.getTextWidth(), 2);
			assertEquals (txt.getTextHWFlag(), "HW");
			
			System.out.println ( "--User Input Text parameter:" + txt.getInputTextString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Symbol/Marker size  	: " + txt.getSymbolMarkerSize() );
			System.out.println ( "    Text size  		  	: " + txt.getTextSize() );
			System.out.println ( "    Text font  			: " + txt.getTextFont() );
			System.out.println ( "    Text width 			: " + txt.getTextWidth() );
			System.out.println ( "    Text hwflg 			: " + txt.getTextHWFlag() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse4 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/font ----------------");

	    
		TextStringParser txt = new TextStringParser("1.286/2");
        
		if ( txt.isTextParsed() ) {
			assertEquals (txt.isTextParsed(),true);
			assertEquals (txt.getSymbolMarkerSize(), 1.286f);
			assertEquals (txt.getTextSize(), 18);
			assertEquals (txt.getTextFont(), 2);
			assertEquals (txt.getTextWidth(), 2);
			assertEquals (txt.getTextHWFlag(), "HW");
			
			System.out.println ( "--User Input Text parameter:" + txt.getInputTextString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Symbol/Marker size  	: " + txt.getSymbolMarkerSize() );
			System.out.println ( "    Text size  		  	: " + txt.getTextSize() );
			System.out.println ( "    Text font  			: " + txt.getTextFont() );
			System.out.println ( "    Text width 			: " + txt.getTextWidth() );
			System.out.println ( "    Text hwflg 			: " + txt.getTextHWFlag() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse5 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/invalid font ----------------");

	    
		TextStringParser txt = new TextStringParser("1.286/5");
        
		if ( txt.isTextParsed() ) {
			assertEquals (txt.isTextParsed(),true);
			assertEquals (txt.getSymbolMarkerSize(), 1.286f);
			assertEquals (txt.getTextSize(), 18);
			assertEquals (txt.getTextFont(), 1);
			assertEquals (txt.getTextWidth(), 2);
			assertEquals (txt.getTextHWFlag(), "HW");
			
			System.out.println ( "--User Input Text parameter:" + txt.getInputTextString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Symbol/Marker size  	: " + txt.getSymbolMarkerSize() );
			System.out.println ( "    Text size  		  	: " + txt.getTextSize() );
			System.out.println ( "    Text font  			: " + txt.getTextFont() );
			System.out.println ( "    Text width 			: " + txt.getTextWidth() );
			System.out.println ( "    Text hwflg 			: " + txt.getTextHWFlag() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse6 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/ font / width ----------------");

	    
		TextStringParser txt = new TextStringParser("1.714/3/8");
        
		if ( txt.isTextParsed() ) {
			assertEquals (txt.isTextParsed(),true);
			assertEquals (txt.getSymbolMarkerSize(), 1.714f);
			assertEquals (txt.getTextSize(), 24);
			assertEquals (txt.getTextFont(), 3);
			assertEquals (txt.getTextWidth(), 8);
			assertEquals (txt.getTextHWFlag(), "HW");
			
			System.out.println ( "--User Input Text parameter:" + txt.getInputTextString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Symbol/Marker size  	: " + txt.getSymbolMarkerSize() );
			System.out.println ( "    Text size  		  	: " + txt.getTextSize() );
			System.out.println ( "    Text font  			: " + txt.getTextFont() );
			System.out.println ( "    Text width 			: " + txt.getTextWidth() );
			System.out.println ( "    Text hwflg 			: " + txt.getTextHWFlag() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse7 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/ font / width /hw flag ----------------");

	    
		TextStringParser txt = new TextStringParser("2.429/2/8/HW");
        
		if ( txt.isTextParsed() ) {
			assertEquals (txt.isTextParsed(),true);
			assertEquals (txt.getSymbolMarkerSize(), 2.429f);
			assertEquals (txt.getTextSize(), 34);
			assertEquals (txt.getTextFont(), 2);
			assertEquals (txt.getTextWidth(), 8);
			assertEquals (txt.getTextHWFlag(), "HW");
			
			System.out.println ( "--User Input Text parameter:" + txt.getInputTextString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Symbol/Marker size  	: " + txt.getSymbolMarkerSize() );
			System.out.println ( "    Text size  		  	: " + txt.getTextSize() );
			System.out.println ( "    Text font  			: " + txt.getTextFont() );
			System.out.println ( "    Text width 			: " + txt.getTextWidth() );
			System.out.println ( "    Text hwflg 			: " + txt.getTextHWFlag() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse8 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/ font / width /hw flag ----------------");

	    
		TextStringParser txt = new TextStringParser("2.429/2/8/SW");
        
		if ( txt.isTextParsed() ) {
			assertEquals (txt.isTextParsed(),true);
			assertEquals (txt.getSymbolMarkerSize(), 2.429f);
			assertEquals (txt.getTextSize(), 34);
			assertEquals (txt.getTextFont(), 2);
			assertEquals (txt.getTextWidth(), 8);
			assertEquals (txt.getTextHWFlag(), "SW");
			
			System.out.println ( "--User Input Text parameter:" + txt.getInputTextString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Symbol/Marker size  	: " + txt.getSymbolMarkerSize() );
			System.out.println ( "    Text size  		  	: " + txt.getTextSize() );
			System.out.println ( "    Text font  			: " + txt.getTextFont() );
			System.out.println ( "    Text width 			: " + txt.getTextWidth() );
			System.out.println ( "    Text hwflg 			: " + txt.getTextHWFlag() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testTEXTInputStringParse9 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/ font/ widthborder/ rotation/ justification /hw flag ----------------");

	    
		TextStringParser txt = new TextStringParser("2.429/3/8/221/N/L/HW");
        
		if ( txt.isTextParsed() ) {
			assertEquals (txt.isTextParsed(),true);
			assertEquals (txt.getTextSize(), 34);
			assertEquals (txt.getTextFont(), 3);
			assertEquals (txt.getTextWidth(), 8);
			assertEquals (txt.getTextBorder(), 221);
			assertEquals (txt.getTextRotation(), 'N');
			assertEquals (txt.getTextJustification(), 'L');
			assertEquals (txt.getTextHWFlag(), "HW");
			
			System.out.println ( "--User Input Text parameter:" + txt.getInputTextString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Symbol/Marker size  	: " + txt.getSymbolMarkerSize() );
			System.out.println ( "    Text size  		  	: " + txt.getTextSize() );
			System.out.println ( "    Text font  			: " + txt.getTextFont() );
			System.out.println ( "    Text width 			: " + txt.getTextWidth() );
			System.out.println ( "    Text border  		  	: " + txt.getTextBorder() );
			System.out.println ( "    Text rotation  	    : " + txt.getTextRotation() );
			System.out.println ( "    Text justification	: " + txt.getTextJustification() );
			System.out.println ( "    Text hwflg 			: " + txt.getTextHWFlag() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testTEXTInputStringParse10 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/ font/ widthborder/ rotation/ justification /hw flag ----------------");

	    
		TextStringParser txt = new TextStringParser("1/23/8////HW");
        
		if ( txt.isTextParsed() ) {
			assertEquals (txt.isTextParsed(),true);
			assertEquals (txt.getTextSize(), 14);
			//assertEquals (txt.getTextFont(), 3);
			//assertEquals (txt.getTextStyle(), 2);
			assertEquals (txt.getTextWidth(), 8);
			assertEquals (txt.getTextHWFlag(), "HW");
			
			System.out.println ( "--User Input Text parameter:" + txt.getInputTextString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Symbol/Marker size  	: " + txt.getSymbolMarkerSize() );
			System.out.println ( "    Text size  		  	: " + txt.getTextSize() );
			System.out.println ( "    Text font  			: " + txt.getTextFont() );
			System.out.println ( "    Text style            : " + txt.getTextStyle() );
			System.out.println ( "    Text width 			: " + txt.getTextWidth() );
			System.out.println ( "    Text border  		  	: " + txt.getTextBorder() );
			System.out.println ( "    Text rotation  	    : " + txt.getTextRotation() );
			System.out.println ( "    Text justification	: " + txt.getTextJustification() );
			System.out.println ( "    Text hwflg 			: " + txt.getTextHWFlag() );
		}
		
		testCaseNumber ++;
	}
}
