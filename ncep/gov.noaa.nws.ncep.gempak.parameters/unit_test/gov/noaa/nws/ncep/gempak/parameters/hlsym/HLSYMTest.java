package gov.noaa.nws.ncep.gempak.parameters.hlsym;

import gov.noaa.nws.ncep.gempak.parameters.hlsym.HLSYM;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * HLSYM format: sizes;sizev/position/fonts;fontv/widths;widthv/hwflgs;hwflgv
 *
 */
public class HLSYMTest {
	private static int testCaseNumber=1;
	
	@Test
	public void testHLSYMInputStringParse1 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with blank string----------------");

	    
		HLSYM hlsym = new HLSYM("");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286");
			assertEquals (hlsym.getValueString(), "1.0");
			assertEquals (hlsym.getLabelLoc(), 2);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse2 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with sizes;sizv----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;2");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286");
			assertEquals (hlsym.getValueString(), "1.714");
			assertEquals (hlsym.getLabelLoc(), 2);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse3 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with sizes;----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286");
			assertEquals (hlsym.getValueString(), "1.0");
			assertEquals (hlsym.getLabelLoc(), 2);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse4 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with ;sizev----------------");

	    
		HLSYM hlsym = new HLSYM(";2");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286");
			assertEquals (hlsym.getValueString(), "1.714");
			assertEquals (hlsym.getLabelLoc(), 2);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse5 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size----------------");

	    
		HLSYM hlsym = new HLSYM("1.8");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.714");
			assertEquals (hlsym.getValueString(), "1.714");
			assertEquals (hlsym.getLabelLoc(), 2);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse6 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size and location----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/3");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286");
			assertEquals (hlsym.getValueString(), "1.0");
			assertEquals (hlsym.getLabelLoc(), 3);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse7 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size,location and fonts;fontv----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/2;3");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2");
			assertEquals (hlsym.getValueString(), "1.0/3");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse8 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size,location and fonts;----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/2;");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2");
			assertEquals (hlsym.getValueString(), "1.0/1");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse9 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size,location and ;fontv----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/;3");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/1");
			assertEquals (hlsym.getValueString(), "1.0/3");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse10 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size,location and font----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/2");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2");
			assertEquals (hlsym.getValueString(), "1.0/2");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse11 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/location/font/widths;widthv----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/2;3/5;7");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2/5");
			assertEquals (hlsym.getValueString(), "1.0/3/7");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse12 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/location/font/widths;----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/2;3/8;");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2/8");
			assertEquals (hlsym.getValueString(), "1.0/3/2");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse13 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/location/font/;widthv----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/2;3/;4");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2/2");
			assertEquals (hlsym.getValueString(), "1.0/3/4");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse14 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/location/font/width----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/2;3/5");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2/5");
			assertEquals (hlsym.getValueString(), "1.0/3/5");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse15 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/location/font/width/hwflgs;hwflgv----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/2;3/5;7/hw;sw");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2/5/HW");
			assertEquals (hlsym.getValueString(), "1.0/3/7/SW");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse16 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/location/font/width/hwflgs;----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/2;3/5;7/hw;");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2/5/HW");
			assertEquals (hlsym.getValueString(), "1.0/3/7");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse17 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/location/font/width/;hwflgv----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/2;3/5;7/;sw");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2/5");
			assertEquals (hlsym.getValueString(), "1.0/3/7/SW");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse18 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/location/font/width/hwflg----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1/2;3/5;7/hw");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2/5/HW");
			assertEquals (hlsym.getValueString(), "1.0/3/7/HW");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse19 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with /location/font/width/hwflg----------------");

	    
		HLSYM hlsym = new HLSYM("/1/2;3/5;7/hw");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2/5/HW");
			assertEquals (hlsym.getValueString(), "1.0/3/7/HW");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse20 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size//font/width/hwflg----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1//2;3/5;7/hw");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2/5/HW");
			assertEquals (hlsym.getValueString(), "1.0/3/7/HW");
			assertEquals (hlsym.getLabelLoc(), 2);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse21 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/location//width/hwflg----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/1//5;7/hw");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/1/5/HW");
			assertEquals (hlsym.getValueString(), "1.0/1/7/HW");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse22 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/location/font//hwflg----------------");

	    
		HLSYM hlsym = new HLSYM("1.5;1/3/2;3//hw");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/2/2/HW");
			assertEquals (hlsym.getValueString(), "1.0/3/2/HW");
			assertEquals (hlsym.getLabelLoc(), 3);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;
	}
	
	@Test
	public void testHLSYMInputStringParse23 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with size/location/font/width/----------------");

	    
		HLSYM hlsym = new HLSYM("0.8;1.125/1/2;3/5;7");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "0.857/2/5");
			assertEquals (hlsym.getValueString(), "1.0/3/7");
			assertEquals (hlsym.getLabelLoc(), 1);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
		
		testCaseNumber ++;

	}
	
	@Test
	public void testHLSYMInputStringParse24 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with ////----------------");

	    
		HLSYM hlsym = new HLSYM("////");
        
		if ( hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),true);
			assertEquals (hlsym.getMarkerString(), "1.286/1/2");
			assertEquals (hlsym.getValueString(), "1.0/1/2");
			assertEquals (hlsym.getLabelLoc(), 2);
			
			System.out.println ( "--User Input HL Symbol parameter:" + hlsym.getHLSymbolInputString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    Text output marker string: " + hlsym.getMarkerString() );
			System.out.println ( "    Text output value string : " + hlsym.getValueString() );
			System.out.println ( "              Label location : " + hlsym.getLabelLoc() );
		}
	}
	
	@Test
	public void testHLSYMInputStringParse25 () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" with null string----------------");

	    
		HLSYM hlsym = new HLSYM(null);
        
		if ( ! hlsym.isHLSymbolStringParsed() ) {
			assertEquals (hlsym.isHLSymbolStringParsed(),false);
			
			System.out.println ( "--User Input HL Symbol parameter:" + ((hlsym.getHLSymbolInputString()==null)? "NULL": hlsym.getHLSymbolInputString()));
			System.out.println ( "-----------Fail to parse NULL string------------------" );
		}
		testCaseNumber ++;
	}
}
