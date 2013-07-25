package gov.noaa.nws.ncep.gempak.parameters.hilo;

import gov.noaa.nws.ncep.gempak.parameters.hilo.HILOStringParser;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;
import org.eclipse.swt.graphics.RGB;
import org.junit.Test;
import static org.junit.Assert.*;

/*
 * HILO format: colorh;colorl/symbolh;symboll/rangeh;rangel/radius/counth;countl/interp
 */
public class HILOTest {
	
	private static final float MIN_RANGE_VAL = -3.4E+38f;
	
	private static final float MAX_RANGE_VAL = 3.4E+38f;
	
	private static int testCaseNumber=1;
	
	@Test
	public void testValidHILOSWithallInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with all valid values----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/H12#4;SA3#17/100-500;700-1000/4/15;25/yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 1 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), " ");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), 12);
			assertEquals(hiloBuild.getSymbolHiType(), 2);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), true);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 4);
			assertEquals(hiloBuild.getRangeHiMinval(), 100);
			assertEquals(hiloBuild.getRangeHiMaxval(), 500);
			
			assertEquals(hiloBuild.getSymbolLo(), "SA3");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), true);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 9);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 1000);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 15);
			assertEquals(hiloBuild.getCountLo(), 25);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithoutInterpolationInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string without interpolation input----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/H12#4;SA3#17/100-500;700-1000/4/15;25");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 1 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), " ");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), 12);
			assertEquals(hiloBuild.getSymbolHiType(), 2);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), true);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 4);
			assertEquals(hiloBuild.getRangeHiMinval(), 100);
			assertEquals(hiloBuild.getRangeHiMaxval(), 500);
			
			assertEquals(hiloBuild.getSymbolLo(), "SA3");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), true);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 9);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 1000);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 15);
			assertEquals(hiloBuild.getCountLo(), 25);
			assertEquals(hiloBuild.getInterp(), false);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithoutCountHiInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string without count high----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/H12#4;SA3#17/100-500;700-1000/4/;25/yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 1 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), " ");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), 12);
			assertEquals(hiloBuild.getSymbolHiType(), 2);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), true);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 4);
			assertEquals(hiloBuild.getRangeHiMinval(), 100);
			assertEquals(hiloBuild.getRangeHiMaxval(), 500);
			
			assertEquals(hiloBuild.getSymbolLo(), "SA3");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), true);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 9);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 1000);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 25);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithoutCountLoInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string without count low----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/H12#4;SA3#17/100-500;700-1000/4/25;/yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 1 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), " ");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), 12);
			assertEquals(hiloBuild.getSymbolHiType(), 2);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), true);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 4);
			assertEquals(hiloBuild.getRangeHiMinval(), 100);
			assertEquals(hiloBuild.getRangeHiMaxval(), 500);
			
			assertEquals(hiloBuild.getSymbolLo(), "SA3");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), true);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 9);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 1000);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 25);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithOneCountInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string without one count----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/H12#4;SA3#17/100-500;700-1000/4/25/yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 1 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), " ");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), 12);
			assertEquals(hiloBuild.getSymbolHiType(), 2);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), true);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 4);
			assertEquals(hiloBuild.getRangeHiMinval(), 100);
			assertEquals(hiloBuild.getRangeHiMaxval(), 500);
			
			assertEquals(hiloBuild.getSymbolLo(), "SA3");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), true);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 9);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 1000);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 25);
			assertEquals(hiloBuild.getCountLo(), 25);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithoutCountInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string without count, use default values for count high/low----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/H12#4;SA3#17/100-500;700-1000/4//yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 1 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), " ");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), 12);
			assertEquals(hiloBuild.getSymbolHiType(), 2);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), true);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 4);
			assertEquals(hiloBuild.getRangeHiMinval(), 100);
			assertEquals(hiloBuild.getRangeHiMaxval(), 500);
			
			assertEquals(hiloBuild.getSymbolLo(), "SA3");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), true);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 9);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 1000);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithOneHighRangeInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with one high range, set the same value to high range min/max----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/H12#4;SA3#17/500;700-1000/4//yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 1 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), " ");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), 12);
			assertEquals(hiloBuild.getSymbolHiType(), 2);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), true);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 4);
			assertEquals(hiloBuild.getRangeHiMinval(), 500);
			assertEquals(hiloBuild.getRangeHiMaxval(), 500);
			
			assertEquals(hiloBuild.getSymbolLo(), "SA3");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), true);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 9);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 1000);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithOneRangeInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with one range, will use one values for all hi/low max/min values----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/H12#4;SA3#17/700/4//yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 1 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), " ");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), 12);
			assertEquals(hiloBuild.getSymbolHiType(), 2);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), true);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 4);
			assertEquals(hiloBuild.getRangeHiMinval(), 700);
			assertEquals(hiloBuild.getRangeHiMaxval(), 700);
			
			assertEquals(hiloBuild.getSymbolLo(), "SA3");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), true);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 9);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 700);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithLoSymoblMarkerInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with low symbol marker----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/H12#4;S10#17/700/4//yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 1 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), " ");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), 12);
			assertEquals(hiloBuild.getSymbolHiType(), 2);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), true);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 4);
			assertEquals(hiloBuild.getRangeHiMinval(), 700);
			assertEquals(hiloBuild.getRangeHiMaxval(), 700);
			
			assertEquals(hiloBuild.getSymbolLo(), " ");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), 10);
			assertEquals(hiloBuild.getSymbolLoType(), 3);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), true);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 9);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 700);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithHiLoSymoblInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with high/low symbol----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/HH12#4;LL10#17/700/4//yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 1 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), "HH12");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolHiType(), 1);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), true);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 4);
			assertEquals(hiloBuild.getRangeHiMinval(), 700);
			assertEquals(hiloBuild.getRangeHiMaxval(), 700);
			
			assertEquals(hiloBuild.getSymbolLo(), "LL10");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), true);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 9);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 700);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSymoblWithoutPrecisionInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string Symbol without precision----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/HH12;LL10/700/4//yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 1 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), "HH12");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolHiType(), 1);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), false);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeHiMinval(), 700);
			assertEquals(hiloBuild.getRangeHiMaxval(), 700);
			
			assertEquals(hiloBuild.getSymbolLo(), "LL10");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), false);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 700);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSymoblWithLoColorInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with low color inout----------------");
	
		HILOStringParser hilo = new HILOStringParser(";4/HH12;LL10/700/4//yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 32 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), "HH12");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolHiType(), 1);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), false);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeHiMinval(), 700);
			assertEquals(hiloBuild.getRangeHiMaxval(), 700);
			
			assertEquals(hiloBuild.getSymbolLo(), "LL10");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), false);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 700);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSymoblWithColorInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with color inout----------------");
	
		HILOStringParser hilo = new HILOStringParser("4/HH12;LL10/700/4//yes");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 4 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), "HH12");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolHiType(), 1);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), false);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeHiMinval(), 700);
			assertEquals(hiloBuild.getRangeHiMaxval(), 700);
			
			assertEquals(hiloBuild.getSymbolLo(), "LL10");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), false);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeLoMinval(), 700);
			assertEquals(hiloBuild.getRangeLoMaxval(), 700);
			
			assertEquals(hiloBuild.getRadius(), 4);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), true);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOOnlyWithOneColorInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string only contains one color----------------");
	
		HILOStringParser hilo = new HILOStringParser("5");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 5 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 5 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), "H");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolHiType(), 1);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), false);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeHiMinval(), MIN_RANGE_VAL);
			assertEquals(hiloBuild.getRangeHiMaxval(), MAX_RANGE_VAL);
			
			assertEquals(hiloBuild.getSymbolLo(), "L");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), false);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeLoMinval(), MIN_RANGE_VAL);
			assertEquals(hiloBuild.getRangeLoMaxval(), MAX_RANGE_VAL);
			
			assertEquals(hiloBuild.getRadius(), 3);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), false);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOOnlyWithSlashesInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string only contains slashes----------------");
	
		HILOStringParser hilo = new HILOStringParser("///////");
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 32 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 32 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), "H");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolHiType(), 1);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), false);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeHiMinval(), MIN_RANGE_VAL);
			assertEquals(hiloBuild.getRangeHiMaxval(), MAX_RANGE_VAL);
			
			assertEquals(hiloBuild.getSymbolLo(), "L");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), false);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeLoMinval(), MIN_RANGE_VAL);
			assertEquals(hiloBuild.getRangeLoMaxval(), MAX_RANGE_VAL);
			
			assertEquals(hiloBuild.getRadius(), 3);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), false);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testEmptyHILOInputString () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" Empty HILO string, use all default values----------------");
	
		HILOStringParser hilo = new HILOStringParser();
		if ( hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),true);
			HILOBuilder hiloBuild = hilo.getInstanceOfHiLoBuilder();
			RGB rgb = GempakColor.convertToRGB ( 32 );
			assertEquals (hiloBuild.getColorHi(),rgb);
			rgb = GempakColor.convertToRGB ( 32 );
			assertEquals (hiloBuild.getColorLo(),rgb);
			assertEquals(hiloBuild.getSymbolHi(), "H");
			assertEquals(hiloBuild.getSymbolHiMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolHiType(), 1);
			assertEquals(hiloBuild.getSymbolHiPlotValue(), false);
			assertEquals(hiloBuild.getSymbolHiNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeHiMinval(), MIN_RANGE_VAL);
			assertEquals(hiloBuild.getRangeHiMaxval(), MAX_RANGE_VAL);
			
			assertEquals(hiloBuild.getSymbolLo(), "L");
			assertEquals(hiloBuild.getSymbolLoMarkerNumber(), -9999);
			assertEquals(hiloBuild.getSymbolLoType(), 1);
			assertEquals(hiloBuild.getSymbolLoPlotValue(), false);
			assertEquals(hiloBuild.getSymbolLoNumOfDecPls(), 0);
			assertEquals(hiloBuild.getRangeLoMinval(), MIN_RANGE_VAL);
			assertEquals(hiloBuild.getRangeLoMaxval(), MAX_RANGE_VAL);
			
			assertEquals(hiloBuild.getRadius(), 3);
			assertEquals(hiloBuild.getCountHi(), 20);
			assertEquals(hiloBuild.getCountLo(), 20);
			assertEquals(hiloBuild.getInterp(), false);
			
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "-----------------------------" );
			System.out.println ( "    HIGH -- Color\t\t[" + hiloBuild.getInputColorHi() + "][" + hiloBuild.getColorHi().red + " " + hiloBuild.getColorHi().green + " " + hiloBuild.getColorHi().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolHi() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolHiMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolHiType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolHiPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolHiNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeHiMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeHiMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountHi() );
	
			System.out.println ( "    LOW -- Color\t\t[" + hiloBuild.getInputColorLo() + "][" + hiloBuild.getColorLo().red + " " + hiloBuild.getColorLo().green + " " + hiloBuild.getColorLo().blue + "]");
			System.out.println ( "            symbol\t\t" + hiloBuild.getSymbolLo() );
			System.out.println ( "            Marker Num\t\t" + hiloBuild.getSymbolLoMarkerNumber() );
			System.out.println ( "            symbol type\t\t" + hiloBuild.getSymbolLoType() );
			System.out.println ( "            value flg\t\t" + ((hiloBuild.getSymbolLoPlotValue())? "T":"F") );
			System.out.println ( "            precision\t\t" + hiloBuild.getSymbolLoNumOfDecPls() );
			System.out.println ( "            range min\t\t" + hiloBuild.getRangeLoMinval() );
			System.out.println ( "            range max\t\t" + hiloBuild.getRangeLoMaxval() );
			System.out.println ( "            count\t\t" + hiloBuild.getCountLo() );
		
			System.out.println ( "    Radius = " + hiloBuild.getRadius() + "    interp flag = " + ((hiloBuild.getInterp())? "T":"F"));
			System.out.println ( "    Error message: " + hilo.getErrorMessage());
		}
		else {
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidColorInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid color----------------");
	
		HILOStringParser hilo = new HILOStringParser("abc/H12#4;SA3#17/100-500;700-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidHiColorInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid high color----------------");
	
		HILOStringParser hilo = new HILOStringParser("ad;4/H12#4;SA3#17/100-500;700-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSHiColorOutOfRangeInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string high color out of range----------------");
	
		HILOStringParser hilo = new HILOStringParser("-10;4/H12#4;SA3#17/100-500;700-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidLoColorInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid low color----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;aa/H12#4;SA3#17/100-500;700-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOLoColorOutOfRangeInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with low color out of range----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;-9/H12#4;SA3#17/100-500;700-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidHiSymbolPrecisionInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid high symbol precision----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/H12#da;SA3#17/100-500;700-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidLoSymbolPrecisionInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid low symbol precision----------------");
	
		HILOStringParser hilo = new HILOStringParser("1;4/H12#11;SA3#ee/100-500;700-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidHiRangeMinvalInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid high range minval----------------");
		HILOStringParser hilo = new HILOStringParser("1;4/H12#11;SA3#5/10wa-500;700-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidHiRangeMaxvalInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid high range maxval----------------");
		HILOStringParser hilo = new HILOStringParser("1;4/H12#11;SA3#5/100-5qa;700-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithHiRangeMinvalGtMinvalInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string high range minval greater than maxval----------------");
		HILOStringParser hilo = new HILOStringParser("1;4/H12#11;SA3#5/100-50;700-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidLoRangeMinvalInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid Lo range minval----------------");
		HILOStringParser hilo = new HILOStringParser("1;4/H12#11;SA3#5/100-500;7a0-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidLoRangeMaxvalInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid low range maxval----------------");
		HILOStringParser hilo = new HILOStringParser("1;4/H12#11;SA3#5/100-500;700-LOw0/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithLoRangeMinvalGtMinvalInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string low range minval greater than maxval----------------");
		HILOStringParser hilo = new HILOStringParser("1;4/H12#11;SA3#5/100-500;7000-1000/4/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidRadiusInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid radius----------------");
		HILOStringParser hilo = new HILOStringParser("1;4/H12#11;SA3#5/100-500;700-1000/-10/15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidHiCountInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid high count----------------");
		HILOStringParser hilo = new HILOStringParser("1;4/H12#11;SA3#5/100-500;700-1000/10/-15;25/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithInvalidLoCountInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with invalid high count----------------");
		HILOStringParser hilo = new HILOStringParser("1;4/H12#11;SA3#5/100-500;700-1000/10/15;ab/yes");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithNullInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with null string----------------");
		HILOStringParser hilo = new HILOStringParser("");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
	
	@Test
	public void testValidHILOSWithBlankInputStringParse () {
		
		System.out.println("------------------Test-case "+ testCaseNumber +" HILO string with blank string----------------");
		HILOStringParser hilo = new HILOStringParser("     ");
		if (  ! hilo.isHiLoStringParsed()) {
			assertEquals (hilo.isHiLoStringParsed(),false);
			System.out.println ( "--User Input HILO parameters:" + hilo.getInputHiLoString() );
			System.out.println ( "--Parse Error :" + hilo.getErrorMessage() );
		}
		testCaseNumber ++;
	}
}
