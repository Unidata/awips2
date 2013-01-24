package gov.noaa.nws.ncep.gempak.parameters.core.contourinterval;


import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

/**
 *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 12-Nov-2009     174       Archana.S   Initial Creation 
 * 10-Jun-2010     174       Archana.S   Added assertion to check that
 *                                       the list of String equivalents for the contour
 *                                       values is generated correctly.
 * 15-Jun-2010     174       Archana.S   Updated test-cases per changes in the code design
 * 02-Aug-2010     174       Archana.S   Updated test-cases per changes in the code design                                              
 * </pre>
 * @author Archana.S
 * @version 1
 */

public class CINTTest {
	
    private static final double ALLOWABLE_DOUBLE_DELTA = 0.0001;
	private static int testCaseNumber;
	private List<Double> testList;
	private List<Double> testList2;
	private List<Double> keySetList;
	List<String> contourValuesList;

		@Test
	/* Test for valid fill contour string of the form contourInterval/minContourValue/maxContourValue */
	public void testPositiveContourIntervalWithMinMaxValues(){
		    testCaseNumber = 1;
		    
		    CINT cint = new CINT("10/0.5/9");
		    
		    assertEquals(cint.isCINTStringParsed(),true);
		    testList = new ArrayList<Double>(Arrays.asList(0.5, 9.0));
		    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
		    assertEquals(keySetList,testList);
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 10, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 0.5, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 9, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(
                cint.getNumPaddingDigits(CINT.FIRST_ZOOM_LEVEL).intValue(), 0);
		    System.out.println("=====================Test-Case "+testCaseNumber+"a ========================");
		    System.out.println("The input string = " + cint.getUserInputString());
			System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		    contourValuesList =  cint.getContourValuesListAsString(CINT.FIRST_ZOOM_LEVEL);
		    System.out.println("The set of contour values: " + contourValuesList);
		    System.out.println("The contour interval: "               + cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));	
		    System.out.println("The minimum contour value:  "  + cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));	
		    System.out.println("The maximum contour value:  " + cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));	

		    CINT cint2 = new CINT("  10 /   -57  / 86 / 4 ");
			assertEquals(cint2.isCINTStringParsed(),true);
		    
            testList2 = new ArrayList<Double>(Arrays.asList(-50.0, -40.0, -30.0, -20.0, -10.0, 0.0, 10.0, 
                    20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0));
		    keySetList = cint2.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
		    assertEquals(testList2,keySetList);
        assertEquals(cint2.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 10, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint2.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), -57, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint2.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 86, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint2.getNumPaddingDigits(CINT.FIRST_ZOOM_LEVEL)
                .intValue(), 4);
			System.out.println("=====================Test-Case "+testCaseNumber+"b ========================");
			System.out.println("The input string = " + cint2.getUserInputString());
		    System.out.println("Is the contour data string parsed correctly? " + cint2.isCINTStringParsed());

		    contourValuesList =  cint2.getContourValuesListAsString(CINT.FIRST_ZOOM_LEVEL);
		    System.out.println("The set of contour values: " + contourValuesList);
		    System.out.println("The contour interval: "               + cint2.getContourInterval(CINT.FIRST_ZOOM_LEVEL));	
		    System.out.println("The minimum contour value:  "  + cint2.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));	
		    System.out.println("The maximum contour value:  " + cint2.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));
		    

			testCaseNumber++;
	}
	
	@Test
	
	/* Test for valid contour data string of the form contourInterval/minContourValue/maxContourValue 
	 * with a negative contourInterval 
	 * */
	public void testNegativeContourIntervalWithMinMaxValues(){
		CINT cint = new CINT("-5/-11/23");
	    
		assertEquals(cint.isCINTStringParsed(),true);
	    testList = new ArrayList<Double>(Arrays.asList(-10.0, -5.0, 0.0, 5.0, 10.0, 15.0, 20.0));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
	    assertEquals(keySetList,testList);
	    
    
	    System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
	    System.out.println("The input string = " + cint.getUserInputString());
	    System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
	    contourValuesList =  cint.getContourValuesListAsString(CINT.FIRST_ZOOM_LEVEL);
	    System.out.println("The set of contour values: " + contourValuesList);
		
		testCaseNumber++;
	}
	
	@Test
	
	/*Test for valid contour value string of the form val1;val2;val3;val4..... */
	public void testValidContourLevelValuesString(){

		CINT cint = new CINT("66.1;0.1;5000;76;-.999;12233459390;0.00009988;1234.567890");

		assertEquals(cint.isCINTStringParsed(), true);
		testList = new ArrayList<Double>(Arrays.asList(66.1, 0.1, 5000.0, 76.0, -0.999, 
				                                          1.223345939E10, 9.988E-5, 1234.56789));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
	    assertEquals(keySetList,testList);

	    System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("The input string = " + cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
	    contourValuesList =  cint.getContourValuesListAsString(CINT.FIRST_ZOOM_LEVEL);
	    System.out.println("The set of contour values: " + contourValuesList);
		
		
		testCaseNumber++;
	}
	
	@Test
	/*Test for valid contour value string of the form val1   ;   val2  ;   val3  */
	public void testValidContourLevelValuesStringWithBlanks(){

		CINT cint = new CINT("  66.1  ;        0.1        ;            5000        ");

		assertEquals(cint.isCINTStringParsed(), true);
		testList = new ArrayList<Double>(Arrays.asList(66.1, 0.1, 5000.0));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
	    assertEquals(keySetList,testList);

	    System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("The input string = " + cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
	    contourValuesList =  cint.getContourValuesListAsString(CINT.FIRST_ZOOM_LEVEL);
	    System.out.println("The set of contour values: " + contourValuesList);
		
		
		testCaseNumber++;
	}
	
	
	@Test
	
	/*Test for valid contour string of the form contourInterval/minContourValue/ */
	public void testContourIntervalWithMinValueOnly(){
		CINT cint = new CINT("-0.345/0/");
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), -0.345, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 0, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
		
		System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("The input string = " + cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		System.out.println("Contour Interval = "+cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "+cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "+cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));
		
		testCaseNumber++;
	}
	
	@Test
	
	/*Test for valid contour data string of the form contourInterval//maxContourValue */
	public void testContourIntervalWithMaxValueOnly(){
		CINT cint = new CINT("15//30");
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 15, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 30, ALLOWABLE_DOUBLE_DELTA);
	
		System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		System.out.println("Contour Interval = "+cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "+cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "+cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));			
	
		
		testCaseNumber++;
	}
	
	@Test
	public void testCINTWithOnlyContourIntervalSpecified(){
		
		CINT cint = new CINT("-0.5/");
		assertEquals(cint.isCINTStringParsed(),true);
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), -0.5, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
		
	    System.out.println("=====================Test-Case "+testCaseNumber+"a ========================");
	    System.out.println("The input string = " + cint.getUserInputString());
	    System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		System.out.println("Contour Interval = "+cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "+cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "+cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));

		CINT cint2 = new CINT("-.89//");
		assertEquals(cint2.isCINTStringParsed(),true);
        assertEquals(cint2.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), -0.89, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint2.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint2.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
		
	    System.out.println("=====================Test-Case "+testCaseNumber+"b ========================");
	    System.out.println("The input string = " + cint2.getUserInputString());
	    System.out.println("Is the contour data string parsed correctly? " + cint2.isCINTStringParsed());
		System.out.println("Contour Interval = "+cint2.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "+cint2.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "+cint2.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));
		
		
		testCaseNumber++;
	}

	@Test
	
	/*Test for valid contour data string of the form /minContourValue/maxContourValue */
	public void testContourIntervalWithNoContourIntervalAndWithMinMaxValueOnly(){
		CINT cint = new CINT("/10/30");
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 10, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 30, ALLOWABLE_DOUBLE_DELTA);
	
		System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("The input string = " + cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		System.out.println("Contour Interval = "+cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "+cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "+cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));			
	
		
		testCaseNumber++;
	}
	
	@Test
	
	/*Test for valid contour data string of the form /minContourValue/maxContourValue */
	public void testContourIntervalWithMinValSameAsMaxValAndNoContourIntervalSpecified(){
		CINT cint = new CINT("/10/10");
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 10, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 10, ALLOWABLE_DOUBLE_DELTA);
	    testList = new ArrayList<Double>(Arrays.asList(10.0));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
	    assertEquals(keySetList,testList);
		System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("The input string = " + cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		System.out.println("Contour Interval = "+cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "+cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "+cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));			

	    contourValuesList =  cint.getContourValuesListAsString(CINT.FIRST_ZOOM_LEVEL);
	    System.out.println("The set of contour values: " + contourValuesList);
		
		testCaseNumber++;
	}
	
	@Test
	
	/*Test input string containing a single contour value */
	public void testLessNumArgsContourIntervalString(){
	
	    
		CINT cint = new CINT("-0.6");
		
	    assertEquals(cint.isCINTStringParsed(),true);
	    testList = new ArrayList<Double>(Arrays.asList(-0.6));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
//	    assertEquals(keySetList,testList);
	    
		System.out.println("=====================Test-Case "+testCaseNumber+"a ========================");	
	    System.out.println("The input string = "                           + cint.getUserInputString());
	    System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
	    System.out.println("The contour interval is: " + cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
	    contourValuesList =  cint.getContourValuesListAsString(CINT.FIRST_ZOOM_LEVEL);
	    System.out.println("List with single contour value = " + contourValuesList);
		CINT cint2 = new CINT("0.7;");
		assertEquals(cint2.isCINTStringParsed(),true);
		
	    testList2 = new ArrayList<Double>(Arrays.asList(0.7));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
//	    assertEquals(keySetList,testList);
	    
		System.out.println("=====================Test-Case "+testCaseNumber+"b ========================");
	    System.out.println("The input string = "                           + cint2.getUserInputString());
	    System.out.println("Is the contour data string parsed correctly? " + cint2.isCINTStringParsed());			
	    System.out.println("The contour interval is: " + cint2.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
	    contourValuesList =  cint2.getContourValuesListAsString(CINT.FIRST_ZOOM_LEVEL);
	    System.out.println("List with single contour value = " + contourValuesList);
       

        testCaseNumber++;
	}
	
	@Test
	
	/*Test for contour data string of the form contourInterval/minContourValue/maxContourValue/extraNumbers/extraNumbers/extraNumbers*/
	public void testExtraNumArgsContourIntervalString(){
		
		CINT cint = new CINT("20/10/70/30/40/500");
		assertEquals(cint.isCINTStringParsed(), true);
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 20, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 10, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 70, ALLOWABLE_DOUBLE_DELTA);
		//
		testList = new ArrayList<Double>(Arrays.asList(10.0, 30.0, 50.0, 60.0));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
	    assertEquals(keySetList,testList);
	    
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+cint.getUserInputString());
		System.out.println("Contour Interval      = "+cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "+cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "+cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));
	    contourValuesList =  cint.getContourValuesListAsString(CINT.FIRST_ZOOM_LEVEL);
	    System.out.println("The set of contour values: " + contourValuesList);
	
	testCaseNumber++;
	}
	
	
	@Test
	
	/*Test for non-numeric values in contour interval string*/
	public void testNonNumericContourIntervalString(){
		CINT cint = new CINT("-def/abc/%^&/30/40");
		assertEquals(cint.isCINTStringParsed(), false);
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
		
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");	
		System.out.println("The input string      = "+cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());		
		System.out.println("Contour Interval = "+cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "+cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "+cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));		
	
	testCaseNumber++;
	}
	
	@Test
	
	/*Test contour interval string with invalid delimiters*/
	public void testInvalidDelimitersInContourIntervalString(){
		
		CINT cint = new CINT("5.10.60.9");
		assertEquals(cint.isCINTStringParsed(), false);
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), Double.NaN, ALLOWABLE_DOUBLE_DELTA);
    	
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		System.out.println("Contour Interval      = "+cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "+cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "+cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));
		
		testCaseNumber++;
	}
	
	@Test
	
	/*Test contour interval string by interchanging minContourValue and maxContourValue */
	public void testMinMaxValuesInterchangedContourIntervalString(){
		CINT cint = new CINT("-5/20/5");
		assertEquals(cint.isCINTStringParsed(), true);
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), -5, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 5, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 20, ALLOWABLE_DOUBLE_DELTA);
		
	    testList = new ArrayList<Double>(Arrays.asList(5.0, 10.0, 15.0, 20.0));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
	    assertEquals(keySetList,testList);
	    
		System.out.println("=====================Test-Case "+testCaseNumber+"a========================");
		System.out.println("The input string      = "+cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		System.out.println("Contour Interval = "                           + cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "					   + cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "                      + cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));	 
	    contourValuesList =  cint.getContourValuesListAsString(CINT.FIRST_ZOOM_LEVEL);
	    System.out.println("List with single contour value = " + contourValuesList);
		
		CINT cint2 = new CINT("5/20/5");
		assertEquals(cint2.isCINTStringParsed(), true);
        assertEquals(cint2.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 5, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint2.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 5, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint2.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 20, ALLOWABLE_DOUBLE_DELTA);
	    testList = new ArrayList<Double>(Arrays.asList(5.0, 10.0, 15.0, 20.0));
//	    assertEquals(cint2.getContourValuesList(),testList);	
		
	    System.out.println("=====================Test-Case "+testCaseNumber+"b========================");
	    System.out.println("The input string      = "+cint2.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint2.isCINTStringParsed());
		System.out.println("Contour Interval      = "+cint2.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "+cint2.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "+cint2.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));	 
		System.out.println("Contour Values List   = "+cint2.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL));		

		testCaseNumber++;
	}
	
	@Test
	
	/*Test for non-numeric vales in contour values' string*/
	public void testInvalidContourLevelValuesString(){
		CINT cint = new CINT("66.1;abc;5000;76;;@#$%;12233459390");
	    assertEquals(cint.isCINTStringParsed(), false);
	    
	    System.out.println("=====================Test-Case "+testCaseNumber+"========================");
	    System.out.println("The input string      = "+cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		testCaseNumber++;
		
		}
	
	@Test
	
	/*Test for invalid delimiters in contour values' string */
	public void testInvalidDelimiterContourValuesString(){
		CINT cint = new CINT("66.1,0,1,5000,76,-.999,12233459390");
	    assertEquals(cint.isCINTStringParsed(), false);
	    
	    System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		testCaseNumber++;
		
		}
	
	@Test
	/*Test for empty contour data string*/
	public void testEmptyString(){
		CINT cint = new CINT();
		
		assertEquals(cint.isCINTStringParsed(), false);
		
		System.out.println("=====================Test-Case "+testCaseNumber+"a========================");
		System.out.println("The input string = " + cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());	
		
		CINT cint2 = new CINT("");
		assertEquals(cint2.isCINTStringParsed(), false);		
		
		System.out.println("=====================Test-Case "+testCaseNumber+"b========================");
		System.out.println("The input string = " + cint2.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint2.isCINTStringParsed());		
		
		CINT cint3 = new CINT(null);
		assertEquals(cint3.isCINTStringParsed(), false);		
		
		System.out.println("=====================Test-Case "+testCaseNumber+"c========================");
		System.out.println("The input string = " + cint3.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint3.isCINTStringParsed());		
		
		testCaseNumber++;
	}
	
	@Test
	/*Test for contour data string with blank-spaces*/
	
	public void testContourDataStringWithBlankSpaces(){

		CINT cint = new CINT("     ");
		assertEquals(cint.isCINTStringParsed(), false);
		
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string = " + cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		
		testCaseNumber++;
	}

	@Test
	
	/*Test contour interval string by interchanging minContourValue and maxContourValue */
	public void testSingleLabelledContourIntervalString(){
		CINT cint = new CINT("5=good/5/5");
		assertEquals(cint.isCINTStringParsed(), true);
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 0, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 5, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 5, ALLOWABLE_DOUBLE_DELTA);
		
	    testList = new ArrayList<Double>(Arrays.asList(5.0));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
	    assertEquals(keySetList,testList);		
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		System.out.println("Contour Interval = "                           + cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "					   + cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "                      + cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));	 
	    contourValuesList =  cint.getContourValuesListAsString(CINT.FIRST_ZOOM_LEVEL);
	    System.out.println("List with single contour value = " + contourValuesList);

		testCaseNumber++;
	}

	@Test
	
	/*Test contour interval string by interchanging minContourValue and maxContourValue */
	public void testLabelledContourIntervalStrings(){

 		CINT cint = new CINT("10=ab;35=sdnf");
		assertEquals(cint.isCINTStringParsed(), true);
		
	    testList = new ArrayList<Double>(Arrays.asList(10.0,35.0));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
	    assertEquals(keySetList,testList);		
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
	    System.out.println("HashMap for labeled contour string = " + cint.getCintHashMap(CINT.FIRST_ZOOM_LEVEL));

		testCaseNumber++;
	}	
	
	
	@Test
	
	/*Test contour interval string of the form: 
	 *contourInterval/minimumContourValue/maximumContourValue/numPaddingDigits*/
	public void testCINTStringWithNumPaddingDigits(){
//		CINT cint = new CINT("1/2/10/3");
		CINT cint = new CINT("1/2/10/3 > 4/2/18");
		
		assertEquals(cint.isCINTStringParsed(), true);
        assertEquals(cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 1, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 2, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 10, ALLOWABLE_DOUBLE_DELTA);
        assertEquals(cint.getNumPaddingDigits(CINT.FIRST_ZOOM_LEVEL)
                .doubleValue(), 3, ALLOWABLE_DOUBLE_DELTA);
		
	    testList = new ArrayList<Double>(Arrays.asList(2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
	    assertEquals(keySetList,testList);
	    
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");

		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		System.out.println("The input string      = "+  cint. getUserInputString());
		System.out.println("The parsed input string at first zoom level     = "+  cint. getCINTString(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Contour Interval = "                           + cint.getContourInterval(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "					   + cint.getMinContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "                      + cint.getMaxContourValue(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum number of digits in label = "          + cint.getNumPaddingDigits(CINT.FIRST_ZOOM_LEVEL));
		List<Double>contourList =  cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
		System.out.println("Contour values list  as double= "                           + contourList);
		System.out.println("Set of contour values with padding digits = " + cint.getContourLabelsForZoomLevel(CINT.FIRST_ZOOM_LEVEL));		
		System.out.println("HashMap at first zoom level = "                           + cint.getCintHashMap(CINT.FIRST_ZOOM_LEVEL));

		System.out.println("The parsed input string at second zoom level     = "+  cint. getCINTString(CINT.SECOND_ZOOM_LEVEL));
		System.out.println("Contour Interval = "                           + cint.getContourInterval(CINT.SECOND_ZOOM_LEVEL));
		System.out.println("Minimum Contour Level = "					   + cint.getMinContourValue(CINT.SECOND_ZOOM_LEVEL));
		System.out.println("Maximum Contour Level = "                      + cint.getMaxContourValue(CINT.SECOND_ZOOM_LEVEL));	 
		System.out.println("Minimum number of digits in label = "          + cint.getNumPaddingDigits(CINT.SECOND_ZOOM_LEVEL));
		System.out.println("HashMap at second zoom level = "                           + cint.getCintHashMap(CINT.SECOND_ZOOM_LEVEL));
		contourList = new ArrayList<Double> (cint.getContourValuesListAsDouble(CINT.SECOND_ZOOM_LEVEL));
		System.out.println("Contour values list  as double= "                           + contourList);	    

		testCaseNumber++;
	}

	/*
	 * Test contour interval string with multiple zoom levels 
	 */
	@Test
	public void testCINTMultipleZoomLevels(){
	 	CINT cint = new CINT("2/-6/6/3 > 30;50;80 > 60=abc; 80=def > 0.00009999 > 0.0000001;");
	 	assertEquals(cint.isCINTStringParsed(), true);
	 	testList = new ArrayList<Double>(Arrays.asList(-6.0, -4.0,-2.0, 0.0, 2.0, 4.0, 6.0));
	    keySetList = cint.getContourValuesListAsDouble(CINT.FIRST_ZOOM_LEVEL);
	    assertEquals(testList,keySetList);
	 	testList = new ArrayList<Double>(Arrays.asList(30.0,50.0,80.0));
	    keySetList = cint.getContourValuesListAsDouble(CINT.SECOND_ZOOM_LEVEL);
	    assertEquals(testList,keySetList);	    
	 	testList = new ArrayList<Double>(Arrays.asList(60.0,80.0));
	    keySetList = cint.getContourValuesListAsDouble(CINT.THIRD_ZOOM_LEVEL);
	    assertEquals(testList,keySetList);
        assertEquals(0.00009999, cint
                .getContourInterval(CINT.FOURTH_ZOOM_LEVEL).doubleValue(),
                ALLOWABLE_DOUBLE_DELTA);
        assertEquals(0.0000001, cint.getContourInterval(CINT.FIFTH_ZOOM_LEVEL)
                .doubleValue(), ALLOWABLE_DOUBLE_DELTA);
//	 	testList = new ArrayList<Double>(Arrays.asList(0.00009999));
//	    keySetList = cint.getContourValuesListAsDouble(CINT.FOURTH_ZOOM_LEVEL);
//	    assertEquals(testList,keySetList);
//	 	testList = new ArrayList<Double>(Arrays.asList(0.0000001));
//	    keySetList = cint.getContourValuesListAsDouble(CINT.FIFTH_ZOOM_LEVEL);
	    assertEquals(testList,keySetList);	
	 	testList = new ArrayList<Double>(Arrays.asList(-6.0, -4.0, -2.0, 0.0, 2.0, 4.0, 6.0, 30.0, 50.0, 60.0, 80.0));
	 	keySetList = cint.getUniqueSortedContourValuesFromAllZoomLevels();
	 	assertEquals(testList,keySetList);
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		System.out.println("The HashMap at 1st zoom level"+ cint.getCintHashMap(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("The HashMap at 2nd zoom level"+ cint.getCintHashMap(CINT.SECOND_ZOOM_LEVEL));
		System.out.println("The HashMap at 3rd zoom level"+ cint.getCintHashMap(CINT.THIRD_ZOOM_LEVEL));
		System.out.println("The HashMap at 4th zoom level"+ cint.getCintHashMap(CINT.FOURTH_ZOOM_LEVEL));
		System.out.println("The HashMap at 5th zoom level"+ cint.getCintHashMap(CINT.FIFTH_ZOOM_LEVEL));
		System.out.println("The unique contour values sorted in ascending order: "+ cint.getUniqueSortedContourValuesFromAllZoomLevelsAsString());
	}
	
	/*
	 * Test contour interval string with more than 5 zoom levels (currently 5 is the maximum)
	 */
	@Test
	public void testCINTWithMoreThanFiveZoomLevels(){
		CINT cint = new CINT("2/-6/6/3 > 30;50;80 > 60=abc; 80=def > 0.00009999 > 0.0000001;1.2 > 90=GoodLuck > 100;200;300;400");
	 	testList = new ArrayList<Double>(Arrays.asList(0.0000001,1.2));
	    keySetList = cint.getContourValuesListAsDouble(CINT.MAX_ZOOM_LEVEL);
	    assertEquals(testList,keySetList);
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+cint.getUserInputString());
		System.out.println("Is the contour data string parsed correctly? " + cint.isCINTStringParsed());
		System.out.println("The HashMap at 1st zoom level"+ cint.getCintHashMap(CINT.FIRST_ZOOM_LEVEL));
		System.out.println("The HashMap at 2nd zoom level"+ cint.getCintHashMap(CINT.SECOND_ZOOM_LEVEL));
		System.out.println("The HashMap at 3rd zoom level"+ cint.getCintHashMap(CINT.THIRD_ZOOM_LEVEL));
		System.out.println("The HashMap at 4th zoom level"+ cint.getCintHashMap(CINT.FOURTH_ZOOM_LEVEL));
		System.out.println("The HashMap at 5th zoom level"+ cint.getCintHashMap(CINT.FIFTH_ZOOM_LEVEL));  
	}
	
	/*
	 * Test contour interval string with multiple zoom levels, some of which are invalid 
	 */	
	@Test
	public void testCINTWithInvalidZoomLevels() throws IndexOutOfBoundsException{
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string    = "+"2/-6/6/3 >  > 60=abc; 80=def >  > ");
		CINT cint = new CINT("2/-6/6/3 >  > 60=abc; 80=def >  > ");
         System.out.println( "Is CINT String parsed correctly? " + cint.isCINTStringParsed());
	}	
	
	
}

















