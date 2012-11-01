package gov.noaa.nws.ncep.gempak.parameters.infill;



import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

/**
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 10-Nov-2009     184       Archana.S   Initial Creation 
 * 17-Jun-2010     184       Archana.S   Updated test-cases to test
 *                                                            updated code in FINT  
 * 03-Aug-2010     184       Archana.S   Updated test-cases to test
 *                                                            updated code in FINT
 * 25-Aug-2012     743       djohnson    Upgrade to JUnit 4.10.
 * </pre>
 * 
 * @author Archana.S
 * @version 1
 */
public class FINTTest {
	
	private static int testCaseNumber;
	private List<Double> testList;
	private List<Double> testList2;
	private List<Double> keySetList;

	
	@Test
	/* Test for valid FINT string of the form fillInterval/minFillValue/maxFillValue */
	public void testPositiveFillFillIntervalWithMinMaxValues(){
		    testCaseNumber = 1;
		    
		    FINT fint = new FINT("10/0.5/9");
		    
		    assertEquals(fint.isFINTStringParsed(),true);
		    testList = new ArrayList<Double>(Arrays.asList(0.5,9.0));
		    keySetList = fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL);
		    assertEquals(keySetList,testList);
		    assertEquals(fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL).doubleValue(), 10);
		    assertEquals(fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL).doubleValue(), 0.5);
		    assertEquals(fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL).doubleValue(), 9);
		    
		    System.out.println("=====================Test-Case "+testCaseNumber+"a ========================");
		    System.out.println("The input string = " + fint.getUserInputString());
			System.out.println("Is the fill data string parsed correctly? " + fint.isFINTStringParsed());
			System.out.println("Fill Values List = "                                  + fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL));			
		    System.out.println("Fill Interval in First Zoom Level"      + fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL).doubleValue());
		    System.out.println("Min Fill Value  in First Zoom Level" + fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL).doubleValue());
		    System.out.println("Max Fill Value in First Zoom Level" + fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL).doubleValue());		    			
			
		    FINT fint2 = new FINT("  10 /   -57  / 86 ");
			assertEquals(fint2.isFINTStringParsed(),true);
			testList2 = new ArrayList<Double>(Arrays.asList(-50.0, -40.0, -30.0, -20.0, -10.0, 0.0, 10.0, 
					                                                  20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0));
		    keySetList = fint2.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL);
		    assertEquals(keySetList,testList2);
		    assertEquals(fint2.getFillInterval(FINT.FIRST_ZOOM_LEVEL).doubleValue(), 10);
		    assertEquals(fint2.getMinFillValue(FINT.FIRST_ZOOM_LEVEL).doubleValue(), -57.0);
		    assertEquals(fint2.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL).doubleValue(), 86);
		    
			System.out.println("=====================Test-Case "+testCaseNumber+"b ========================");
			System.out.println("The input string = " + fint2.getUserInputString());
		    System.out.println("Is the fill data string parsed correctly? " + fint2.isFINTStringParsed());
            System.out.println("Fill Values List = "+fint2.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL));			
		    System.out.println("Fill Interval in First Zoom Level" + fint2.getFillInterval(FINT.FIRST_ZOOM_LEVEL).doubleValue());
		    System.out.println("Min Fill Value  in First Zoom Level" + fint2.getMinFillValue(FINT.FIRST_ZOOM_LEVEL).doubleValue());
		    System.out.println("Max Fill Value in First Zoom Level" + fint2.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL).doubleValue());
            
            testCaseNumber++;
	}
	
    /**
     * @param doubleValue
     * @param i
     */
    // private static void assertEquals(double doubleValue, int i) {
    // Assert.assertEquals(doubleValue, i, 0.001);
    // }

    @Test
	
	/* Test for valid FINT string of the form fillInterval/minFillValue/maxFillValue 
	 * with a negative contourInterval 
	 * */
	public void testNegativeFillIntervalWithMinMaxValues(){
		FINT fint = new FINT("-5/-11/23");
	    
		assertEquals(fint.isFINTStringParsed(),true);
	    testList = new ArrayList<Double>(Arrays.asList(-10.0, -5.0, 0.0, 5.0, 10.0, 15.0, 20.0));
	    		    keySetList = fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL);
		    assertEquals(keySetList,testList);
    
	    System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
	    System.out.println("The input string = " + fint.getUserInputString());
	    System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
        System.out.println("Fill Values List = "+fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL));
	
		
		testCaseNumber++;
	}
	
	@Test
	
	/*Test for valid FINT string of the form val1;val2;val3;val4..... */
	public void testValidFillLevelValuesString(){

		FINT fint = new FINT("66.1;0.1;5000;76;-.999;12233459390;0.00009988;1234.567890");

        Assert.assertTrue(fint.isFINTStringParsed());
		testList = new ArrayList<Double>(Arrays.asList(66.1, 0.1, 5000.0, 76.0, -0.999, 
				                                          1.223345939E10, 9.988E-5, 1234.56789));
	    		    keySetList = fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL);
		    assertEquals(keySetList,testList);

	    System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("The input string = " + fint.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
System.out.println("Fill Values List = "+fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL));
		
		
		testCaseNumber++;
	}
	
	@Test
	/*Test for valid FINT string of the form val1   ;   val2  ;   val3  */
	public void testValidFillLevelValuesStringWithBlanks(){

		FINT fint = new FINT("  66.1  ;        0.1        ;            5000        ");

        Assert.assertTrue(fint.isFINTStringParsed());
		testList = new ArrayList<Double>(Arrays.asList(66.1, 0.1, 5000.0));
	    		    keySetList = fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL);
		    assertEquals(keySetList,testList);

	    System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("The input string = " + fint.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
        System.out.println("Fill Values List = "+fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL));
		
		
		testCaseNumber++;
	}
	
	
	@Test
	
	/*Test for valid FINT string of the form fillInterval/minFillValue/ */
	public void testFillIntervalWithMinValueOnly(){
		FINT fint = new FINT("-0.345/0/");
        assertDoubleEquals(fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL), -0.345);
        assertDoubleEquals(fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL), 0);
        assertDoubleEquals(fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
		
		System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("The input string = " + fint.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
		System.out.println("Fill Interval = "+fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Fill Level = "+fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Fill Level = "+fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL));
		
		testCaseNumber++;
	}
	
    /**
     * @param fillInterval
     * @param d
     */
    private void assertDoubleEquals(Double fillInterval, double d) {
        Assert.assertEquals(d, fillInterval.doubleValue(), 0.01);
    }

    @Test
	
	/*Test for valid FINT string of the form fillInterval//maxFillValue */
	public void testFillIntervalWithMaxValueOnly(){
		FINT fint = new FINT("15//30");
        assertDoubleEquals(fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL), 15);
        assertDoubleEquals(fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
        assertDoubleEquals(fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL), 30);
	
		System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
		System.out.println("Fill Interval = "+fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Fill Level = "+fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Fill Level = "+fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL));			
	
		
		testCaseNumber++;
	}
	
	@Test
	/*Test for valid FINT string containing only the fill interval*/
	public void testFINTWithOnlyFillIntervalSpecified(){
		
		FINT fint = new FINT("-0.5/");
		assertEquals(fint.isFINTStringParsed(),true);
        assertDoubleEquals(fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL), -0.5);
        assertDoubleEquals(fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
        assertDoubleEquals(fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
		
	    System.out.println("=====================Test-Case "+testCaseNumber+"a ========================");
	    System.out.println("The input string = " + fint.getUserInputString());
	    System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
		System.out.println("Fill Interval = "+fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Fill Level = "+fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Fill Level = "+fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL));

		FINT fint2 = new FINT("-.89//");
		assertEquals(fint2.isFINTStringParsed(),true);
        assertDoubleEquals(fint2.getFillInterval(FINT.FIRST_ZOOM_LEVEL), -0.89);
        assertDoubleEquals(fint2.getMinFillValue(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
        assertDoubleEquals(fint2.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
		
	    System.out.println("=====================Test-Case "+testCaseNumber+"b ========================");
	    System.out.println("The input string = " + fint2.getUserInputString());
	    System.out.println("Is the FINT string parsed correctly? " + fint2.isFINTStringParsed());
		System.out.println("Fill Interval = "+fint2.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Fill Level = "+fint2.getMinFillValue(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Fill Level = "+fint2.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL));
		
		
		testCaseNumber++;
	}

	@Test
	
	/*Test for valid FINT string of the form /minFillValue/maxFillValue */
	public void testFillIntervalWithNoFillIntervalAndWithMinMaxValueOnly(){
		FINT fint = new FINT("/10/30");
        assertDoubleEquals(fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
        assertDoubleEquals(fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL), 10);
        assertDoubleEquals(fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL), 30);
	
		System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("The input string = " + fint.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
		System.out.println("Fill Interval = "+fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Fill Level = "+fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Fill Level = "+fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL));			
	
		
		testCaseNumber++;
	}
	
	@Test
	
	/*Test for valid FINT string of the form /minFillValue/maxFillValue,where both minFillValue
	 *and maxFillValue are the same*/
	public void testFillIntervalWithMinValSameAsMaxValAndNoFillIntervalSpecified(){
		FINT fint = new FINT("/10/10");
        assertDoubleEquals(fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
        assertDoubleEquals(fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL), 10);
        assertDoubleEquals(fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL), 10);
	    testList = new ArrayList<Double>(Arrays.asList(10.0));
	    		    keySetList = fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL);
		    assertEquals(keySetList,testList);
		System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("The input string = " + fint.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
		System.out.println("Fill Interval = "+fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Fill Level = "+fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Fill Level = "+fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL));			

		System.out.println("Fill Values List = "+fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL));
		
		testCaseNumber++;
	}
	
	@Test
	
	/*Test input string containing a single fill contour value */
	public void testLessNumArgsFillIntervalString(){
	
	    
		FINT fint = new FINT("-0.6");
		
	    assertEquals(fint.isFINTStringParsed(),true);
    
		System.out.println("=====================Test-Case "+testCaseNumber+"a ========================");	
	    System.out.println("The input string = "                           + fint.getUserInputString());
	    System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
	    System.out.println("Fill Values List = "+fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL));
	    System.out.println("Fill Interval = "+fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		
	    FINT fint2 = new FINT("0.7;");
		assertEquals(fint2.isFINTStringParsed(),true);
    
		System.out.println("=====================Test-Case "+testCaseNumber+"b ========================");
	    System.out.println("The input string = "                           + fint2.getUserInputString());
	    System.out.println("Is the FINT string parsed correctly? " + fint2.isFINTStringParsed());			
        System.out.println("Fill Interval = "+fint2.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
	    System.out.println("Fill Values List = "+fint2.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL));
       

        testCaseNumber++;
	}
	
	@Test
	
	/*Test for FINT string of the form fillInterval/minFillValue/maxFillValue/extraNumbers/extraNumbers/extraNumbers*/
	public void testExtraNumArgsFillIntervalString(){
		
		FINT fint = new FINT("20/10/70/30/40/500");
        Assert.assertTrue(fint.isFINTStringParsed());
        assertDoubleEquals(fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL), 20);
        assertDoubleEquals(fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL), 10);
        assertDoubleEquals(fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL), 70);
		
	    testList = new ArrayList<Double>(Arrays.asList(10.0,30.0,50.0,60.0));
	    		    keySetList = fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL);
		    assertEquals(keySetList,testList);
	    
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+fint.getUserInputString());
		System.out.println("Fill Interval      = "+fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Fill Level = "+fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Fill Level = "+fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL));
System.out.println("Fill Values List = "+fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL));
	
	testCaseNumber++;
	}
	
	
	@Test
	
	/*Test for non-numeric values in FINT string*/
	public void testNonNumericFillIntervalString(){
		FINT fint = new FINT("-def/abc/%^&/30/40");
		assertEquals(fint.isFINTStringParsed(), false);
        assertDoubleEquals(fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
        assertDoubleEquals(fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
        assertDoubleEquals(fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
		
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");	
		System.out.println("The input string      = "+fint.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());		
		System.out.println("Fill Interval = "+fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Fill Level = "+fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Fill Level = "+fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL));		
	
	testCaseNumber++;
	}
	
	@Test
	
	/*Test FINT string with invalid delimiters*/
	public void testInvalidDelimitersInFillIntervalString(){
		
		FINT fint = new FINT("5.10.60.9");
		assertEquals(fint.isFINTStringParsed(), false);
        assertDoubleEquals(fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
        assertDoubleEquals(fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
        assertDoubleEquals(fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL),
                Double.NaN);
    	
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+fint.getUserInputString());
		System.out.println("Is the fill data string parsed correctly? " + fint.isFINTStringParsed());
		System.out.println("Fill Interval      = "+fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Fill Level = "+fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Fill Level = "+fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL));
		
		testCaseNumber++;
	}
	
	@Test
	
	/*Test FINT string by interchanging minFillValue and maxFillValue */
	public void testMinMaxValuesInterchangedFillIntervalString(){
		FINT fint = new FINT("-5/20/5");
        Assert.assertTrue(fint.isFINTStringParsed());
        assertDoubleEquals(fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL), -5);
        assertDoubleEquals(fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL), 5);
        assertDoubleEquals(fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL), 20);
		
	    testList = new ArrayList<Double>(Arrays.asList(5.0, 10.0, 15.0, 20.0));
	    		    keySetList = fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL);
		    assertEquals(keySetList,testList);		
		System.out.println("=====================Test-Case "+testCaseNumber+"a========================");
		System.out.println("The input string      = "+fint.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
		System.out.println("Fill Interval = "                           + fint.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Fill Level = "					   + fint.getMinFillValue(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Fill Level = "                      + fint.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL));	 
		System.out.println("Fill Values List = "+fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL));
		
		FINT fint2 = new FINT("5/20/5");
		assertEquals(fint2.isFINTStringParsed(), true);
        assertDoubleEquals(fint2.getFillInterval(FINT.FIRST_ZOOM_LEVEL), 5);
        assertDoubleEquals(fint2.getMinFillValue(FINT.FIRST_ZOOM_LEVEL), 5);
        assertDoubleEquals(fint2.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL), 20);
	    testList = new ArrayList<Double>(Arrays.asList(5.0, 10.0, 15.0, 20.0));
	    keySetList = fint2.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL);
	    assertEquals(testList,keySetList);	
		
	    System.out.println("=====================Test-Case "+testCaseNumber+"b========================");
	    System.out.println("The input string      = "+fint2.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint2.isFINTStringParsed());
		System.out.println("Fill Interval      = "+fint2.getFillInterval(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Minimum Fill Level = "+fint2.getMinFillValue(FINT.FIRST_ZOOM_LEVEL));
		System.out.println("Maximum Fill Level = "+fint2.getMaxFillValue(FINT.FIRST_ZOOM_LEVEL));	 
	

		testCaseNumber++;
	}
	
	@Test
	
	/*Test for non-numeric vales in fill values' string*/
	public void testInvalidFillLevelValuesString(){
		FINT fint = new FINT("66.1;abc;5000;76;;@#$%;12233459390");
	    assertEquals(fint.isFINTStringParsed(), false);
	    
	    System.out.println("=====================Test-Case "+testCaseNumber+"========================");
	    System.out.println("The input string      = "+fint.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
        System.out.println("Fill Values List = "+fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL));
		testCaseNumber++;
		
		}
	
	@Test
	
	/*Test for invalid delimiters in fill values' string */
	public void testInvalidDelimiterFillValuesString(){
		FINT fint = new FINT("66.1,0,1,5000,76,-.999,12233459390");
	    assertEquals(fint.isFINTStringParsed(), false);
	    
	    System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+fint.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());

		testCaseNumber++;
		
		}
	
	@Test
	/*Test for empty FINT string*/
	public void testEmptyString(){
		FINT fint = new FINT();
		assertEquals(fint.isFINTStringParsed(), false);
		System.out.println("=====================Test-Case "+testCaseNumber+"a========================");
		System.out.println("The input string = " + fint.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());	

		
		FINT fint2 = new FINT("");
		assertEquals(fint2.isFINTStringParsed(), false);		
		System.out.println("=====================Test-Case "+testCaseNumber+"b========================");
		System.out.println("The input string = " + fint2.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint2.isFINTStringParsed());		
	
		
		FINT fint3 = new FINT(null);
		assertEquals(fint3.isFINTStringParsed(), false);		
		System.out.println("=====================Test-Case "+testCaseNumber+"c========================");
		System.out.println("The input string = " + fint3.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint3.isFINTStringParsed());		
		
		testCaseNumber++;
	}
	
	@Test
	/*Test for FINT string with blank-spaces*/
	
	public void testFillDataStringWithBlankSpaces(){

		FINT fint = new FINT("     ");
		assertEquals(fint.isFINTStringParsed(), false);
		
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string = " + fint.getUserInputString());
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
		
		testCaseNumber++;
	}
	
	/*
	 * Test contour interval string with multiple zoom levels 
	 */
	@Test
	public void testFINTMultipleZoomLevels(){
	 	FINT fint = new FINT("2/-6/6/3 > 30;50;80 > 60 ; 80 > 0.00009999 > 0.0000001;");
        Assert.assertTrue(fint.isFINTStringParsed());
	 	testList = new ArrayList<Double>(Arrays.asList(-6.0, -4.0,-2.0, 0.0, 2.0, 4.0, 6.0));
	    keySetList = fint.getFillValuesListAsDouble(FINT.FIRST_ZOOM_LEVEL);
	    assertEquals(testList,keySetList);
	 	testList = new ArrayList<Double>(Arrays.asList(30.0,50.0,80.0));
	    keySetList = fint.getFillValuesListAsDouble(FINT.SECOND_ZOOM_LEVEL);
	    assertEquals(testList,keySetList);	    
	 	testList = new ArrayList<Double>(Arrays.asList(60.0,80.0));
	    keySetList = fint.getFillValuesListAsDouble(FINT.THIRD_ZOOM_LEVEL);
	    assertEquals(testList,keySetList);	   	    
	    keySetList = fint.getFillValuesListAsDouble(FINT.FOURTH_ZOOM_LEVEL);
	    assertEquals(Collections.EMPTY_LIST,keySetList);
	    keySetList = fint.getFillValuesListAsDouble(FINT.FIFTH_ZOOM_LEVEL);
	    assertEquals(Collections.EMPTY_LIST,keySetList);	
	 	testList = new ArrayList<Double>(Arrays.asList(-6.0, -4.0, -2.0, 0.0, 2.0, 4.0, 6.0, 30.0, 50.0, 60.0, 80.0));
	 	keySetList = fint.getUniqueSortedFillValuesFromAllZoomLevels();
	 	assertEquals(testList,keySetList);
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+fint.getUserInputString());
		System.out.println("Is the fill data string parsed correctly? " + fint.isFINTStringParsed());
		System.out.println("Fill Interval at the 4th zoom level " + fint.getFillInterval(FINT.FOURTH_ZOOM_LEVEL)   );
		System.out.println("Fill Interval at the 5th zoom level " + fint.getFillInterval(FINT.FIFTH_ZOOM_LEVEL)   );
		System.out.println("The unique fill values sorted in ascending order: "+ fint.getUniqueSortedFillValuesFromAllZoomLevelsAsString());
	}
	
	/*
	 * Test contour interval string with more than 5 zoom levels (currently 5 is the maximum)
	 */
	@Test
	public void testFINTWithMoreThanFiveZoomLevels(){
		FINT fint = new FINT("2/-6/6/3 > 30;50;80 > 60;80 > 0.00009999 > 0.0000001;0.2 > 90 > 100;200;300;400");
 	   testList = new ArrayList<Double>(Arrays.asList(0.0000001,0.2));
	    keySetList = fint.getFillValuesListAsDouble(FINT.MAX_ZOOM_LEVEL);
	    assertEquals(testList,keySetList);
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");
		System.out.println("The input string      = "+fint.getUserInputString());
		System.out.println("Is the fill data string parsed correctly? " + fint.isFINTStringParsed());
		System.out.println("Fill Values at the maximum zoom level " +  fint.getFillValuesListAsDouble(FINT.MAX_ZOOM_LEVEL)  ); 
	}
	
	/*
	 * Test contour interval string with multiple zoom levels, some of which are invalid 
	 */	
	@Test 
	public void testFINTWithInvalidZoomLevels() throws IndexOutOfBoundsException{
		System.out.println("=====================Test-Case "+testCaseNumber+"========================");

		FINT fint = new FINT("2/-6/6/3 >  > 60; 80 >  > ");
		System.out.println("The input string    = "+"2/-6/6/3 >  > 60; 80 >  > ");
		System.out.println("Is the FINT string parsed correctly? " + fint.isFINTStringParsed());
	}	
	
	
	
	
}




















