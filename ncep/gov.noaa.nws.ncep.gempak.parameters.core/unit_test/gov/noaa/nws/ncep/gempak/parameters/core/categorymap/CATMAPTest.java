package gov.noaa.nws.ncep.gempak.parameters.core.categorymap;

import org.junit.Assert;
import org.junit.Test;

/**
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 16-Nov-2009    194        Archana.S   Initial Creation
 * 20-Nov-2009    194        Archana.S   Updated per review comments:
 *                                         Added a test-case to retrieve the label
 *                                         given a value
 *                                         Updated method names per design changes in the class
 *                                         CATMAP
 * 25-Aug-2012    743        djohnson    Upgrade to JUnit 4.10.
 * 
 * </pre>
 * 
 * @author Archana.S
 * @version 1
 *          <p>
 *          {@link gov.noaa.nws.ncep.gempak.parameters.core.categorymap.CatMap}.
 */
public class CATMAPTest {
private static int testCaseNumber;

    private static final double ALLOWABLE_DOUBLE_DELTA = 0.0001;

    /**
	 *Test for valid label/value pair 
	 */
	@Test
	public void testGetValueForLabel() {
		testCaseNumber=1;
		CatMap catmap = new CatMap("ABC=4;acd=2;lmn=0.5");
		assertEquals(catmap.getMatchingValueForLabel("abc"),4.0);
		assertEquals(catmap.getMatchingValueForLabel("ABC"),4.0);
		assertEquals(catmap.getMatchingValueForLabel("LmN"),0.5);
		System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for valid label/value pairs without wild-card");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("ABC"));
		System.out.println("The value for label abc = "+ catmap.getMatchingValueForLabel("abc"));
		System.out.println("The value for label LmN = "+ catmap.getMatchingValueForLabel("LmN"));
		testCaseNumber++;
	
	}

    /**
     * @param val1
     * @param expected
     */
    private void assertEquals(Float val1, double expected) {
        Assert.assertEquals(expected, val1.doubleValue(),
                ALLOWABLE_DOUBLE_DELTA);
    }

    /**
     * Test to retrieve matching label given a floating point value
     */
	@Test
	public void testGetLabelForValue() {

		CatMap catmap = new CatMap("ABC=4;acd=2;lmn=0.5;efg=2;hij=2");

        Assert.assertEquals(catmap.getMatchingLabelForValue(0.5f), "lmn");
        Assert.assertEquals(catmap.getMatchingLabelForValue(4f), "ABC");
        Assert.assertEquals(catmap.getMatchingLabelForValue(2f), "acd");

		System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for getting the matching label, given a  floating point value");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		System.out.println("The label for the value 4 = "+ catmap.getMatchingLabelForValue(4f));
		System.out.println("The label for the value 0.5 = "+ catmap.getMatchingLabelForValue(0.5f));
		System.out.println("The label for the value 2 = "+ catmap.getMatchingLabelForValue(2f));
		testCaseNumber++;
	
	}
	
    /**
	 *Test to retrieve the label for a floating point value not in the list 
	 */
	@Test
	public void testGetLabelForNonExistentValue() {
	
		CatMap catmap = new CatMap("ABC=4;acd=2;lmn=0.5;efg=2;hij=2");
	
        Assert.assertNull(catmap.getMatchingLabelForValue(0.00005f));

		System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for getting the matching label, given a floating point value not in the list");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		System.out.println("The label for the value 0.00005 = "+ catmap.getMatchingLabelForValue(0.00005f));
		testCaseNumber++;
	
	}
	
    /**
	 *Test for valid label with a wild-card character
	 */
	@Test
	public void testGetValueForWildcardLabel() {
		
		CatMap catmap = new CatMap("Abc=4;a*=0.5;acd=2");
		assertEquals(catmap.getMatchingValueForLabel("abc"),4.0);
		assertEquals(catmap.getMatchingValueForLabel("ACD"),0.5);
		assertEquals(catmap.getMatchingValueForLabel("A*"),0.5);
		System.out.println("\n=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for valid label/value pairs with a wild-card label included");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("ABC"));
		System.out.println("The value for label aCD = "+ catmap.getMatchingValueForLabel("aCD"));
		System.out.println("The value for label a = "+ catmap.getMatchingValueForLabel("a"));
		testCaseNumber++;
	
	}	

    /**
	 *Test CATMAP String with invalid delimiters
	 */
	@Test
	public void testInvalidDelimiter() {
		
		CatMap catmap = new CatMap("Abc=4:a*=0.5:acd=2");
		assertEquals(catmap.getMatchingValueForLabel("abc"),Float.NaN);
		assertEquals(catmap.getMatchingValueForLabel("ACD"),Float.NaN);
		assertEquals(catmap.getMatchingValueForLabel("A*"),Float.NaN);
		
		System.out.println("\n=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for invalid delimiters");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("ABC"));
		System.out.println("The value for label aCD = "+ catmap.getMatchingValueForLabel("aCD"));
		System.out.println("The value for label a = "+ catmap.getMatchingValueForLabel("a"));
		testCaseNumber++;
	
	}	
	
	/**
	 * Test CATMAP String without '=' character between label/value pairs
	 */
	@Test
	public void testCATMAPStringWithoutEqualToSign() {
		
		CatMap catmap = new CatMap("Abc-4;a*-0.5;acd+2");
		assertEquals(catmap.getMatchingValueForLabel("abc"),Float.NaN);
		assertEquals(catmap.getMatchingValueForLabel("ACD"),Float.NaN);
		assertEquals(catmap.getMatchingValueForLabel("A*"),Float.NaN);
		
		System.out.println("\n=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for missing '=' character");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("ABC"));
		System.out.println("The value for label aCD = "+ catmap.getMatchingValueForLabel("aCD"));
		System.out.println("The value for label a = "+ catmap.getMatchingValueForLabel("a"));
		testCaseNumber++;
	
	}
	
	/**
	 * Test empty CATMAP String
	 */
	@Test
	public void testEmptyCATMAPString(){
		CatMap catmap = new CatMap("");
		System.out.println("\n=====================Test-Case "+testCaseNumber+"a ========================");
		System.out.println("Test for empty input string");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		System.out.println("The value for label \"\" = "+ catmap.getMatchingValueForLabel(""));
		
		
		CatMap catmap3 = new CatMap(null);
		System.out.println("\n=====================Test-Case "+testCaseNumber+"b ========================");
		System.out.println("Test for null string");
		System.out.println("The input string = "+catmap3.getCategoricalMappingString());
		System.out.println("The value for null string = "+ catmap3.getMatchingValueForLabel(null));
		
		CatMap catmap4 = new CatMap("     ");
		System.out.println("\n=====================Test-Case "+testCaseNumber+"c ========================");
		System.out.println("Test for string with only blanks");
		System.out.println("The input string = "+catmap4.getCategoricalMappingString());
		System.out.println("The value for string with only blanks= "+ catmap4.getMatchingValueForLabel("     "));		
		testCaseNumber++;
	}
	
	/**
	 * Test CATMAP String with a missing label
	 */
	
	@Test
	public void testMissingLabel(){
		CatMap catmap = new CatMap("Abc=4;bingo=-0.5; =2");
		assertEquals(catmap.getMatchingValueForLabel("abc"),4.0);
		assertEquals(catmap.getMatchingValueForLabel("bINgO"),-0.5);
		assertEquals(catmap.getMatchingValueForLabel(" "),Float.NaN);
		
		System.out.println("\n=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for missing Label");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("ABC"));
		System.out.println("The value for label bINgO = "+ catmap.getMatchingValueForLabel("bINgO"));
		System.out.println("The value for the missing label= "+ catmap.getMatchingValueForLabel(" "));
		testCaseNumber++;
	}
	
	/**
	 * Test CATMAP String with a missing value
	 */
	@Test
	public void testMissingValue(){
		CatMap catmap = new CatMap("Abc= ;bingo=-.5;label3=2");
		assertEquals(catmap.getMatchingValueForLabel("abc"),Float.NaN);
		assertEquals(catmap.getMatchingValueForLabel("bINgO"),-0.5);
		assertEquals(catmap.getMatchingValueForLabel("label3"),2);
		
		System.out.println("\n=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for missing value");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("ABC"));
		System.out.println("The value for label bINgO = "+ catmap.getMatchingValueForLabel("bINgO"));
		System.out.println("The value for the label3= "+ catmap.getMatchingValueForLabel("label3"));
		testCaseNumber++;
	}
	
	/**
	 * Test CATMAP string with a missing label and a missing value
	 */
	@Test
	public void testMissingValueAndLabel(){
		CatMap catmap = new CatMap("Abc=99.99999;  =  ;bingo=-.5");
		assertEquals(catmap.getMatchingValueForLabel("abc"),99.99999);
		assertEquals(catmap.getMatchingValueForLabel("bINgO"),-0.5);
		assertEquals(catmap.getMatchingValueForLabel(" "),Float.NaN);
		
		System.out.println("\n=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for missing label and value");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("ABC"));
		System.out.println("The value for label bINgO = "+ catmap.getMatchingValueForLabel("bINgO"));
		System.out.println("The value for the missing label= "+ catmap.getMatchingValueForLabel(" "));
		testCaseNumber++;
	}
	
	/**
	 * Test CATMAP string with missing label/value pairs
	 */
	@Test
	public void testMissingLabelValuePairs(){
		CatMap catmap = new CatMap("Abc=99.99999;;;;bingo=-.5");
		assertEquals(catmap.getMatchingValueForLabel("abc"),99.99999);
		assertEquals(catmap.getMatchingValueForLabel("bINgO"),-0.5);
		
		System.out.println("\n=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test with missing label/value pairs");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("ABC"));
		System.out.println("The value for label bINgO = "+ catmap.getMatchingValueForLabel("bINgO"));
		testCaseNumber++;
	}
	
	/**
	 * Test CATMAP string with non-numeric data for value
	 */
	@Test
	public void testNonNumericDataForValue(){
		CatMap catmap = new CatMap("Abc=99acd.99999;bingo=-.5");
		assertEquals(catmap.getMatchingValueForLabel("abc"),Float.NaN);
		assertEquals(catmap.getMatchingValueForLabel("bINgO"),Float.NaN);
		
		
		System.out.println("\n=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for non-numeric characters in the value");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("ABC"));
		System.out.println("The value for label bINgO = "+ catmap.getMatchingValueForLabel("bINgO"));
		
		testCaseNumber++;
	}
	
	
	/**
	 * Test CATMAP string with multiple '=' characters
	 */
	@Test
	public void testMultipleEqualToCharaters(){
		CatMap catmap = new CatMap("Abc=99;bingo==-.5");
		assertEquals(catmap.getMatchingValueForLabel("abc"),99);
		assertEquals(catmap.getMatchingValueForLabel("bINgO"),Float.NaN);
		
		
		System.out.println("\n=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for multiple '=' characters");
		System.out.println("The input string = "+catmap.getCategoricalMappingString());
		
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("ABC"));
		System.out.println("The value for label bingo = "+ catmap.getMatchingValueForLabel("bingo"));

		
		testCaseNumber++;
	}
	/**
	 * Test CATMAP string with labels containing punctuation symbols
	 */
	
	@Test
	public void testLabelWithPunctuationSymbols(){
		CatMap catmap = new CatMap("@!'Ab#c=99;@#$%^=19.08");
		assertEquals(catmap.getMatchingValueForLabel("@!'Ab#c"),99);
		assertEquals(catmap.getMatchingValueForLabel("@#$%^"),19.08);
		
		
		System.out.println("\n=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for labels containing punctuation symbols");
		System.out.println("The input string = "+catmap.getCategoricalMappingString()+"\n");
		
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("@!'Ab#c"));
		System.out.println("The value for label bINgO = "+ catmap.getMatchingValueForLabel("@#$%^"));
		
		testCaseNumber++;
	}
	
	/**
	 * Test valid CATMAP string with blanks between labels and values
	 */
	@Test
	public void testCATMAPStringWithBlanksBetweenLabelsAndValues(){
		CatMap catmap = new CatMap("   @!'Ab#c   =   99   ;  @#$%^    =      19.08   ");
		assertEquals(catmap.getMatchingValueForLabel("@!'Ab#c"),99);
		assertEquals(catmap.getMatchingValueForLabel("@#$%^"),19.08);
		
		
		System.out.println("\n=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for string containing blanks between labels and values");
		System.out.println("The input string = "+catmap.getCategoricalMappingString()+"\n");
		
		System.out.println("The value for label ABC = "+ catmap.getMatchingValueForLabel("@!'Ab#c"));
		System.out.println("The value for label bINgO = "+ catmap.getMatchingValueForLabel("@#$%^"));
		
		testCaseNumber++;
	}
	
}
























