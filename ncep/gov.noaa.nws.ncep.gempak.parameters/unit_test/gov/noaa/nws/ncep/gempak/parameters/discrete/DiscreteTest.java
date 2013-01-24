/**
 * 
 */
package gov.noaa.nws.ncep.gempak.parameters.discrete;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

/**
 * <pre>
 * 
 * Junit test for {@link gov.noaa.nws.ncep.gempak.parameters.discrete.Discrete}.
 * 
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 10-Dec-2009    205       Archana.S   Initial Creation
 * 18-Dec-2009    205       Archana.S   Updated per review comments:
 *                                      ----------------------------
 *                                      1. Removed all references to hard-coded tolerance value
 *                                         (0.000001) and replaced it with the method
 *                                         getTolerance(). Accordingly updated Javadoc for the corresponding
 *                                         tests. 
 *                                      2. Updated all test-cases, to replace the '-' delimiter in the
 *                                         input string with '~' 
 *                                      3. Added the following test-cases: 
 *                                      a. To parse negative numbers in the Discrete string.
 *                                      b. Test for extra arguments.
 * 25-Aug-2012   743        djohnson    Upgrade to JUnit 4.10.
 * </pre>
 * 
 * @author Archana.S
 * @version 1
 */
public class DiscreteTest {
    private static final double ALLOWABLE_DOUBLE_DELTA = 0.1;

    private static int testCaseNumber;
	private List<Float> testList;
	/**
	 * Test valid input string with multiple start/end/discrete value strings.
	 */
	@Test
	public void testGetDiscreteDataList() {
		testCaseNumber=1;

		Discrete discreteObj = new Discrete("0.1~.5  =  0.444 ;   0.6  ~  0.9  =   0.75  ;   0.99  ~  1.15  =  1.105");
		testList = new ArrayList<Float>(Arrays.asList(0.1f,0.5f,0.444f,0.6f,0.9f,0.75f,0.99f,1.15f,1.05f));
		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test valid input string with multiple start/end/discrete value pair strings");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		for(int i=0;i < discreteObj.getDiscreteDataList().size(); i++){

			System.out.println("-----------------");
			System.out.println("Element number:"+ (i+1));
			System.out.println("Start value = "+discreteObj
					.getDiscreteDataList().get(i).getStartValue());
			System.out.println("End value = "+discreteObj
					.getDiscreteDataList().get(i).getEndValue());
			System.out.println("Discrete value = "+discreteObj
					.getDiscreteDataList().get(i).getDiscreteValue());
			if(i==0){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(0));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(1));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(2));
			}
			else if(i==1){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(3));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(4));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(5));
			}
			else if(i==2){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(6));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(7));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(8));
			}
		}
		System.out.println("==========================================");
		testCaseNumber++;
	}


	/**
	 * Test valid input string with multiple start/end/discrete value strings, all of which are negative numbers
	 */
	@Test
	public void testGetDiscreteDataListForNegativeNumbers() {

		Discrete discreteObj = new Discrete("-0.1~ -.5  =  -0.444 ;   -0.6  ~  -0.9  =    -0.75  ;   -0.99  ~  -1.15  =  -1.105");
		testList = new ArrayList<Float>(Arrays.asList(-0.1f,-0.5f,-0.444f,-0.6f,-0.9f,-0.75f,-0.99f,-1.15f,-1.05f));
		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test valid input string with multiple start/end/discrete value pair strings,  all of which are negative numbers");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		for(int i=0;i < discreteObj.getDiscreteDataList().size(); i++){

			System.out.println("-----------------");
			System.out.println("Element number:"+ (i+1));
			System.out.println("Start value = "+discreteObj
					.getDiscreteDataList().get(i).getStartValue());
			System.out.println("End value = "+discreteObj
					.getDiscreteDataList().get(i).getEndValue());
			System.out.println("Discrete value = "+discreteObj
					.getDiscreteDataList().get(i).getDiscreteValue());
			if(i==0){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(0));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(1));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(2));
			}
			else if(i==1){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(3));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(4));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(5));
			}
			else if(i==2){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(6));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(7));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(8));
			}
		}
		System.out.println("==========================================");
		testCaseNumber++;
	}
	
	/**
	 * Test valid input string with a missing start value in the first string.
	 */
	@Test
	public void testDiscreteDataListWithMissingStartValueInFirstString() {

		Discrete discreteObj = new Discrete("~.5=0.444;0.6~0.9=0.75;0.99~1.15=1.105");
		testList = new ArrayList<Float>(Arrays.asList(-Float.MAX_VALUE,0.5f,0.444f,0.6f,0.9f,
				0.75f,0.99f,1.15f,1.05f));

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test valid input string with a missing start value in the first string");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		for(int i=0;i < discreteObj.getDiscreteDataList().size(); i++){

			System.out.println("-----------------");
			System.out.println("Element number:"+ (i+1));
			System.out.println("Start value = "+discreteObj
					.getDiscreteDataList().get(i).getStartValue());
			System.out.println("End value = "+discreteObj
					.getDiscreteDataList().get(i).getEndValue());
			System.out.println("Discrete value = "+discreteObj
					.getDiscreteDataList().get(i).getDiscreteValue());
			if(i==0){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(0));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(1));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(2));
			}
			else if(i==1){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(3));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(4));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(5));
			}
			else if(i==2){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(6));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(7));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(8));
			}
		}

		testCaseNumber++;
		System.out.println("==========================================");
	}

	/**
	 * Test input string with a missing start value in the second string.
	 */
	@Test
	public void testDiscreteDataListWithMissingStartValueInSecondString() {

		Discrete discreteObj = new Discrete("0.1~.5=0.444;~0.9=0.75;0.99~1.15=1.105");
		testList = new ArrayList<Float>(Arrays.asList(0.1f,0.5f,0.444f,Float.NaN ,0.9f,
				0.75f,0.99f,1.15f,1.05f));

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test input string with a missing start value in the second string.");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		for(int i=0;i < discreteObj.getDiscreteDataList().size(); i++){

			System.out.println("-----------------");
			System.out.println("Element number:"+(i+1));
			System.out.println("Start value = "+discreteObj
					.getDiscreteDataList().get(i).getStartValue());
			System.out.println("End value = "+discreteObj
					.getDiscreteDataList().get(i).getEndValue());
			System.out.println("Discrete value = "+discreteObj
					.getDiscreteDataList().get(i).getDiscreteValue());
			if(i==0){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(0));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(1));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(2));
			}
			else if(i==1){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(3));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(4));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(5));
			}
			else if(i==2){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(6));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(7));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(8));
			}
		}

		testCaseNumber++;
		System.out.println("==========================================");
	}

	/**
	 * Test valid input string with a missing end value in the last string.
	 */
	@Test
	public void testDiscreteDataListWithMissingEndValueInLastString() {


		Discrete discreteObj = new Discrete("0.1~.5=0.444;0.6~0.9=0.75;0.99~=1.105");
		testList = new ArrayList<Float>(Arrays.asList(0.1f,0.5f,0.444f,0.6f ,0.9f,
				0.75f,0.99f,Float.MAX_VALUE,1.05f));
		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test valid input string with a missing end value in the last string\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		for(int i=0;i < discreteObj.getDiscreteDataList().size(); i++){

			System.out.println("-----------------");
			System.out.println("Element number:"+(i+1));
			System.out.println("Start value = "+discreteObj
					.getDiscreteDataList().get(i).getStartValue());
			System.out.println("End value = "+discreteObj
					.getDiscreteDataList().get(i).getEndValue());
			System.out.println("Discrete value = "+discreteObj
					.getDiscreteDataList().get(i).getDiscreteValue());
			if(i==0){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(0));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(1));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(2));
			}
			else if(i==1){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(3));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(4));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(5));
			}
			else if(i==2){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(6));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(7));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(8));
			}
		}

		testCaseNumber++;
		System.out.println("==========================================");
	}

	/**
	 * Test input string with a missing end value in the second string.
	 */
	@Test
	public void testDiscreteDataListWithMissingEndValueInSecondString() {

		Discrete discreteObj = new Discrete("0.1~.5=0.444;0.6~=0.75;0.99~1.15=1.105");
		testList = new ArrayList<Float>(Arrays.asList(0.1f,0.5f,0.444f,0.6f ,Float.NaN,
				0.75f,0.99f,1.15f,1.05f));

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test input string with a missing end value in the second string\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		for(int i=0;i < discreteObj.getDiscreteDataList().size(); i++){

			System.out.println("-----------------");
			System.out.println("Element number:"+(i+1));
			System.out.println("Start value = "+discreteObj
					.getDiscreteDataList().get(i).getStartValue());
			System.out.println("End value = "+discreteObj
					.getDiscreteDataList().get(i).getEndValue());
			System.out.println("Discrete value = "+discreteObj
					.getDiscreteDataList().get(i).getDiscreteValue());
			if(i==0){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(0));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(1));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(2));
			}
			else if(i==1){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(3));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(4));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(5));
			}
			else if(i==2){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(6));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(7));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(8));
			}
		}

		testCaseNumber++;
		System.out.println("==========================================");
	}
	/**
	 * Test input string with missing discrete values in all strings.
	 */
	@Test
	public void testDiscreteDataListWithMissingDiscreteValuesInAllStrings() {

		Discrete discreteObj = new Discrete("0.1~.5=;0.6~0.9=;0.99~1.15=");
		testList = new ArrayList<Float>(Arrays.asList(0.1f,0.5f,Float.NaN,0.6f ,0.9f,
				0.75f,Float.NaN,1.15f,Float.NaN));

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test input string with missing discrete values in all strings\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		for(int i=0;i < discreteObj.getDiscreteDataList().size(); i++){

			System.out.println("-----------------");
			System.out.println("Element number:"+(i+1));
			System.out.println("Start value = "+discreteObj
					.getDiscreteDataList().get(i).getStartValue());
			System.out.println("End value = "+discreteObj
					.getDiscreteDataList().get(i).getEndValue());
			System.out.println("Discrete value = "+discreteObj
					.getDiscreteDataList().get(i).getDiscreteValue());
			if(i==0){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(0));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(1));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(2));
			}
			else if(i==1){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(3));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(4));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(5));
			}
			else if(i==2){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(6));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(7));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(8));
			}
		}

		testCaseNumber++;
		System.out.println("==========================================");
	}

	/**
	 * Test input string with invalid delimiters
	 */
	@Test
	public void testDiscreteDataListWithInvalidDelimiters() {

		Discrete discreteObj = new Discrete("0.1-.5=0.3,0.6-0.9=0.75,0.99-1.15=1.05");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test input string with invalid delimiters\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		System.out.println("Size of discrete data list = "+discreteObj.getDiscreteDataList().size());
        assertEquals(discreteObj.getDiscreteDataList().size(), 0);
		testCaseNumber++;
		System.out.println("==========================================");
	}
	

	/**
	 * Test input string with all numbers missing
	 */

	@Test
	public void testDiscreteDataListWithAllNumbersMissing() {

		Discrete discreteObj = new Discrete(" ~ = ; ~ = ; ~ =");
		testList = new ArrayList<Float>(Arrays.asList(-Float.MAX_VALUE,Float.NaN,Float.NaN,Float.NaN,Float.NaN,
				Float.NaN,Float.NaN,Float.MAX_VALUE,Float.NaN));

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test input string with  all numbers missing\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		System.out.println("Size of discrete data list = "+discreteObj.getDiscreteDataList().size());
		for(int i=0;i < discreteObj.getDiscreteDataList().size(); i++){

			System.out.println("-----------------");
			System.out.println("Element number:"+(i+1));
			System.out.println("Start value = "+discreteObj
					.getDiscreteDataList().get(i).getStartValue());
			System.out.println("End value = "+discreteObj
					.getDiscreteDataList().get(i).getEndValue());
			System.out.println("Discrete value = "+discreteObj
					.getDiscreteDataList().get(i).getDiscreteValue());
			if(i==0){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(0));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(1));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(2));
			}
			else if(i==1){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(3));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(4));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(5));
			}
			else if(i==2){
                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getStartValue(),testList.get(6));

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getEndValue(),testList.get(7));			

                assertFloatComparison(discreteObj.getDiscreteDataList()
						.get(i).getDiscreteValue(),testList.get(8));
			}
		}
		testCaseNumber++;
		System.out.println("==========================================");
	}

	/**
	 * Test Discrete with extra arguments
	 */
	@Test
	public void testGetDiscreteDataListForExtraArguments() {

		Discrete discreteObj = new Discrete("-0.1 ~ -.5 ~ -0.5999  =  -0.444 ;   -0.6  ~  -0.9 ~ -0.9111  =    -0.75  ;   -0.99  ~  -1.15  =  -1.105");
		
		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test Discrete with extra arguments");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		System.out.println( "Size of the discrete data list is " + discreteObj.getDiscreteDataList().size());

		System.out.println("==========================================");
		testCaseNumber++;
	}
	
	/**
	 * Test empty string
	 */
	@Test
	public void testDiscreteDataListWithEmptyString() {

		Discrete discreteObj = new Discrete("");

		System.out.println("Test-case: "+testCaseNumber+" a");
		System.out.println("Test empty string\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		System.out.println("Size of discrete data list = "+discreteObj.getDiscreteDataList().size());
        assertEquals(discreteObj.getDiscreteDataList().size(), 0);
		
		
		Discrete discreteObj2 = new Discrete(null);

		System.out.println("Test-case: "+testCaseNumber+" b");
		System.out.println("Test empty string\n");
		System.out.println("The input string is "+discreteObj2.getStrDiscrete());
		System.out.println("Size of discrete data list = "+discreteObj2.getDiscreteDataList().size());
        assertEquals(discreteObj2.getDiscreteDataList().size(), 0);
		
		Discrete discreteObj3 = new Discrete("           ");

		System.out.println("Test-case: "+testCaseNumber+" c");
		System.out.println("Test empty string\n");
		System.out.println("The input string is "+discreteObj3.getStrDiscrete());
		System.out.println("Size of discrete data list = "+discreteObj3.getDiscreteDataList().size());
        assertEquals(discreteObj3.getDiscreteDataList().size(), 0);
		
		System.out.println("==========================================");
		testCaseNumber++;
	}

	/**
	 * Test input string with non numeric inputs
	 */
	@Test
	public void testDiscreteDataListWithNonNumericInputs() {

		Discrete discreteObj = new Discrete("0.1l~.S5=0.O3;0.O6~0.90=0.S75;0.J99~1Q.1$5=1.^05");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test input string with non numeric inputs\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		System.out.println("Size of discrete data list = "+discreteObj.getDiscreteDataList().size());
        assertEquals(discreteObj.getDiscreteDataList().size(), 0);
		testCaseNumber++;
		System.out.println("==========================================");
	}
	
	/**
	 * Test the method getDiscreteValueLeftOfCurrentDiscreteValue()
	 */
	@Test
	public void testGetDiscreteValueLeftOfCurrentDiscreteValue(){
		Discrete discreteObj = new Discrete("0.1~.5=0.444;0.6~0.9=0.75;0.99~1.15=1.105;1.25~4.5=3.6");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test the method getDiscreteValueLeftOfCurrentDiscreteValue()\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		
		System.out.println("Discrete Value to the left of 3.6 = "+discreteObj.getPrecedingDiscreteValue(3.6f));
		System.out.println("Discrete Value to the left of 0.444 = "+discreteObj.getPrecedingDiscreteValue(0.444f));
        assertFloatComparison(discreteObj.getPrecedingDiscreteValue(3.6f),
                1.105f);
        assertFloatComparison(discreteObj.getPrecedingDiscreteValue(0.444f),
                0.444f);
	testCaseNumber++;
		System.out.println("==========================================");
	}

    /**
     * @param precedingDiscreteValue
     * @param f
     */
    private void assertFloatComparison(Float precedingDiscreteValue, float f) {
        assertEquals(precedingDiscreteValue.floatValue(), f,
                ALLOWABLE_DOUBLE_DELTA);
    }

    /**
     * Test the method getDiscreteValueClosestToTwoContourLines() when the
     * following conditions are satisfied: The absolute difference between the
     * current start value and the first parameter in the function is less than
     * the tolerance value and The absolute difference between the current end
     * value and the second parameter in the function is less than the tolerance
     * value
     */
	@Test
	public void testGetDiscreteValueClosestToTwoContourLines1(){
		Discrete discreteObj = new Discrete("0.1~.5=0.444;0.6~0.9=0.75;0.99~1.15=1.105;1.25~3.5=3.6");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test the method getDiscreteValueClosestToTwoContourLines() when the following conditions are satisfied:\n"+
			     "The absolute difference between the current start value and the first parameter in the function is\n"+ 
			     "less than " + discreteObj.getTOLERANCE() +"and \n" + 
			     "The absolute difference between the current end value and the second parameter in the function\nis "+ 
			     "less than " + discreteObj.getTOLERANCE() +"\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		
		System.out.println("Discrete Value between 0.99 and 1.15 = "+discreteObj.getDiscreteValueClosestToTwoContourLines(0.99f,1.15f));
        assertFloatComparison(
                discreteObj.getDiscreteValueClosestToTwoContourLines(0.99f,
                        1.15f), 1.105f);
		
		testCaseNumber++;
		System.out.println("==========================================");	
	}
	
	/**
	 * Test the method getDiscreteValueClosestToTwoContourLines() when the following conditions are satisfied:
	 * The absolute difference between the current start value and the first parameter in the function is 
	 * exactly the tolerance value and   
	 * The absolute difference between the current end value and the second parameter in the function is 
	 * less than the tolerance value
	 */	
	@Test
	public void testGetDiscreteValueClosestToTwoContourLines2(){
		Discrete discreteObj = new Discrete("0.1~.5=0.444;0.6~0.9=0.75;0.99~1.15=1.105;1.25~2.5=3.6");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test the method getDiscreteValueClosestToTwoContourLines() when the following conditions are satisfied:\n"+
	     "The absolute difference between the current start value and the first parameter in the function is\n"+ 
	     "exactly " + discreteObj.getTOLERANCE() +" and\n" + 
	     "The absolute difference between the current end value and the second parameter in the function\nis "+ 
	     "less than " + discreteObj.getTOLERANCE() +"\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		
		System.out.println("Discrete Value between 0.990001 and 1.15 = "+discreteObj.getDiscreteValueClosestToTwoContourLines(0.990001f,1.15f));
        assertFloatComparison(
                discreteObj.getDiscreteValueClosestToTwoContourLines(0.990001f,
                        1.15f), Float.NaN);
		
		testCaseNumber++;
		System.out.println("==========================================");	
	}
	
	/**
	 * Test the method getDiscreteValueClosestToTwoContourLines() when the following conditions are satisfied:
	 * The absolute difference between the current start value and the first parameter in the function is 
	 * less than the tolerance value and   
	 * The absolute difference between the current end value and the second parameter in the function is 
	 * exactly the tolerance value
	 */
	@Test
	public void testGetDiscreteValueClosestToTwoContourLines3(){
		Discrete discreteObj = new Discrete("0.1~.5=0.444;0.6~0.9=0.75;0.99~1.15=1.105;1.25~2.5=3.6");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test the method getDiscreteValueClosestToTwoContourLines() when the following conditions are satisfied:\n"+
			     "The absolute difference between the current start value and the first parameter in the function is\n"+ 
			     "less than " + discreteObj.getTOLERANCE() +"\n" + 
			     "The absolute difference between the current end value and the second parameter in the function\nis "+ 
			     "exactly " + discreteObj.getTOLERANCE() +"\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		
		System.out.println("Discrete Value between 0.99 and 1.150001 = "+discreteObj.getDiscreteValueClosestToTwoContourLines(0.99f,1.150001f));
        assertFloatComparison(
                discreteObj.getDiscreteValueClosestToTwoContourLines(0.99f,
                        1.150001f), Float.NaN);
		
		testCaseNumber++;
		System.out.println("==========================================");	
	}
	
	
	/**
	 * Test the method getDiscreteValueClosestToTwoContourLines() when the following conditions are satisfied:
	 * The absolute difference between the negative of current start value and the Float MAX
	 * is less than the tolerance value 
	 * The first parameter in the function is >= the current starting value  
	 * The absolute difference between the current end value and the second parameter in the function is 
	 * less than the tolerance value
	 */
	@Test
	public void testGetDiscreteValueClosestToTwoContourLines4(){
		Discrete discreteObj = new Discrete("~.5=0.444;0.6~0.9=0.75;0.99~1.15=1.105;1.25~2.5=3.6");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test the method getDiscreteValueClosestToTwoContourLines() when the following conditions are satisfied:\n"+
			     "The absolute difference between the negative of the current start value and the Float MAX is\n"+ 
			     "less than " + discreteObj.getTOLERANCE() +"\n" + 
			     "The first parameter in the function is >= the current starting value and \n"+
			     "The absolute difference between the current end value and the second parameter in the function\nis "+ 
			     "less than " + discreteObj.getTOLERANCE() +"\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		
		System.out.println("Discrete Value between Float.MAX-VALUE and 1.15 = "+discreteObj.getDiscreteValueClosestToTwoContourLines(Float.MAX_VALUE, 0.5f));
        assertFloatComparison(
                discreteObj.getDiscreteValueClosestToTwoContourLines(
                        Float.MAX_VALUE, 0.5f), 0.444f);
		
		testCaseNumber++;
		System.out.println("==========================================");	
	}
	
	/**
	 * Test the method getDiscreteValueClosestToTwoContourLines() when the following conditions are satisfied:
	 * The absolute difference between the negative of current start value and the Float MAX
	 * is less than the tolerance value 
	 * The first parameter in the function is >= the current starting value  
	 * The absolute difference between the current end value and the second parameter in the function is 
	 * less than the tolerance value
	 */
	@Test
	public void testGetDiscreteValueClosestToTwoContourLines5(){
		Discrete discreteObj = new Discrete("0.1~.5=0.444;0.6~0.9=0.75;0.99~1.15=1.105;1.25~=3.6");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test the method getDiscreteValueClosestToTwoContourLines() when the following conditions are satisfied:\n"+
			     "The absolute difference between the current end value and the Float MAX is\n"+ 
			     "less than" + discreteObj.getTOLERANCE() +"\n" + 
			     "The second parameter in the function is <= the current ending value and \n"+
			     "The absolute difference between the current start value and the first parameter in the function\nis "+ 
			     "less than" + discreteObj.getTOLERANCE() +"\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		
		System.out.println("Discrete Value between 1.25 and Float.MAX-VALUE = "+discreteObj.getDiscreteValueClosestToTwoContourLines(1.25f,Float.MAX_VALUE));
        assertFloatComparison(
                discreteObj.getDiscreteValueClosestToTwoContourLines(1.25f,
                        Float.MAX_VALUE), 3.6f);
		
		testCaseNumber++;
		System.out.println("==========================================");	
	}
	
	/**
	 * Test Discrete class when only a single value1-value2=value3; pair is given
	 */
	
	@Test
	public void testSingleString(){
		Discrete discreteObj = new Discrete("0.1~.5=0.444;");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test Discrete class when only a single value1-value2=value3; pair is given\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		System.out.println("The size of the list"+discreteObj.getDiscreteDataList().size());
		System.out.println("The starting value="+discreteObj.getDiscreteDataList().get(0).getStartValue());
		System.out.println("The ending value="+discreteObj.getDiscreteDataList().get(0).getEndValue());
		System.out.println("The discrete value="+discreteObj.getDiscreteDataList().get(0).getDiscreteValue());
        assertEquals(discreteObj.getDiscreteDataList().size(), 1);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getStartValue(), 0.1f);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getEndValue(), 0.5f);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getDiscreteValue(), 0.444f);
		testCaseNumber++;
		System.out.println("==========================================");	
		
		
	}
	
	/**
	 * Test Discrete class when only a single value1-value2=value3 pair is given
	 */
	
	@Test
	public void testSingleString2(){
		Discrete discreteObj = new Discrete("0.1~.5=0.444");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test Discrete class when only a single value1-value2=value3; pair is given\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		System.out.println("The size of the list"+discreteObj.getDiscreteDataList().size());
		System.out.println("The starting value="+discreteObj.getDiscreteDataList().get(0).getStartValue());
		System.out.println("The ending value="+discreteObj.getDiscreteDataList().get(0).getEndValue());
		System.out.println("The discrete value="+discreteObj.getDiscreteDataList().get(0).getDiscreteValue());
        assertEquals(discreteObj.getDiscreteDataList().size(), 1);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getStartValue(), 0.1f);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getEndValue(), 0.5f);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getDiscreteValue(), 0.444f);
		testCaseNumber++;
		System.out.println("==========================================");	
	}
	
	/**
	 * Test Discrete class when only a single value1-value2=value3 pair is given, but with value1 missing
	 */
	
	@Test
	public void testSingleString3(){
		Discrete discreteObj = new Discrete("~.5=0.444");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test Discrete class when only a single value1-value2=value3; pair is given, but with value1 missing\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		System.out.println("The size of the list"+discreteObj.getDiscreteDataList().size());
		System.out.println("The starting value="+discreteObj.getDiscreteDataList().get(0).getStartValue());
		System.out.println("The ending value="+discreteObj.getDiscreteDataList().get(0).getEndValue());
		System.out.println("The discrete value="+discreteObj.getDiscreteDataList().get(0).getDiscreteValue());
        assertEquals(discreteObj.getDiscreteDataList().size(), 1);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getStartValue(), -Float.MAX_VALUE);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getEndValue(), 0.5f);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getDiscreteValue(), 0.444f);
		testCaseNumber++;
		System.out.println("==========================================");	
	}
	
	
	/**
	 * Test Discrete class when only a single value1-value2=value3 pair is given, but with value2 missing
	 */
	
	@Test
	public void testSingleString4(){
		Discrete discreteObj = new Discrete(".5~=0.444");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test Discrete class when only a single value1-value2=value3; pair is given, but with value1 missing\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		System.out.println("The size of the list"+discreteObj.getDiscreteDataList().size());
		System.out.println("The starting value="+discreteObj.getDiscreteDataList().get(0).getStartValue());
		System.out.println("The ending value="+discreteObj.getDiscreteDataList().get(0).getEndValue());
		System.out.println("The discrete value="+discreteObj.getDiscreteDataList().get(0).getDiscreteValue());
        assertEquals(discreteObj.getDiscreteDataList().size(), 1);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getStartValue(), 0.5f);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getEndValue(), Float.MAX_VALUE);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getDiscreteValue(), 0.444f);
		testCaseNumber++;
		System.out.println("==========================================");	
	}	

	/**
	 * Test Discrete class when only a single value1-value2=value3 pair is given, but with 
	 * both value1 and value2 missing
	 */
	
	@Test
	public void testSingleString5(){
		Discrete discreteObj = new Discrete("  ~  =0.444");

		System.out.println("Test-case: "+testCaseNumber);
		System.out.println("Test Discrete class when only a single value1-value2=value3; pair is given, but " +
				"with both value1 and value 2 missing\n");
		System.out.println("The input string is "+discreteObj.getStrDiscrete());
		System.out.println("The size of the list"+discreteObj.getDiscreteDataList().size());
		System.out.println("The starting value="+discreteObj.getDiscreteDataList().get(0).getStartValue());
		System.out.println("The ending value="+discreteObj.getDiscreteDataList().get(0).getEndValue());
		System.out.println("The discrete value="+discreteObj.getDiscreteDataList().get(0).getDiscreteValue());
        assertEquals(discreteObj.getDiscreteDataList().size(), 1);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getStartValue(), -Float.MAX_VALUE);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getEndValue(), Float.MAX_VALUE);
        assertFloatComparison(discreteObj.getDiscreteDataList().get(0)
                .getDiscreteValue(), 0.444f);
		testCaseNumber++;
		System.out.println("==========================================");	
	}
	
	
	
}


















