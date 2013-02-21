package gov.noaa.nws.ncep.gempak.parameters.gridlimits;

import org.junit.Assert;
import org.junit.Test;

public class GGLIMSTest {
    private static int testCaseNumber;

    /**
     * Test for valid GGLIMS string with all parameters
     */
	@Test
	public void testGGLIMSString() {
		testCaseNumber = 1;
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1|0.9;1|0.5");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(),0.9);
        assertEquals(gglimitsObj.getMaxValue(), 1);
        assertEquals(gglimitsObj.getDefaultGridValue(),0.5);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for valid GGLIMS string with all parameters");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}

    /**
     * @param lowerLimit
     * @param d
     */
    private static void assertEquals(Float lowerLimit, double d) {
        Assert.assertEquals(d, lowerLimit.doubleValue(), 0.01);
    }

    /**
     * Test valid GGLIMS string of the form
     * <tt>lowerLimit;minValue|upperLimit;maxValue</tt>
     */
	@Test
	public void testGGLIMSStringWithoutDefaultValue() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1|0.9;1");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(),0.9);
        assertEquals(gglimitsObj.getMaxValue(), 1);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;minValue|upperLimit;maxValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string of the form <tt>lowerLimit;minValue|upperLimit;maxValue|</tt>
     */
	@Test
	public void testGGLIMSStringWithoutDefaultValue2() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1|0.9;1|");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(),0.9);
        assertEquals(gglimitsObj.getMaxValue(), 1);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;minValue|upperLimit;maxValue|");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string of the form <tt>lowerLimit;minValue|upperLimit;|defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithoutMaxValue() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1|0.9;|2");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(), Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),2.0);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;minValue|upperLimit;|defaultValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string of the form <tt>lowerLimit;minValue|upperLimit|defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithoutMaxValue2() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1|0.9|2");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(), Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),2.0);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;minValue|upperLimit|defaultValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string of the form <tt>lowerLimit;minValue|  ; maxValue|defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithoutUpperLimit() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1|   ; 0.9|1");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),1);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;minValue|  ;maxValue|defaultValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
    /**
     * Test for valid GGLIMS string of the form <tt>  lowerLimit|upperLimit;maxValue|defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithoutMinValue() {

		GGLIMS gglimitsObj= new GGLIMS("0.9|2.8;3|0.6");
		assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(),2.8);
        assertEquals(gglimitsObj.getMaxValue(),3);
        assertEquals(gglimitsObj.getDefaultGridValue(),0.6);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for valid GGLIMS string of the form lowerLimit|upperLimit;maxValue|defaultValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	 /**
     * Test for valid GGLIMS string of the form <tt>  ;minValue|upperLimit;maxValue|defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithoutLowerLimit() {

		GGLIMS gglimitsObj= new GGLIMS("   ;0.9|1.8;1.855|0.6");
		assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(),1.855);
        assertEquals(gglimitsObj.getMaxValue(),1.7);
        assertEquals(gglimitsObj.getDefaultGridValue(),0.6);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for valid GGLIMS string of the form ;minValue|upperLimit;maxValue|defaultValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	

    /**
     * Test valid GGLIMS string of the form <tt>lowerLimit;minValue|  ;  |defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithoutUpperLimitAndMaxValue() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1|   ;   |1");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),1);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;minValue|   ;  |defautValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}

    /**
     * Test valid GGLIMS string of the form <tt>lowerLimit;minValue||defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithoutUpperLimitAndMaxValue2() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1||-.6666");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(), Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),-0.6666);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;minValue||defautValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
   /**
     * Test valid GGLIMS string of the form <tt>|upperLimit;maxValue|</tt>
     */
	@Test
	public void testGGLIMSStringWithUpperLimitAndMaxValueOnly() {
		
		GGLIMS gglimitsObj= new GGLIMS("|0.3;0.4|");
        assertEquals(gglimitsObj.getLowerLimit(), -Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMinValue(),-Float.MAX_VALUE);
		assertEquals(gglimitsObj.getUpperLimit(),0.3);
        assertEquals(gglimitsObj.getMaxValue(),  0.4);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form |upperLimit;maxValue|");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}	
	
	   /**
     * Test valid GGLIMS string of the form <tt>  ;   |upperLimit;maxValue|   </tt>
     */
	@Test
	public void testGGLIMSStringWithUpperLimitAndMaxValueOnly2() {
		
		GGLIMS gglimitsObj= new GGLIMS("   ;    |0.3;0.4|  ");
        assertEquals(gglimitsObj.getLowerLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),Float.NaN);
		assertEquals(gglimitsObj.getUpperLimit(),0.3);
        assertEquals(gglimitsObj.getMaxValue(),  0.4);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form  ;   |upperLimit;maxValue|   ");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}	
	
    /**
     * Test valid GGLIMS string of the form <tt>lowerLimit;minValue||</tt>
     */
	@Test
	public void testGGLIMSStringWithLowerLimitAndMinValueOnly() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1||");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(), Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;minValue||");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string of the form <tt>lowerLimit;minValue|</tt>
     */
	@Test
	public void testGGLIMSStringWithLowerLimitAndMinValueOnly2() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1|  ");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(), Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;minValue|");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string of the form <tt>lowerLimit;minValue </tt>
     */
	@Test
	public void testGGLIMSStringWithLowerLimitAndMinValueOnly3() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1 ");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(), Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;minValue ");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}

    /**
     * Test valid GGLIMS string of the form <tt>lowerLimit;  ||</tt>
     */
	@Test
	public void testGGLIMSStringWithLowerLimitOnly() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2; ||");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(), Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;  ||");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string of the form <tt>lowerLimit;</tt>
     */
	@Test
	public void testGGLIMSStringWithLowerLimitOnly2() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit;");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}	

    /**
     * Test valid GGLIMS string of the form <tt><b>lowerLimit</b></tt>
     */
	@Test
	public void testGGLIMSStringWithLowerLimitOnly3() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string of the form <tt><b>lowerLimit|</b></tt>
     */
	@Test
	public void testGGLIMSStringWithLowerLimitOnly4() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2|");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(), Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form lowerLimit|");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
     * Test valid GGLIMS string of the form <tt> ;minValue||</tt>
     */
	@Test
	public void testGGLIMSStringWithMinValueOnly() {
		
		GGLIMS gglimitsObj= new GGLIMS("  ;0.1||");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(), Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form  ;minValue||");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
     * Test valid GGLIMS string of the form <tt> ;minValue|</tt>
     */
	@Test
	public void testGGLIMSStringWithMinValueOnly2() {
		
		GGLIMS gglimitsObj= new GGLIMS("  ;0.1|");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(), Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form  ;minValue|");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	
	/**
     * Test valid GGLIMS string of the form <tt>  ;minValue  </tt>
     */
	@Test
	public void testGGLIMSStringWithMinValueOnly3() {
		
		GGLIMS gglimitsObj= new GGLIMS("  ;0.1  ");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form   ;minValue  ");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string of the form <tt> | upperLimit;  |</tt>
     */
	@Test
	public void testGGLIMSStringWithUpperLimitOnly() {
		
		GGLIMS gglimitsObj= new GGLIMS("| 0.75; |");
        assertEquals(gglimitsObj.getLowerLimit(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMinValue(),  -Float.MAX_VALUE);
        assertEquals(gglimitsObj.getUpperLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form | upperLimit;  |");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string of the form <tt> |upperLimit|</tt>
     */
	@Test
	public void testGGLIMSStringWithUpperLimitOnly2() {
		
		GGLIMS gglimitsObj= new GGLIMS("|0.75|");
        assertEquals(gglimitsObj.getLowerLimit(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMinValue(),  -Float.MAX_VALUE);
        assertEquals(gglimitsObj.getUpperLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form |upperLimit|");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string of the form <tt   ;   |upperLimit|</tt>
     */
	@Test
	public void testGGLIMSStringWithUpperLimitOnly3() {
		
		GGLIMS gglimitsObj= new GGLIMS("	;	|0.75|");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form 	;	|upperLimit|");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
     * Test valid GGLIMS string of the form <tt> |;maxValue|</tt>
     */
	@Test
	public void testGGLIMSStringWithMaxValueOnly() {
		
		GGLIMS gglimitsObj= new GGLIMS("  |;0.4545|");
        assertEquals(gglimitsObj.getLowerLimit(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMinValue(),  -Float.MAX_VALUE);
        assertEquals(gglimitsObj.getUpperLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form  |;maxValue|");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
     * Test valid GGLIMS string of the form <tt>  ; |;maxValue|</tt>
     */
	@Test
	public void testGGLIMSStringWithMaxValueOnly2() {
		
		GGLIMS gglimitsObj= new GGLIMS(" ;  |;0.4545|");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(), Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string of the form   ;  |;maxValue|");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
     * Test for valid GGLIMS string of the form <tt>  |   |defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithDefaultOnly() {
		
		GGLIMS gglimitsObj= new GGLIMS("    |     |0.5");
        assertEquals(gglimitsObj.getLowerLimit(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMinValue(),  -Float.MAX_VALUE);
        assertEquals(gglimitsObj.getUpperLimit(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),  Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),0.5);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for valid GGLIMS string of the form  |   |defaultValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test for valid GGLIMS string of the form <tt>  lowerLimit|   |defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithLowerLimitAndDefaultOnly() {

		GGLIMS gglimitsObj= new GGLIMS("-0.0009|     |0.5");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),  Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),0.5);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for valid GGLIMS string of the form  lowerLimit|   |defaultValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test for valid GGLIMS string of the form <tt>  ;minValue|   |defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithMinValueAndDefaultOnly() {

		GGLIMS gglimitsObj= new GGLIMS(";-0.0009|     |0.5");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(), Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),  Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),0.5);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for valid GGLIMS string of the form  ;minValue|   |defaultValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test for valid GGLIMS string of the form <tt>  |upperLimit|defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithUpperLimitAndDefaultOnly() {

		GGLIMS gglimitsObj= new GGLIMS("|-0.0009|0.5");
		assertEquals(gglimitsObj.getLowerLimit(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMinValue(),  -Float.MAX_VALUE);
        assertEquals(gglimitsObj.getUpperLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),  Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),0.5);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for valid GGLIMS string of the form   |upperLimit|defaultValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}

    /**
     * Test for valid GGLIMS string of the form <tt>  |  ;maxValue|defaultValue</tt>
     */
	@Test
	public void testGGLIMSStringWithMaxValueAndDefaultOnly() {

		GGLIMS gglimitsObj= new GGLIMS("|  ;-0.0009|0.5");
		assertEquals(gglimitsObj.getLowerLimit(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMinValue(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getUpperLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),0.5);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for valid GGLIMS string of the form   |   ; maxValue|defaultValue");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string with lower and upper limits exchanged
     */
	@Test
	public void testGGLIMSStringWithLowerAndUpperLimitsExchanged() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.9;0.1|0.2;1|0.7878");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),  Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string with lower and upper limits exchanged");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test valid GGLIMS string with lower and upper limits set to the same value
     */
	@Test
	public void testGGLIMSStringWithEqualLowerAndUpperLimits() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.9;.1|.9;1.1|3.4");
        assertEquals(gglimitsObj.getLowerLimit(),0.9);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(),0.9);
        assertEquals(gglimitsObj.getMaxValue(),  1.1);
        assertEquals(gglimitsObj.getDefaultGridValue(),3.4);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test valid GGLIMS string with lower and upper limits set to the same value");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
    /**
     * Test for valid GGLIMS string with extra arguments
     */
	@Test
	public void testGGLIMSStringWithExtraArguments() {
		
		GGLIMS gglimitsObj= new GGLIMS("0.2;0.1;0.8880.9999|0.9;1;345|0.5;14.369");
        assertEquals(gglimitsObj.getLowerLimit(),0.2);
        assertEquals(gglimitsObj.getMinValue(),  0.1);
        assertEquals(gglimitsObj.getUpperLimit(),0.9);
        assertEquals(gglimitsObj.getMaxValue(), 1);
        assertEquals(gglimitsObj.getDefaultGridValue(),Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test for GGLIMS string with extra arguments");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
	 * Test GGLIMS with all arguments missing
	 */
	@Test
	public void testGGLIMSWithAllArgumentsMissing() {
        GGLIMS gglimitsObj= new GGLIMS("||");
        assertEquals(gglimitsObj.getLowerLimit(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMinValue(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getUpperLimit(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),  Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("TestGGLIMS with all arguments missing");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
	 * Test GGLIMS with invalid delimiters - substituting '|' with '/'
	 */
	@Test
	public void testGGLIMSWithInvalidDelimiters1() {
        GGLIMS gglimitsObj= new GGLIMS("1;2/3;4/5");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),  Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),  Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test GGLIMS for invalid delimiter - when '|' is replaced with '/'");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
		
	/**
	 * Test GGLIMS with invalid delimiters - substituting ';' with ','
	 */
	@Test
	public void testGGLIMSWithInvalidDelimiters2() {
        GGLIMS gglimitsObj= new GGLIMS("1,2|3,4|5");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),  Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),  5);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test GGLIMS for invalid delimiter - when ';' is replaced with ','");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
	 * Test GGLIMS with non-numeric inputs
	 */
	@Test
	public void testGGLIMSNonNumericInputs() {
        GGLIMS gglimitsObj= new GGLIMS("0.2341l;-.02O;S5|abcd|efg");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),  Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),  Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test GGLIMS with non-numeric inputs");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
	 * Test default constructor in GGLIMS
	 */
	@Test
	public void testGGLIMS() {
        GGLIMS gglimitsObj= new GGLIMS();
        assertEquals(gglimitsObj.getLowerLimit(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMinValue(),  -Float.MAX_VALUE);
        assertEquals(gglimitsObj.getUpperLimit(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),  Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),  Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test the default constructor in GGLIMS");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
	 * Test GGLIMS with blanks only
	 */
	@Test
	public void testGGLIMSWithBlanksOnly() {
        GGLIMS gglimitsObj= new GGLIMS("              ");
        assertEquals(gglimitsObj.getLowerLimit(),Float.NaN);
        assertEquals(gglimitsObj.getMinValue(),  Float.NaN);
        assertEquals(gglimitsObj.getUpperLimit(), Float.NaN);
        assertEquals(gglimitsObj.getMaxValue(),Float.NaN);
        assertEquals(gglimitsObj.getDefaultGridValue(),  Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test GGLIMS with blanks only");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
	 * Test GGLIMS with empty string
	 */
	@Test
	public void testGGLIMSWithEmptyString() {
        GGLIMS gglimitsObj= new GGLIMS("");
        assertEquals(gglimitsObj.getLowerLimit(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMinValue(),  -Float.MAX_VALUE);
        assertEquals(gglimitsObj.getUpperLimit(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),  Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),  Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test GGLIMS with empty string");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}
	
	/**
	 * Test GGLIMS with null string
	 */
	@Test
	public void testGGLIMSWithNullString() {
        GGLIMS gglimitsObj= new GGLIMS(null);
        assertEquals(gglimitsObj.getLowerLimit(),-Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMinValue(),  -Float.MAX_VALUE);
        assertEquals(gglimitsObj.getUpperLimit(),Float.MAX_VALUE);
        assertEquals(gglimitsObj.getMaxValue(),  Float.MAX_VALUE);
        assertEquals(gglimitsObj.getDefaultGridValue(),  Float.NaN);
        System.out.println("=====================Test-Case "+testCaseNumber+" ========================");
		System.out.println("Test GGLIMS with null string");
		System.out.println("The input string       = " + gglimitsObj.getGgLimitsStr());
		System.out.println("The lower grid limit   = " + gglimitsObj.getLowerLimit());
		System.out.println("The minimum grid value = " + gglimitsObj.getMinValue());
		System.out.println("The upper grid limit   = " + gglimitsObj.getUpperLimit());
		System.out.println("The maximum grid value = " + gglimitsObj.getMaxValue());		
		System.out.println("The default value      = " + gglimitsObj.getDefaultGridValue());
		
        testCaseNumber++;
	}	
	
}



















