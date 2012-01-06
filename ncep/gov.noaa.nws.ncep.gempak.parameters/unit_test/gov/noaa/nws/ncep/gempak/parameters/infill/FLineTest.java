package gov.noaa.nws.ncep.gempak.parameters.infill;

import gov.noaa.nws.ncep.gempak.parameters.infill.FLine;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 04-Nov-2009    185        Archana.S   Initial Creation 
 * </pre>
 * @author Archana.S
 * @version 1
 */
public class FLineTest {
	private static Integer testCaseNumber=1;
	
	private List<Integer>testColorList;
	private List<Integer>testPatternList;
	private List<Integer>testColorList2;
	private List<Integer>testPatternList2;
	private List<Integer>testColorList3;
	private List<Integer>testPatternList3;    
    
	@Test
	public void testValidListOfColorsWithRepeatingPatterns(){
    	
    	System.out.println("------------------Test-case "+ testCaseNumber +"a----------------");
    	FLine fline = new FLine("1;2;3;4/4;5");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"a? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());
    	System.out.println("The list of patterns: "+fline.getFillTypeList());

	    assertEquals(fline.isFLineStringParsed(),true);
	    testColorList    = new ArrayList<Integer>(Arrays.asList(1,2,3,4));
	    testPatternList  = new ArrayList<Integer>(Arrays.asList(4,5,4,5));
	    assertEquals(fline.getFillColorList(), testColorList);
	    assertEquals(fline.getFillTypeList(), testPatternList);
    	
    	System.out.println("------------------Test-case "+ testCaseNumber +"b----------------");
    	FLine fline2 = new FLine("6;5;4;3;2;1/1;2;3;4;5;6");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"b? "+fline2.isFLineStringParsed());
    	System.out.println("The input string : "           +fline2.getFLineString());
    	System.out.println("The list of colors: "+fline2.getFillColorList());
    	System.out.println("The list of patterns: "+fline2.getFillTypeList());
    	    
	    assertEquals(fline2.isFLineStringParsed(),true);
	    testColorList2    = new ArrayList<Integer>(Arrays.asList(6,5,4,3,2,1));
	    testPatternList2  = new ArrayList<Integer>(Arrays.asList(1,2,3,4,5,6));
	    assertEquals(fline2.getFillColorList(), testColorList2);
	    assertEquals(fline2.getFillTypeList(), testPatternList2);
	    
	    System.out.println("--------------------------------------------------------");
	    testCaseNumber++;
	}

    @Test
	public void testValidListOfColors(){
    	
    	System.out.println("------------------Test-case "+ testCaseNumber +"a----------------");
    	FLine fline = new FLine("1;2;3;4");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"a? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());
    	System.out.println("The list of patterns: "+fline.getFillTypeList());
    	
    	assertEquals(fline.isFLineStringParsed(),true);
    	
	    testColorList    = new ArrayList<Integer>(Arrays.asList(1,2,3,4));
	    assertEquals(fline.getFillColorList(), testColorList);
	    
	    testPatternList  = new ArrayList<Integer>(Arrays.asList(1,1,1,1));
	    assertEquals(fline.getFillTypeList(), testPatternList);
    	
    	System.out.println("------------------Test-case "+ testCaseNumber +"b----------------");
    	FLine fline2 = new FLine("6;5;4;3;2;1/");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"b? "+fline2.isFLineStringParsed());
    	System.out.println("The input string : "           +fline2.getFLineString());
    	System.out.println("The list of colors: "+fline2.getFillColorList());
    	System.out.println("The list of patterns: "+fline2.getFillTypeList());
	    System.out.println("--------------------------------------------------------");
	    
	    assertEquals(fline2.isFLineStringParsed(),true);
	    testColorList2    = new ArrayList<Integer>(Arrays.asList(6,5,4,3,2,1));
	    assertEquals(fline2.getFillColorList(), testColorList2);
	    testPatternList2  = new ArrayList<Integer>(Arrays.asList(1,1,1,1,1,1));
	    assertEquals(fline2.getFillTypeList(), testPatternList2);
	    
	    testCaseNumber++;
	}
    @Test
	public void testListOfColorsWithSomeMissingNumbers(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
    	FLine fline = new FLine(";5;;15;;25");
   	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());
    	System.out.println("The list of patterns: "+fline.getFillTypeList());
	    System.out.println("--------------------------------------------------------");
    	assertEquals(fline.isFLineStringParsed(),true);
    	
	    testColorList    = new ArrayList<Integer>(Arrays.asList(0,5,0,15,0,25));
	    assertEquals(fline.getFillColorList(), testColorList);
	    
	    testPatternList  = new ArrayList<Integer>(Arrays.asList(1,1,1,1,1,1));
	    assertEquals(fline.getFillTypeList(), testPatternList);
	    
	    testCaseNumber++;
	}
    
    @Test
	public void testListOfColorsWithBlanksAndMissingNumbers(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"----------------");

    	FLine fline = new FLine("5;  4   ;6 ;; ; 8");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());
    	System.out.println("The list of patterns: "+fline.getFillTypeList());
	    System.out.println("--------------------------------------------------------");
    	assertEquals(fline.isFLineStringParsed(),true);
    	
	    testColorList    = new ArrayList<Integer>(Arrays.asList(5, 4, 6, 0, 0, 8));
	    assertEquals(fline.getFillColorList(), testColorList);
	    
	    testPatternList  = new ArrayList<Integer>(Arrays.asList(1,1,1,1,1,1));
	    assertEquals(fline.getFillTypeList(), testPatternList);
	    
	    testCaseNumber++;
	}
    
    
    @Test
    public void testLisOfColorsWithAllNumbersMissing(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
    	FLine fline2 = new FLine(";;;;;");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+fline2.isFLineStringParsed());
    	System.out.println("The input string : "           +fline2.getFLineString());
    	System.out.println("The list of colors: "+fline2.getFillColorList());
    	System.out.println("--------------------------------------------------------");
    	assertEquals(fline2.isFLineStringParsed(),false);
    	
    	testCaseNumber++;
    }
    
    @Test
    public void testValidSingleColorInputs(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"a----------------");
    	FLine fline = new FLine("10;");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"a? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());
    	System.out.println("The list of patterns: "+fline.getFillTypeList());  	
    	System.out.println("------------------Test-case "+ testCaseNumber +"b----------------");
    	FLine fline2 = new FLine("11/");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"b? "+fline2.isFLineStringParsed());
    	System.out.println("The input string : "           +fline2.getFLineString());
    	System.out.println("The list of colors: "+fline2.getFillColorList());
    	System.out.println("The list of patterns: "+fline2.getFillTypeList());
    	System.out.println("------------------Test-case "+ testCaseNumber +"c----------------");
    	FLine fline3 = new FLine("13");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"c? "+fline3.isFLineStringParsed());
    	System.out.println("The input string : "           +fline3.getFLineString());
    	System.out.println("The list of colors: "+fline3.getFillColorList());
    	System.out.println("The list of patterns: "+fline3.getFillTypeList());
	    System.out.println("--------------------------------------------------------");
	    
    	assertEquals(fline.isFLineStringParsed(),true);
    	
	    testColorList    = new ArrayList<Integer>(Arrays.asList(10));
	    assertEquals(fline.getFillColorList(), testColorList);
	    
	    testPatternList  = new ArrayList<Integer>(Arrays.asList(1));
	    assertEquals(fline.getFillTypeList(), testPatternList);
	    
	    assertEquals(fline2.isFLineStringParsed(),true);
	    testColorList2    = new ArrayList<Integer>(Arrays.asList(11));
	    assertEquals(fline2.getFillColorList(), testColorList2);
	    
	    testPatternList2  = new ArrayList<Integer>(Arrays.asList(1));
	    assertEquals(fline2.getFillTypeList(), testPatternList2);
	    	    
	    
	    assertEquals(fline3.isFLineStringParsed(),true);
    	    	
	    testColorList3    = new ArrayList<Integer>(Arrays.asList(13));
	    assertEquals(fline3.getFillColorList(), testColorList3);
	    
	    testPatternList3  = new ArrayList<Integer>(Arrays.asList(1));
	    assertEquals(fline3.getFillTypeList(), testPatternList3);
	    
	    testCaseNumber++;
    }
    
    @Test
    public void testValidColorInputRange(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
    	FLine fline = new FLine("30-45");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());
    	System.out.println("The list of patterns: "+fline.getFillTypeList());
	    System.out.println("--------------------------------------------------------");
    	assertEquals(fline.isFLineStringParsed(),true);
    	
	    testColorList    = new ArrayList<Integer>(Arrays.asList(30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45));
	    assertEquals(fline.getFillColorList(), testColorList);
	    
	    testPatternList  = new ArrayList<Integer>(Arrays.asList(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1));
	    assertEquals(fline.getFillTypeList(), testPatternList);

	    testCaseNumber++;    	
    }

    @Test
    public void testColorInputRangeWithMinMaxValuesAltered(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
    	FLine fline = new FLine("30-4");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());
    	System.out.println("The list of patterns: "+fline.getFillTypeList());
    	System.out.println("--------------------------------------------------------");
	    assertEquals(fline.isFLineStringParsed(),true);
	    
	    testColorList    = new ArrayList<Integer>(Arrays.asList(30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4));
	    assertEquals(fline.getFillColorList(), testColorList);
	    
	    testPatternList  = new ArrayList<Integer>(Arrays.asList(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1));
	    assertEquals(fline.getFillTypeList(), testPatternList);

	    testCaseNumber++;    	
    }   

    @Test
    public void testColorInputRangeWithMissingMinValue(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
    	FLine fline = new FLine("-20");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());
    	System.out.println("The list of patterns: "+fline.getFillTypeList());
    	System.out.println("--------------------------------------------------------");
	    
	    assertEquals(fline.isFLineStringParsed(),true);
	    
	    testColorList    = new ArrayList<Integer>(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20));
	    assertEquals(fline.getFillColorList(), testColorList);
	    
	    testPatternList  = new ArrayList<Integer>(Arrays.asList(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1));
	    assertEquals(fline.getFillTypeList(), testPatternList);

	    testCaseNumber++;    	
    }    
        
    
    @Test
    public void testColorInputRangeWithMissingMaxValue(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
    	FLine fline = new FLine("10-");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());
    	System.out.println("The list of patterns: "+fline.getFillTypeList());
	    System.out.println("--------------------------------------------------------");
	    
	    assertEquals(fline.isFLineStringParsed(),true); 
	    testColorList    = new ArrayList<Integer>(Arrays.asList(10, 9, 8, 7, 6, 5, 4, 3, 2, 1));
	    assertEquals(fline.getFillColorList(), testColorList);
	    
	    testPatternList  = new ArrayList<Integer>(Arrays.asList(1, 1, 1, 1, 1, 1, 1, 1, 1, 1));
	    assertEquals(fline.getFillTypeList(), testPatternList);

	    testCaseNumber++;    	
    }
    
    @Test
    public void testColorInputRangeWithExcessParameters(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
    	FLine fline = new FLine("30-45-70-80");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());
    	System.out.println("The list of patterns: "+fline.getFillTypeList());
    	System.out.println("--------------------------------------------------------");
	    
	    assertEquals(fline.isFLineStringParsed(),true);
	    testColorList    = new ArrayList<Integer>(Arrays.asList(30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45));
	    assertEquals(fline.getFillColorList(), testColorList);
	    
	    testPatternList  = new ArrayList<Integer>(Arrays.asList(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1));
	    assertEquals(fline.getFillTypeList(), testPatternList);
    
	    testCaseNumber++;    	
    }
    
    @Test
    public void testFLineWithEmptyStringInput(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"a----------------");
    	FLine fline = new FLine("");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"a? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());

    	System.out.println("------------------Test-case "+ testCaseNumber +"b----------------");
    	FLine fline2 = new FLine();
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"b? "+fline2.isFLineStringParsed());
    	System.out.println("The input string : "           +fline2.getFLineString());
    	System.out.println("The list of colors: "+fline2.getFillColorList());
	    
    	System.out.println("------------------Test-case "+ testCaseNumber +"c----------------");
    	FLine fline3 = new FLine("           ");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"c? "+fline3.isFLineStringParsed());
    	System.out.println("The input string : "           +fline3.getFLineString());
    	System.out.println("The list of colors: "+fline3.getFillColorList());
    	
    	System.out.println("------------------Test-case "+ testCaseNumber +"d----------------");
    	FLine fline4 = new FLine(null);
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"d? "+fline4.isFLineStringParsed());
    	System.out.println("The input string : "           +fline4.getFLineString());
    	System.out.println("The list of colors: "+fline4.getFillColorList());
    	
    	
    	
    	System.out.println("--------------------------------------------------------");

    	
    	assertEquals(fline.isFLineStringParsed(),false);
    	assertEquals(fline2.isFLineStringParsed(),false);
    	assertEquals(fline3.isFLineStringParsed(),false);
    	assertEquals(fline4.isFLineStringParsed(),false);
    	
	    testCaseNumber++;
    	
    }
    
    @Test
    public void testFLineWithInvalidDelimiters(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"a----------------");
    	FLine fline = new FLine("10,20,30,");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"a? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());

    	assertEquals(fline.isFLineStringParsed(),false);
    	
    	
    	System.out.println("------------------Test-case "+ testCaseNumber +"b----------------");
    	FLine fline2 = new FLine("40;50//60");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"b? "+fline2.isFLineStringParsed());
    	System.out.println("The input string : "           +fline2.getFLineString());
    	System.out.println("The list of colors: "+fline2.getFillColorList());
    	
    	assertEquals(fline2.isFLineStringParsed(),true);
	    
	    testColorList    = new ArrayList<Integer>(Arrays.asList(40, 50));
	    assertEquals(fline2.getFillColorList(), testColorList);
	    
	    testPatternList  = new ArrayList<Integer>(Arrays.asList(1,1));
	    assertEquals(fline2.getFillTypeList(), testPatternList);    	
    	
    	System.out.println("------------------Test-case "+ testCaseNumber +"c----------------");
    	FLine fline3 = new FLine("10=40");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"c? "+fline3.isFLineStringParsed());
    	System.out.println("The input string : "           +fline3.getFLineString());
    	System.out.println("The list of colors: "+fline3.getFillColorList());
    	assertEquals(fline3.isFLineStringParsed(),false);
    	
    	
    	System.out.println("--------------------------------------------------------");

	    testCaseNumber++;
    	
    }
   
    @Test
    public void testFLineWithNonNumericInputs(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"a----------------");
    	FLine fline = new FLine("10a;gp##$%;*jsao/");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"a? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());

    	System.out.println("------------------Test-case "+ testCaseNumber +"b----------------");
    	FLine fline2 = new FLine("10a;bcd");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"b? "+fline2.isFLineStringParsed());
    	System.out.println("The input string : "           +fline2.getFLineString());
    	System.out.println("The list of colors: "+fline2.getFillColorList());
	    
    	System.out.println("------------------Test-case "+ testCaseNumber +"c----------------");
    	FLine fline3 = new FLine("abc-def");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"c? "+fline3.isFLineStringParsed());
    	System.out.println("The input string : "           +fline3.getFLineString());
    	System.out.println("The list of colors: "+fline3.getFillColorList());
    	
    	System.out.println("--------------------------------------------------------");

    	assertEquals(fline.isFLineStringParsed(),false);
    	assertEquals(fline2.isFLineStringParsed(),false);
    	assertEquals(fline3.isFLineStringParsed(),false);
    	
    	
    	
	    testCaseNumber++;
    	
    }    
    
    @Test
    public void testColorRange(){
    	System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
    	FLine fline = new FLine("0;30-20");
    	
    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+fline.isFLineStringParsed());
    	System.out.println("The input string : "           +fline.getFLineString());
    	System.out.println("The list of colors: "+fline.getFillColorList());
    	System.out.println("The list of patterns: "+fline.getFillTypeList());
    	System.out.println("--------------------------------------------------------");
	    
	    assertEquals(fline.isFLineStringParsed(),true);
	    testColorList    = new ArrayList<Integer>(Arrays.asList(0, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20));
	    assertEquals(fline.getFillColorList(), testColorList);
	    
	  
    
	    testCaseNumber++;    	
    }
    
}





































