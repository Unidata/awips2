package gov.noaa.nws.ncep.gempak.parameters.line;

import static org.junit.Assert.assertEquals;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;

import org.junit.Test;

/**
 * *
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 30-Oct-2009    186       Archana.S   Initial Creation 
 * 25-Aug-2012    743       djohnson    Upgrade to JUnit 4.10.
 * </pre>
 * 
 * @author Archana.S
 * @version 1
 */


public class LineDataStringParserTest {
private static Integer testCaseNumber=1;

	@Test
	public void testValidLineDataStringWithAllInputsAndBlanksWithinInputString(){
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		LineDataStringParser ldsp1 = new LineDataStringParser("2 ; 6 ; 12 /3  ; 4/5;6/1/2/0.5/T");
		System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());

		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.5);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 2);
	    	System.out.println("The input string : "           +ldsp1.getLineDataString());
	    	System.out.println("List of line colors:  "        +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
	    	int firstColor = ldsp1.getInstanceOfLineBuilder().getLineColorsList().get(0);
	    	System.out.println("First color index:    "+        firstColor+"    RGB="+GempakColor.convertToRGB(firstColor));
	    	System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
	    	System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
	    	System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
	    	System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
	    	System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
	    	System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
	    }
	    testCaseNumber++;
	
	}
	
	@Test
	public void testValidLineDataStringWithOnlyLineColorInput(){
		
		System.out.println("------------------Test-case "+ testCaseNumber +"a----------------");
		
		LineDataStringParser ldsp1 = new LineDataStringParser("7;");	
		

		if(ldsp1.isLineDataParsed()){
	    	assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
	    	System.out.println("The input string : "           +ldsp1.getLineDataString());
	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"a? "+ldsp1.isLineDataParsed());
	    	System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
	    	System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
	    	System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
	    	System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
	    	System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
	    	System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
	    	System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
		    System.out.println("--------------------------------------------------------");
	    }
	    
		System.out.println("------------------Test-case "+ testCaseNumber +"b----------------");
		LineDataStringParser ldsp2 = new LineDataStringParser("6/");

	    if(ldsp2.isLineDataParsed()){
	    	assertEquals(ldsp2.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp2.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp2.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
	    	System.out.println("The input string : "           +ldsp2.getLineDataString());
	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"b? "+ldsp1.isLineDataParsed());
	    	System.out.println("List of line colors: "         +ldsp2.getInstanceOfLineBuilder().getLineColorsList());
	    	System.out.println("List of line patterns: "       +ldsp2.getInstanceOfLineBuilder().getLineStyleList());
	    	System.out.println("List of line widths: "         +ldsp2.getInstanceOfLineBuilder().getLineWidthList());
	    	System.out.println("List of lines to be labelled: "+ldsp2.getInstanceOfLineBuilder().getLineLabelPresentList());
	    	System.out.println("Smoothing Level: "             +ldsp2.getInstanceOfLineBuilder().getLineSmoothingLevel());
	    	System.out.println("Points filter data: "          +ldsp2.getInstanceOfLineBuilder().getPointsFilter());
	    	System.out.println("Small Contour Flag: "          +ldsp2.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
	    	System.out.println("--------------------------------------------------------");
	    }		
		
	    System.out.println("------------------Test-case "+ testCaseNumber +"c----------------");
		LineDataStringParser ldsp3 = new LineDataStringParser("5");
	    if(ldsp3.isLineDataParsed()){
	    	assertEquals(ldsp3.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp3.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp3.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
	    	System.out.println("The input string : "           +ldsp3.getLineDataString());
	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"c? "+ldsp1.isLineDataParsed());
	    	System.out.println("List of line colors: "         +ldsp3.getInstanceOfLineBuilder().getLineColorsList());
	    	System.out.println("List of line patterns: "       +ldsp3.getInstanceOfLineBuilder().getLineStyleList());
	    	System.out.println("List of line widths: "         +ldsp3.getInstanceOfLineBuilder().getLineWidthList());
	    	System.out.println("List of lines to be labelled: "+ldsp3.getInstanceOfLineBuilder().getLineLabelPresentList());
	    	System.out.println("Smoothing Level: "             +ldsp3.getInstanceOfLineBuilder().getLineSmoothingLevel());
	    	System.out.println("Points filter data: "          +ldsp3.getInstanceOfLineBuilder().getPointsFilter());
	    	System.out.println("Small Contour Flag: "          +ldsp3.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
	    	System.out.println("--------------------------------------------------------");
	    }
	    
	    testCaseNumber++;
	}
	
	@Test
	public void testValidLineDataStringWithMultipleColorsAndFilterDataOnly(){
		    System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
   		    LineDataStringParser ldsp1 = new LineDataStringParser("3;2;16/0.67654");
   		    
   		    if(ldsp1.isLineDataParsed()){
    	    	assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.67654);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
    	    	System.out.println("The input string : "           +ldsp1.getLineDataString());
    	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
    	    	System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
    	    	System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
    	    	System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
    	    	System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
    	    	System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
    	    	System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
    	    	System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
    		    System.out.println("--------------------------------------------------------");
    	    } 
    	    testCaseNumber++;
	}

	@Test
	public void testValidLineDataStringWithSingleColorAndFilterDataOnly(){
		    System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
   		    LineDataStringParser ldsp1 = new LineDataStringParser("3/0.67654");
   		    
   		    if(ldsp1.isLineDataParsed()){
    	    	assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.67654);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
    	    	System.out.println("The input string : "           +ldsp1.getLineDataString());
    	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
    	    	System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
    	    	System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
    	    	System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
    	    	System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
    	    	System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
    	    	System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
    	    	System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
    		    System.out.println("--------------------------------------------------------");
    	    } 
    	    testCaseNumber++;
	}	
	
	@Test
	public void testValidLineDataStringWithLineColorPointsFilterDataAndSmallContourFlag(){
		LineDataStringParser ldsp1 = new LineDataStringParser("7/0.5/true");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.5);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;	
	}

	@Test
	public void testValidLineDataStringWithPointsFilterDataOnly(){
		LineDataStringParser ldsp1 = new LineDataStringParser(".7");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.7);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;
	}

	@Test
	public void testValidLineDataStringWithPointsFilterDataAndSmallContourFlag(){
		LineDataStringParser ldsp1 = new LineDataStringParser("1.0/TRUE");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 1.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     


	}

	@Test
	public void testValidLineDataStringWithLineColorAndPatternOnly(){
		LineDataStringParser ldsp1 = new LineDataStringParser("7/7");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}  

	@Test
	public void testValidLineDataStringWithIntegerLineAttributesOnly(){
		LineDataStringParser ldsp1 = new LineDataStringParser("8;9;10/2;4/3/2/2");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 2);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}  

	@Test
	public void testValidLineDataStringWithSmallContourFlagOnly(){
		LineDataStringParser ldsp1 = new LineDataStringParser("true");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}  

	@Test
	public void testEmptyStringFunction1(){
		LineDataStringParser ldsp1 = new LineDataStringParser();
		
		if(!ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}

	@Test
	public void testEmptyStringFunction2(){
		LineDataStringParser ldsp1 = new LineDataStringParser("");
		
		if(!ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}

	@Test
	public void testInputStringWithBlanks(){
		LineDataStringParser ldsp1 = new LineDataStringParser("     ");
		
		if(!ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}

	@Test
	public void testValidLineDataParserWithoutSmoothFactorInInputString(){
		LineDataStringParser ldsp1 = new LineDataStringParser("132;240;255/4;4;4/5;5;5/3");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}

	@Test
	public void testValidLineDataParserWithMissingInputLineColor(){
		LineDataStringParser ldsp1 = new LineDataStringParser("/4;10/6/1/1/0.65789/true");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.65789);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 1);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}	
	
	@Test
	public void testValidLineDataParserWithMissingInputLinePattern(){
		LineDataStringParser ldsp1 = new LineDataStringParser("144;110//6/1/1/0.65789/true");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.65789);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 1);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}	

	@Test
	public void testValidLineDataParserWithMissingInputLineWidth(){
		LineDataStringParser ldsp1 = new LineDataStringParser("4;10/8;9//1/1/0.9999/true");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.9999);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 1);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}	
	
	@Test
	public void testValidLineDataParserWithMissingInputLineLabel(){
		LineDataStringParser ldsp1 = new LineDataStringParser("4;10/8;9/6;7//1/0.65789/true");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.65789);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 1);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}

	@Test
	public void testValidLineDataParserWithMissingInputLineSmoothingLevel(){
		LineDataStringParser ldsp1 = new LineDataStringParser("4;10/8;9/6;7/2//0.65789/true");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.65789);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}	

    @Test
	public void testValidLineDataParserWithMissingInputPointsFilter(){
		LineDataStringParser ldsp1 = new LineDataStringParser("4;10/8;9/6;7/2/2//true");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 2);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}	

	@Test
	public void testValidLineDataParserWithMissingInputSmallContourFlag(){
		LineDataStringParser ldsp1 = new LineDataStringParser("4;10/8;9/6;7/2/1/0.65789/");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.65789);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 1);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}    

	@Test
	public void testValidLineDataParserWithMultipleMissingInputs(){
		LineDataStringParser ldsp1 = new LineDataStringParser("4;10//6;7///0.65789/");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.65789);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}	

	@Test
	public void testLineDataParserWithPatternOutOfRange(){
		LineDataStringParser ldsp1 = new LineDataStringParser("240;234/28;36/6/1/1/0.4477");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.4477);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 1);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}	
	
	@Test
	public void testLineDataParserWithPointsDataFilterOutOfRange(){
		LineDataStringParser ldsp1 = new LineDataStringParser("2/4/6/1/1/3.65789");
		
		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 1);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("List of line colors: "         +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
			System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
			System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
			System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
			System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
			System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
			System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
			System.out.println("--------------------------------------------------------");
		} 
		testCaseNumber++;     
	}	

	@Test
	public void testLineDataStringWithNegativeValuesForLineColor(){
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		LineDataStringParser ldsp1 = new LineDataStringParser("5;-6;-12");
		

		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
	    	System.out.println("The input string : "           +ldsp1.getLineDataString());
	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
	    	System.out.println("List of line colors:  "        +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
	    	System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
	    	System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
	    	System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
	    	System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
	    	System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
	    	System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
	    }
	    testCaseNumber++;
	}	
	
	@Test
	public void testLineDataStringWithNegativeValuesForIntegerAttributes(){
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		LineDataStringParser ldsp1 = new LineDataStringParser("5;-6;-12/5;-10/-4/-2/2/0.7");
		

		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.7);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 2);
	    	System.out.println("The input string : "           +ldsp1.getLineDataString());
	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
	    	System.out.println("List of line colors:  "        +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
	    	System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
	    	System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
	    	System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
	    	System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
	    	System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
	    	System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
	    }
	    testCaseNumber++;
	}
	
	@Test
	public void testLineDataStringWithNegativeValueForPointsFilter(){
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		LineDataStringParser ldsp1 = new LineDataStringParser("-0.7");
		

		if(!ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
	    	System.out.println("The input string : "           +ldsp1.getLineDataString());
	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
	    	System.out.println("List of line colors:  "        +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
	    	System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
	    	System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
	    	System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
	    	System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
	    	System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
	    	System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
	    }
	    testCaseNumber++;
	}
	
	@Test
	public void testLineDataStringWithInvalidDelimiters(){
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		LineDataStringParser ldsp1 = new LineDataStringParser("5,6,12");
		

		if(!ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
	    	System.out.println("The input string : "           +ldsp1.getLineDataString());
	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
	    	System.out.println("List of line colors:  "        +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
	    	System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
	    	System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
	    	System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
	    	System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
	    	System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
	    	System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
	    }
	    testCaseNumber++;
	}	

	@Test
	public void testLineDataStringWithNonNumericInputs(){
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		LineDataStringParser ldsp1 = new LineDataStringParser("lmn;abc;def/2;4/5/1/2/0.6/t");
		

		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.6);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 2);
	    	System.out.println("The input string : "           +ldsp1.getLineDataString());
	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
	    	System.out.println("List of line colors:  "        +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
	    	System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
	    	System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
	    	System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
	    	System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
	    	System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
	    	System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
	    }
	    testCaseNumber++;
	}	

	@Test
	public void testLineDataStringOutOffOrderInputs(){
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		LineDataStringParser ldsp1 = new LineDataStringParser("0.6667/2/true/4/5");
		

		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),false);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.0);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
	    	System.out.println("The input string : "           +ldsp1.getLineDataString());
	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
	    	System.out.println("List of line colors:  "        +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
	    	System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
	    	System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
	    	System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
	    	System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
	    	System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
	    	System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
	    }
	    testCaseNumber++;
	}	

	@Test
	public void testLineDataStringWithExcessInputs(){
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		LineDataStringParser ldsp1 = new LineDataStringParser("5;6;12/3;4/5;6/1/3/4/5/2/0.5/T");
		

		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed(),true);
            assertEquals(ldsp1.getInstanceOfLineBuilder().getPointsFilter()
                    .doubleValue(), 0.5);
            assertEquals(ldsp1.getInstanceOfLineBuilder()
                    .getLineSmoothingLevel().intValue(), 0);
	    	System.out.println("The input string : "           +ldsp1.getLineDataString());
	    	System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
	    	System.out.println("List of line colors:  "        +ldsp1.getInstanceOfLineBuilder().getLineColorsList());
	    	System.out.println("List of line patterns: "       +ldsp1.getInstanceOfLineBuilder().getLineStyleList());
	    	System.out.println("List of line widths: "         +ldsp1.getInstanceOfLineBuilder().getLineWidthList());
	    	System.out.println("List of lines to be labelled: "+ldsp1.getInstanceOfLineBuilder().getLineLabelPresentList());
	    	System.out.println("Smoothing Level: "             +ldsp1.getInstanceOfLineBuilder().getLineSmoothingLevel());
	    	System.out.println("Points filter data: "          +ldsp1.getInstanceOfLineBuilder().getPointsFilter());
	    	System.out.println("Small Contour Flag: "          +ldsp1.getInstanceOfLineBuilder().isSmallContourFlagSuppressed());
	    }
	    testCaseNumber++;
	
	}

	@Test
	public void testLineDataStringWithNegativeLabel(){
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		LineDataStringParser ldsp1 = new LineDataStringParser("5;6;12/3;4/5;6/-2/0.5/T");
		

		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isBreakInLineForLabel(),false);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"? "+ldsp1.isLineDataParsed());
			System.out.println("Is there a break in the line for the label: "          +ldsp1.getInstanceOfLineBuilder().isBreakInLineForLabel());
	    }
	    testCaseNumber++;
	
	}	
	
	@Test
	public void testLineDataStringWithPositiveLabel(){
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		LineDataStringParser ldsp1 = new LineDataStringParser("5;6;12/3;4/5;6/2/0.5/T");
		

		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isBreakInLineForLabel(),true);
			System.out.println("The input string : "           +ldsp1.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"a? "+ldsp1.isLineDataParsed());
			System.out.println("Is there a break in the line for the label: "      +ldsp1.getInstanceOfLineBuilder().isBreakInLineForLabel());
	    }
		
		LineDataStringParser ldsp2= new LineDataStringParser("5;6;12/3;4/5;6/1;2/0.5/T");
		

		if(ldsp1.isLineDataParsed()){
			assertEquals(ldsp1.getInstanceOfLineBuilder().isBreakInLineForLabel(),true);
			System.out.println("The input string : "                            +ldsp2.getLineDataString());
			System.out.println("Is line data parsed in test-case "+testCaseNumber+"b? "+ldsp1.isLineDataParsed());
	    	System.out.println("Is there a break in the line for the label: "   +ldsp2.getInstanceOfLineBuilder().isBreakInLineForLabel());
	    }		
		
		
		
	    testCaseNumber++;
	
	}

	
	
	
}





