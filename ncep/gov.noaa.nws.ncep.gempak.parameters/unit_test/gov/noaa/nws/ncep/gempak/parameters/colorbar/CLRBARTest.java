/**
 * 
 */
package gov.noaa.nws.ncep.gempak.parameters.colorbar;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;

import org.junit.Test;


/**<pre>
*Junit test-case for CLRBAR
*
* SOFTWARE HISTORY
* Date          Ticket#     Engineer     Description
* ------------ ---------- ----------- --------------------------
* 19-Jul-2012    743        Archana.S    Initial Creation
* </pre>
* @author Archana.S
* @version 1
*/
public class CLRBARTest {

	private static int testCaseNumber=1;
	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameters.colorbar.CLRBAR#CLRBAR(java.lang.String)}.
	 */
	@Test
	public void testCLRBAR() {
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		CLRBAR clbar = new CLRBAR("21/H/CC/0.0005;0.003/0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();
		assertEquals(cbarAttrBuilder.isDrawColorBar(), true);
		assertEquals(cbarAttrBuilder.isDrawBoxAroundColorBar(), true);
		assertEquals(cbarAttrBuilder.getAnchorLocation().toString(), "CenterCenter");
		assertEquals(cbarAttrBuilder.getColorBarOrientation().toString(), "Horizontal");
		assertEquals(cbarAttrBuilder.getX(), 0.0005);
		assertEquals(cbarAttrBuilder.getY(), 0.003);
		assertEquals(cbarAttrBuilder.getLength(), 0.85);
		assertEquals(cbarAttrBuilder.getWidth(), 0.01);
		assertEquals(cbarAttrBuilder.getColor(), GempakColor.LWNGREEN.getRGB());
		System.out.println("Draw the colorbar: " + cbarAttrBuilder.isDrawColorBar());
		System.out.println("Draw the box around the colorbar: " + cbarAttrBuilder.isDrawBoxAroundColorBar());
		System.out.println("Anchor location " + cbarAttrBuilder.getAnchorLocation().toString());
		System.out.println("ColorBar Orientation: " + cbarAttrBuilder.getColorBarOrientation().toString());
		System.out.println("View Coordinate X: " + cbarAttrBuilder.getX());
		System.out.println("View Coordinate Y: " + cbarAttrBuilder.getY());
		System.out.println("ColorBar length as a percentage of screen height: " + cbarAttrBuilder.getLength()*100 + "%");
		System.out.println("ColorBar width as a fraction of the screen width: " + cbarAttrBuilder.getWidth()*1000 + "%");
		testCaseNumber++;
	}

	
	@Test
	public void testCLRBARNoBoundingBox() {
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		CLRBAR clbar = new CLRBAR("-21/H/CC/0.0005;0.003/0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();
		
		assertEquals(cbarAttrBuilder.isDrawBoxAroundColorBar(), false);
		
		System.out.println("Draw the box around the colorbar: " + cbarAttrBuilder.isDrawBoxAroundColorBar());
		testCaseNumber++;
	  }
	
	
	@Test
	public void testCLRBARNoColorBar() {
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		CLRBAR clbar = new CLRBAR("/H/CC/0.0005;0.003/0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();
		
		assertEquals(cbarAttrBuilder.isDrawColorBar(), false);
		
		System.out.println("Draw the colorbar: " + cbarAttrBuilder.isDrawColorBar());
		testCaseNumber++;

		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		 clbar = new CLRBAR("H/CC/0.0005;0.003/0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		cbarAttrBuilder = clbar.getcBarAttributesBuilder();
		
		assertEquals(cbarAttrBuilder.isDrawColorBar(), false);
		
		System.out.println("Draw the colorbar: " + cbarAttrBuilder.isDrawColorBar());
		testCaseNumber++;

		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		 clbar = new CLRBAR("0/H/CC/0.0005;0.003/0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		cbarAttrBuilder = clbar.getcBarAttributesBuilder();
		
		assertEquals(cbarAttrBuilder.isDrawColorBar(), false);
		
		System.out.println("Draw the colorbar: " + cbarAttrBuilder.isDrawColorBar());
		testCaseNumber++;		
		
	  }
	
	@Test
	public void testCLRBARNoOrientation() {
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		CLRBAR clbar = new CLRBAR("21//CC/0.0005;0.003/0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();		
		assertEquals(cbarAttrBuilder.getColorBarOrientation().toString(), ColorBarOrientation.Vertical.toString());		
		System.out.println("Default orientation: " + cbarAttrBuilder.getColorBarOrientation().toString());
		testCaseNumber++;
	  }

	@Test
	public void testCLRBARNoAnchorLocation() {
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		CLRBAR clbar = new CLRBAR("21/H//0.0005;0.003/0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();		
		assertEquals(cbarAttrBuilder.getAnchorLocation().toString(), ColorBarAnchorLocation.LowerLeft.toString());		
		System.out.println("Default anchor location: " + cbarAttrBuilder.getAnchorLocation().toString());
		testCaseNumber++;
	  }	

	@Test
	public void testCLRBARNoXViewCoord() {
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		CLRBAR clbar = new CLRBAR("21/H/CL/;0.003/0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();		
		assertEquals(cbarAttrBuilder.getX(), 0.005);		
		assertEquals(cbarAttrBuilder.getY(), 0.003);
		System.out.println("Default view coordinate X: " + cbarAttrBuilder.getX());
		System.out.println("View coordinate Y: " + cbarAttrBuilder.getY());
		testCaseNumber++;
	  }
	
	@Test
	public void testCLRBARNoYViewCoord() {
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		CLRBAR clbar = new CLRBAR("21/H/CL/0.00323/0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();		
		assertEquals(cbarAttrBuilder.getX(), 0.00323);		
		assertEquals(cbarAttrBuilder.getY(), 0.05);
		System.out.println("View coordinate X: " + cbarAttrBuilder.getX());
		System.out.println("Default view coordinate Y: " + cbarAttrBuilder.getY());
		testCaseNumber++;
	
	System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		clbar = new CLRBAR("21/H/CL/0.01;/0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		cbarAttrBuilder = clbar.getcBarAttributesBuilder();		
		assertEquals(cbarAttrBuilder.getX(), 0.01);		
		assertEquals(cbarAttrBuilder.getY(), 0.05);
		System.out.println("View coordinate X: " + cbarAttrBuilder.getX());
		System.out.println("Default view coordinate Y: " + cbarAttrBuilder.getY());
		testCaseNumber++;
	
	 }	

	@Test
	public void testCLRBARNoXAndY() {
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		CLRBAR clbar = new CLRBAR("21/H/CL//0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();		
		assertEquals(cbarAttrBuilder.getX(), 0.005);		
		assertEquals(cbarAttrBuilder.getY(), 0.05);
		System.out.println("Default view coordinate X: " + cbarAttrBuilder.getX());
		System.out.println("Default view coordinate Y: " + cbarAttrBuilder.getY());
		testCaseNumber++;

		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		clbar = new CLRBAR("21/H/CL/;/0.85;0.01");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		cbarAttrBuilder = clbar.getcBarAttributesBuilder();		
		assertEquals(cbarAttrBuilder.getX(), 0.005);		
		assertEquals(cbarAttrBuilder.getY(), 0.05);
		System.out.println("Default view coordinate X: " + cbarAttrBuilder.getX());
		System.out.println("Default view coordinate Y: " + cbarAttrBuilder.getY());
		testCaseNumber++;		
	  }	

	@Test
	public void testCLRBARNoLength() {
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		CLRBAR clbar = new CLRBAR("21/H/CL/0.06;0.007/;0.02");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();		
		assertEquals(cbarAttrBuilder.getLength(), 0.5);		
		assertEquals(cbarAttrBuilder.getWidth(), 0.02);
		System.out.println("Default length: " + cbarAttrBuilder.getLength()*100 + "%");
		System.out.println("Width: " + cbarAttrBuilder.getWidth());
		testCaseNumber++;

	  }	

	@Test
	public void testCLRBARNoWidth() {
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		CLRBAR clbar = new CLRBAR("21/H/CL/0.06;0.007/0.98");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();		
		assertEquals(cbarAttrBuilder.getLength(), 0.98);		
		assertEquals(cbarAttrBuilder.getWidth(), 0.01);
		System.out.println("Length: " + cbarAttrBuilder.getLength()*100 + "%");
		System.out.println("Default Width: " + cbarAttrBuilder.getWidth());
		testCaseNumber++;

		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		clbar = new CLRBAR("21/H/CL/0.06;0.007/0.87;");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		cbarAttrBuilder = clbar.getcBarAttributesBuilder();		
		assertEquals(cbarAttrBuilder.getLength(), 0.87);		
		assertEquals(cbarAttrBuilder.getWidth(), 0.01);
		System.out.println("Length: " + cbarAttrBuilder.getLength()*100 + "%");
		System.out.println("Default Width: " + cbarAttrBuilder.getWidth());
		testCaseNumber++;		
	  }	
	

	@Test
	public void testCLRBARNoLengthAndWidth() {
		System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		CLRBAR clbar = new CLRBAR("21/H/CL/0.06;0.007");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();		
		assertEquals(cbarAttrBuilder.getLength(), 0.5);		
		assertEquals(cbarAttrBuilder.getWidth(), 0.01);
		System.out.println("Default length: " + cbarAttrBuilder.getLength()*100 + "%");
		System.out.println("Default  width: " + cbarAttrBuilder.getWidth());
		testCaseNumber++;

	  }	
	
	@Test
	public void testDefaultsForColorBar(){
        System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		
		CLRBAR clbar = new CLRBAR("15");
		System.out.println("CLRBAR = " + clbar.getStrToParse());
		
		ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();
		assertEquals(cbarAttrBuilder.isDrawColorBar(), true);
		assertEquals(cbarAttrBuilder.isDrawBoxAroundColorBar(), true);
		assertEquals(cbarAttrBuilder.getAnchorLocation().toString(), "LowerLeft");
		assertEquals(cbarAttrBuilder.getColorBarOrientation().toString(), "Vertical");
		assertEquals(cbarAttrBuilder.getX(), 0.005);
		assertEquals(cbarAttrBuilder.getY(), 0.05);
		assertEquals(cbarAttrBuilder.getLength(), 0.5);
		assertEquals(cbarAttrBuilder.getWidth(), 0.01);
		assertEquals(cbarAttrBuilder.getColor(), GempakColor.FIREBRIC.getRGB());
		System.out.println("Draw the colorbar               : " + cbarAttrBuilder.isDrawColorBar());
		System.out.println("Draw the box around the colorbar: " + cbarAttrBuilder.isDrawBoxAroundColorBar());
		System.out.println("Anchor location                 :" + cbarAttrBuilder.getAnchorLocation().toString());
		System.out.println("ColorBar Orientation            : " + cbarAttrBuilder.getColorBarOrientation().toString());
		System.out.println("View Coordinate X               : " + cbarAttrBuilder.getX());
		System.out.println("View Coordinate Y               : " + cbarAttrBuilder.getY());
		System.out.println("ColorBar length as a percentage of screen height: " + cbarAttrBuilder.getLength()*100 + "%");
		System.out.println("ColorBar width as a fraction of the screen width: " + cbarAttrBuilder.getWidth()*1000 + "%");
		testCaseNumber++;
 	 }
	
	   @Test
	   public void testEmptyCLRBAR(){
		   System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		   CLRBAR clbar = new CLRBAR("");
		   System.out.println("CLRBAR = " + clbar.getStrToParse());
		   ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();
		   assertEquals(cbarAttrBuilder.isDrawColorBar(), false);
		   System.out.println("Draw the colorbar               : " + cbarAttrBuilder.isDrawColorBar());
		   testCaseNumber++;
		   
		   System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
		   clbar = new CLRBAR("               ");
		   System.out.println("CLRBAR = " + clbar.getStrToParse());
		   cbarAttrBuilder = clbar.getcBarAttributesBuilder();
		   assertEquals(cbarAttrBuilder.isDrawColorBar(), false);
		   System.out.println("Draw the colorbar               : " + cbarAttrBuilder.isDrawColorBar());
		   testCaseNumber++;		   
	  }

	   
		@Test
		public void testExtraInputsForColorBar(){
	        System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
			
			CLRBAR clbar = new CLRBAR("15/H/UR/0.005;0.08/0.45;0.02/abc/@#");
			System.out.println("CLRBAR = " + clbar.getStrToParse());
			
			ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();
			assertEquals(cbarAttrBuilder.isDrawColorBar(), true);
			assertEquals(cbarAttrBuilder.isDrawBoxAroundColorBar(), true);
			assertEquals(cbarAttrBuilder.getAnchorLocation().toString(), "UpperRight");
			assertEquals(cbarAttrBuilder.getColorBarOrientation().toString(), "Horizontal");
			assertEquals(cbarAttrBuilder.getX(), 0.005);
			assertEquals(cbarAttrBuilder.getY(), 0.08);
			assertEquals(cbarAttrBuilder.getLength(), 0.45);
			assertEquals(cbarAttrBuilder.getWidth(), 0.02);
			assertEquals(cbarAttrBuilder.getColor(), GempakColor.FIREBRIC.getRGB());
			System.out.println("Draw the colorbar               : " + cbarAttrBuilder.isDrawColorBar());
			System.out.println("Draw the box around the colorbar: " + cbarAttrBuilder.isDrawBoxAroundColorBar());
			System.out.println("Anchor location                 :" + cbarAttrBuilder.getAnchorLocation().toString());
			System.out.println("ColorBar Orientation            : " + cbarAttrBuilder.getColorBarOrientation().toString());
			System.out.println("View Coordinate X               : " + cbarAttrBuilder.getX());
			System.out.println("View Coordinate Y               : " + cbarAttrBuilder.getY());
			System.out.println("ColorBar length as a percentage of screen height: " + cbarAttrBuilder.getLength()*100 + "%");
			System.out.println("ColorBar width as a fraction of the screen width: " + cbarAttrBuilder.getWidth()*1000 + "%");
			testCaseNumber++;
	 	 }	 
		
		
		@Test
		public void testCLRBARMalformedInput() {
			System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
			CLRBAR clbar = new CLRBAR("21/H1234/CC/0.0005*)(;0.003|/0.85asdf;0.01efgh");
			System.out.println("CLRBAR = " + clbar.getStrToParse());
			ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();
			assertEquals(cbarAttrBuilder.isDrawColorBar(), true);
			assertEquals(cbarAttrBuilder.isDrawBoxAroundColorBar(), true);
			assertEquals(cbarAttrBuilder.getAnchorLocation().toString(), "CenterCenter");
			assertEquals(cbarAttrBuilder.getColorBarOrientation().toString(), "Vertical");
			assertEquals(cbarAttrBuilder.getX(), 0.005);
			assertEquals(cbarAttrBuilder.getY(), 0.05);
			assertEquals(cbarAttrBuilder.getLength(), 0.5);
			assertEquals(cbarAttrBuilder.getWidth(), 0.01);
			assertEquals(cbarAttrBuilder.getColor(), GempakColor.LWNGREEN.getRGB());
			System.out.println("Draw the colorbar: " + cbarAttrBuilder.isDrawColorBar());
			System.out.println("Draw the box around the colorbar: " + cbarAttrBuilder.isDrawBoxAroundColorBar());
			System.out.println("Anchor location " + cbarAttrBuilder.getAnchorLocation().toString());
			System.out.println("ColorBar Orientation: " + cbarAttrBuilder.getColorBarOrientation().toString());
			System.out.println("View Coordinate X: " + cbarAttrBuilder.getX());
			System.out.println("View Coordinate Y: " + cbarAttrBuilder.getY());
			System.out.println("ColorBar length as a percentage of screen height: " + cbarAttrBuilder.getLength()*100 + "%");
			System.out.println("ColorBar width as a fraction of the screen width: " + cbarAttrBuilder.getWidth()*1000 + "%");
			testCaseNumber++;
		}
		
		
		@Test
		public void testCLRBARLengthRatioInvalidLowLimit() {
			System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
			CLRBAR clbar = new CLRBAR("21/H/CC/0.0005;0.003/-0.85;0.01");
			System.out.println("CLRBAR = " + clbar.getStrToParse());
			ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();

			assertEquals(cbarAttrBuilder.getLength(), 0.5);


			System.out.println("ColorBar length as a percentage of screen height: " + cbarAttrBuilder.getLength()*100 + "%");

			testCaseNumber++;
		}

		@Test
		public void testCLRBARLengthRatioInvalidHighLimit() {
			System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
			CLRBAR clbar = new CLRBAR("21/H/CC/0.0005;0.003/10.85;0.01");
			System.out.println("CLRBAR = " + clbar.getStrToParse());
			ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();

			assertEquals(cbarAttrBuilder.getLength(), 0.5);


			System.out.println("ColorBar length as a percentage of screen height: " + cbarAttrBuilder.getLength()*100 + "%");

			testCaseNumber++;
		}		
		
		
		@Test
		public void testCLRBARWidthRatioInvalidLowLimit() {
			System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
			CLRBAR clbar = new CLRBAR("21/H/CC/0.0005;0.003/0.85;-10.45");
			System.out.println("CLRBAR = " + clbar.getStrToParse());
			ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();

			assertEquals(cbarAttrBuilder.getWidth(), 0.01);


			System.out.println("ColorBar width as a fraction of screen width: " + cbarAttrBuilder.getWidth());

			testCaseNumber++;
		}

		@Test
		public void testCLRBARWidthRatioInvalidHighLimit() {
			System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
			CLRBAR clbar = new CLRBAR("21/H/CC/0.0005;0.003/0.85;50.45");
			System.out.println("CLRBAR = " + clbar.getStrToParse());
			ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();

			assertEquals(cbarAttrBuilder.getWidth(), 0.01);


			System.out.println("ColorBar width as a fraction of screen width: " + cbarAttrBuilder.getWidth());

			testCaseNumber++;
		}		
		
		
		@Test
		public void testCLRBARXViewCoordInvalidLowLimit() {
			System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
			CLRBAR clbar = new CLRBAR("21/H/CC/-0.7855;0.003/0.85;0.04");
			System.out.println("CLRBAR = " + clbar.getStrToParse());
			ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();

			assertEquals(cbarAttrBuilder.getX(), 0.005);


			System.out.println("View coordinate X: " + cbarAttrBuilder.getX());

			testCaseNumber++;
		}		

		
		@Test
		public void testCLRBARXViewCoordInvalidHighLimit() {
			System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
			CLRBAR clbar = new CLRBAR("21/H/CC/7.855;0.003/0.85;0.04");
			System.out.println("CLRBAR = " + clbar.getStrToParse());
			ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();

			assertEquals(cbarAttrBuilder.getX(), 0.005);


			System.out.println("View coordinate X: " + cbarAttrBuilder.getX());

			testCaseNumber++;
		}		
		
		
		@Test
		public void testCLRBARYViewCoordInvalidLowLimit() {
			System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
			CLRBAR clbar = new CLRBAR("21/H/CC/0.7855;-10.003/0.85;0.04");
			System.out.println("CLRBAR = " + clbar.getStrToParse());
			ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();

			assertEquals(cbarAttrBuilder.getY(), 0.05);


			System.out.println("View coordinate Y: " + cbarAttrBuilder.getY());

			testCaseNumber++;
		}		

		
		@Test
		public void testCLRBARYViewCoordInvalidHighLimit() {
			System.out.println("------------------Test-case "+ testCaseNumber +"----------------");
			CLRBAR clbar = new CLRBAR("21/H/CC/0.7855;1.0001/0.85;0.04");
			System.out.println("CLRBAR = " + clbar.getStrToParse());
			ColorBarAttributesBuilder cbarAttrBuilder = clbar.getcBarAttributesBuilder();

			assertEquals(cbarAttrBuilder.getY(), 0.05);


			System.out.println("View coordinate Y: " + cbarAttrBuilder.getY());

			testCaseNumber++;
		}				
	}











