package gov.noaa.nws.ncep.gempak.parameters.dlines;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * <pre>
 * Junit test-case for DLINES and DLINEData
 * 
 *  SOFTWARE HISTORY
 *  Date          Ticket#     Engineer     Description
 *  ------------ ---------- ----------- --------------------------
 *  29-Dec-2009    211        Archana.S    Initial Creation
 *  25-Aug-2012    743        djohnson     Upgrade to JUnit 4.10.
 * </pre>
 * 
 * @author Archana.S
 * @version 1
 */
public class DLINESTest {

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameters.dlines.DLines#getDlineData()}.
	 */
	@Test
	public void testGetDlineData() {
		
		String[] testCaseDescription = {"Test DLINES with both right and left states set to true and epsilon set to 0.25",
				                        "Test DLINES with both right and left states set to true and epsilon undefined",    		                       
				                        "Test DLINES with only right state (set to true)",
				                        "Test DLINES with both right and left states set to false and epsilon set to 1.3",
				                        "Test DLINES with both right and left states set to false and epsilon undefined",
				                        "Test DLINES with only right state (set to false)",
				                        "Test DLINES with right state set to false and left state set to true",
				                        "Test DLINES with right state set to true and left state set to false",
				                        "Test DLINES for case-insensitive comparisons",
				                        "Test DLINES with an invalid delimiter - using , instead of ;",
				                        "Test DLINES with an invalid delimiter - using / instead of |",
				                        "Test DLINES with a malformed input",
				                        "Test DLINES with an empty string",
				                        "Test DLINES with a null string",
				                        "Test DLINES with a string containing only blanks",
				                        "Test DLINES with extra arguments",
				                        "Test DLINES with input parameters in reverse order"
		
		
		};
		
		
		String[] inputString = {"yes;yes|0.25", 
                "yes;yes",
                "yes",
                "no;no|1.3",
                "no;no",
                "no",
                "no ; yes",
                "yes ; no",
                "YES ; YeS",
                "Yes , yes ",
                "Yes ; yes / 0.9",
                " ; |",
                "",
                null,
                "          ",
                "yes;yes;yes;yes | 0.9 | 0.8",
                "0.9 | yes ; yes"};
		
		
		
		for(int i = 0; i < inputString.length; i++){
			DLines dlines = new DLines(inputString[i]);

			System.out.println("Test-case number: "+ (i+1));
			System.out.println(testCaseDescription[i]);
			System.out.println("\nThe input string is: "+ dlines.getDString());
			if(dlines.getDlineData() != null){
				System.out.println("Contour values to the right of the current contour line are greater = "+ dlines.getDlineData().isRightOfContourLineGreater());
				System.out.println("Contour values to the left of the current contour line are greater = "+ dlines.getDlineData().isLeftOfContourLineGreater());
				System.out.println("Epsilon value     = "+ dlines.getDlineData().getEpsilon());
			} 
			else{
				System.out.println("\nEmpty DLINES string");
			}
			
			System.out.println("==================================================");			
			
			switch(i){
			    case 0:
			          assertEquals("Contour values to the right of the current contour line should be greater",dlines.getDlineData()
			    		             .isRightOfContourLineGreater().booleanValue(),true);
			          assertEquals("Contour values to the left of the current contour line should be greater",dlines.getDlineData()
			    		             .isLeftOfContourLineGreater(),true);
                assertEquals("Epsilon should be set to 0.25", dlines
                        .getDlineData().getEpsilon(), 0.25, 0.01);
				break;
				
			    case 1:
				      assertEquals("Contour values to the right of the current contour line should be greater",dlines.getDlineData()
				    		       .isRightOfContourLineGreater().booleanValue(),true);
				      assertEquals("Contour values to the left of the current contour line should be greater",dlines.getDlineData()
				    		       .isLeftOfContourLineGreater(),true);
				      assertEquals("Epsilon should be undefined",dlines.getDlineData()
.getEpsilon(), Double.NaN, 0.01);
				break;
				
			    case 2:
				      assertEquals("Contour values to the right of the current contour line should be greater",dlines.getDlineData()
				    		       .isRightOfContourLineGreater().booleanValue(),true);
				      assertEquals("Contour values to the left of the current contour line should be lesser",dlines.getDlineData()
				    		       .isLeftOfContourLineGreater(),false);
				      assertEquals("Epsilon should be undefined",dlines.getDlineData()
.getEpsilon(), Double.NaN, 0.01);
				break;				
				
			    case 3:
			          assertEquals("Contour values to the right of the current contour line should be lesser",dlines.getDlineData()
			    		             .isRightOfContourLineGreater().booleanValue(),false);
			          assertEquals("Contour values to the left of the current contour line should be lesser",dlines.getDlineData()
			    		             .isLeftOfContourLineGreater(),false);
                assertEquals("Epsilon should be set to 1.3", dlines
                        .getDlineData().getEpsilon(), 1.3, 0.01);
				break;
				
			    case 6:
			          assertEquals("Contour values to the right of the current contour line should be lesser",dlines.getDlineData()
			    		             .isRightOfContourLineGreater().booleanValue(),false);
			          assertEquals("Contour values to the left of the current contour line should be greater",dlines.getDlineData()
			    		             .isLeftOfContourLineGreater(),true);
			          assertEquals("Epsilon should be undefined",dlines.getDlineData()
.getEpsilon(), Double.NaN, 0.01);
				break;				
				
			    case 7:
			          assertEquals("Contour values to the right of the current contour line should be greater",dlines.getDlineData()
			    		             .isRightOfContourLineGreater().booleanValue(),true);
			          assertEquals("Contour values to the left of the current contour line should be lesser",dlines.getDlineData()
			    		             .isLeftOfContourLineGreater(),false);
			          assertEquals("Epsilon should be undefined",dlines.getDlineData()
.getEpsilon(), Double.NaN, 0.01);
				break;				
				
			    case 8:
				      assertEquals("Contour values to the right of the current contour line should be greater",dlines.getDlineData()
				    		       .isRightOfContourLineGreater().booleanValue(),true);
				      assertEquals("Contour values to the left of the current contour line should be greater",dlines.getDlineData()
				    		       .isLeftOfContourLineGreater(),true);
				      assertEquals("Epsilon should be undefined",dlines.getDlineData()
.getEpsilon(), Double.NaN, 0.01);
				break;
			
			    case 10:
			          assertEquals("Contour values to the right of the current contour line should be greater",dlines.getDlineData()
			    		             .isRightOfContourLineGreater().booleanValue(),true);
			          assertEquals("Contour values to the left of the current contour line should be lesser",dlines.getDlineData()
			    		             .isLeftOfContourLineGreater(),false);
			          assertEquals("Epsilon should be undefined",dlines.getDlineData()
.getEpsilon(), Double.NaN, 0.01);
				break;				
			    
			    default:
			          if (dlines.getDlineData() != null) {
			        	  assertEquals(
			        			  "Contour values to the right of the current contour line should be lesser",
			        			  dlines.getDlineData().isRightOfContourLineGreater()
			        			  .booleanValue(), false);
			        	  assertEquals("Contour values to the left of the current contour line should be lesser",
			        			  dlines.getDlineData().isLeftOfContourLineGreater(),
			        			  false);
			        	  assertEquals("Epsilon should be undefined", dlines
                            .getDlineData().getEpsilon(), Double.NaN, 0.01);
					 }else{
						 assertEquals("DLINES should be null", dlines.getDlineData(), null);
					 }
			    
				break;		
			
			}
		}
	}
}























