package gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.GraphicsAreaCoordinates;

import org.junit.Test;
/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29-Sep-2009     171      Archana    Initial Version
 * 
 * </pre>   
 * @author Archana
 * @version 1
 */


/**
 * This junit test file tests the methods in the class GraphicsAreaCoordinates.
 */

public class GraphicsAreaCoordinatesTest {
private boolean flag;
private String error_msg;	

	@Test
	
	//Test that all Latitude/Longitude values can be set to 0
	public void testAllZeroLatLon(){
		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("0;0;0;0"); 
		flag = gac.isGraphicsAreaStringValid();
		if(flag ==  true){
		double lat_lon[] = gac.getGAREACoordinates();
		assertEquals(lat_lon[0],0.0f);
		assertEquals(lat_lon[1],0.0f);
		assertEquals(lat_lon[2],0.0f);
		assertEquals(lat_lon[3],0.0f);	
		assertEquals(lat_lon[4],0.0f);
		assertEquals(lat_lon[5],0.0f);		
		}
	}
	
	@Test
	// Test that Lower Left Latitude is not <-91 or > 90
	public void testLowerLeftLatOutOfBounds(){
		
		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("-90.1;0;20;40"); 
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("Lower Left Latitude can only take values between -90.00 and 90.00", flag);
//		if(!flag){
//			error_msg = gac.getErrorCode();
//			assertEquals(error_msg,"Lower Left Latitude can only take values between -90.00 and 90.00");
//		}
		
		GraphicsAreaCoordinates gac2 = new GraphicsAreaCoordinates("90.1;0;90.0;40"); 
		flag = gac2.isGraphicsAreaStringValid();
		assertFalse("Lower Left Latitude can only take values between -90.00 and 90.00", flag);
//		if(!flag){
//			error_msg = gac2.getErrorCode();
//			assertEquals(error_msg,"Lower Left Latitude can only take values between -90.00 and 90.00");
//		}		
	}
	
	@Test
	// Test that Upper Right Latitude is not <-91 or > 90
	public void testUpperRightLatOutOfBounds(){
		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("-60;50;-90.1;0"); 
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("Upper Right Latitude can only take values between -90.00 and 90.00", flag);
//		if(!flag){
//			error_msg = gac.getErrorCode();
//			assertEquals(error_msg,"Upper Right Latitude can only take values between -90.00 and 90.00");
//		}
		
		GraphicsAreaCoordinates gac2 = new GraphicsAreaCoordinates("0;0;90.1;50"); 
		flag = gac2.isGraphicsAreaStringValid();
		assertFalse("Upper Right Latitude can only take values between -90.00 and 90.00", flag);

//		if(!flag){
//			error_msg = gac2.getErrorCode();
//			assertEquals(error_msg,"Upper Right Latitude can only take values between -90.00 and 90.00");
//		}		
	}
	
	@Test
	// Test that Lower Left Longitude is not > 360 or < -181
	public void testLowerLeftLonOutOfBounds(){
		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("-89;-180.1;60;0"); 
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("Lower Left Longitude can only take values between -180.00 to 180.00 or 0.00 to 360.00", flag);
//		if(!flag){
//			error_msg = gac.getErrorCode();
//			assertEquals(error_msg,"Lower Left Longitude can only take values between -180.00 to 180.00 or 0.00 to 360.00");
//		}
		
		GraphicsAreaCoordinates gac2= new GraphicsAreaCoordinates("0;360.1;50;0"); 
		flag = gac2.isGraphicsAreaStringValid();
		assertFalse("Lower Left Longitude can only take values between -180.00 to 180.00 or 0.00 to 360.00", flag);
//		if(!flag){
//			error_msg = gac2.getErrorCode();
//			assertEquals(error_msg,"Lower Left Longitude can only take values between -180.00 to 180.00 or 0.00 to 360.00");
//		}		
	}
	
	@Test
	// Test that Upper Right Longitude is not > 360 or < -181
	public void testUpperRightLonOutOfBounds(){
		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("-89;59;50;-180.1"); 
		flag = gac.isGraphicsAreaStringValid();
		assertTrue("After adding convertLongitudeValue(...) logic, Upper Right Longitude -180.1 is valid now", flag);
//		if(!flag){
//			error_msg = gac.getErrorCode();
//			assertEquals(error_msg,"Upper Right Longitude can only take values between -180.00 to 180.00 or 0.00 to 360.00");
//		}	
		
		GraphicsAreaCoordinates gac2 = new GraphicsAreaCoordinates("0;0;0;360.1"); 
		flag = gac2.isGraphicsAreaStringValid();
		assertTrue("After adding convertLongitudeValue(...) logic, Upper Right Longitude 360.1 is valid now", flag);
//		if(!flag){
//			error_msg = gac2.getErrorCode();
//			assertEquals(error_msg,"Upper Right Longitude can only take values between -180.00 to 180.00 or 0.00 to 360.00");
//		}		
	}

	@Test
	//Test for valid values of Lower Left and Upper Right Latitude/Longitude values
	public void testValidLatLonDataRange(){

		String gAreaString = "-90;-180;90;180"; 
    	GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates(gAreaString); 
//		flag = gac.parseGraphicsAreaString(gAreaString);
		flag = gac.isGraphicsAreaStringValid();
		
		assertTrue("The input Graphics Area Coordinates should be valid", flag);
		double lat_lon[] = gac.getGAREACoordinates();
		assertEquals(lat_lon[0],-90.0f);
		assertEquals(lat_lon[1],-180.0f);
		assertEquals(lat_lon[2],90.0f);
		assertEquals(lat_lon[3],180.0f);	
		assertEquals(lat_lon[4],0.0f);
		assertEquals(lat_lon[5],0.0f);

		gAreaString = "-89;-179;89;179"; 
		GraphicsAreaCoordinates gac2 = new GraphicsAreaCoordinates(gAreaString); 
		flag = gac2.isGraphicsAreaStringValid();
//		flag = gac2.parseGraphicsAreaString(gAreaString);
		assertTrue("The input Graphics Area Coordinates should be valid", flag);
		lat_lon = gac2.getGAREACoordinates();
		assertEquals(lat_lon[0],-89f);
		assertEquals(lat_lon[1],-179.0f);
		assertEquals(lat_lon[2],89.0f);
		assertEquals(lat_lon[3],179.0f);	
		assertEquals(lat_lon[4],0.0f);
		assertEquals(lat_lon[5],0.0f);		
		
	}

	@Test
	//Test if Latitude/Longitude values can be entered without any digit preceding the '.' character.	
	public void testValiLatLonNoLeadingDigit(){
		
		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("-.55;.66;30.45678;170"); 
		flag = gac.isGraphicsAreaStringValid();
		assertTrue("The input Graphics Area Coordinates should be valid", flag);
		double lat_lon[] = gac.getGAREACoordinates();
		assertEquals(lat_lon[0],-0.55f);
		assertEquals(lat_lon[1],0.66f);
		assertEquals(lat_lon[2],30.45678f);
		assertEquals(lat_lon[3],170.0f);	
		assertEquals(lat_lon[4],14.95339f);
		assertEquals(lat_lon[5],85.33f);	       	
	}	
	
	@Test
	// Test for less than 4 Latitude/Longitude values	
	public void testLessThanFourLatLonValues(){
			
		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("10;20;30"); 
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("Too few arguments entered", flag);
//		if(!flag){
//			error_msg = gac.getErrorCode();
//			assertEquals(error_msg,"Too few arguments entered");
//		}

	}

	@Test
	//Test for more than 4 Latitude/Longitude values
    public void testMoreThanFourLatLonValues(){

	    GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("10;20;30;40;50"); 
	    flag = gac.isGraphicsAreaStringValid();
		assertTrue("For the number of arguments more than 4, it should be considered a valid string, the extra arguments is simply ignored", flag);
//	    if(!flag){
//		     error_msg = gac.getErrorCode();
//	    }		
    }
    
	@Test
    //Test that Lower Left Latitude value cannot exceed the Upper Right Latitude	
    public void testLowerLeftLatLessThanUpperRightLat(){

		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("70.55;40.45;30.02;80.44"); 
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("Lower left latitude must be less than or equal to upper right latitude", flag);       	
//        if(!flag){
//			error_msg = gac.getErrorCode();
//			assertEquals(error_msg,"Lower left latitude must be less than or equal to upper right latitude");       	
//        } 
        
		GraphicsAreaCoordinates gac_1 = new GraphicsAreaCoordinates("90;180;-90;-180"); 
		flag = gac_1.isGraphicsAreaStringValid();
		assertFalse("Lower left latitude must be less than or equal to upper right latitude", flag);            	
//        if(!flag){
//			error_msg = gac_1.getErrorCode();
//			assertEquals(error_msg,"Lower left latitude must be less than or equal to upper right latitude");            	
//        }
    }

	@Test
    //Test that Lower Left Longitude value cannot exceed the Upper Right Longitude    
    public void testLowerLeftLonLessThanUpperRightLon(){
 
		GraphicsAreaCoordinates gac19 = new GraphicsAreaCoordinates("20.55;140.45;30.02;80.44"); 
		flag = gac19.isGraphicsAreaStringValid();
		assertFalse("Lower left longitude must be less than or equal to upper right longitude", flag);       	
//		if(!flag){
//			error_msg = gac19.getErrorCode();
//			assertEquals(error_msg,"Lower left longitude must be less than or equal to upper right longitude");       	
//		} 

		GraphicsAreaCoordinates gac_1 = new GraphicsAreaCoordinates("90;180;90;-180"); 
		flag = gac_1.isGraphicsAreaStringValid();
		assertFalse("The two coordinate points are the same point, thus it is invalid", flag);            	
//		if(!flag){
//			error_msg = gac_1.getErrorCode();
//			assertEquals(error_msg,"Lower left longitude must be less than or equal to upper right longitude");            	
//		}         
    }
    
	@Test
	//Test for invalid separator like a ',' character or a '.' character instead of ';'
	public void testInvalidDelimiter(){
		
	    GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("0.0.0.0");
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("Invalid String Format", flag);
//		if(!flag){
//			error_msg = gac.getErrorCode();
//			assertEquals(error_msg,"Invalid String Format");
//		}
	    GraphicsAreaCoordinates gac2 = new GraphicsAreaCoordinates("#0,0,0,0");
	    flag = gac2.isGraphicsAreaStringValid(); 
		assertFalse("String Format '#0,0,0,0' should be invalid", flag);
//		if(!flag){
//			error_msg = gac2.getErrorCode();
//			assertEquals(error_msg,"Invalid String Format");
//		}
	}		


	@Test
    //Test for the presence of an extra '.' character
	public void testExtraDemialPoint(){

		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("-.55;90..77;30.45678;170"); 
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("Invalid lat/lon String Format", flag);
//        if(!flag){
//			error_msg = gac.getErrorCode();
//			assertEquals(error_msg,"Invalid lat/lon values entered");       	
//        } 		
	}
	
	@Test
	//Test for non-numeric characters in the input Latitude/Longitude string
	public void testInvalidNonNumericLatLonInput(){

		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("0;$0;0-;0");
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("Invalid lat/lon values entered", flag);
//		if(!flag){
//			error_msg = gac.getErrorCode();
//			assertEquals(error_msg,"Invalid lat/lon values entered");
//		}		
	}
	
	@Test
	//Test that the Upper Right and Lower Left Latitude Values computed from the Center/Delta Lat/Lon Values 
	// lie in the correct range from -90.0 to 90.0
    public void testInvalidCenterDeltaLatValues(){
	 
	       GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("#-90.0;10.4;0.1;0.45"); 
	       flag = gac.isGraphicsAreaStringValid();
	       assertFalse("center_lat - delta_lat should be >= -90.0 and center_lat + delta_lat should be <=90.0", flag);  	
//	       if(!flag){
//	           error_msg = gac.getErrorCode();
//	           assertEquals(error_msg,"center_lat - delta_lat should be >= -90.0 and center_lat + delta_lat should be <=90.0");   
//	       } 		

	       GraphicsAreaCoordinates gac2 = new GraphicsAreaCoordinates("#90.0;10.4;0.1;0.45"); 
	       flag = gac2.isGraphicsAreaStringValid();
	       assertFalse("center_lat - delta_lat should be >= -90.0 and center_lat + delta_lat should be <=90.0", flag);   
	
//	       if(!flag){
//	           error_msg = gac2.getErrorCode();
//	           assertEquals(error_msg,"center_lat - delta_lat should be >= -90.0 and center_lat + delta_lat should be <=90.0");   
//	       } 
    
      }
 
	@Test
	//Test that the Upper Right and Lower Left Longitude Values computed from the Center/Delta Latitude/Longitude Values 
	// lie in the correct range from -180.0 to 360.0   
    public void testInvalidCenterDeltaLonValues(){
	       GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("#89.0;-180;0.1;0.45"); 
	       flag = gac.isGraphicsAreaStringValid();
	       assertFalse("center_lon - delta_lon should be >= -180.0 and center_lon + delta_lon should be <=360.0", flag);   
	
//	       if(!flag){
//	           error_msg = gac.getErrorCode();
//	           assertEquals(error_msg,"center_lon - delta_lon should be >= -180.0 and center_lon + delta_lon should be <=360.0");   
//	       }	
	       
	       GraphicsAreaCoordinates gac2 = new GraphicsAreaCoordinates("#89.0;180;0.1;0.45"); 
	       flag = gac2.isGraphicsAreaStringValid();
	       assertTrue("A valid string pattern meets the logic of center_lon - delta_lon should be >= -180.0 and center_lon + delta_lon should be <=360.0", flag);   
	
//	       if(!flag){
//	           error_msg = gac2.getErrorCode();
//	           assertEquals(error_msg,"center_lon - delta_lon should be >= -180.0 and center_lon + delta_lon should be <=360.0");   
//	       }

	       GraphicsAreaCoordinates gac3 = new GraphicsAreaCoordinates("#89.0;360;0.1;0.45"); 
	       flag = gac3.isGraphicsAreaStringValid();
	       /*
	        * After convertLongitudeValue(..) logic, the center Lon=360 is converted to Lon=0, thus, 
	        * (center_lon + delta_lon=0.45)  <=360.0
	        */
	       assertTrue("After convertLongitudeValue(..) logic,, this string should be valid", flag);   
	
//	       if(!flag){
//	           error_msg = gac3.getErrorCode();
//	           assertEquals(error_msg,"center_lon - delta_lon should be >= -180.0 and center_lon + delta_lon should be <=360.0");   
//	       }	    	
    }
 
	@Test
  //Test for negative delta latitude value
    public void testNegativeDeltaLat(){

		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("#30.0;160.00;-0.761;0.45"); 
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("Delta Latitude values cannot be negative", flag);   

//		if(!flag){
//			error_msg = gac.getErrorCode();
//			assertEquals(error_msg,"Delta Latitude values cannot be negative");   
//		} 	
 	
    }

	@Test
  //Test for negative delta longitude value
   public void testNegativeDeltaLon(){

		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("#30.0;160.00;0.761;-0.45"); 
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("Delta Longitude values cannot be negative", flag);   

//		if(!flag){
//			error_msg = gac.getErrorCode();
//			assertEquals(error_msg,"Delta Longitude values cannot be negative");   
//		}	
  }
  
	@Test
   //Test if the Latitude/Longitude and Projection String Data can be retrieved from the Geog Table (geog.xml)
   // When the input string is a Geographical Area Code.
   public void testValidInputGeogTable(){

       GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("MAAR");
		flag = gac.isGraphicsAreaStringValid();
		assertTrue("MAAR is a valid name in geog.xml table", flag);   
		assertEquals(gac.getGeogAreaCode(),"MAAR");
		assertEquals(gac.getMapProjectionString(),"MER");
		double lat_lon[] = gac.getGAREACoordinates();
		assertEquals(lat_lon[0],17.00f);
		assertEquals(lat_lon[1],-63.90f);
		assertEquals(lat_lon[2],18.60f);
		assertEquals(lat_lon[3],-62.30f);	
		assertEquals(lat_lon[4],17.80f);
		assertEquals(lat_lon[5],-63.10f);			

	    GraphicsAreaCoordinates gac2 = new GraphicsAreaCoordinates("105");
		flag = gac2.isGraphicsAreaStringValid();
		assertTrue("105 is a valid name in geog.xml table", flag);   
		assertEquals(gac2.getMapProjectionString(),"STR/90;-105;0");
		lat_lon = gac2.getGAREACoordinates();
		assertEquals(lat_lon[0],17.52f);
		assertEquals(lat_lon[1],-129.30f);
		assertEquals(lat_lon[2],53.78f);
		assertEquals(lat_lon[3],-22.37f);	
		assertEquals(lat_lon[4],35.65f);
		assertEquals(lat_lon[5],-105.00f);				

		GraphicsAreaCoordinates gac3 = new GraphicsAreaCoordinates("105**");
		flag = gac3.isGraphicsAreaStringValid();
		assertTrue("105** is a valid name in geog.xml table", flag);   
		assertEquals(gac3.getMapProjectionString(),"STR/90;-105;0");
		lat_lon = gac3.getGAREACoordinates();
		assertEquals(lat_lon[0],26.585f);
		assertEquals(lat_lon[1],-102.567f);
		assertEquals(lat_lon[2],46.981f);
		assertEquals(lat_lon[3],-42.419f);	
		assertEquals(lat_lon[4],35.65f);
		assertEquals(lat_lon[5],-105.00f);	

		GraphicsAreaCoordinates gac4 = new GraphicsAreaCoordinates("105---");
		flag = gac4.isGraphicsAreaStringValid();
		assertTrue("105--- is a valid name in geog.xml table", flag);   
		assertEquals(gac4.getMapProjectionString(),"STR/90;-105;0");
		lat_lon = gac4.getGAREACoordinates();
		assertEquals(lat_lon[0],-0.609f);
		assertEquals(lat_lon[1],-182.765f);
		assertEquals(lat_lon[2],80.975f);
		assertEquals(lat_lon[3],57.827f);	
		assertEquals(lat_lon[4],35.65f);
		assertEquals(lat_lon[5],-105.00f);				
			
   }
	
	
	@Test 
	public void testInvalidInputGeogTable(){
		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("105$%&");
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("105$%& is NOT a valid name in geog.xml table", flag);   
//		if(!flag){	
//			assertEquals(gac.getGeogAreaCode(),"Invalid String Format");
//		}
	}

    @Test
    public void testValidInputStationTable(){
    	GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("LFK");
    	flag = gac.isGraphicsAreaStringValid();
    	assertTrue("LFK is a valid name in sfstas.xml station table", flag);   
    	assertEquals(gac.getStationCode(),"LFK");
    	double lat_lon[] = gac.getGAREACoordinates();
    	assertEquals(lat_lon[4],31.00f);
    	assertEquals(lat_lon[5],-94.00f);			

    	GraphicsAreaCoordinates gac2 = new GraphicsAreaCoordinates("BLI++");
    	flag = gac2.isGraphicsAreaStringValid();
    	assertTrue("BLI++ is a valid name in sfstas.xml station table", flag);   
    	assertEquals(gac2.getStationCode(),"BLI");
    	lat_lon = gac2.getGAREACoordinates();
    	assertEquals(lat_lon[0],46.00f);
    	assertEquals(lat_lon[1],-125.50f);
    	assertEquals(lat_lon[2],51.50f);
    	assertEquals(lat_lon[3],-118.50f);	
    	assertEquals(lat_lon[4],48.00f);
    	assertEquals(lat_lon[5],-122.00f);			

    	GraphicsAreaCoordinates gac3 = new GraphicsAreaCoordinates("BLI-");
    	flag = gac3.isGraphicsAreaStringValid();
    	assertTrue("BLI- is a valid name in sfstas.xml station table", flag);   
    	assertEquals(gac3.getStationCode(),"BLI");
    	lat_lon = gac3.getGAREACoordinates();
    	assertEquals(lat_lon[0],40.00f);
    	assertEquals(lat_lon[1],-136.00f);
    	assertEquals(lat_lon[2],62.00f);
    	assertEquals(lat_lon[3],-108.00f);	
    	assertEquals(lat_lon[4],48.00f);
    	assertEquals(lat_lon[5],-122.00f);			
    }
    
	@Test 
	public void testInvalidInputStationTable(){
		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("#BLI!@#$%&");
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("invalid station table name is entered", flag);
//		if(!flag){	
//			assertEquals(gac.getGeogAreaCode(),"Invalid String Format");
//		}
	}    
	
	@Test
	public void testEmptyString(){
		GraphicsAreaCoordinates gac = new GraphicsAreaCoordinates("");
		flag = gac.isGraphicsAreaStringValid();
		assertFalse("Empty String is not a valid Graphic Area Coordinate string", flag);
//		if(!flag){	
//			assertEquals(gac.getGeogAreaCode(),"Empty String");
//		}		
	}

}
