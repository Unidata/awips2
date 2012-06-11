package gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.MapProjection;

import org.junit.Test;

/**
{@link gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.MapProjection}.
*/
public class MapProjectionTest {

	@Test
	public void testIsProjectionStringValid() {

		MapProjection map_obj = new MapProjection("LCC");
		assertEquals(map_obj.isProjectionStringValid(),true);
		Integer[] margins = map_obj.getProjectionMargins();
	    assertEquals(margins[0],0);
	    assertEquals(margins[1],3);
	    assertEquals(margins[2],0);
	    assertEquals(margins[3],0);
	    Float[] angles = map_obj.getProjectionAngles();
	    assertEquals(angles[0],0);
	    assertEquals(angles[1],0);
	    assertEquals(angles[2],0);	    

	}

	@Test
	public void testSetProjectionString() {
		MapProjection map_obj = new MapProjection("DEF");
		assertEquals(map_obj.getProjectionString(),"DEF");
	}

	@Test
	public void testGetProjectionString() {
		MapProjection m4 = new MapProjection("GNO/40.5;50.5;60.5/5;6;7;8");
    	assertEquals(m4.getProjectionString(),"GNO");
	}

	@Test
	public void testGetProjectionClass() {
		MapProjection m1 = new MapProjection("CED/90.0;-90.0;-360.0/2;4;6;8");
		assertEquals(m1.getProjectionClass(),"CYL");
		MapProjection m2 = new MapProjection("SCC/90.0;-90.0;-360.0/2;4;6;8");
		assertEquals(m2.getProjectionClass(),"CON");
		MapProjection m3 = new MapProjection("STR/-90.0;90.0;360.0/NM");
		assertEquals(m3.getProjectionClass(),"AZM");
	}

}
