/**
 * 
 */
package gov.noaa.nws.ncep.viz.ui.locator.resource;



import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.geotools.feature.IllegalAttributeException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author mli
 *
 */
public class PointDataResourceTest {

	private PointDataResource mypoint; 
	
	private Coordinate ll;
	
	private String output;
	
	private DisplayOptions displayOption;
	
	/**
	 * @throws java.lang.Exception
	 */
	
	@Before
	public void setUp() throws Exception {
		 mypoint = new PointDataResource(
		    		"/usr1/mli/TO10/workspace/build.cave/static/common/cave/basemaps/cities/cities.shp", "NAME");
		 displayOption = new DisplayOptions();
		 displayOption.setRoundingToNearest(1);
		 displayOption.setDistanceUnit(LocatorTool.DISTANCEUNIT_OPTIONS[2]);
 		 displayOption.setDirectionUnit(LocatorTool.DIRECTIONUNIT_OPTIONS[1]);
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	@Test
	public final void testDistanceAndDir() throws RuntimeException, 
			IllegalAttributeException, Exception {
		
		// point with distance and direction
        ll = new Coordinate(-92.71, 39.96);
        output = mypoint.getNearestPoint(ll, displayOption);        
        assertEquals("3 W ELMER", output);
        
        // right on the point, no distance and direction
        ll = new Coordinate(-76.85, 39.27);
        output = mypoint.getNearestPoint(ll, displayOption);        
        //assertEquals("ELLICOTT CITY", output);
        
        // test special points
        ll = new Coordinate(0.0, 0.0);
        assertNotNull(mypoint.getNearestPoint(ll, displayOption));
        ll = new Coordinate(0.0, 90);
        assertNotNull(mypoint.getNearestPoint(ll, displayOption));
        ll = new Coordinate(0.0, -90);
        assertNotNull(mypoint.getNearestPoint(ll, displayOption));
        ll = new Coordinate(180, 0.0);
        assertNotNull(mypoint.getNearestPoint(ll, displayOption));
        ll = new Coordinate(-180, 90.0);
        assertNotNull(mypoint.getNearestPoint(ll, displayOption));
        
        // test invalid points
        ll = new Coordinate(0.0, 120.0);
        assertNull(mypoint.getNearestPoint(ll, displayOption));
        ll = new Coordinate(-999.99, -999.99);
        assertNull(mypoint.getNearestPoint(ll, displayOption));
        
	}
}
