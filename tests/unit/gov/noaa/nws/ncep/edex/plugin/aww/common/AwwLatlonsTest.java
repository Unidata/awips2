/**
 * This Java class is the JUnit test for the AwwLatlons.
 *
 * <pre>
 *
 * L. Lin       04/09   Creation
 * </pre>
 *
 */
package gov.noaa.nws.ncep.edex.plugin.aww.common;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwLatlons;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.edex.plugin.aww.util.AwwParser;
import java.util.Iterator;
import org.junit.Test;
import java.util.ArrayList;


public class AwwLatlonsTest {

	private final String testLatlons = "LAT...LON 4257 8255 4255 8265 4264 8269 4265 8268";

	AwwUgc testUgc = new AwwUgc();
	
	AwwLatlons ll = new AwwLatlons();
	
	public void setUp() throws Exception {
	}
	
	@Test
	public void testProcessLatlons() {
		
		ArrayList<Float> flatList = new ArrayList<Float>();
		ArrayList<Float> flonList = new ArrayList<Float>();
		
		int[] latlonIndex = new int[1];
		
		latlonIndex[0] = 0;
		
		flatList.add((float)(4257/100.0));
		flonList.add((float)(-8255/100.0));
		flatList.add((float)(4255/100.0));
		flonList.add((float)(-8265/100.0));
		flatList.add((float)(4264/100.0));
		flonList.add((float)(-8269/100.0));
		flatList.add((float)(4265/100.0));
		flonList.add((float)(-8268/100.0));
		
		AwwParser.processLatlons(testLatlons, testUgc, latlonIndex);
		
		// test the county fips
		if (testUgc.getAwwLatLon() !=null && testUgc.getAwwLatLon().size() >0) {
			for (Iterator<AwwLatlons> iter = testUgc.getAwwLatLon().iterator(); iter.hasNext();) {
	            AwwLatlons cond = iter.next();
	            assertTrue(flatList.contains(cond.getLat()));
	            assertTrue(flonList.contains(cond.getLon()));
	         }
		}
		
	}

}
