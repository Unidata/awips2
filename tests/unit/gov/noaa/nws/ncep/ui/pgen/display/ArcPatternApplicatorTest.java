/*
 * ArcPatternApplicatorTest
 * 
 * Date created: 04 DECEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * Unit test for class ArcPatternApplicator.  This class tests only the implemented classes in 
 * ArcPatternApplicator that were declared abstract in AbstractPatternApplicator.  Inherited methods
 * are tested by class AbstractPatternApplicatorTest.
 * @author sgilbert
 *
 */
public class ArcPatternApplicatorTest {

	private static LengthIndexedLine line;
	
	private final double epsilon = 0.000000001;
	
	/**
	 * Sets up a line path to use with all the tests
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		
		GeometryFactory gf = new GeometryFactory();
		Coordinate[] coords = new Coordinate[] { new Coordinate(0.0,0.0),
				                                 new Coordinate(0.0,4.0),
				                                 new Coordinate(2.0,6.0),
				                                 new Coordinate(4.0,4.0),
				                                 new Coordinate(4.0,0.0),
				                                 new Coordinate(0.0,0.0) };
		LineString path = gf.createLineString(coords);
		line = new LengthIndexedLine(path);
		
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.ui.pgen.display.ArcPatternApplicator#calculateFillArea()}.
	 */
	@Test
	public void testCalculateFillArea() {

		/*
		 * Create arc pattern applicator for segment length 1 to 3
		 * and calculate fill area.  
		 */
		ArcPatternApplicator arcapp = new ArcPatternApplicator(line,1.0,3.0);
		arcapp.setArcAttributes(0.0, 180.0, 2);
		arcapp.addSegmentToFill(true);
		Coordinate[] fill = arcapp.calculateFillArea();

		// result should be:
		Coordinate[] result = new Coordinate[] { new Coordinate(0.0,1.0),  new Coordinate(0.0,3.0),
				                                 new Coordinate(0.0,3.0),  new Coordinate(-1.0,2.0),
				                                 new Coordinate(0.0,1.0),  new Coordinate(0.0, 1.0)  };

		// Compare coordinates
		assertEquals("TEST1: Different number of points", result.length, fill.length);
		for (int i=0; i<result.length; i++) {
			String message="TEST1 for point "+i+" x-coord failed:";
			assertEquals(message, result[i].x, fill[i].x, epsilon);
			message="TEST1 for point "+i+" y-coord failed:";
			assertEquals(message, result[i].y, fill[i].y, epsilon);
		}

	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.ui.pgen.display.ArcPatternApplicator#calculateLines()}.
	 */
	@Test
	public void testCalculateLines() {
		
		/*
		 * Create Arc pattern applicator for segment 
		 * and calculate circle at 4 points
		 */
		ArcPatternApplicator arcapp = new ArcPatternApplicator(line, 4.0, 4.0+4.0*Math.sqrt(2.0) );
		arcapp.setArcAttributes(0.0, 360.0, 4);
		double[][] pts = arcapp.calculateLines();

		// result should be:
		double[][] exp = new double[][] { {4.0, 4.0, 0.0}, {2.0, 6.0, 0.0},
				                             {0.0, 4.0, 0.0}, {2.0, 2.0, 0.0},  
				                             {4.0, 4.0, 0.0}  };

		// Compare coordinates
		assertEquals("TEST1: Different number of points", exp.length, pts.length);
		for ( int i=0; i <exp.length; i++) {
			for ( int j=0; j <3; j++) {
				String message = "TEST1 - Comparing "+ i + "th point - position " + j +":";
				assertEquals(message, exp[i][j], pts[i][j], epsilon);
			}
		}
		
		/*
		 * check negative angle
		 */
		arcapp.setStartLoc(-8.0);
		arcapp.setEndLoc(-4.0);
		arcapp.setArcAttributes(0.0, -180., 2);
		pts = arcapp.calculateLines();

		// result should be:
		exp = new double[][] { {4.0, 0.0, 0.0}, {2.0, 2.0, 0.0},
				                             {4.0, 4.0, 0.0}  };

		// Compare coordinates
		assertEquals("TEST2: Different number of points", exp.length, pts.length);
		for ( int i=0; i <exp.length; i++) {
			for ( int j=0; j <3; j++) {
				String message = "TEST2 - Comparing "+ i + "th point - position " + j +":";
				assertEquals(message, exp[i][j], pts[i][j], epsilon);
			}
		}
		
		
	}

}
