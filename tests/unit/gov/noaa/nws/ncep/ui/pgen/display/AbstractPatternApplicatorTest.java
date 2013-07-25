/*
 * AbstractPatternApplicatorTest
 * 
 * Date created: 17 NOVEMBER 2008
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
 * This class tests the only the implemented methods in abstract class AbstractPatternApplicator
 * <P>
 * To do this, an internal class, DefaultPatternApplicator, is defined that extends the 
 * AbstractPatternApplicator class and provides a "null" implementation of the abstract methods.
 * This should be no problem, since we are not testing the abstract methods here.
 * 
 * @author sgilbert
 *
 */
public class AbstractPatternApplicatorTest {

	/*
	 * need a concrete class to test with.  No need to implement abstract methods.
	 */
	class DefaultPatternApplicator extends AbstractPatternApplicator {
		DefaultPatternApplicator(LengthIndexedLine line) { super(line); }
		DefaultPatternApplicator(LengthIndexedLine line, double start, double end) { super(line,start,end); }
		public Coordinate[] calculateFillArea() { return null; }
		public double[][] calculateLines() { return null; }
	}

	private static LengthIndexedLine line;
	
	private final double epsilon = 0.000000001;
	
	/**
	 * Sets up a line path to use with all the tests. 
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
	 * Test method for {@link gov.noaa.nws.ncep.ui.pgen.display.AbstractPatternApplicator#getStartPoint()}.
	 */
	@Test
	public void testGetStartPoint() {
		Coordinate test;
		
		DefaultPatternApplicator dpa = new DefaultPatternApplicator(line);
		
		test = dpa.getStartPoint();
		assertTrue("Test 1 failed", test.equals2D(new Coordinate(0.0,0.0)));
		
		dpa.setStartLoc(4.0);
		test = dpa.getStartPoint();
		assertTrue("Test 2 failed", test.equals2D(new Coordinate(0.0,4.0)));

		dpa.setStartLoc(4.0*(1.0+Math.sqrt(2.0)));
		test = dpa.getStartPoint();
		assertTrue("Test 3 failed", test.equals2D(new Coordinate(4.0,4.0)));

		// Setting start point to negative location, is same as setting distance from end of the
		// line instead of the beginning.
		dpa.setStartLoc(-8.0);
		test = dpa.getStartPoint();
		assertTrue("Test 4 failed", test.equals2D(new Coordinate(4.0,4.0)));
		
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.ui.pgen.display.AbstractPatternApplicator#getEndPoint()}.
	 */
	@Test
	public void testGetEndPoint() {
		Coordinate test;
		
		DefaultPatternApplicator dpa = new DefaultPatternApplicator(line);
		
		test = dpa.getEndPoint();
		assertTrue("Test 1 failed.", test.equals2D(new Coordinate(0.0,0.0)));
		
		dpa.setEndLoc(4.0+2.0*Math.sqrt(2.0));
		test = dpa.getEndPoint();
		assertTrue("Test 2 failed", test.equals2D(new Coordinate(2.0,6.0)));

		// Setting endpoint before start point is still valid, just reverses direction
		dpa.setEndLoc(0.0);
		dpa.setStartLoc(4.0);
		test = dpa.getEndPoint();
		assertTrue("Test 3 failed", test.equals2D(new Coordinate(0.0,0.0)));
		
		// Setting endpoint to location longer than line path should crop back to end of line
		dpa.setStartLoc(100.0);
		test = dpa.getEndPoint();
		assertTrue("Test 4 failed", test.equals2D(new Coordinate(0.0,0.0)));
		
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.ui.pgen.display.AbstractPatternApplicator#getMidpoint()}.
	 */
	@Test
	public void testGetMidpoint() {
		Coordinate midpoint;
		
		DefaultPatternApplicator dpa = new DefaultPatternApplicator(line);

		dpa.setStartLoc(4.0);
		dpa.setEndLoc(4.0*(1.0+Math.sqrt(2.0)));
		midpoint = dpa.getMidpoint();
		assertTrue("Test 1 failed.", midpoint.equals2D(new Coordinate(2.0,4.0)));
		
		dpa.setEndLoc(4.0*(2+Math.sqrt(2.0)));
		midpoint = dpa.getMidpoint();
		assertTrue("Test 2 failed.", midpoint.equals2D(new Coordinate(2.0,2.0)));
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.ui.pgen.display.AbstractPatternApplicator#getDistance()}.
	 */
	@Test
	public void testGetDistance() {
		double dist;

		DefaultPatternApplicator dpa = new DefaultPatternApplicator(line);
		
		dist = dpa.getDistance();
		assertEquals("Test 1 failed.", 0.0, dist, epsilon);

		dpa.setStartLoc(4.0);
		dpa.setEndLoc(4.0+(2.0*Math.sqrt(2.0)));
		dist = dpa.getDistance();
		assertEquals("Test 1 failed.", 2*Math.sqrt(2.0), dist, epsilon);
		
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.ui.pgen.display.AbstractPatternApplicator#getSegmentAngle()}.
	 */
	@Test
	public void testGetSegmentAngle() {
		double angle;
		
		DefaultPatternApplicator dpa = new DefaultPatternApplicator(line);

		angle = dpa.getSegmentAngle();
		assertEquals("Test 1 failed.", 0.0, angle, epsilon);

		dpa.setEndLoc(4.0);
		angle = dpa.getSegmentAngle();
		assertEquals("Test 2 failed", 90.0, angle, epsilon);

		dpa.setStartLoc(4.0+2.0*Math.sqrt(2.0));
		dpa.setEndLoc(4.0+4.0*Math.sqrt(2.0));
		angle = dpa.getSegmentAngle();
		assertEquals("Test 3 failed", -45.0, angle, epsilon);

		dpa.setStartLoc(4.0);
		angle = dpa.getSegmentAngle();
		assertEquals("Test 4 failed", 0.0, angle, epsilon);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.ui.pgen.display.AbstractPatternApplicator#getSegmentPath()}.
	 */
	@Test
	public void testGetSegmentPath() {
		Coordinate[] path, coords;

		DefaultPatternApplicator dpa = new DefaultPatternApplicator(line);
		
		path = dpa.getSegmentPath();
		coords = new Coordinate[] { new Coordinate(0.0,0.0),
                                    new Coordinate(0.0,4.0),
                                    new Coordinate(2.0,6.0),
                                    new Coordinate(4.0,4.0),
                                    new Coordinate(4.0,0.0),
                                    new Coordinate(0.0,0.0) };
		assertArrayEquals("Test 1 failed.", coords, path);
		
		dpa.setStartLoc(2.0);
		dpa.setEndLoc(6.0 + 4.0*Math.sqrt(2.0));
		path = dpa.getSegmentPath();
		coords = new Coordinate[] { new Coordinate(0.0,2.0),
									new Coordinate(0.0,4.0),
                                    new Coordinate(2.0,6.0),
                                    new Coordinate(4.0,4.0),
                                    new Coordinate(4.0,2.0) };
		assertArrayEquals("Test 2 failed.", coords, path);
		
		// Test endloc before startLoc to make sure points are returned in reverse order
		dpa.setStartLoc(-6.0);
		dpa.setEndLoc(4.0 + 2.0*Math.sqrt(2.0));
		path = dpa.getSegmentPath();
		coords = new Coordinate[] { new Coordinate(4.0,2.0),
									new Coordinate(4.0,4.0),
                                    new Coordinate(2.0,6.0) };
		assertArrayEquals("Test 3 failed.", coords, path);

	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.ui.pgen.display.AbstractPatternApplicator#getSegmentPts()}.
	 */
	@Test
	public void testGetSegmentPts() {
		double[][] pts, exp;
		
		DefaultPatternApplicator dpa = new DefaultPatternApplicator(line);
		
		dpa.setEndLoc(4.0);
		exp = new double[][] { {0.0, 0.0, 0.0}, 
		 		   		       {0.0, 4.0, 0.0} };
		pts = dpa.getSegmentPts();
		assertEquals("Different number of points", exp.length, pts.length);
		for ( int i=0; i <exp.length; i++) {
			for ( int j=0; j <3; j++) {
				String message = "TEST1 - Comparing "+ i + "th point - position " + j +":";
				assertEquals(message, exp[i][j], pts[i][j], epsilon);
			}
		}

		// test start and end point are same.  Should get back 2 point segment with both points equal 
		dpa.setStartLoc(4.0);
		dpa.setEndLoc(4.0);
		exp = new double[][] { {0.0, 4.0, 0.0},
				               {0.0, 4.0, 0.0} };
		pts = dpa.getSegmentPts();
		assertEquals("Different number of points", exp.length, pts.length);
		for ( int i=0; i <exp.length; i++) {
			for ( int j=0; j <3; j++) {
				String message = "TEST2 - Comparing "+ i + "th point - position " + j +":";
				assertEquals(message, exp[i][j], pts[i][j], epsilon);
			}
		}
		
	}

}
