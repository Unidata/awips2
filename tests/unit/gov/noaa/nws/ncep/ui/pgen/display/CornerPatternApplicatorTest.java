/*
 * CornerPatternApplicatorTest
 * 
 * Date created: 24 NOVEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.ui.pgen.display.CornerPatternApplicator.CornerPattern;

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
 * Unit test for class CornerPatternApplicator.  This class tests only the implemented classes in 
 * CornerPatternApplicator that were declared abstract in AbstractPatternApplicator.  Inherited methods
 * are tested by class AbstractPatternApplicatorTest.
 * @author sgilbert
 *
 */
public class CornerPatternApplicatorTest {

	private final double epsilon = 0.000000001;

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
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
	 * Test method for {@link gov.noaa.nws.ncep.ui.pgen.display.CornerPatternApplicator#calculateFillArea()}.
	 */
	@Test
	public void testCalculateFillArea() {

		//create a new line path
		GeometryFactory gf = new GeometryFactory();
		Coordinate[] coords = new Coordinate[] { new Coordinate(0.0,0.0),
				                                 new Coordinate(0.0,4.0),
				                                 new Coordinate(2.0,6.0),
				                                 new Coordinate(4.0,4.0),
				                                 new Coordinate(4.0,0.0),
				                                 new Coordinate(0.0,0.0) };
		LineString path = gf.createLineString(coords);
		LengthIndexedLine line = new LengthIndexedLine(path);
		
		// Create corner pattern applicator for segment length 1 to 3
		// and calculate box.  Testing vertical segment
		CornerPatternApplicator boxapp = new CornerPatternApplicator(line,1.0,3.0);
		boxapp.setHeight(2.0);
		boxapp.setPatternType(CornerPattern.BOX);
		Coordinate[] box = boxapp.calculateFillArea();

		// result should be:
		Coordinate[] result = new Coordinate[] { new Coordinate(-2.0,1.0), new Coordinate(2.0,1.0),
				                                 new Coordinate(2.0,3.0),  new Coordinate(-2.0,3.0),
				                                 new Coordinate(-2.0,1.0)  };

		// Compare coordinates
		for (int i=0; i<result.length; i++) {
			String message="TEST1 for point "+i+" x-coord failed:";
			assertEquals(message, result[i].x, box[i].x, epsilon);
			message="TEST1 for point "+i+" y-coord failed:";
			assertEquals(message, result[i].y, box[i].y, epsilon);
		}
	
		// test horizontal segment  with BOX
		boxapp.setHeight(1.0);
		boxapp.setStartLoc(4.0+Math.sqrt(2.0));
		boxapp.setEndLoc(4.0+3.0*Math.sqrt(2.0));
		boxapp.setPatternType(CornerPattern.BOX);
		box = boxapp.calculateFillArea();

		// result should be:
		result = new Coordinate[] { new Coordinate(1.0,6.0), new Coordinate(1.0,4.0),
				   new Coordinate(3.0,4.0),  new Coordinate(3.0,6.0),
				   new Coordinate(1.0,6.0) };

		// Compare coordinates
		for (int i=0; i<result.length; i++) {
			String message="TEST2 for point "+i+" x-coord failed:";
			assertEquals(message, result[i].x, box[i].x, epsilon);
			message="TEST2 for point "+i+" y-coord failed:";
			assertEquals(message, result[i].y, box[i].y, epsilon);
		}

	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.ui.pgen.display.CornerPatternApplicator#calculateLines()}.
	 */
	@Test
	public void testCalculateLines() {

		//create a new line path
		GeometryFactory gf = new GeometryFactory();
		Coordinate[] coords = new Coordinate[] { new Coordinate(0.0,0.0),
				                                 new Coordinate(0.0,4.0),
				                                 new Coordinate(2.0,6.0),
				                                 new Coordinate(4.0,4.0),
				                                 new Coordinate(4.0,0.0),
				                                 new Coordinate(0.0,0.0) };
		LineString path = gf.createLineString(coords);
		LengthIndexedLine line = new LengthIndexedLine(path);
		
		// Create Corner pattern applicator for segment 
		// and calculate double line.  Testing 45 degree angle segment
		CornerPatternApplicator boxapp = new CornerPatternApplicator(line, 4.0+2.0*Math.sqrt(2.0), -8.0);
		boxapp.setHeight(Math.sqrt(2.0));
		boxapp.setPatternType(CornerPattern.DOUBLE_LINE);
		double[][] pts = boxapp.calculateLines();

		// result should be:
		double[][] exp = new double[][] { {3.0, 7.0, 0.0}, {5.0, 5.0, 0.0},
				                             {1.0, 5.0, 0.0}, {3.0, 3.0, 0.0}  };

		// Compare coordinates
		assertEquals("Different number of points", exp.length, pts.length);
		for ( int i=0; i <exp.length; i++) {
			for ( int j=0; j <3; j++) {
				String message = "TEST3 - Comparing "+ i + "th point - position " + j +":";
				assertEquals(message, exp[i][j], pts[i][j], epsilon);
			}
		}

		//  Z_PATTERN should return the same points as double line!
		boxapp.setPatternType(CornerPattern.Z_PATTERN);
		pts = boxapp.calculateLines();

		// result should be:
		exp = new double[][] { {3.0, 7.0, 0.0}, {5.0, 5.0, 0.0},
				                             {1.0, 5.0, 0.0}, {3.0, 3.0, 0.0}  };

		// Compare coordinates
		assertEquals("Different number of points", exp.length, pts.length);
		for ( int i=0; i <exp.length; i++) {
			for ( int j=0; j <3; j++) {
				String message = "TEST4 - Comparing "+ i + "th point - position " + j +":";
				assertEquals(message, exp[i][j], pts[i][j], epsilon);
			}
		}
	
		// Test 30-60-90 segment using X_PATTERN
		boxapp.setHeight(5.0);
		boxapp.setStartLoc(0.0);
		boxapp.setEndLoc(-7.0);
		boxapp.setPatternType(CornerPattern.X_PATTERN);
		pts = boxapp.calculateLines();

		// result should be:
		exp = new double[][] { {-3.0, 4.0, 0.0}, {7.0, -1.0, 0.0},
                               {3.0, -4.0, 0.0}, {1.0, 7.0, 0.0}  };

		// Compare coordinates
		assertEquals("Different number of points", exp.length, pts.length);
		for ( int i=0; i <exp.length; i++) {
			for ( int j=0; j <3; j++) {
				String message = "TEST5 - Comparing "+ i + "th point - position " + j +":";
				assertEquals(message, exp[i][j], pts[i][j], epsilon);
			}
		}

		// Test segment using TICK
		boxapp.setHeight(1.5);
		boxapp.setStartLoc(4.0);
		boxapp.setEndLoc(-8.0);
		boxapp.setPatternType(CornerPattern.TICK);
		pts = boxapp.calculateLines();

		// result should be:
		exp = new double[][] { {4.0, 4.0, 0.0}, {4.0, 2.5, 0.0}  };

		// Compare coordinates
		assertEquals("Different number of points", exp.length, pts.length);
		for ( int i=0; i <exp.length; i++) {
			for ( int j=0; j <3; j++) {
				String message = "TEST5 - Comparing "+ i + "th point - position " + j +":";
				assertEquals(message, exp[i][j], pts[i][j], epsilon);
			}
		}

	}

}
