/**
 * 
 */
package gov.noaa.nws.ncep.viz.common.graphicUtil;

/**
 * Test Reduce points of a polygon by examining the angle at each point.
 * 
 * @author Q.Zhou
 * @version 1.0
 */
import static org.junit.Assert.*;


import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;

/**
 * @author qzhou
 *
 */
public class ReducePointsTest {
	
	public ReducePointsTest() {
		
	}
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
	 * Test method for {@link gov.noaa.nws.ncep.viz.common.graphicUtil.ReducePoints#reduceByAngle(com.vividsolutions.jts.geom.CoordinateList, java.util.List, int)}.
	 */
	@Test
	public void testReduceByAngle1() {
		// TODO
		Coordinate[] coord = {new Coordinate(1,1), new Coordinate(1,-1), new Coordinate(0,-2), new Coordinate(-1,-1), new Coordinate(-1,1)};
		CoordinateList cl= new CoordinateList(coord);
		Coordinate[] coordAfter = { new Coordinate(1,1), new Coordinate(0,-2), new Coordinate(-1,1)}; 
		CoordinateList clAfter= new CoordinateList(coordAfter);		
//		List<Integer> reduceFlg = new ArrayList<Integer>();
//		for (int i=0; i<cl.size(); i++)
//			reduceFlg.add(i, 1);
		
		CoordinateList result = ReducePoints.reduceByAngle(cl, null, 3);		
		assertEquals("Result", clAfter, cl);
		System.out.println(result);
	}
	
	@Test
	public void testReduceByAngle2() {
		Coordinate[] coord2 = {new Coordinate(1,1), new Coordinate(1,0), new Coordinate(1,-1), new Coordinate(0,-1), 
				new Coordinate(-1,-1), new Coordinate(-1,0), new Coordinate(-1,1), new Coordinate(0,1)};
		CoordinateList cl2 = new CoordinateList(coord2);
		Coordinate[] coordAfter2 = {  new Coordinate(1,-1), new Coordinate(-1,-1),  new Coordinate(-1,1) }; 
		CoordinateList clAfter2 = new CoordinateList(coordAfter2);		
		
		CoordinateList result = ReducePoints.reduceByAngle(cl2, null, 3);		
		assertEquals("Result", clAfter2, cl2);
		System.out.println(result);
	}
	
	@Test
	public void testReduceByAngle3() {
		Coordinate[] coord3 = {new Coordinate(1,1), new Coordinate(1,1), new Coordinate(1,-1), new Coordinate(1,-1), 
				new Coordinate(0,-2), new Coordinate(-1,-1), new Coordinate(-1,1)};
		CoordinateList cl3 = new CoordinateList(coord3);
		Coordinate[] coordAfter3 = { new Coordinate(1,1),  
				new Coordinate(0,-2), new Coordinate(-1,1)}; 
		CoordinateList clAfter3 = new CoordinateList(coordAfter3);		
		
		CoordinateList result = ReducePoints.reduceByAngle(cl3, null, 3);		
		assertEquals("Result", clAfter3, cl3);
		System.out.println(result);
	}
		
	@Test
	public void testReduceByAngle4() {	
		Coordinate[] coord3 = {new Coordinate(1,1), new Coordinate(1,1), new Coordinate(1,-1), new Coordinate(1,-1), 
				new Coordinate(0,-1), new Coordinate(0,-1), new Coordinate(-1,-1), new Coordinate(-1,-1),
				new Coordinate(-1,1), new Coordinate(-1,1)};
		CoordinateList cl3 = new CoordinateList(coord3);
		Coordinate[] coordAfter3 = {  new Coordinate(1,1),  new Coordinate(1,-1), 
				new Coordinate(-1,-1), 
				new Coordinate(-1,1), new Coordinate(-1,1)}; 
		CoordinateList clAfter3 = new CoordinateList(coordAfter3);		
		
		CoordinateList result = ReducePoints.reduceByAngle(cl3, null, 5);		
		assertEquals("Result", clAfter3, cl3);
		System.out.println(result);
	}
	
	@Test
	public void testReduceByAngle5() {
		Coordinate[] coord3 = {new Coordinate(20,20), new Coordinate(23,0), new Coordinate(20,-20), new Coordinate(0,-22), 
				new Coordinate(-20,-20), new Coordinate(-24,0), new Coordinate(-20,20), new Coordinate(0,21)};
		CoordinateList cl3 = new CoordinateList(coord3);
		Coordinate[] coordAfter3 = {  new Coordinate(20,20), new Coordinate(23,0), new Coordinate(20,-20), 
				new Coordinate(-20,-20), new Coordinate(-24,0), new Coordinate(-20,20)}; 
		CoordinateList clAfter3 = new CoordinateList(coordAfter3);		
		
		CoordinateList result = ReducePoints.reduceByAngle(cl3, null, 6);		
		assertEquals("Result", clAfter3, cl3);
		System.out.println(result);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.viz.common.graphicUtil.ReducePoints#reduceBySize(com.vividsolutions.jts.geom.CoordinateList, java.util.List, int, double, double)}.
	 */
//	@Test
//	public final void testReduceBySize() {
//		// TODO
//		Coordinate[] coord = {new Coordinate(1,1), new Coordinate(1,-1), new Coordinate(0,-2), new Coordinate(-1,-1), new Coordinate(-1,1)};
//		CoordinateList cl= new CoordinateList(coord);
//		
//		List<Integer> reduceFlg = new ArrayList<Integer>();
//		for (int i=0; i<cl.size(); i++)
//			reduceFlg.add(1);
//		List<Integer> orig = new ArrayList<Integer>();
//		
//		Coordinate[] coordAfter = { new Coordinate(1.6666666666666667,1.0), new Coordinate(0.12499999999999996,-3.625), new Coordinate(-1.8571428571428572,1.0)}; 
//		CoordinateList clAfter= new CoordinateList(coordAfter);
//
//		CoordinateList result = ReducePoints.reduceBySize(cl, null, orig, 3, 60.0, 100.0);		
//		assertEquals("Result", clAfter, cl);
//	}
//
//	@Test
//	public final void testReduceBySize2() {
//		// TODO
//		Coordinate[] coord = {new Coordinate(1,1), new Coordinate(1,0), new Coordinate(1,-1), new Coordinate(0,-1), 
//				new Coordinate(-1,-1), new Coordinate(-1,0), new Coordinate(-1,1), new Coordinate(0,1)};
//		CoordinateList cl= new CoordinateList(coord);
//		
//		List<Integer> reduceFlg = new ArrayList<Integer>();
//		for (int i=0; i<cl.size(); i++)
//			reduceFlg.add(1);
//		List<Integer> orig = new ArrayList<Integer>();
//		
//		Coordinate[] coordAfter = { new Coordinate(3,-1), new Coordinate(-1,-1),  new Coordinate(-1,3)}; 
//		CoordinateList clAfter= new CoordinateList(coordAfter);
//
//		CoordinateList result = ReducePoints.reduceBySize(cl, null, orig, 3, 60.0, 150.0);		
//		assertEquals("Result", clAfter, cl);
//		
//	}
	
	/**
	 * Test method for {@link gov.noaa.nws.ncep.viz.common.graphicUtil.ReducePoints#reduceByPctDist(com.vividsolutions.jts.geom.CoordinateList, java.util.List, java.util.List, double, double, double, java.lang.String)}.
	 */
//	@Test
//	public final void testReduceByPctDist() {
//		// TODO //set LENFROM=35
//		Coordinate[] coord = {new Coordinate(-110.57, 54.72), new Coordinate(-100.19, 54.54), new Coordinate(-89.37,48.92), 
//				new Coordinate(-96.65,41.84), new Coordinate(-103.69,37.25), new Coordinate(-113.93,38.34),
//				new Coordinate(-117.95,42.4), new Coordinate(-118.43,47.81), new Coordinate(-115.17,52.63)};
//		CoordinateList cl= new CoordinateList(coord);
//		
//		List<Integer> reduceFlg = new ArrayList<Integer>();
//		for (int i=0; i<cl.size(); i++)
//			reduceFlg.add(1);
//		List<Integer> orig = new ArrayList<Integer>();
//		
//		Coordinate[] coordAfter = { new Coordinate(-110.57, 54.72), new Coordinate(-100.19, 54.54), new Coordinate(-89.37,48.92), 
//				new Coordinate(-96.65,41.84), new Coordinate(-103.69,37.25), new Coordinate(-113.93,38.34),
//				new Coordinate(-117.95,42.4), new Coordinate(-118.43,47.81), new Coordinate(-115.17,52.63)};
//		CoordinateList clAfter= new CoordinateList(coordAfter);
//
//		CoordinateList result = ReducePoints.reduceByPctDist(cl, null, orig, 60.0, 80.0, 450.0, "BOUNDED BY");		
//		assertEquals("Result", clAfter, cl);
//	}
	
//	@Test
//	public final void testReduceByPctDist2() {
//		// TODO //set LENFROM=23
//		Coordinate[] coord = { new Coordinate(-90,40), new Coordinate(-96,33), 
//				new Coordinate(-155,30), new Coordinate(-177,33), new Coordinate(-100,50)};
//		CoordinateList cl= new CoordinateList(coord);
//		
//		List<Integer> reduceFlg = new ArrayList<Integer>();
//		for (int i=0; i<cl.size(); i++)
//			reduceFlg.add(1);
//		List<Integer> orig = new ArrayList<Integer>();
//		
//		Coordinate[] coordAfter = { new Coordinate(-92.7, 51.6), new Coordinate(-88.4, 33.4), new Coordinate(-155,30), new Coordinate(-177,33)}; 
//		CoordinateList clAfter= new CoordinateList(coordAfter);
//
//		CoordinateList result = ReducePoints.reduceByPctDist(cl, null, orig, 60.0, 80.0, 450.0, "BOUNDED BY");		
//		assertEquals("Result", clAfter, cl);
//	}
	
//	@Test
//	public final void testReduceByPctDist3() {
//		// TODO FROM 4400ESE YSJ TO 4480ESE YSJ TO 		4490ESE YSJ TO 4400ESE YSJ TO     	4320ESE YSJ TO 4400ESE YSJ
//		Coordinate[] coord = {new Coordinate(1,1), new Coordinate(1,-1), new Coordinate(0,-2), new Coordinate(-1,-1), new Coordinate(-1,1)};
//		CoordinateList cl= new CoordinateList(coord);
//		
//		List<Integer> reduceFlg = new ArrayList<Integer>();
//		for (int i=0; i<cl.size(); i++)
//			reduceFlg.add(1);
//		List<Integer> orig = new ArrayList<Integer>();
//		
//		Coordinate[] coordAfter = { new Coordinate(1.6666666666666667,1.0), new Coordinate(0.12499999999999996,-3.625), new Coordinate(-1.8571428571428572,1.0)}; //1,-1
//		CoordinateList clAfter= new CoordinateList(coordAfter);
//
//		CoordinateList result = ReducePoints.reduceByPctDist(cl, null, orig, 60.0, 80.0, 150.0, "FROM");		
//		assertEquals("Result", clAfter, cl);
//	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.viz.common.graphicUtil.ReducePoints#reduceKeepConcav(com.vividsolutions.jts.geom.CoordinateList, java.util.List, java.util.List, double, double, java.lang.String)}.
	 * @throws JAXBException 
	 */
//	@Test
//	public final void testReduceKeepConcav() {
//		// TODO //to test, set LENFROM=23
//		Coordinate[] coord = {new Coordinate(-100,50), new Coordinate(-90,40), new Coordinate(-96,33), new Coordinate(-155,30), new Coordinate(-177,33)};
//		CoordinateList cl= new CoordinateList(coord);
//		
//		List<Integer> reduceFlg = new ArrayList<Integer>();
//		for (int i=0; i<cl.size(); i++)
//			reduceFlg.add(1);
//		List<Integer> orig = new ArrayList<Integer>();
//		
//		Coordinate[] coordAfter = { new Coordinate(-92.7, 51.6), new Coordinate(-88.4, 33.4), new Coordinate(-155,30), new Coordinate(-177,33)}; 
//		CoordinateList clAfter= new CoordinateList(coordAfter);
//
//		CoordinateList result = ReducePoints.reduceKeepConcav(cl, null, orig, 60.0, 450.0, "FROM");		
//		assertEquals("Result", clAfter, cl);
//		
//	}

//	@Test
//	public final void testReduceKeepConcav2() {
//		// TODO
//		Coordinate[] coord = {new Coordinate(-110.57, 54.72), new Coordinate(-100.19, 54.54), new Coordinate(-89.37,48.92), 
//				new Coordinate(-96.65,41.84), new Coordinate(-103.69,37.25), new Coordinate(-113.93,38.34),
//				new Coordinate(-117.95,42.4), new Coordinate(-118.43,47.81), new Coordinate(-115.17,52.63)};
//		CoordinateList cl= new CoordinateList(coord);
//		
//		List<Integer> reduceFlg = new ArrayList<Integer>();
//		for (int i=0; i<cl.size(); i++)
//			reduceFlg.add(1);
//		List<Integer> orig = new ArrayList<Integer>();
//		
//		Coordinate[] coordAfter = { new Coordinate(-112.75, 54.76), new Coordinate(-100.19, 54.54), new Coordinate(-89.37,48.92), 
//				new Coordinate(-96.65,41.84), new Coordinate(-103.69,37.25), new Coordinate(-113.93,38.34),
//				new Coordinate(-117,33), new Coordinate(-118.59,49.62)}; 
//		CoordinateList clAfter= new CoordinateList(coordAfter);
//
//		CoordinateList result = ReducePoints.reduceKeepConcav(cl, null, orig, 60.0, 450.0, "FROM");		
//		assertEquals("Result", clAfter, cl);		
//	}
	
//	@Test
//	public final void testReduceKeepConcav3() {
//		// TODO
//		Coordinate[] coord = {new Coordinate(1,1), new Coordinate(1,-1), new Coordinate(0,-2), new Coordinate(-1,-1), new Coordinate(-1,1)};
//		CoordinateList cl= new CoordinateList(coord);
//		
//		List<Integer> reduceFlg = new ArrayList<Integer>();
//		for (int i=0; i<cl.size(); i++)
//			reduceFlg.add(1);
//		List<Integer> orig = new ArrayList<Integer>();
//		
//		Coordinate[] coordAfter = { new Coordinate(1.6666666666666667,1.0), new Coordinate(0.12499999999999996,-3.625), new Coordinate(-1.8571428571428572,1.0)}; 
//		CoordinateList clAfter= new CoordinateList(coordAfter);
//
//		CoordinateList result = ReducePoints.reduceKeepConcav(cl, null, orig, 60.0, 150.0, "FROM");		
//		assertEquals("Result", clAfter, cl);
//		
//	}
	@RunWith(Suite.class)
	@Suite.SuiteClasses( { ReducePointsTest.class })
	public class AllTests {
	}

//	public static void main(String[] args) {
//		Result result = JUnitCore.runClasses(MyClassTest.class);
//		for (Failure failure : result.getFailures()) {
//			System.out.println(failure.toString());
//		}
//	}

}
