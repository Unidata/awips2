package gov.noaa.nws.ncep.ui.pgen.gfa;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import com.vividsolutions.jts.geom.Coordinate;

//TODO fix?
@Ignore
public class ReduceGfaPointsTest {
    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.ui.pgen.tools.ReduceGfaPoints#reduceBySize(com.vividsolutions.jts.geom.CoordinateList, java.util.List, int, double, double)}
     * .
     */
    @Test
    public final void testReduceBySize() {
        // TODO
        Coordinate[] coord = { new Coordinate(1, 1), new Coordinate(1, -1),
                new Coordinate(0, -2), new Coordinate(-1, -1),
                new Coordinate(-1, 1) };
        List<Coordinate> cl = Arrays.asList(coord);

        List<Integer> reduceFlg = new ArrayList<Integer>();
        for (int i = 0; i < cl.size(); i++)
            reduceFlg.add(1);
        List<Integer> orig = new ArrayList<Integer>();

        Coordinate[] coordAfter = { new Coordinate(1.6666666666666667, 1.0),
                new Coordinate(0.12499999999999996, -3.625),
                new Coordinate(-1.8571428571428572, 1.0) };
        List<Coordinate> clAfter = Arrays.asList(coordAfter);

        int result = ReduceGfaPoints.reduceBySize(cl, null, orig, 3, 60.0,
                100.0);
        assertEquals("Result", clAfter, cl);
    }

    @Test
    public final void testReduceBySize2() {
        // TODO
        Coordinate[] coord = { new Coordinate(1, 1), new Coordinate(1, 0),
                new Coordinate(1, -1), new Coordinate(0, -1),
                new Coordinate(-1, -1), new Coordinate(-1, 0),
                new Coordinate(-1, 1), new Coordinate(0, 1) };
        List<Coordinate> cl = Arrays.asList(coord);

        List<Integer> reduceFlg = new ArrayList<Integer>();
        for (int i = 0; i < cl.size(); i++)
            reduceFlg.add(1);
        List<Integer> orig = new ArrayList<Integer>();

        Coordinate[] coordAfter = { new Coordinate(1, -1),
                new Coordinate(-1, -1), new Coordinate(-1, 1) };
        List<Coordinate> clAfter = Arrays.asList(coordAfter);

        int result = ReduceGfaPoints.reduceBySize(cl, null, orig, 3, 60.0,
                100.0);
        assertEquals("Result", clAfter, cl);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.ui.pgen.tools.ReduceGfaPoints#reduceByPctDist(com.vividsolutions.jts.geom.CoordinateList, java.util.List, java.util.List, double, double, double, java.lang.String)}
     * .
     */
    @Test
    public final void testReduceByPctDist() {
        // TODO //set LENFROM=35
        Coordinate[] coord = { new Coordinate(-110.57, 54.72),
                new Coordinate(-100.19, 54.54), new Coordinate(-89.37, 48.92),
                new Coordinate(-96.65, 41.84), new Coordinate(-103.69, 37.25),
                new Coordinate(-113.93, 38.34), new Coordinate(-117.95, 42.4),
                new Coordinate(-118.43, 47.81), new Coordinate(-115.17, 52.63) };
        List<Coordinate> cl = Arrays.asList(coord);

        List<Integer> reduceFlg = new ArrayList<Integer>();
        for (int i = 0; i < cl.size(); i++)
            reduceFlg.add(1);
        List<Integer> orig = new ArrayList<Integer>();

        Coordinate[] coordAfter = { new Coordinate(-110.57, 54.72),
                new Coordinate(-100.19, 54.54), new Coordinate(-89.37, 48.92),
                new Coordinate(-103.69, 37.25), new Coordinate(-113.93, 38.34),
                new Coordinate(-118.43, 47.81) };
        List<Coordinate> clAfter = Arrays.asList(coordAfter);

        ReduceGfaPoints.reduceByPctDist(cl, null, orig, 60.0, 80.0, 450.0,
                "BOUNDED BY");
        assertEquals("Result", clAfter, cl);
    }

    @Test
    public final void testReduceByPctDist2() {
        // TODO //set LENFROM=23
        Coordinate[] coord = { new Coordinate(-90, 40),
                new Coordinate(-96, 33), new Coordinate(-155, 30),
                new Coordinate(-177, 33), new Coordinate(-100, 50) };
        List<Coordinate> cl = Arrays.asList(coord);

        List<Integer> reduceFlg = new ArrayList<Integer>();
        for (int i = 0; i < cl.size(); i++)
            reduceFlg.add(1);
        List<Integer> orig = new ArrayList<Integer>();

        Coordinate[] coordAfter = { new Coordinate(-90, 40),
                new Coordinate(-96, 33), new Coordinate(-155, 30),
                new Coordinate(-177, 33), new Coordinate(-100, 50) };
        List<Coordinate> clAfter = Arrays.asList(coordAfter);

        ReduceGfaPoints.reduceByPctDist(cl, null, orig, 60.0, 80.0, 450.0,
                "BOUNDED BY");
        assertEquals("Result", clAfter, cl);
    }

    @Test
    public final void testReduceByPctDist3() {
        // TODO FROM 4400ESE YSJ TO 4480ESE YSJ TO 4490ESE YSJ TO 4400ESE YSJ TO
        // 4320ESE YSJ TO 4400ESE YSJ
        Coordinate[] coord = { new Coordinate(1, 1), new Coordinate(1, -1),
                new Coordinate(0, -2), new Coordinate(-1, -1),
                new Coordinate(-1, 1) };
        List<Coordinate> cl = Arrays.asList(coord);

        List<Integer> reduceFlg = new ArrayList<Integer>();
        for (int i = 0; i < cl.size(); i++)
            reduceFlg.add(1);
        List<Integer> orig = new ArrayList<Integer>();

        Coordinate[] coordAfter = { new Coordinate(1.6666666666666667, 1.0),
                new Coordinate(0.12499999999999996, -3.625),
                new Coordinate(-1.8571428571428572, 1.0) }; // 1,-1
        List<Coordinate> clAfter = Arrays.asList(coordAfter);

        ReduceGfaPoints.reduceByPctDist(cl, null, orig, 60.0, 80.0, 150.0,
                "FROM");
        assertEquals("Result", clAfter, cl);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.ui.pgen.tools.ReduceGfaPoints#reduceKeepConcav(com.vividsolutions.jts.geom.CoordinateList, java.util.List, java.util.List, double, double, java.lang.String)}
     * .
     * 
     * @throws JAXBException
     */
    @Test
    public final void testReduceKeepConcav() {
        // TODO //to test, set LENFROM=23
        Coordinate[] coord = { new Coordinate(-100, 50),
                new Coordinate(-90, 40), new Coordinate(-96, 33),
                new Coordinate(-155, 30), new Coordinate(-177, 33) };
        List<Coordinate> cl = Arrays.asList(coord);

        List<Integer> reduceFlg = new ArrayList<Integer>();
        for (int i = 0; i < cl.size(); i++)
            reduceFlg.add(1);
        List<Integer> orig = new ArrayList<Integer>();

        Coordinate[] coordAfter = { new Coordinate(-92.7, 51.6),
                new Coordinate(-88.4, 33.4), new Coordinate(-155, 30),
                new Coordinate(-177, 33) };
        List<Coordinate> clAfter = Arrays.asList(coordAfter);

        ReduceGfaPoints.reduceKeepConcav(cl, null, orig, 60.0, 450.0, "FROM");
        assertEquals("Result", clAfter, cl);

    }

    @Test
    public final void testReduceKeepConcav2() {
        // TODO
        Coordinate[] coord = { new Coordinate(-110.57, 54.72),
                new Coordinate(-100.19, 54.54), new Coordinate(-89.37, 48.92),
                new Coordinate(-96.65, 41.84), new Coordinate(-103.69, 37.25),
                new Coordinate(-113.93, 38.34), new Coordinate(-117.95, 42.4),
                new Coordinate(-118.43, 47.81), new Coordinate(-115.17, 52.63) };
        List<Coordinate> cl = Arrays.asList(coord);

        List<Integer> reduceFlg = new ArrayList<Integer>();
        for (int i = 0; i < cl.size(); i++)
            reduceFlg.add(1);
        List<Integer> orig = new ArrayList<Integer>();

        Coordinate[] coordAfter = { new Coordinate(-112.75, 54.76),
                new Coordinate(-100.19, 54.54), new Coordinate(-89.37, 48.92),
                new Coordinate(-96.65, 41.84), new Coordinate(-103.69, 37.25),
                new Coordinate(-113.93, 38.34), new Coordinate(-117, 33),
                new Coordinate(-118.59, 49.62) };
        List<Coordinate> clAfter = Arrays.asList(coordAfter);

        ReduceGfaPoints.reduceKeepConcav(cl, null, orig, 60.0, 450.0, "FROM");
        assertEquals("Result", clAfter, cl);
    }

    @Test
    public final void testReduceKeepConcav3() {
        // TODO
        Coordinate[] coord = { new Coordinate(1, 1), new Coordinate(1, -1),
                new Coordinate(0, -2), new Coordinate(-1, -1),
                new Coordinate(-1, 1) };
        List<Coordinate> cl = Arrays.asList(coord);

        List<Integer> reduceFlg = new ArrayList<Integer>();
        for (int i = 0; i < cl.size(); i++)
            reduceFlg.add(1);
        List<Integer> orig = new ArrayList<Integer>();

        Coordinate[] coordAfter = { new Coordinate(1.6666666666666667, 1.0),
                new Coordinate(0.12499999999999996, -3.625),
                new Coordinate(-1.8571428571428572, 1.0) };
        List<Coordinate> clAfter = Arrays.asList(coordAfter);

        ReduceGfaPoints.reduceKeepConcav(cl, null, orig, 60.0, 150.0, "FROM");
        assertEquals("Result", clAfter, cl);

    }

    @RunWith(Suite.class)
    @Suite.SuiteClasses({ ReduceGfaPointsTest.class })
    public class AllTests {
    }

    // public static void main(String[] args) {
    // Result result = JUnitCore.runClasses(MyClassTest.class);
    // for (Failure failure : result.getFailures()) {
    // System.out.println(failure.toString());
    // }
    // }

}
