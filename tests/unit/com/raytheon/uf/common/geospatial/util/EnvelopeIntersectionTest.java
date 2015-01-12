/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.geospatial.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.geotools.factory.FactoryIteratorProvider;
import org.geotools.factory.GeoTools;
import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.crs.DefaultProjectedCRS;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.junit.Assert;
import org.junit.Test;
import org.opengis.geometry.Envelope;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.projection.Geostationary;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.PackedCoordinateSequence;
import com.vividsolutions.jts.simplify.TopologyPreservingSimplifier;

/**
 * Unit tests for EnvelopeIntersection. Also contains methods for creating and
 * visualizing test cases.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Sep 13, 2013  2309     bsteffen    Initial creation
 * Nov 18, 2013  2528     bsteffen    Add test for geostationary.
 * Dec 11, 2013  2619     bsteffen    Add alaska test for dateline issues.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class EnvelopeIntersectionTest {

    private enum KNOWN_ENVELOPES {

        UKMET {
            public Envelope getEnvelope() {
                ProjectedCRS crs = MapUtil.constructEquidistantCylindrical(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        149.375, 0);
                return new ReferencedEnvelope(-2.0015806220738135E7,
                        2.0015806220738135E7, -69499.32715534112,
                        1.0077402437524462E7, crs);
            }
        },

        CONUS {
            public Envelope getEnvelope() {
                return new ReferencedEnvelope(-3217360.2108624117,
                        2889016.9103023135, -535763.0304681085,
                        4023747.8031403385, MapUtil.AWIPS_LAMBERT_NORTHAMERICA);
            }
        },

        NORTH_AMERICAN {
            public Envelope getEnvelope() {
                return new ReferencedEnvelope(-5309286.482967425,
                        5840160.537429935, -1.0583136599376587E7,
                        566310.4145612845,
                        MapUtil.AWIPS_POLARSTEREO_NORTHAMERICA);
            }
        },

        N_HEMISPHERE {
            public Envelope getEnvelope() {
                return new ReferencedEnvelope(-1.1026966741467461E7,
                        1.1272003702071536E7, -1.197446662274791E7,
                        1.0324516833231976E7,
                        MapUtil.AWIPS_POLARSTEREO_NORTHAMERICA);
            }
        },

        GEOSTATIONARY {
            public Envelope getEnvelope() {
                try {
                    // Must manually register geostation
                    GeoTools.addFactoryIteratorProvider(new FactoryIteratorProvider() {

                        @Override
                        public <T> Iterator<T> iterator(Class<T> category) {
                            if (category
                                    .isAssignableFrom(Geostationary.Provider.class)) {
                                List<T> tmp = new ArrayList<T>(1);
                                tmp.add(category
                                        .cast(new Geostationary.Provider()));
                                return tmp.iterator();
                            }
                            return null;
                        }
                    });
                    ParameterValueGroup parameters = new DefaultMathTransformFactory()
                            .getDefaultParameters(Geostationary.PROJECTION_NAME);

                    parameters.parameter("semi_major").setValue(6378137.0);
                    parameters.parameter("semi_minor").setValue(6356732.31414);
                    parameters.parameter("latitude_of_origin").setValue(0.0);
                    parameters.parameter("central_meridian").setValue(-137);
                    parameters.parameter("false_easting").setValue(0);
                    parameters.parameter("false_northing").setValue(0);
                    parameters.parameter(Geostationary.ORBITAL_HEIGHT)
                            .setValue(35785863.0);
                    parameters.parameter(Geostationary.SWEEP_AXIS)
                            .setValue(0.0);
                    DefaultProjectedCRS crs = MapUtil.constructProjection(
                            Geostationary.PROJECTION_NAME, parameters);
                    return new ReferencedEnvelope(-5434870.792806381,
                            -2356713.972039082, -3799599.721331195,
                            -721442.9005638957, crs);
                } catch (Exception e) {
                    throw new RuntimeException(
                            "Unable to create geostationary projection", e);
                }
            }
        },

        /*
         * This envelope has really weird issues with the dataline. Specifically
         * the lower left corner longitude is just right so if you convert it to
         * LatLon space, add 360 to longitude and then subtract 360 from
         * longitude you will get a slightly different LatLon point. This
         * combined with the fact that the lewer left corner ends up being the
         * last point in a polygon and the second to last point is on the
         * opposite side of the dataline results in strange problems.
         */
        ALASKA {
            public Envelope getEnvelope() {
                return new ReferencedEnvelope(-2047000.023104792,
                        999.9768952080049, -3879539.946249751,
                        -1831539.946249751, MapUtil.constructNorthPolarStereo(
                                6371200, 6371200, 52.5, -149.5));
            }
        };

        public abstract Envelope getEnvelope();

    }

    /*
     * Driver method for any test where the result is a polygon.
     */
    private void test(KNOWN_ENVELOPES source, KNOWN_ENVELOPES target,
            double threshold, int maxHorDivisions, int maxVertDivisions,
            float[] expectedPolygonCoords) throws TransformException,
            FactoryException {
        long startTime = System.currentTimeMillis();
        Geometry testGeom = EnvelopeIntersection.createEnvelopeIntersection(
                source.getEnvelope(), target.getEnvelope(), threshold,
                maxHorDivisions, maxVertDivisions);

        /*
         * This assumes the targetPolygon was created by taking a known good
         * result and optionally simplifying it within threshold to reduce the
         * number of points.
         */
        GeometryFactory gf = new GeometryFactory();
        CoordinateSequence seq = new PackedCoordinateSequence.Float(
                expectedPolygonCoords, 2);
        Polygon expectedPolygon = gf.createPolygon(gf.createLinearRing(seq),
                null);
        /*
         * The intersection is only accurate within threshold, so one threshold
         * error is allowed. Allow an additional threshold of error so that
         * expected can be simplified to a reasonable number of points.
         */
        double maxError = threshold * 3;

        /*
         * The following line can be enabled to print out simplified coordinates
         * for embedding in a new test case.
         */
        // printSimplePolygon((Polygon) testGeom, threshold);

        /*
         * The following lines can be enabled to output kml which can be copied
         * to a kml document for visualization of the result.
         */
        // toKML(target.getEnvelope(), (Polygon) testGeom, expectedPolygon,
        // maxError);

        long endTime = System.currentTimeMillis();
        System.out.println(source + " intersected with " + target + " took: "
                + (endTime - startTime) + "ms");

        Assert.assertTrue(source + " intersected with " + target
                + " is too large.",
                expectedPolygon.buffer(maxError).contains(testGeom));
        Assert.assertTrue(source + " intersected with " + target
                + " is too small.",
                testGeom.contains(expectedPolygon.buffer(-maxError)));
    }

    @Test
    public final void testNHemisphereToUkmet() throws TransformException,
            FactoryException {
        /* This was created using printSimplePolygon. */
        float[] expected = { -11679819, -69499, -16048823, -69499, -16434625,
                68892, -17180132, 244287, -17957145, 331217, -18743790, 322310,
                -19516540, 218337, -20015806, 99542, -20015806, 10077402,
                20015806, 10077402, 20015806, 99542, 19521645, -69499,
                12154251, -69499, 11810572, -46037, 11320266, -69499, 4329636,
                -69499, 3754453, 154739, 3002505, 352893, 2213256, 461310,
                1409180, 470603, 615384, 379942, -144865, 197255, -867679,
                -69499, -4863898, -69499, -5174879, 115261, -5884130, 440773,
                -6661072, 692043, -7489914, 848418, -7916371, 886151, -8346022,
                895453, -8774988, 876090, -9199425, 828547, -10020702, 654209,
                -10786615, 388282, -11483038, 52055, -11679819, -69499 };
        test(KNOWN_ENVELOPES.N_HEMISPHERE, KNOWN_ENVELOPES.UKMET, 13899, 288,
                288, expected);
    }

    @Test
    public final void testNorthAmericanToUkmet() throws TransformException,
            FactoryException {
        /* This was created using printSimplePolygon. */
        float[] expected = { 14979502, -69499, 14883593, -69499, 14279565,
                223324, 13534652, 486146, 12736140, 662693, 11905150, 737580,
                11068720, 703800, 10254941, 564550, 9487633, 332385, 8782854,
                26116, 8407468, 721768, 8185666, 1087068, 7654069, 1843813,
                7334228, 2229381, 6970379, 2614075, 6555697, 2992009, 6083071,
                3355750, 5545856, 3696201, 4939171, 4002664, 4261830, 4263239,
                3518659, 4465690, 2722419, 4598888, 1894090, 4654600, 1060461,
                4629147, 959127, 5259852, 822364, 5914390, 627884, 6589490,
                496399, 6933265, 330138, 7280051, 113554, 7628615, -179446,
                7977147, -595709, 8322716, -875639, 8492870, -1226401, 8659923,
                -1675566, 8822228, -2628925, 9050724, -3547032, 9186090,
                -4790089, 9298295, -6411468, 9374670, -7343422, 9394925,
                -9288019, 9393585, -10214098, 9372078, -11818030, 9293680,
                -13043896, 9180123, -13948805, 9043906, -14889285, 8814688,
                -15332995, 8652109, -15679868, 8484877, -15956965, 8314607,
                -16369506, 7968921, -16526569, 7794783, -16775361, 7445958,
                -16963160, 7098143, -17109710, 6752805, -17323290, 6073466,
                -17471219, 5413681, -17662441, 4165987, -18420699, 4188358,
                -19174963, 4139363, -20015806, 3997563, -20015806, 10077402,
                20015806, 10077402, 20015806, 3997563, 19437890, 3842069,
                18801195, 3608974, 18222147, 3332535, 17701527, 3022831,
                17236859, 2689235, 16823782, 2339983, 16457096, 1982015,
                16131460, 1620989, 15583455, 906675, 14979502, -69499 };
        test(KNOWN_ENVELOPES.NORTH_AMERICAN, KNOWN_ENVELOPES.UKMET, 13899, 288,
                288, expected);
    }

    @Test
    public final void testConusToUkmet() throws TransformException,
            FactoryException {
        /* This was created using printSimplePolygon. */
        float[] expected = { 9498087, 1887148, 10277277, 2035375, 11070627,
                2144941, 11874038, 2214687, 12683075, 2243889, 13493117,
                2232245, 14299505, 2179876, 15097708, 2087322, 15883466,
                1955556, 16139651, 3068241, 16442331, 4167431, 16805100,
                5218278, 17014747, 5715560, 17247244, 6189536, 16129540,
                6350309, 14974126, 6462169, 13793179, 6525070, 12600643,
                6539016, 11411305, 6504013, 10239717, 6420051, 9099162,
                6287138, 8000839, 6105371, 8254983, 5631862, 8484566, 5135495,
                8882714, 4087842, 9215714, 2993555, 9498087, 1887148 };
        test(KNOWN_ENVELOPES.CONUS, KNOWN_ENVELOPES.UKMET, 13899, 288, 288,
                expected);
    }

    @Test
    public final void testConusToNHemisphere() throws TransformException,
            FactoryException {
        /* This was created using printSimplePolygon. */
        float[] expected = { -3040707, -8260076, 1216715, -8207329, 5263771,
                -6931420, 2792186, -2386639, 444185, -3289054, -2084476,
                -3128577, -3040707, -8260076 };
        test(KNOWN_ENVELOPES.CONUS, KNOWN_ENVELOPES.N_HEMISPHERE, 381000, 65,
                65, expected);
    }

    @Test
    public final void testUkmetToNorthAmerican() throws TransformException,
            FactoryException {
        /* This was created using printSimplePolygon. */
        float[] expected = { 5690083, -10583137, -5309286, -10583137, -5309286,
                566310, 5840161, 566310, 5840161, -10503444, 5690083, -10583137 };
        test(KNOWN_ENVELOPES.UKMET, KNOWN_ENVELOPES.NORTH_AMERICAN, 13899, 73,
                288, expected);
    }

    @Test
    public final void testGeostationaryToUkmet() throws TransformException,
            FactoryException {
        /* This was created using printSimplePolygon. */
        float[] expected = { -803884, -836926, 5717769, -737647, 5605322,
                -1820270, 5412823, -2767932, 5122653, -3661810, 4764378,
                -4405659, 2533964, -4570863, -74925, -4826265, 141761,
                -4699883, 265720, -4585495, 398396, -4374090, -401980,
                -4451987, -214394, -4335532, -140376, -4231895, -111147,
                -4039970, -191289, -3862333, -279094, -3778945, -431268,
                -3701630, 203526, -3560640, 5468, -3401972, -348043, -3256781,
                130869, -3136911, -224309, -2994515, -678028, -2942195, -31273,
                -2816425, -277475, -2750530, 73248, -2647495, -149634,
                -2580518, -547236, -2523641, -92802, -2417432, -447576,
                -2357760, -82722, -2259030, -453907, -2199805, -112694,
                -2104320, -555394, -2047966, -184709, -1952826, 22519,
                -1866703, -312976, -1804583, -88257, -1718835, -567370,
                -1661644, -78052, -1491513, -585469, -1433545, -305862,
                -1348754, -19066, -1190667, -484493, -1128908, -299531,
                -1049446, -76719, -896167, -803884, -836926, };
        test(KNOWN_ENVELOPES.GEOSTATIONARY, KNOWN_ENVELOPES.UKMET,
                19097.596365784917, 64, 49, expected);
    }

    @Test
    public final void testGeostationaryToGeostationary()
            throws TransformException, FactoryException {
        /* This was created using printSimplePolygon. */
        float[] expected = { -5434871, -3799600, -5434871, -721443, -2356714,
                -721443, -2356714, -3799600, -5434871, -3799600 };
        test(KNOWN_ENVELOPES.GEOSTATIONARY, KNOWN_ENVELOPES.GEOSTATIONARY,
                19097.596365784917, 64, 49, expected);
    }

    @Test
    public final void testAlaskaToUkmet() throws TransformException,
            FactoryException {
        /* This was created using printSimplePolygon. */
        float[] expected = { 3703712, 5337060, 3317194, 5775708, 2834468,
                6206521, 2222747, 6619845, 1439512, 7001657, 2352844, 7380677,
                3551651, 7694056, 5063542, 7906556, 6800513, 7982537, 6798676,
                5836944, 5962640, 5803467, 5154389, 5704811, 4396584, 5546637,
                3703712, 5337060 };
        test(KNOWN_ENVELOPES.ALASKA, KNOWN_ENVELOPES.UKMET, 29863.10130618981,
                16, 16, expected);
    }

    /**
     * This method can be used to assist in generating test cases. It starts
     * with a known good result and minimizes the amount of text to put in the
     * source while still maintaining an accuracy of threshold.
     */
    public void printSimplePolygon(Polygon poly, double threshold) {
        poly = (Polygon) TopologyPreservingSimplifier.simplify(poly, threshold);
        for (Coordinate c : poly.getCoordinates()) {
            System.out.print((int) Math.round(c.x) + ", "
                    + (int) Math.round(c.y) + ", ");
        }
        System.out.println();
    }

    /**
     * This method dumps kml to std out that can be used to visualize a test
     * case. KML includes 4 placemarks for the test result, expected result, max
     * expected result(expanded to 2*threshold), and min expected result.
     */
    public void toKML(Envelope targetEnvelope, Polygon testPolygon,
            Polygon expectedPolygon, double bufferWidth)
            throws TransformException, FactoryException {
        MathTransform toLL = MapUtil.getTransformToLatLon(targetEnvelope
                .getCoordinateReferenceSystem());

        System.out.println("<kml>");
        System.out.println(" <Document>");

        Geometry ll = JTS.transform(testPolygon, toLL);
        System.out.println("  <Placemark>");
        System.out.println("   <name>Test Result</name>");
        System.out.println("   <LinearRing>");
        System.out.println("    <coordinates>");
        for (Coordinate c : ll.getCoordinates()) {
            System.out.println(c.x + "," + c.y);
        }
        System.out.println("    </coordinates>");
        System.out.println("   </LinearRing>");
        System.out.println("  </Placemark>");

        ll = JTS.transform(expectedPolygon, toLL);
        System.out.println("  <Placemark>");
        System.out.println("   <name>Expected</name>");
        System.out.println("   <LinearRing>");
        System.out.println("    <coordinates>");
        for (Coordinate c : ll.getCoordinates()) {
            System.out.println(c.x + "," + c.y);
        }
        System.out.println("    </coordinates>");
        System.out.println("   </LinearRing>");
        System.out.println("  </Placemark>");

        ll = JTS.transform(expectedPolygon.buffer(bufferWidth), toLL);
        System.out.println("  <Placemark>");
        System.out.println("   <name>Max Expected</name>");
        System.out.println("   <LinearRing>");
        System.out.println("    <coordinates>");
        for (Coordinate c : ll.getCoordinates()) {
            System.out.println(c.x + "," + c.y);
        }
        System.out.println("    </coordinates>");
        System.out.println("   </LinearRing>");
        System.out.println("  </Placemark>");

        ll = JTS.transform(expectedPolygon.buffer(-bufferWidth), toLL);
        System.out.println("  <Placemark>");
        System.out.println("   <name>Min Expected</name>");
        System.out.println("   <LinearRing>");
        System.out.println("    <coordinates>");
        for (Coordinate c : ll.getCoordinates()) {
            System.out.println(c.x + "," + c.y);
        }
        System.out.println("    </coordinates>");
        System.out.println("   </LinearRing>");
        System.out.println("  </Placemark>");

        System.out.println(" </Document>");
        System.out.println("</kml>");
    }
}
