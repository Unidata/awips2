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
package com.raytheon.uf.edex.plugin.mpe.gather.radar;

import java.lang.ref.SoftReference;
import java.nio.ByteBuffer;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.locationtech.jts.geom.Coordinate;

/**
 * Utility methods for decoding radar products.
 *
 * Methods have been ported from decode_dhr_dsp/TEXT/build_lookup_table.c,
 * decode_dhr_dsp/TEXT/find_holes.c, and
 * decode_dhr_dsp/TEXT/quarterhrap_to_az_range.c.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2016 5588       nabowle     Initial creation
 * Dec 14, 2015 5588       nabowle     Fix caching.
 * Jul 19, 2018 5588       mapeters    Reverse lat/lon in Coordinates so
 *                                     lon=x and lat=y
 *
 * </pre>
 *
 * @author nabowle
 */

public class MpeRadarDecodeUtils {

    private static final String EOF_ERR_PATTERN = "End of file reached before fully reading the %s. Invalid radar product?";

    private static final Map<Coordinate, SoftReference<int[][][]>> lookupCache = Collections
            .synchronizedMap(new HashMap<>());

    private MpeRadarDecodeUtils() {
    }

    /**
     * This function for building lookup table for conversion between quarterly
     * HRAP and radar polar coordinate systems.
     *
     * Port of decode_dhr_dsp/TEXT/build_lookup_table.c
     *
     *
     * @param radLat
     *            Radar latitude
     * @param radLon
     *            Radar longitude
     * @param quarterHrapToRadarAzimuth
     *            an array of size
     *            [MpeRadarDecodeConstants.MAX_IHRAP][MpeRadarDecodeConstants.MAX_JHRAP]
     *            that will be initialized.
     * @param quarterHrapToRadarRange
     *            an array of size
     *            [MpeRadarDecodeConstants.MAX_IHRAP][MpeRadarDecodeConstants.MAX_JHRAP]
     *            that will be initialized.
     */
    public static void buildLookupTable(int radLat, int radLon,
            int[][] quarterHrapToRadarAzimuth,
            int[][] quarterHrapToRadarRange) {
        Coordinate coord = new Coordinate(radLon, radLat);
        SoftReference<int[][][]> cachedRef = lookupCache.get(coord);
        if (cachedRef != null) {
            int[][][] cachedTable = cachedRef.get();
            if (cachedTable != null) {
                for (int i = 0; i < MpeRadarDecodeConstants.MAX_IHRAP; i++) {
                    System.arraycopy(cachedTable[0][i], 0,
                            quarterHrapToRadarAzimuth[i], 0,
                            MpeRadarDecodeConstants.MAX_JHRAP);
                    System.arraycopy(cachedTable[1][i], 0,
                            quarterHrapToRadarRange[i], 0,
                            MpeRadarDecodeConstants.MAX_JHRAP);
                }
                return;
            }
        }

        int[][] radarToQuarterHrapI = new int[MpeRadarDecodeConstants.MAX_RANGE][MpeRadarDecodeConstants.MAX_AZIMUTH];
        int[][] radarToQuarterHrapJ = new int[MpeRadarDecodeConstants.MAX_RANGE][MpeRadarDecodeConstants.MAX_AZIMUTH];

        /*
         * convert adaptation data lat/long to double precision data
         */
        double dblRadarLat = radLat / MpeRadarDecodeConstants.SCALE_FACTOR;
        double dblRadarLon = radLon / MpeRadarDecodeConstants.SCALE_FACTOR;

        /*
         * re-initialize tables before starting lookup table generation
         */

        for (int iaz = 0; iaz < MpeRadarDecodeConstants.MAX_AZIMUTH; iaz++) {
            for (int irg = 0; irg < MpeRadarDecodeConstants.MAX_RANGE; irg++) {
                radarToQuarterHrapI[irg][iaz] = MpeRadarDecodeConstants.BEYOND_GRID;
                radarToQuarterHrapJ[irg][iaz] = MpeRadarDecodeConstants.BEYOND_GRID;
            }
        }

        for (int ihrap = 0; ihrap < MpeRadarDecodeConstants.MAX_IHRAP; ihrap++) {
            for (int jhrap = 0; jhrap < MpeRadarDecodeConstants.MAX_JHRAP; jhrap++) {
                quarterHrapToRadarAzimuth[ihrap][jhrap] = MpeRadarDecodeConstants.BEYOND_GRID;
                quarterHrapToRadarRange[ihrap][jhrap] = MpeRadarDecodeConstants.BEYOND_GRID;
            }
        }

        /*
         * compute parts of equations used multiple times later
         */

        double cosLambasPrime = Math
                .cos((dblRadarLon + MpeRadarDecodeConstants.PRIME)
                        * MpeRadarDecodeConstants.DEGREE_TO_RADIAN);
        double sinLamdasPrime = Math
                .sin((dblRadarLon + MpeRadarDecodeConstants.PRIME)
                        * MpeRadarDecodeConstants.DEGREE_TO_RADIAN);
        double sinSiteLat = Math
                .sin(dblRadarLat * MpeRadarDecodeConstants.DEGREE_TO_RADIAN);
        double cosSiteLat = Math
                .cos(dblRadarLat * MpeRadarDecodeConstants.DEGREE_TO_RADIAN);

        /*
         * compute common part of the gis and gjs equations
         */

        double preGxs = MpeRadarDecodeConstants.R2KO * cosSiteLat
                / (MpeRadarDecodeConstants.ONE + sinSiteLat);

        /*
         * compute reference grid box coordinates
         */

        double siteGridI = preGxs * sinLamdasPrime
                + MpeRadarDecodeConstants.GRID_COORD_I;
        double siteGridJ = preGxs * cosLambasPrime
                + MpeRadarDecodeConstants.GRID_COORD_J;

        /*
         * compute grid box numbers for box 0,0 of local grids
         */

        int siteI = (int) (MpeRadarDecodeConstants.KA * siteGridI)
                - MpeRadarDecodeConstants.OFFSET;
        int siteJ = (int) (MpeRadarDecodeConstants.KA * siteGridJ)
                - MpeRadarDecodeConstants.OFFSET;

        /*
         * initialize bearing
         */

        double bearAngle = -MpeRadarDecodeConstants.HALF;

        /*
         * DO FOR ALL BEARINGS
         */

        for (int iaz = 1; iaz <= MpeRadarDecodeConstants.MAX_AZIMUTH; iaz++) {
            bearAngle += MpeRadarDecodeConstants.ONE;
            double sinBearing = Math
                    .sin(bearAngle * MpeRadarDecodeConstants.DEGREE_TO_RADIAN);
            double cosBearing = Math
                    .cos(bearAngle * MpeRadarDecodeConstants.DEGREE_TO_RADIAN);

            /*
             * initialize range
             */

            double range = -MpeRadarDecodeConstants.HALF;

            /*
             * do for each input data range values (hydro application next)
             */

            for (int irg = 1; irg <= MpeRadarDecodeConstants.MAX_RANGE; irg++) {
                range = range + MpeRadarDecodeConstants.ONE;
                double sinAngleS = (range
                        / MpeRadarDecodeConstants.EARTH_RADIUS)
                        * (MpeRadarDecodeConstants.ONE
                                - (MpeRadarDecodeConstants.CONST * range
                                        / MpeRadarDecodeConstants.EARTH_RADIUS_SQ));
                double cosAngleS = Math.sqrt(
                        MpeRadarDecodeConstants.ONE - sinAngleS * sinAngleS);
                double sinLat = sinSiteLat * cosAngleS
                        + cosSiteLat * sinAngleS * cosBearing;
                double cosLat = Math
                        .sqrt(MpeRadarDecodeConstants.ONE - sinLat * sinLat);
                double sinDeltaLamda = sinAngleS * sinBearing / cosLat;
                double cosDeltaLamda = Math.sqrt(MpeRadarDecodeConstants.ONE
                        - sinDeltaLamda * sinDeltaLamda);
                double rl = MpeRadarDecodeConstants.R2KO * cosLat
                        / (MpeRadarDecodeConstants.ONE + sinLat);
                double gridI = rl
                        * (sinDeltaLamda * cosLambasPrime
                                + cosDeltaLamda * sinLamdasPrime)
                        + MpeRadarDecodeConstants.GRID_COORD_I;
                double gridJ = rl
                        * (cosDeltaLamda * cosLambasPrime
                                - sinDeltaLamda * sinLamdasPrime)
                        + MpeRadarDecodeConstants.GRID_COORD_J;

                /*
                 * compute 1/160 lfm i and j coordinates of the range/azimuth
                 * bin
                 */

                long lfmI = (int) (gridI * MpeRadarDecodeConstants.KA) - siteI;
                long lfmJ = (int) (gridJ * MpeRadarDecodeConstants.KA) - siteJ;

                /*
                 * if lfm coordinates are within local grid save it into the
                 * lookup table and set the 1/160 lfm box range number table to
                 * within range
                 */

                if ((lfmI > 0) && (lfmI <= MpeRadarDecodeConstants.MAX_IHRAP)
                        && (lfmJ > 0)
                        && (lfmJ <= MpeRadarDecodeConstants.MAX_JHRAP)) {
                    radarToQuarterHrapI[irg - 1][iaz - 1] = (int) lfmI;
                    radarToQuarterHrapJ[irg - 1][iaz - 1] = (int) lfmJ;

                    quarterHrapToRadarAzimuth[(int) (lfmI - 1)][(int) (lfmJ
                            - 1)] = iaz;
                    quarterHrapToRadarRange[(int) (lfmI - 1)][(int) (lfmJ
                            - 1)] = irg;

                }
            }
        }

        /*
         * call findHoles() to find holes and determine hole filling data
         */

        findHoles(dblRadarLat, dblRadarLon, quarterHrapToRadarAzimuth,
                quarterHrapToRadarRange);

        int[][][] cachedTable = new int[2][MpeRadarDecodeConstants.MAX_IHRAP][MpeRadarDecodeConstants.MAX_JHRAP];
        for (int i = 0; i < MpeRadarDecodeConstants.MAX_IHRAP; i++) {
            System.arraycopy(quarterHrapToRadarAzimuth[i], 0, cachedTable[0][i],
                    0, MpeRadarDecodeConstants.MAX_JHRAP);
            System.arraycopy(quarterHrapToRadarRange[i], 0, cachedTable[1][i],
                    0, MpeRadarDecodeConstants.MAX_JHRAP);
        }
        cachedRef = new SoftReference<>(cachedTable);
        lookupCache.put(coord, cachedRef);
    }

    /**
     * This function for finding the holes and determine the filling data during
     * the conversion between quarterly HRAP and radar polar coordinate systems.
     *
     * Port of decode_dhr_dsp/TEXT/find_holes.c
     */
    public static void findHoles(double radarLat, double radarLon,
            int[][] quarterHrapToRadarAzimuth,
            int[][] quarterHrapToRadarRange) {

        /*
         * search for holes in the 1/160 lfm lookup table and identify the
         * az/ran of the data to be used to fill in the hole
         */

        for (int ihrap = 0; ihrap < MpeRadarDecodeConstants.MAX_IHRAP; ihrap++) {
            for (int jhrap = 0; jhrap < MpeRadarDecodeConstants.MAX_JHRAP; jhrap++) {
                if ((quarterHrapToRadarAzimuth[ihrap][jhrap] == MpeRadarDecodeConstants.BEYOND_GRID)
                        && (quarterHrapToRadarRange[ihrap][jhrap] == MpeRadarDecodeConstants.BEYOND_GRID)) {
                    int lfm_i = ihrap + 1;
                    int lfm_j = jhrap + 1;
                    Coordinate azRg = quarterhrapToAzRange(radarLat, radarLon,
                            lfm_i, lfm_j);

                    /*
                     * if the range is within the product coverage area
                     * requirement then store azimuth and range index into the
                     * lfm40flag lookup table
                     */

                    if (azRg.y < MpeRadarDecodeConstants.MAX_RANGE) {
                        quarterHrapToRadarAzimuth[ihrap][jhrap] = (int) (azRg.x
                                + MpeRadarDecodeConstants.AZ_RND);
                        quarterHrapToRadarRange[ihrap][jhrap] = (int) (azRg.y
                                + MpeRadarDecodeConstants.ONE);
                    } else {
                        quarterHrapToRadarAzimuth[ihrap][jhrap] = MpeRadarDecodeConstants.BEYOND_RANGE;
                        quarterHrapToRadarRange[ihrap][jhrap] = MpeRadarDecodeConstants.BEYOND_RANGE;
                    }
                }
            }
        }
    }

    /**
     * This function for computing the radar azimuth angle and range from a
     * quarterly HRAP grid for conversion between quarterly HRAP and radar polar
     * coordinate systems.
     *
     * port of decode_dhr_dsp/TEXT/quarterhrap_to_az_range.c
     *
     * @param lat
     *            radar latitude
     * @param lon
     *            radar longitude
     * @param lfmI
     *            quarter HRAP_I
     * @param lfmJ
     *            quarter HRAP_J
     */
    private static Coordinate quarterhrapToAzRange(double lat, double lon,
            int lfmI, int lfmJ) {

        final double cosLamdasPrime = Math
                .cos((lon + MpeRadarDecodeConstants.PRIME)
                        * MpeRadarDecodeConstants.DEGREE_TO_RADIAN);
        final double sinLamdasPrime = Math
                .sin((lon + MpeRadarDecodeConstants.PRIME)
                        * MpeRadarDecodeConstants.DEGREE_TO_RADIAN);
        final double sinLs = Math
                .sin(lat * MpeRadarDecodeConstants.DEGREE_TO_RADIAN);
        final double cosLs = Math
                .cos(lat * MpeRadarDecodeConstants.DEGREE_TO_RADIAN);

        /* COMPUTE COMMON PART OF THE GIS AND GJS EQUATIONS */
        final double preGxs = MpeRadarDecodeConstants.R2KO * cosLs
                / (MpeRadarDecodeConstants.ONE + sinLs);

        /*
         * COMPUTE REGERENCE GRID BOX COORDINATES
         */

        final double gis = preGxs * sinLamdasPrime
                + MpeRadarDecodeConstants.GRID_COORD_I;
        final double gjs = preGxs * cosLamdasPrime
                + MpeRadarDecodeConstants.GRID_COORD_J;

        /*
         * COMPUTE GRID BOX NUMBERS FOR BOX 0,0 OF LOCAL GRIDS
         */

        final int is = (int) (MpeRadarDecodeConstants.KA * gis)
                - MpeRadarDecodeConstants.OFFSET;
        final int js = (int) (MpeRadarDecodeConstants.KA * gjs)
                - MpeRadarDecodeConstants.OFFSET;

        /*
         * COMPUTE AI AND AJ CONSTANTS
         */

        final double ai = (is
                - MpeRadarDecodeConstants.GRID_COORD_I
                        * MpeRadarDecodeConstants.KA
                + MpeRadarDecodeConstants.HALF) / MpeRadarDecodeConstants.KA;
        final double aj = (js
                - MpeRadarDecodeConstants.GRID_COORD_J
                        * MpeRadarDecodeConstants.KA
                + MpeRadarDecodeConstants.HALF) / MpeRadarDecodeConstants.KA;

        final double cii = ai + MpeRadarDecodeConstants.B_CON * lfmI;
        final double cjj = aj + MpeRadarDecodeConstants.B_CON * lfmJ;
        final double lij = MpeRadarDecodeConstants.DEGREE_TO_RADIAN
                * MpeRadarDecodeConstants.NINTY
                - MpeRadarDecodeConstants.TWO
                        * Math.atan(Math.sqrt(cii * cii + cjj * cjj)
                                / MpeRadarDecodeConstants.R2KO);

        /* IF BOTH INPUTS TO DATAN2 ARE 0, DONT CALL FUNCTION */
        final double lamdaIJ;
        if ((cii == MpeRadarDecodeConstants.ZERO)
                && (cjj == MpeRadarDecodeConstants.ZERO)) {
            lamdaIJ = MpeRadarDecodeConstants.ZERO;
        } else {
            /* OTHERWISE COMPUTE LAMDA_IJ */
            lamdaIJ = -MpeRadarDecodeConstants.PRIME
                    * MpeRadarDecodeConstants.DEGREE_TO_RADIAN
                    + Math.atan2(cii, cjj);
        }

        /* COMPUTE INTERMEDIATE VALUES */
        final double cosLij = Math.cos(lij);
        final double sinLij = Math.sin(lij);
        final double sinDLamda = Math
                .sin(lamdaIJ - MpeRadarDecodeConstants.DEGREE_TO_RADIAN * lon);
        final double aa = cosLij * sinDLamda;
        final double bb = cosLs * sinLij - sinLs * cosLij * Math
                .cos(lamdaIJ - MpeRadarDecodeConstants.DEGREE_TO_RADIAN * lon);
        final double sinSS = Math.sqrt(aa * aa + bb * bb);
        final double cosSS = Math
                .sqrt(MpeRadarDecodeConstants.ONE - sinSS * sinSS);

        /* COMPUTE RANGE */
        final double range = (MpeRadarDecodeConstants.CONST * sinSS
                + MpeRadarDecodeConstants.EARTH_RADIUS) * sinSS;

        double azimuth;
        /*
         * IF SIN_SS IS GREATER THAN A SMALL POSITIVE NUMBER COMPUTE THETA_CIJ
         */
        if (sinSS >= MpeRadarDecodeConstants.ANGLE_THRESH) {
            azimuth = Math.atan2(cosLij * cosLs * sinDLamda,
                    (sinLij - sinLs * cosSS));
        } else {
            /* OTHERWISE, SET THETA_CIJ TO 0 */
            azimuth = MpeRadarDecodeConstants.ZERO;
        }

        azimuth = azimuth / MpeRadarDecodeConstants.DEGREE_TO_RADIAN;

        /* IF ANGLE IS LESS THAN 0 ... ADD 360 DEGREES */
        if (azimuth < MpeRadarDecodeConstants.ZERO) {
            azimuth = azimuth + MpeRadarDecodeConstants.R_360;
        }

        return new Coordinate(azimuth, range);
    }

    /**
     * Verifies that both the end of the file has not been reached and that
     * there is a sufficient number of bytes required for the next read
     * operation.
     *
     * @param byteBuffer
     *            the {@link ByteBuffer} to check
     * @param readContent
     *            a description of the content that is currently being read from
     *            the buffer
     * @param bytesNeeded
     *            the number of bytes that should still hopefully be available
     *            from the buffer
     * @throws InvalidMpeRadarException
     */
    public static void checkFileRemaining(final ByteBuffer byteBuffer,
            final String readContent, final int bytesNeeded)
            throws InvalidMpeRadarException {
        if (byteBuffer.hasRemaining()
                && byteBuffer.remaining() >= bytesNeeded) {
            return;
        }
        throw new InvalidMpeRadarException(
                String.format(EOF_ERR_PATTERN, readContent));
    }

}
