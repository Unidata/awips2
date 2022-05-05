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

package com.raytheon.uf.common.dataplugin.radar.util;

import java.util.List;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.geospatial.MapUtil;

/**
 * A series of methods to help in the processing and tiling of radar data.
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 20, 2014  3072     mweeks    Updates to remove caculateExtent's
 *                                  dependence on RadarRecord.
 * Mar 26, 2018  6711     randerso  Updated for RPG build 17. Added default
 *                                  value for formatBits. Code cleanup.
 *
 * </pre>
 *
 * @author brockwoo
 */
public class RadarUtil {

    private static final double TWOTOTHE52 = 1L << 52;

    private static final int[] table = { 0, 16, 22, 27, 32, 35, 39, 42, 45, 48,
            50, 53, 55, 57, 59, 61, 64, 65, 67, 69, 71, 73, 75, 76, 78, 80, 81,
            83, 84, 86, 87, 89, 90, 91, 93, 94, 96, 97, 98, 99, 101, 102, 103,
            104, 106, 107, 108, 109, 110, 112, 113, 114, 115, 116, 117, 118,
            119, 120, 121, 122, 123, 124, 125, 126, 128, 128, 129, 130, 131,
            132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144,
            144, 145, 146, 147, 148, 149, 150, 150, 151, 152, 153, 154, 155,
            155, 156, 157, 158, 159, 160, 160, 161, 162, 163, 163, 164, 165,
            166, 167, 167, 168, 169, 170, 170, 171, 172, 173, 173, 174, 175,
            176, 176, 177, 178, 178, 179, 180, 181, 181, 182, 183, 183, 184,
            185, 185, 186, 187, 187, 188, 189, 189, 190, 191, 192, 192, 193,
            193, 194, 195, 195, 196, 197, 197, 198, 199, 199, 200, 201, 201,
            202, 203, 203, 204, 204, 205, 206, 206, 207, 208, 208, 209, 209,
            210, 211, 211, 212, 212, 213, 214, 214, 215, 215, 216, 217, 217,
            218, 218, 219, 219, 220, 221, 221, 222, 222, 223, 224, 224, 225,
            225, 226, 226, 227, 227, 228, 229, 229, 230, 230, 231, 231, 232,
            232, 233, 234, 234, 235, 235, 236, 236, 237, 237, 238, 238, 239,
            240, 240, 241, 241, 242, 242, 243, 243, 244, 244, 245, 245, 246,
            246, 247, 247, 248, 248, 249, 249, 250, 250, 251, 251, 252, 252,
            253, 253, 254, 254, 255 };

    /**
     * A faster replacement for (int)(java.lang.Math.sqrt(x)). Completely
     * accurate for x < 2147483648 (i.e. 2^31)...
     */
    static int fastSqrt(int x) {
        int xn;

        if (x >= 0x10000) {
            if (x >= 0x1000000) {
                if (x >= 0x10000000) {
                    if (x >= 0x40000000) {
                        xn = table[x >> 24] << 8;
                    } else {
                        xn = table[x >> 22] << 7;
                    }
                } else {
                    if (x >= 0x4000000) {
                        xn = table[x >> 20] << 6;
                    } else {
                        xn = table[x >> 18] << 5;
                    }
                }

                xn = (xn + 1 + (x / xn)) >> 1;
                xn = (xn + 1 + (x / xn)) >> 1;
                return ((xn * xn) > x) ? xn - 1 : xn;
            } else {
                if (x >= 0x100000) {
                    if (x >= 0x400000) {
                        xn = table[x >> 16] << 4;
                    } else {
                        xn = table[x >> 14] << 3;
                    }
                } else {
                    if (x >= 0x40000) {
                        xn = table[x >> 12] << 2;
                    } else {
                        xn = table[x >> 10] << 1;
                    }
                }

                xn = (xn + 1 + (x / xn)) >> 1;

                return ((xn * xn) > x) ? xn - 1 : xn;
            }
        } else {
            if (x >= 0x100) {
                if (x >= 0x1000) {
                    if (x >= 0x4000) {
                        xn = (table[x >> 8]) + 1;
                    } else {
                        xn = (table[x >> 6] >> 1) + 1;
                    }
                } else {
                    if (x >= 0x400) {
                        xn = (table[x >> 4] >> 2) + 1;
                    } else {
                        xn = (table[x >> 2] >> 3) + 1;
                    }
                }

                return ((xn * xn) > x) ? xn - 1 : xn;
            } else {
                if (x >= 0) {
                    return table[x] >> 4;
                }
            }
        }

        throw new IllegalArgumentException(
                "Attempt to take the square root of negative number");
    }

    /**
     * A quick rounding method which is much faster than the Math.round method.
     *
     * @param a
     *            The number to round
     * @return An int representation of the rounded number
     */
    public static int fastRound(double a) {
        double dd = TWOTOTHE52 + Math.abs(a);
        int ll = (int) Double.doubleToRawLongBits(dd);
        int signMask = (int) (Double.doubleToRawLongBits(a) >> 63);
        return (ll ^ signMask) - signMask;
    }

    /**
     * A quick method to get the absolute value which is faster than Math.abs.
     *
     * @param a
     *            The int to get the absolute value for
     * @return The absolute value for the passed in int
     */
    public static int fastAbs(int a) {
        if (a >= 0) {
            return a;
        }
        return (a * -1);
    }

    /**
     * Returns the subarray for the passed byte array.
     *
     * @param from
     *            The full byte array to get something out of
     * @param start
     *            The start of the array to collect
     * @param size
     *            The number of elements out of the array to collect
     * @return The subarray
     */
    public static byte[] subArray(byte[] from, int start, int size) {
        byte[] newArray = new byte[size];
        System.arraycopy(from, start, newArray, 0, size);
        return newArray;
    }

    /**
     * Returns the subarray for the passed float array.
     *
     * @param from
     *            The full float array to get something out of
     * @param start
     *            The start of the array to collect
     * @param size
     *            The number of elements out of the array to collect
     * @return The subarray
     */
    public static float[] subArray(float[] from, int start, int size) {
        float[] newArray = new float[size];
        System.arraycopy(from, 0, newArray, 0, size);
        return newArray;
    }

    /**
     * Returns the subarray for the passed short array.
     *
     * @param from
     *            The full short array to get something out of
     * @param start
     *            The start of the array to collect
     * @param size
     *            The number of elements out of the array to collect
     * @return The subarray
     */
    public static short[] subArray(short[] from, int start, int size) {
        short[] newArray = new short[size];
        System.arraycopy(from, 0, newArray, 0, size);
        return newArray;
    }

    /**
     * Format bit mapped status values to text
     *
     * @param bits
     * @param strings
     * @return the formatted text
     */
    public static String formatBits(int bits, List<String> strings) {
        return formatBits(bits, strings, "Indeterminate");
    }

    /**
     * Format bit mapped status values to text
     *
     * @param bits
     *            status bits
     * @param strings
     *            string array containing bit definitions
     * @param noneSet
     *            value to be returned if no bits are set
     * @return the formatted text
     */
    public static String formatBits(int bits, List<String> strings,
            String noneSet) {
        StringBuilder result = new StringBuilder();
        int mask = 1;
        for (String s : strings) {
            if ((bits & mask) != 0) {
                if (result.length() > 0) {
                    result.append(", ");
                }
                if (s != null) {
                    result.append(s);
                }
            }
            mask <<= 1;
        }

        if (result.length() == 0) {
            result.append(noneSet);
        }
        return result.toString();
    }

    /**
     * Translate numeric mode of operation to its text equivalent
     *
     * @param mode
     *            numeric mode of operation
     * @return the text equivalent
     */
    public static String getModeOfOperation(int mode) {
        String modeString = null;
        if (mode >= 0 && mode < RadarConstants.modeOfOperation.size()) {
            modeString = RadarConstants.modeOfOperation.get(mode);
        }

        if (modeString == null) {
            modeString = "Unknown";
        }
        return modeString;
    }

    /**
     * Translate numeric the RDA channel to its text equivalent
     *
     * @param channel
     *            numeric RDA channel
     * @return the text equivalent
     */
    public static String getRdaChannelName(int channel) {
        if (channel >= 0 && channel < RadarConstants.rdaChannelStr.size()) {
            return RadarConstants.rdaChannelStr.get(channel);
        } else {
            return "";
        }
    }

    /**
     * Create a radar sterographic CRS centered on the specified lat/lon
     *
     * @param latitude
     * @param longitude
     * @return the CRS
     */
    public static ProjectedCRS constructCRS(float latitude, float longitude) {
        return MapUtil.constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
                MapUtil.AWIPS_EARTH_RADIUS, latitude, longitude);
    }

    /**
     * Calculate the extent of the given radar data
     *
     * @param radarData
     * @return the extent
     */
    public static double calculateExtent(RadarRecord radarData) {
        return calculateExtent(radarData.getNumBins(), radarData.getJstart(),
                radarData.getGateResolution(),
                (double) radarData.getTrueElevationAngle(),
                radarData.getFormat());
    }

    /**
     * Calculate the extent of the radar data given the number of bins, starting
     * bin, gate resolution, elevation angle, and format
     *
     * @param numBins
     *            number of bins
     * @param startBin
     *            starting bin, (optional, defaults to 0 if null)
     * @param gateResolution
     *            in km per bin
     * @param elevationAngle
     *            in degrees
     * @param format
     *            Raster, Radial, XY, Graphic, etc. see radarInfo.txt
     * @return the extent
     */
    public static double calculateExtent(int numBins, Integer startBin,
            Integer gateResolution, Double elevationAngle, String format) {
        if (startBin != null) {
            numBins += startBin;
        }
        double maxExtent = gateResolution * numBins
                * Math.cos(Math.toRadians(elevationAngle));
        if ("Raster".equals(format)) {
            maxExtent /= 2;
        }
        return maxExtent;
    }

    /**
     * Construct a GridGeometry2D for the radar given the crs, max extent, and
     * number of bins or radials
     *
     * @param crs
     * @param maxExtent
     * @param numPoints
     *            number of bins or radials
     * @return the GridGeometry2D
     */
    public static GridGeometry2D constructGridGeometry(ProjectedCRS crs,
            double maxExtent, int numPoints) {
        GridGeometry2D gridGeometry2D = null;
        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        generalEnvelope.setCoordinateReferenceSystem(crs);

        generalEnvelope.setRange(0, -maxExtent, maxExtent);
        generalEnvelope.setRange(1, -maxExtent, maxExtent);

        gridGeometry2D = new GridGeometry2D(
                new GeneralGridEnvelope(new int[] { 0, 0 },
                        new int[] { numPoints, numPoints }, false),
                generalEnvelope);

        return gridGeometry2D;
    }
}
