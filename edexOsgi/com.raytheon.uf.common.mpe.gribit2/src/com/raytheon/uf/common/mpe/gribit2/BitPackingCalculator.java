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
package com.raytheon.uf.common.mpe.gribit2;

/**
 * The number of bits required to pack a given field for particular binary and
 * decimal scalings is computed. The field is rounded off to the decimal scaling
 * for packing. The minimum and maximum rounded fields are also returned. Grib
 * bitmap masking for valid data is optionally used. Based on:
 * /rary.ohd.pproc.gribit/TEXT/getbit.f
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2016 4619       bkowal      Initial creation
 * Aug 11, 2016 4619       bkowal      Cleaned up bitmap usage. Added
 *                                     "bits to pack" calculation.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public final class BitPackingCalculator {

    protected BitPackingCalculator() {
    }

    /**
     * Completes the necessary calculations to determine how certain data fields
     * should be packed based on the specified binary and decimal scaling.
     * 
     * @param bitmap
     *            bitmap flag ({@code false} for no bitmap)
     * @param binaryScaling
     *            the specified integer binary scaling (example: binaryScaling=3
     *            to round field to nearest eighth value)
     * @param decimalScaling
     *            the specified integer decimal scaling (Note: both
     *            binaryScaling and decimalScaling can be non-zero, example:
     *            decimalScaling=1 and binaryScaling=1 rounds to the nearest
     *            twentieth)
     * @param xmrgDataAsShort
     *            short representation of the bitmap
     * @param xmrgDataAsFloat
     *            float (decimal) representation of the bitmap
     * @return the generated {@link BitPacking}.
     */
    public static BitPacking calculateBitPacking(final boolean bitmap,
            final int binaryScaling, final int decimalScaling,
            final float[] xmrgDataAsFloat) {
        final double s = Math.pow(2., binaryScaling)
                * Math.pow(10., decimalScaling);
        final double[] ground = new double[xmrgDataAsFloat.length];
        double gmax = -Double.MAX_VALUE;
        double gmin = Double.MAX_VALUE;
        if (!bitmap) {
            /*
             * Differs from the Fortran implementation. The Fortran
             * implementation previously calculated the first value and used it
             * as the mininum and maximum. In this implementation all values are
             * calculated within this loop because the minimum and maximum are
             * set to the negative of the maximum double value and the maximum
             * double value respectively which will ensure that the minimum and
             * maximum will automatically be assigned to the first value.
             */
            for (int i = 0; i < ground.length; i++) {
                ground[i] = Math.rint(xmrgDataAsFloat[i] * s) / s;
                gmax = Math.max(gmax, ground[i]);
                gmin = Math.min(gmin, ground[i]);
            }
        } else {
            /*
             * Differs from the Fortran implementation. The Fortran
             * implementation used to complete a pre-scan of the data array and
             * find the first index that did not map to the xmrg ignore
             * constant. This implementation will just loop through the entire
             * data array and count the number of times the xmrg ignore constant
             * is found in the data array.
             */

            /*
             * Is the entire array filled with the ignore constant?
             */
            int ignoreCount = 0;
            /*
             * Differs from the Fortran implementation. The Fortran
             * implementation previously calculated the first value and used it
             * as the mininum and maximum. In this implementation all values are
             * calculated within this loop because the minimum and maximum are
             * set to the negative of the maximum double value and the maximum
             * double value respectively which will ensure that the minimum and
             * maximum will automatically be assigned to the first value.
             */
            for (int i = 0; i < ground.length; i++) {
                if (xmrgDataAsFloat[i] == XmrgToGribConstants.XMRG_IGNORE_CONSTANT) {
                    ++ignoreCount;
                    continue;
                }
                ground[i] = Math.rint(xmrgDataAsFloat[i] * s) / s;
                gmax = Math.max(gmax, ground[i]);
                gmin = Math.min(gmin, ground[i]);
            }

            if (ignoreCount == ground.length) {
                /*
                 * zero the min and max.
                 */
                gmax = 0;
                gmin = 0;
            }
        }

        /*
         * Compute number of bits.
         */
        final double bitsToPack = Math.log10((gmax - gmin) * s + 0.9)
                / Math.log10(2.) + 1;
        return new BitPacking(ground, gmax, gmin, (int) bitsToPack);
    }
}