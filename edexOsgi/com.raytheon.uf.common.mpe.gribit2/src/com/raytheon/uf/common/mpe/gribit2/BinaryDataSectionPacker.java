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

import java.util.Arrays;

import com.raytheon.uf.common.mpe.gribit2.grib.XmrgGribPropertyUtils;

/**
 * Packs the grib data prior to insertion into the {@link BinaryDataSection}.
 * Based on: /rary.ohd.pproc.gribit/TEXT/w3fi75.f
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2016 4619       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BinaryDataSectionPacker {

    public static final int ZERO_BITS_LENGTH = 7;

    /*
     * Mathematical constants in: /rary.ohd.pproc.gribit/TEXT/w3fi59.f. NATURAL
     * LOGARITHM OF 2 AND 0.5 PLUS NOMINAL SAFE EPSILON.
     */
    private final float ALOG2 = 0.69314718056f;

    private final float HPEPS = 0.500001f;

    /**
     * Packs the specified xmrg data and returns the results of the packing
     * operation in the form of a {@link PackBinaryDataResults}.
     * 
     * @param tossData
     *            boolean flag indicating if values indicated as not set in the
     *            bitmap should be dropped prior to packing.
     * @param xmrgData
     *            the xmrg data to pack
     * @param bitPacking
     *            a {@link BitPacking} containing parameters to use when
     *            determining how the data should be packed.
     * @return the generated {@link PackBinaryDataResults}
     */
    public PackBinaryDataResults run(final Boolean tossData,
            final float[] xmrgData, final BitPacking bitPacking) {
        /*
         * TODO: verify inputs to this method.
         * com.raytheon.uf.common.mpe.gribit2.BitPacking.bitsToPack should be >
         * 0 and <= 32. And the xmrg data array must actually contain data.
         */

        // 1.1 TOSS DATA IF BITMAP BEING USED, MOVING 'DATA' TO WORK AREA...
        /*
         * Note: legacy gribit provided support for both an integer work array
         * and a float work array based on the data type flag. However, the data
         * type flag is hard-coded to 0 (indicating floating-point data), so the
         * initial version of this port will not provide support for
         * integer-based data.
         */
        final float[] floatWorkGridArray;
        if (Boolean.TRUE.equals(tossData)) {
            floatWorkGridArray = new float[xmrgData.length];
            int index = 0;
            for (int i = 0; i < xmrgData.length; i++) {
                if (xmrgData[i] != XmrgToGribConstants.XMRG_IGNORE_CONSTANT) {
                    floatWorkGridArray[index] = xmrgData[i];
                    ++index;
                }
            }
        } else {
            floatWorkGridArray = Arrays.copyOf(xmrgData, xmrgData.length);
        }
        final int[] intWorkGridArray = new int[xmrgData.length];

        // 1.2 CONVERT DATA IF NEEDED PRIOR TO PACKING. (INTEGER TO F.P. OR F.P.
        // TO INTEGER). Based on the result of BitPackingCalculator.
        /*
         * TODO: Will be necessary to create and populate an integer array if:
         * com.raytheon.uf.common.mpe.gribit2.BitPacking.bitsToPack == 0.
         */

        // 1.3 PACK THE DATA.
        /*
         * Legacy gribit includes multiple packing implementations including:
         * SECOND ORDER PACKING with VARIABLE BIT PACKING, SECOND ORDER PACKING
         * with FIXED BIT PACKING, GLAHN SECOND DIFFERENCING FOR NON STANDARD
         * GRIB (invoked when the PDS has a size of 50), SECOND ORDER PACKING
         * WITH NO SECOND ORDER BIT MAP, SIMPLE FIRST ORDER PACKING WITH
         * VARIABLE BIT LENGTH, SIMPLE FIRST ORDER PACKING with FIXED BIT
         * LENGTH. The packing algorithm that is selected is based on the Table
         * 11 Flags in use as well as the size of the PDS. With the number of
         * fields that are hard-coded and due to the fact that the PDS is
         * guaranteed to be 28 bytes, only SIMPLE PACKING is actually ever used.
         */

        /*
         * find the minimum and maximum values within the data. required for
         * both possibilities.
         */
        float dataMin = Float.MAX_VALUE;
        float dataMax = -Float.MAX_VALUE;
        for (float dataValue : floatWorkGridArray) {
            dataMin = Math.min(dataMin, dataValue);
            dataMax = Math.max(dataMax, dataValue);
        }

        int length = 0;
        /*
         * If the data array contains a single value throughout, do not perform
         * any packing.
         */
        if (dataMin == dataMax) {
            /*
             * All the data consists of the same value. Packing is not
             * necessary.
             */
            return new PackBinaryDataResults();
        }

        /*
         * Determine the largest difference.
         */
        final float maxDiff = dataMax - dataMin;

        if (bitPacking.getBitsToPack() == 0) {
            /*
             * variable bit length. Based on:
             * /rary.ohd.pproc.gribit/TEXT/w3fi58.f
             */

            /*
             * NBITS IS COMPUTED AS THE LEAST INTEGER SUCH THAT: BIGDIF <
             * 2**NBITS
             */
            int nbits = (int) (Math.log(maxDiff + 0.5) / ALOG2 + 1);

            /*
             * Form differences in the work array.
             */
            for (int i = 0; i < floatWorkGridArray.length; i++) {
                intWorkGridArray[i] = (int) (floatWorkGridArray[i] - dataMin);
            }

            /*
             * PACK EACH MAGNITUDE IN NBITS (NBITS = THE LEAST POWER OF 2 OR
             * 'N')
             */
            length = (nbits * xmrgData.length - 1) / 8 + 1;
            final short[] packedData = new short[length];

            /*
             * Pack the bytes now.
             */
            XmrgGribPropertyUtils.packBytes(packedData, intWorkGridArray, 0,
                    nbits, 0, xmrgData.length);
            return new PackBinaryDataResults(packedData, length, 0f, dataMin);
        } else {
            /*
             * fixed bit length. Based on: /rary.ohd.pproc.gribit/TEXT/w3fi59.f
             */

            /*
             * ISCALE IS THE POWER OF 2 REQUIRED TO RESTORE THE PACKED DATA.
             * ISCALE IS COMPUTED AS THE LEAST INTEGER SUCH THAT:
             * BIGDIF*2**(-ISCALE) < 2**NBITS-0.5. IN ORDER TO ENSURE THAT THE
             * PACKED INTEGERS (COMPUTED IN LOOP 2000 WITH THE NEAREST INTEGER
             * FUNCTION) STAY LESS THAN 2**NBITS.
             */
            double scale = Math.rint(Math.log(maxDiff
                    / Math.pow(2., (double) bitPacking.getBitsToPack() - 0.5))
                    / ALOG2 + HPEPS);
            /*
             * FORM DIFFERENCES, RESCALE, AND CONVERT TO INTEGER FORMAT.
             */
            final double twon = Math.pow(2., -scale);
            for (int i = 0; i < floatWorkGridArray.length; i++) {
                intWorkGridArray[i] = (int) Math
                        .ceil((floatWorkGridArray[i] - dataMin) * twon);
            }

            length = ((xmrgData.length * bitPacking.getBitsToPack()) + 7) / 8;
            final short[] packedData = new short[length];

            /*
             * Pack the bytes now.
             */
            XmrgGribPropertyUtils.packBytes(packedData, intWorkGridArray, 0,
                    bitPacking.getBitsToPack(), 0, xmrgData.length);
            return new PackBinaryDataResults(packedData, length, scale, dataMin);
        }
    }
}