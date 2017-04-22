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
package com.raytheon.uf.common.mpe.gribit2.grib;

import java.io.IOException;
import java.io.OutputStream;

import com.mchange.lang.ByteUtils;

/**
 * Utility to handle common conversions of xmrg to grib properties.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2016 4619       bkowal      Initial creation
 * Aug 10, 2016 4619       bkowal      Added pack methods and handling of negative values.
 * Aug 18, 2016 4619       bkowal      Updated to support binary data section packing.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public final class XmrgGribPropertyUtils {

    private static final int DIVISOR_5 = 65536;

    public static final int DIVISOR_6 = 256;

    public static final int SIZE_BYTES_LEN = 3;

    public static final int CONST_128 = 128;

    public static final int CONST_255 = 255;

    private static final int CONST_CONVERT_NEGATIVE = 8388608;

    private static final int CONST_REVERT_NEGATIVE = 8388607;

    private static final int[] BIT_ONES = { 1, 3, 7, 15, 31, 63, 127, CONST_255 };

    private static final int BITS_PER_BYTE = 8;

    protected XmrgGribPropertyUtils() {
    }

    /**
     * Converts the specified byte array to unsigned bytes utilizing
     * {@link ByteUtils}.
     * 
     * @param bytes
     *            the specified byte array
     * @return the unsigned byte array that was produced
     */
    public static short[] getUnsignedBytes(final byte[] bytes) {
        if (bytes == null) {
            throw new IllegalArgumentException(
                    "Required argument 'bytes' cannot be NULL.");
        }

        short[] unsignedBytes = new short[bytes.length];
        for (int i = 0; i < bytes.length; i++) {
            unsignedBytes[i] = ByteUtils.toUnsigned(bytes[i]);
        }
        return unsignedBytes;
    }

    /**
     * Converts the specified byte array to a size value. Gribit stored many
     * larger numeric values across three (3) array elements. The specified byte
     * array is expected to have a length of {@link #SIZE_BYTES_LEN}.
     * 
     * @param sizeBytes
     *            the specified byte array
     * @return the size value that was calculated
     */
    public static int convertToSize(final short[] sizeBytes) {
        if (sizeBytes.length != SIZE_BYTES_LEN) {
            throw new IllegalArgumentException(
                    "Expected argument 'sizeBytes' to have a length of "
                            + SIZE_BYTES_LEN + ".");
        }
        return (sizeBytes[0] * DIVISOR_5) + (sizeBytes[1] * DIVISOR_6)
                + sizeBytes[2];
    }

    /**
     * Unpack bits. Extract arbitrary size values from the specified packed bit
     * string, right justifying each value in the unpacked array. Based on:
     * /rary.ohd.pproc.gribit/TEXT/gbytes_char.f
     * 
     * @param destination
     *            the output array to write the unpacked result to
     * @param toUnpack
     *            the specified packed bit string
     * @param skip
     *            initial number of bits to skip
     * @param count
     *            number of bits to take
     */
    public static void unpackBytes(int[] destination, final short[] toUnpack,
            final int skip, final int count) {
        unpackBytes(destination, toUnpack, skip, count, 0, 1);
    }

    /**
     * Unpack bits. Extract arbitrary size values from the specified packed bit
     * string, right justifying each value in the unpacked array. Based on:
     * /rary.ohd.pproc.gribit/TEXT/gbytes_char.f
     * 
     * @param destination
     *            the output array to write the unpacked result to
     * @param toUnpack
     *            the specified packed bit string
     * @param skip
     *            additional number of bits to skip on each iteration
     * @param count
     *            number of bits to take
     * @param adjustNegative
     *            boolean flag specifying whether possible negative
     *            transformations should be applied to unpacked values that are
     *            determined to be negative.
     */
    public static void unpackBytes(int[] destination, final short[] toUnpack,
            final int skip, final int count, final boolean adjustNegative) {
        unpackBytes(destination, toUnpack, skip, count, 0, 1, adjustNegative);
    }

    /**
     * Unpack bits. Extract arbitrary size values from the specified packed bit
     * string, right justifying each value in the unpacked array. Based on:
     * /rary.ohd.pproc.gribit/TEXT/gbytes_char.f
     * 
     * @param destination
     *            the output array to write the unpacked result to
     * @param toUnpack
     *            the specified packed bit string
     * @param skip
     *            additional number of bits to skip on each iteration
     * @param count
     *            number of bits to take
     * @param additional
     *            additional number of bits to skip on each iteration
     * @param iterations
     *            number of iterations
     */
    public static void unpackBytes(int[] destination, final short[] toUnpack,
            final int skip, final int count, final int additional,
            final int iterations) {
        unpackBytes(destination, toUnpack, skip, count, additional, iterations,
                false);
    }

    /**
     * Unpack bits. Extract arbitrary size values from the specified packed bit
     * string, right justifying each value in the unpacked array. Based on:
     * /rary.ohd.pproc.gribit/TEXT/gbytes_char.f
     * 
     * @param destination
     *            the output array to write the unpacked result to
     * @param toUnpack
     *            the specified packed bit string
     * @param skip
     *            additional number of bits to skip on each iteration
     * @param count
     *            number of bits to take
     * @param additional
     *            additional number of bits to skip on each iteration
     * @param iterations
     *            number of iterations
     * @param adjustNegative
     *            boolean flag specifying whether possible negative
     *            transformations should be applied to unpacked values that are
     *            determined to be negative.
     */
    public static void unpackBytes(int[] destination, final short[] toUnpack,
            final int skip, final int count, final int additional,
            final int iterations, final boolean adjustNegative) {
        int nbit = skip;
        for (int i = 0; i < iterations; i++) {
            int bitCount = count;
            int index = (nbit / 8);
            int ibit = nbit % 8;
            nbit = nbit + count + skip;

            /*
             * first byte.
             */
            int tbit = Math.min(bitCount, 8 - ibit);
            int itmp = toUnpack[index] & BIT_ONES[7 - ibit];
            if (tbit != (8 - ibit)) {
                final int shift = tbit - 8 + ibit;
                if (shift < 0) {
                    itmp = (itmp >>> Math.abs(shift));
                } else {
                    itmp = (itmp << shift);
                }
            }
            ++index;
            bitCount = bitCount - tbit;
            while (bitCount >= 8) {
                itmp = (itmp << 8) | toUnpack[index];
                bitCount -= 8;
                ++index;
            }

            // last byte
            if (bitCount > 0) {
                // bit count is between 1 and 7 inclusive.

                /*
                 * 1 is subtracted from the bit count when accessing the
                 * BIT_ONES array because Fortran arrays start at index 1
                 * whereas Java arrays currently start at index 0.
                 */
                itmp = (itmp << bitCount)
                        | ((toUnpack[index] >>> (8 - bitCount)) & BIT_ONES[bitCount - 1]);
            }

            if (adjustNegative && (itmp & CONST_CONVERT_NEGATIVE) != 0) {
                itmp = -(itmp & CONST_REVERT_NEGATIVE);
            }
            destination[i] = itmp;
        }
    }

    /**
     * Pack bits. Put arbitrary size values from the specified unpacked bit
     * string into a packed bit string, taking the low order bits from each
     * value in the unpacked array.
     * 
     * @param destination
     *            the output array to write the packed result to
     * @param toPack
     *            the specified unpacked bit string
     * @param skip
     *            additional number of bits to skip on each iteration
     * @param count
     *            number of bits to take
     */
    public static void packBytes(short[] destination, final int[] toPack,
            final int skip, final int count) {
        packBytes(destination, toPack, skip, count, false);
    }

    /**
     * Pack bits. Put arbitrary size values from the specified unpacked bit
     * string into a packed bit string, taking the low order bits from each
     * value in the unpacked array.
     * 
     * @param destination
     *            the output array to write the packed result to
     * @param toPack
     *            the specified unpacked bit string
     * @param skip
     *            additional number of bits to skip on each iteration
     * @param count
     *            number of bits to take
     * @param adjustNegative
     *            boolean flag specifying whether negative values should be
     *            detected and adjusted prior to packing
     */
    public static void packBytes(short[] destination, final int[] toPack,
            final int skip, final int count, final boolean adjustNegative) {
        packBytes(destination, toPack, skip, count, 0, 1, adjustNegative);
    }

    /**
     * Pack bits. Put arbitrary size values from the specified unpacked bit
     * string into a packed bit string, taking the low order bits from each
     * value in the unpacked array.
     * 
     * @param destination
     *            the output array to write the packed result to
     * @param toPack
     *            the specified unpacked bit string
     * @param skip
     *            additional number of bits to skip on each iteration
     * @param count
     *            number of bits to take
     * @param additional
     *            additional number of bits to skip on each iteration
     * @param iterations
     *            number of iterations
     */
    public static void packBytes(short[] destination, final int[] toPack,
            final int skip, final int count, final int additional,
            final int iterations) {
        packBytes(destination, toPack, skip, count, additional, iterations,
                false);
    }

    /**
     * Pack bits. Put arbitrary size values from the specified unpacked bit
     * string into a packed bit string, taking the low order bits from each
     * value in the unpacked array.
     * 
     * @param destination
     *            the output array to write the packed result to
     * @param toPack
     *            the specified unpacked bit string
     * @param skip
     *            additional number of bits to skip on each iteration
     * @param count
     *            number of bits to take
     * @param additional
     *            additional number of bits to skip on each iteration
     * @param iterations
     *            number of iterations
     * @param adjustNegative
     *            boolean flag specifying whether negative values should be
     *            detected and adjusted prior to packing
     */
    public static void packBytes(short[] destination, final int[] toPack,
            final int skip, final int count, final int additional,
            final int iterations, final boolean adjustNegative) {
        int nbit = skip + count - 1;
        for (int i = 0; i < iterations; i++) {
            int itmp = toPack[i];
            if (adjustNegative && itmp < 0) {
                itmp = Math.abs(itmp) | CONST_CONVERT_NEGATIVE;
            }
            int bitCount = count;
            int index = (nbit / 8);
            int ibit = nbit % 8;
            nbit = nbit + count + skip;

            /*
             * make byte aligned
             */
            if (ibit != 7) {
                int tbit = Math.min(bitCount, ibit + 1);
                final int shift = 7 - ibit;
                int itmp2;
                int imask;
                if (shift < 0) {
                    imask = (BIT_ONES[tbit - 1] >>> Math.abs(shift));
                    itmp2 = (itmp >>> Math.abs(shift)) & imask;
                } else {
                    imask = (BIT_ONES[tbit - 1] << shift);
                    itmp2 = (itmp << shift) & imask;
                }
                int itmp3 = destination[index] & (CONST_255 - imask);
                destination[index] = (short) (itmp2 | itmp3);
                bitCount -= tbit;
                final int opTbit = -tbit;
                if (opTbit < 0) {
                    itmp = itmp >>> Math.abs(opTbit);
                } else {
                    itmp = itmp << opTbit;
                }
                --index;
            }

            /*
             * now byte aligned
             */
            while (bitCount >= 8) {
                destination[index] = (short) (itmp & CONST_255);
                itmp = itmp >>> 8;
                bitCount -= 8;
                --index;
            }

            if (bitCount > 0) {
                int itmp2 = itmp & BIT_ONES[bitCount - 1];
                int itmp3 = destination[index]
                        & (CONST_255 - BIT_ONES[bitCount - 1]);
                destination[index] = (short) (itmp2 | itmp3);
            }
        }
    }

    /**
     * Calculates the bit offset excluding the size bytes. An additional 1 is
     * subtracted because the first index in a Fortran array is 1 whereas in
     * Java the first index in an array is 0.
     * 
     * @param startIndex
     *            the index of the first byte
     * @return the calculated bit offset
     */
    public static int calculateBitOffset(final int startIndex) {
        return (startIndex - SIZE_BYTES_LEN - 1) * BITS_PER_BYTE;
    }

    /**
     * Writes an array of shorts as bytes to the specified {@link OutputStream}.
     * In Fortran, characters are only one byte compared to the two byte
     * characters in Java.
     * 
     * @param os
     *            the specified {@link OutputStream}
     * @param toWrite
     *            the array of shorts to write to the buffer as bytes
     */
    public static void writeShortArrayAsBytes(final OutputStream os,
            final short[] toWrite) throws IOException {
        for (short value : toWrite) {
            os.write(value);
        }
    }
}