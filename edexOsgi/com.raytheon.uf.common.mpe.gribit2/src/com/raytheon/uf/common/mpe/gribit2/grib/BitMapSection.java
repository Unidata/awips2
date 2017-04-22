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
import java.nio.ByteBuffer;

import com.raytheon.uf.common.mpe.gribit2.XmrgToGribConstants;

/**
 * POJO representation of a Grib (specifically GRIB 1) BitMap Section (BMS).
 * Based on: /rary.ohd.pproc.gribit/TEXT/engrib.f and
 * /rary.ohd.pproc.gribit/TEXT/w3fi73.f.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 1, 2016  4619       bkowal      Initial creation
 * Aug 18, 2016 4619       bkowal      Implemented section write.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BitMapSection {

    private static final int PACK_CONSTANT = 8;

    private static final int DATA_PREFIX_LENGTH = 6;

    /**
     * number of bytes in BMS
     */
    private int numberBytes;

    /**
     * number of fill bytes
     */
    private int fillBytes;

    private Boolean ibFlag;

    /**
     * the full bitmap. Will be packed and unpacked as needed.
     */
    private int[] bitMap;

    /**
     * Reads the contents of the bit map section from the specified
     * {@link ByteBuffer} and populates fields in this instance of the
     * {@link BitMapSection}.
     * 
     * @param byteBuffer
     *            the specified {@link ByteBuffer}
     */
    public void readSection(final ByteBuffer byteBuffer) {
        /*
         * Read the size of the data section.
         */
        final byte[] lengthBytes = new byte[XmrgGribPropertyUtils.SIZE_BYTES_LEN];
        byteBuffer.get(lengthBytes);
        final short[] unsignedLengthBytes = XmrgGribPropertyUtils
                .getUnsignedBytes(lengthBytes);
        final int[] lengthDestination = new int[1];
        XmrgGribPropertyUtils.unpackBytes(lengthDestination,
                unsignedLengthBytes, 0, 24);
        numberBytes = lengthDestination[0];

        /*
         * Read the number of fill bytes appended to the end of the bitmap.
         */
        final byte[] fillBytes = new byte[1];
        byteBuffer.get(fillBytes);
        final short[] unsignedFillBytes = XmrgGribPropertyUtils
                .getUnsignedBytes(fillBytes);
        final int[] fillDestination = new int[1];
        XmrgGribPropertyUtils.unpackBytes(fillDestination, unsignedFillBytes,
                0, 8);
        this.fillBytes = fillDestination[0];

        /*
         * Read and interpret the ib flag.
         */
        final byte[] ibFlag = new byte[2];
        byteBuffer.get(ibFlag);
        final short[] unsignedIBFlag = XmrgGribPropertyUtils
                .getUnsignedBytes(ibFlag);
        final int[] ibFlagDestination = new int[1];
        XmrgGribPropertyUtils.unpackBytes(ibFlagDestination, unsignedIBFlag, 0,
                16);
        this.ibFlag = (ibFlagDestination[0] == 1);

        /*
         * Calculate the size of the fully expanded data set.
         */
        final int dataSetSize = ((numberBytes - DATA_PREFIX_LENGTH) * PACK_CONSTANT);
        /*
         * TODO: this quantity should be equivalent to the product of the x and
         * y points read from the grid definition section.
         */

        /*
         * Read and unpack the full bitmap.
         */
        final byte[] bitmapBytes = new byte[(numberBytes - DATA_PREFIX_LENGTH)];
        byteBuffer.get(bitmapBytes);
        final short[] unsignedBitmapBytes = XmrgGribPropertyUtils
                .getUnsignedBytes(bitmapBytes);
        bitMap = new int[dataSetSize];
        XmrgGribPropertyUtils.unpackBytes(bitMap, unsignedBitmapBytes, 0, 1, 0,
                dataSetSize);
    }

    /**
     * Writes the contents of the Product Definition Section to the specified
     * {@link OutputStream}.
     * 
     * @param os
     *            the specified {@link OutputStream}
     */
    public void writeSection(final OutputStream os) throws IOException {
        short[] packedValues = new short[XmrgGribPropertyUtils.SIZE_BYTES_LEN];
        final int[] toPack = new int[1];

        /*
         * the length.
         */
        toPack[0] = numberBytes;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
        try {
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

            /*
             * the fill bytes
             */
            short[] packedValue = new short[1];
            toPack[0] = fillBytes;
            XmrgGribPropertyUtils.packBytes(packedValue, toPack, 0, 8);
            os.write(packedValue[0]);

            /*
             * the ib flag
             */
            packedValues = new short[2];
            toPack[0] = (ibFlag != null && ibFlag) ? 1 : 0;
            XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 16);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

            /*
             * the packed bitmap
             */
            final short[] packedBitMap = new short[numberBytes
                    - DATA_PREFIX_LENGTH];
            XmrgGribPropertyUtils.packBytes(packedBitMap, bitMap, 0, 1, 0,
                    bitMap.length);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedBitMap);
        } catch (IOException e) {
            throw new IOException("Failed to write the grib bit map section.",
                    e);
        }
    }

    /**
     * Constructs a {@link BitMapSection} based on the specified float array of
     * xmrg data.
     * 
     * @param xmrgData
     *            the specified float array of xmrg data.
     * @return the constructed {@link BitMapSection}.
     */
    public static BitMapSection generateBitMapSection(final float[] xmrgData) {
        BitMapSection bms = new BitMapSection();
        /*
         * Calculate the size of the bitmap including fill bytes.
         */
        final int length = xmrgData.length;
        bms.fillBytes = 0;
        if (length % 16 != 0) {
            bms.fillBytes = 16 - (length % 16);
        }

        bms.bitMap = new int[length + bms.fillBytes];
        for (int i = 0; i < length; i++) {
            bms.bitMap[i] = (xmrgData[i] == XmrgToGribConstants.XMRG_IGNORE_CONSTANT) ? 0
                    : 1;
        }

        bms.numberBytes = ((bms.bitMap.length + bms.fillBytes) / 8)
                + DATA_PREFIX_LENGTH;
        return bms;
    }

    public int getNumberBytes() {
        return numberBytes;
    }

    public void setNumberBytes(int numberBytes) {
        this.numberBytes = numberBytes;
    }

    public int getFillBytes() {
        return fillBytes;
    }

    public void setFillBytes(int fillBytes) {
        this.fillBytes = fillBytes;
    }

    public Boolean getIbFlag() {
        return ibFlag;
    }

    public void setIbFlag(Boolean ibFlag) {
        this.ibFlag = ibFlag;
    }

    public int[] getBitMap() {
        return bitMap;
    }

    public void setBitMap(int[] bitMap) {
        this.bitMap = bitMap;
    }
}