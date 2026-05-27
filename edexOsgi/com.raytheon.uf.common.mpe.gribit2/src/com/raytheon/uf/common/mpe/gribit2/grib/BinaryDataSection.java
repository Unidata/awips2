/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted dataContentsType whose
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
import java.util.Arrays;

/**
 * POJO representation of a Binary Data Section -- Pack Data. Based on:
 * /rary.ohd.pproc.gribit/TEXT/engrib.f and
 * /rary.ohd.pproc.gribit/TEXT/w3fi75.f.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2016 4619       bkowal      Initial creation
 * Aug 11, 2016 4619       bkowal      Fully read individual attributes of the BDS.
 * Aug 18, 2016 4619       bkowal      Implemented section write.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BinaryDataSection {

    public static final int PREFIX_BYTES_LENGTH = XmrgGribPropertyUtils.SIZE_BYTES_LEN + 8;

    /**
     * length of the BDS.
     */
    private int numberBytes;

    private Table11Flags table11Flags;

    /**
     * Number of fill bits used to pad the binary dataContentsType array.
     */
    private int numberFillBits;

    /**
     * POWER OF 2 FOR RESTORING DATA, SUCH THAT DATUM = (DIFFERENCE * 2**ISCALE)
     * + RMIN
     */
    private int scaleFactor;

    /**
     * Octet 11 - number of bits
     */
    private int bitsToPack;

    /**
     * packed version of the binary data
     */
    private short[] packedData;

    /**
     * Reads the contents of the binary dataContentsType section from the
     * specified {@link ByteBuffer} and populates fields in this instance of the
     * {@link BinaryDataSection}.
     * 
     * @param byteBuffer
     *            the specified {@link ByteBuffer}
     */
    public void readSection(final ByteBuffer byteBuffer) {
        final byte[] lengthBytes = new byte[XmrgGribPropertyUtils.SIZE_BYTES_LEN];
        byteBuffer.get(lengthBytes);
        final short[] unsignedLengthBytes = XmrgGribPropertyUtils
                .getUnsignedBytes(lengthBytes);
        final int[] lengthDestination = new int[1];
        XmrgGribPropertyUtils.unpackBytes(lengthDestination,
                unsignedLengthBytes, 0, 24);
        numberBytes = lengthDestination[0];

        final byte[] remainingBDSBytes = new byte[numberBytes
                - XmrgGribPropertyUtils.SIZE_BYTES_LEN];
        byteBuffer.get(remainingBDSBytes);
        final short[] unsignedBSDBytes = XmrgGribPropertyUtils
                .getUnsignedBytes(remainingBDSBytes);

        final int[] unpackedValue = new int[1];
        table11Flags = new Table11Flags();

        /* various flags - reading sections of a byte each time */
        XmrgGribPropertyUtils
                .unpackBytes(unpackedValue, unsignedBSDBytes, 0, 1);
        table11Flags.setData(unpackedValue[0]);
        XmrgGribPropertyUtils
                .unpackBytes(unpackedValue, unsignedBSDBytes, 1, 1);
        table11Flags.setPacking(unpackedValue[0]);
        XmrgGribPropertyUtils
                .unpackBytes(unpackedValue, unsignedBSDBytes, 2, 1);
        table11Flags.setOriginalDataType(unpackedValue[0]);
        XmrgGribPropertyUtils
                .unpackBytes(unpackedValue, unsignedBSDBytes, 3, 1);
        table11Flags.setOctet14(unpackedValue[0]);

        /* number of fill bits */
        XmrgGribPropertyUtils
                .unpackBytes(unpackedValue, unsignedBSDBytes, 4, 4);
        numberFillBits = unpackedValue[0];

        /* scale factor */
        XmrgGribPropertyUtils
                .unpackBytes(unpackedValue, unsignedBSDBytes, 8, 1);
        if (unpackedValue[0] == 1) {
            /*
             * signed bit = 1.
             */
            XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedBSDBytes,
                    9, 15);
            scaleFactor = -unpackedValue[0];
        } else {
            XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedBSDBytes,
                    8, 16);
            scaleFactor = unpackedValue[0];
        }

        /*
         * reference value first test to see if on 32 or 64 bit computer.
         * consists of an exponent and mantissa
         */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedBSDBytes, 24,
                8);
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedBSDBytes, 32,
                24);
        /*
         * TODO: determine what to do with the exponent and mantissa.
         */

        /* number of bits */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedBSDBytes, 56,
                8);
        bitsToPack = unpackedValue[0];

        packedData = Arrays.copyOfRange(unsignedBSDBytes, 8,
                unsignedBSDBytes.length);
    }

    /**
     * Writes the contents of the Binary Data Section to the specified
     * {@link ByteBuffer}.
     * 
     * @param byteBuffer
     *            the specified {@link ByteBuffer}
     */
    public void writeSection(final OutputStream os) throws IOException {
        try {
            short[] packedValues = new short[XmrgGribPropertyUtils.SIZE_BYTES_LEN];
            final int[] toPack = new int[1];

            /*
             * the length.
             */
            toPack[0] = numberBytes;
            XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

            /*
             * table 11 flags
             */
            final short[] bds11Oout = new short[8];
            toPack[0] = table11Flags.getData();
            XmrgGribPropertyUtils.packBytes(bds11Oout, toPack, 0, 1);
            toPack[0] = table11Flags.getPacking();
            XmrgGribPropertyUtils.packBytes(bds11Oout, toPack, 1, 1);
            toPack[0] = table11Flags.getOriginalDataType();
            XmrgGribPropertyUtils.packBytes(bds11Oout, toPack, 2, 1);
            toPack[0] = table11Flags.getOctet14();
            XmrgGribPropertyUtils.packBytes(bds11Oout, toPack, 3, 1);

            /* number of fill bits */
            toPack[0] = numberFillBits;
            XmrgGribPropertyUtils.packBytes(bds11Oout, toPack, 4, 4);

            /* scale factor */
            int writeScaleFactor = scaleFactor;
            if (writeScaleFactor < 0) {
                toPack[0] = 1;
                writeScaleFactor = Math.abs(writeScaleFactor);
            } else {
                toPack[0] = 0;
            }
            XmrgGribPropertyUtils.packBytes(bds11Oout, toPack, 8, 1);
            toPack[0] = writeScaleFactor;
            XmrgGribPropertyUtils.packBytes(bds11Oout, toPack, 9, 15);

            /* reference values - for now just write 0s */
            toPack[0] = 0;
            XmrgGribPropertyUtils.packBytes(bds11Oout, toPack, 24, 8);
            XmrgGribPropertyUtils.packBytes(bds11Oout, toPack, 32, 24);

            /* number of bits to pack */
            toPack[0] = bitsToPack;
            XmrgGribPropertyUtils.packBytes(bds11Oout, toPack, 56, 8);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, bds11Oout);

            /* data */
            if (packedData != null) {
                XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedData);
            }
        } catch (IOException e) {
            throw new IOException(
                    "Failed to write the grib binary data section.", e);
        }
    }

    /**
     * Note: ibdsfl(8), ibdsfl(9), ibdsfl(10), ibdsfl(11) have all been set to
     * 0. So, 0-padding may be necessary when this POJO is written out.
     */

    public String toString() {
        StringBuilder sb = new StringBuilder("BinaryDataSection [");
        sb.append("]");
        return sb.toString();
    }

    public Table11Flags getTable11Flags() {
        return table11Flags;
    }

    public void setTable11Flags(Table11Flags table11Flags) {
        this.table11Flags = table11Flags;
    }

    public int getNumberBytes() {
        return numberBytes;
    }

    public void setNumberBytes(int numberBytes) {
        this.numberBytes = numberBytes;
    }

    public int getNumberFillBits() {
        return numberFillBits;
    }

    public void setNumberFillBits(int numberFillBits) {
        this.numberFillBits = numberFillBits;
    }

    public int getScaleFactor() {
        return scaleFactor;
    }

    public void setScaleFactor(int scaleFactor) {
        this.scaleFactor = scaleFactor;
    }

    public int getBitsToPack() {
        return bitsToPack;
    }

    public void setBitsToPack(int bitsToPack) {
        this.bitsToPack = bitsToPack;
    }

    public short[] getPackedData() {
        return packedData;
    }

    public void setPackedData(short[] packedData) {
        this.packedData = packedData;
    }
}