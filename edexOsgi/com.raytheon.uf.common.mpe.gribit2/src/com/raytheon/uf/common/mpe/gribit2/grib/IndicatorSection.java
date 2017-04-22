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

/**
 * POJO representation of a Grib Indicator Section (IS). Based on:
 * /rary.ohd.pproc.gribit/TEXT/engrib.f.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2016 4619       bkowal      Initial creation
 * Aug 18, 2016 4619       bkowal      Implemented section write.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class IndicatorSection {

    public static final int NUM_BYTES = 8;

    private static final String BEGIN_GRIB = "GRIB";

    private static final char FINAL_CHAR = (char) 1;

    private int totalGribSize;

    /**
     * Reads the contents of the indicator section from the specified
     * {@link ByteBuffer} and populates fields in this instance of the
     * {@link IndicatorSection}.
     * 
     * @param byteBuffer
     *            the specified {@link ByteBuffer}
     */
    public void readSection(final ByteBuffer byteBuffer)
            throws InvalidGribException {
        final byte[] sectionBytes = new byte[BEGIN_GRIB.length()];
        byteBuffer.get(sectionBytes);
        final String grib = new String(sectionBytes);
        if (!BEGIN_GRIB.equals(grib)) {
            throw new InvalidGribException(
                    "Read unexpected grib indicator section beggining: " + grib
                            + ". Expected: " + BEGIN_GRIB + ".");
        }
        final byte[] lengthBytes = new byte[XmrgGribPropertyUtils.SIZE_BYTES_LEN];
        byteBuffer.get(lengthBytes);
        short[] unsignedLengthBytes = XmrgGribPropertyUtils
                .getUnsignedBytes(lengthBytes);
        /*
         * Do the reverse of the calculation that is performed during the write
         * of this section to determine the total grib size.
         */
        totalGribSize = XmrgGribPropertyUtils
                .convertToSize(unsignedLengthBytes);

        final byte lastChar = byteBuffer.get();
        /*
         * Verify that the last element in the byte array is equivalent to:
         * (char) 1.
         */
        if (lastChar != FINAL_CHAR) {
            throw new InvalidGribException(
                    "Read unexpected grib indicator final character: '"
                            + lastChar + "'. Expected: '" + FINAL_CHAR + "'.");
        }
    }

    /**
     * Writes the contents of the Indicator Section to the specified
     * {@link OutputStream}.
     * 
     * @param os
     *            the specified {@link OutputStream}
     */
    public void writeSection(final OutputStream os) throws IOException {
        try {
            os.write(BEGIN_GRIB.getBytes());

            /*
             * the length.
             */
            final short[] packedLength = new short[XmrgGribPropertyUtils.SIZE_BYTES_LEN];
            final int[] toPack = new int[] { totalGribSize };
            XmrgGribPropertyUtils.packBytes(packedLength, toPack, 0, 24);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedLength);

            /*
             * the final character.
             */
            os.write((int) FINAL_CHAR);
        } catch (IOException e) {
            throw new IOException(
                    "Failed to write the grib indicator section.", e);
        }
    }

    public int getTotalGribSize() {
        return totalGribSize;
    }

    public void setTotalGribSize(int totalGribSize) {
        this.totalGribSize = totalGribSize;
    }
}