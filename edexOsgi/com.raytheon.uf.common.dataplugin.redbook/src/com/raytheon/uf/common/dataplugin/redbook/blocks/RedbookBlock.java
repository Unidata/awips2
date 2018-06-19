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
package com.raytheon.uf.common.dataplugin.redbook.blocks;

import java.nio.ByteBuffer;

/**
 * 
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2008 1131       jkorman     Initial implementation.
 * Apr 29, 2013 1958       bgonzale    Added class RedbookBlockHeader, and
 *                                     nested Factory interface.
 * May 06, 2013 1979       bgonzale    Catch Header at the end of buffer.
 * Mar 13, 2014 2907       njensen     split edex.redbook plugin into common and
 *                                     edex redbook plugins
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public abstract class RedbookBlock {

    private static final int LEN_MASK = 0x8000;

    private static final int CHKSUM_MASK = 0x4000;

    private static final int LENGTH_MASK = 0x3FFF;

    private boolean hasLength = false;

    private boolean hasChkSum = false;

    private RedbookBlockHeader header;

    private final int length;

    public interface RedbookBlockFactory {
        public abstract RedbookBlock createBlock(RedbookBlockHeader header,
                ByteBuffer data);
    }

    /**
     * 
     * @param separator
     */
    public RedbookBlock(RedbookBlockHeader header, ByteBuffer data) {

        this.header = header;

        hasLength = ((this.header.hdr & LEN_MASK) == 0 && data.hasRemaining());

        hasChkSum = (this.header.hdr & CHKSUM_MASK) == 0;

        length = (hasLength) ? (this.header.hdr & LENGTH_MASK) : -1;
    }

    public boolean isEndBlock() {
        return false;
    }

    /**
     * @return the length
     */
    public int getLength() {
        return length;
    }

    /**
     * @return the mode
     */
    public int getMode() {
        return this.header.mode;
    }

    /**
     * @return the subMode
     */
    public int getSubMode() {
        return this.header.subMode;
    }

    /**
     * @return the hasLength
     */
    public boolean hasLength() {
        return hasLength;
    }

    /**
     * @param hasLength
     *            the hasLength to set
     */
    public void setHasLength(boolean hasLength) {
        this.hasLength = hasLength;
    }

    /**
     * @return the hasChkSum
     */
    public boolean hasChkSum() {
        return hasChkSum;
    }

    /**
     * @param hasChkSum
     *            the hasChkSum to set
     */
    public void setHasChkSum(boolean hasChkSum) {
        this.hasChkSum = hasChkSum;
    }

    public StringBuilder toString(StringBuilder sb) {
        if (sb == null) {
            sb = new StringBuilder();
        }
        sb.append((hasLength) ? 'L' : '.');
        sb.append((hasChkSum) ? 'C' : '.');
        sb.append(':');

        sb.append(String.format("%05d:mode=%02X:submode=%02X", length,
                header.mode, header.subMode));

        return sb;
    }

    /**
     * 
     */
    @Override
    public String toString() {
        return toString((StringBuilder) null).toString();
    }

    public static float getFloat2(ByteBuffer dataBuf) {
        float f = Float.NaN;

        if (dataBuf.remaining() >= 2) {

            short s = dataBuf.getShort();

            f = s / 100.0f;
        }
        return f;
    }

    /**
     * @return true if this is a Product Id block; false otherwise.
     */
    public boolean isProductId() {
        return header.isProductId();
    }

    /**
     * @return true if this is a Product Id block; false otherwise.
     */
    public boolean isUpperAirPlot() {
        return header.isUpperAirPlot();
    }

    protected void dropShortsFromTheBuffer(ByteBuffer data) {
        int newPosition = (data.position() + ((getLength() - 2) << 1));
        data.position(newPosition);
    }
}
