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

import java.nio.ByteBuffer;

/**
 * POJO representation of a Radar Header.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2016 4625       bkowal      Initial creation
 * Nov 22, 2016 5588       nabowle     Refactored to just the header block,
 *                                     moving some fields into the product description.
 *
 * </pre>
 *
 * @author bkowal
 */

public class MpeRadarMessageHeader {
    public static final int BYTE_SIZE = 18;

    private short messageCode;

    private short julianDate;

    private int seconds;

    private int messageLength;

    private short sourceID;

    private short destinationID;

    private short numBlocks;

    public MpeRadarMessageHeader(ByteBuffer byteBuffer)
            throws InvalidMpeRadarException {
        super();
        MpeRadarDecodeUtils.checkFileRemaining(byteBuffer, "Radar Header", BYTE_SIZE);
        messageCode = byteBuffer.getShort();
        julianDate = byteBuffer.getShort();
        seconds = byteBuffer.getInt();
        messageLength = byteBuffer.getInt();
        sourceID = byteBuffer.getShort();
        destinationID = byteBuffer.getShort();
        numBlocks = byteBuffer.getShort();
        MpeRadarDecodeUtils.checkFileRemaining(byteBuffer, "Radar body",
                messageLength - BYTE_SIZE);
    }

    /**
     * @return the messageCode
     */
    public short getMessageCode() {
        return messageCode;
    }

    /**
     * @return the julianDate
     */
    public short getJulianDate() {
        return julianDate;
    }

    /**
     * @return the seconds
     */
    public int getSeconds() {
        return seconds;
    }

    /**
     * @return the messageLength
     */
    public int getMessageLength() {
        return messageLength;
    }

    /**
     * @return the sourceID
     */
    public short getSourceID() {
        return sourceID;
    }

    /**
     * @return the destinationID
     */
    public short getDestinationID() {
        return destinationID;
    }

    /**
     * @return the numBlocks
     */
    public short getNumBlocks() {
        return numBlocks;
    }
}
