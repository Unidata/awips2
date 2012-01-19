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
package com.raytheon.uf.common.dataplugin.radar.level3;

import java.io.DataInputStream;
import java.io.IOException;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1-26-2009               mnash       Initial Creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class PrecipDataPacket extends SymbologyPacket {

    public static final int DIGITAL_PRECIP_DATA17 = 17;

    public static final int PRECIP_RATE_DATA18 = 18;

    static {
        PacketFactory.registerPacketType(PrecipDataPacket.class,
                DIGITAL_PRECIP_DATA17, PRECIP_RATE_DATA18);
    }

    protected int numRows;

    protected int numCols;

    protected int numLFMBoxes;

    protected byte[] precipData;

    /**
     * @param packetId
     * @param in
     * @throws IOException
     */
    public PrecipDataPacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    public int getNumRows() {
        return numRows;
    }

    public void setNumRows(int numRows) {
        this.numRows = numRows;
    }

    public int getNumCols() {
        return numCols;
    }

    public void setNumCols(int numCols) {
        this.numCols = numCols;
    }

    public int getNumLFMBoxes() {
        return numLFMBoxes;
    }

    public void setNumLFMBoxes(int numLFMBoxes) {
        this.numLFMBoxes = numLFMBoxes;
    }

    public PrecipDataPacket() {

    }

    @Override
    protected void init(DataInputStream in) throws IOException {
        in.skip(4);
        numLFMBoxes = in.readUnsignedShort();
        numRows = numCols = in.readUnsignedShort();
        precipData = new byte[numRows * numCols];

        readPrecipData(in);
    }

    /**
     * @param in
     * @throws IOException
     */
    protected void readPrecipData(DataInputStream in) throws IOException {
        for (int row = 0; row < numRows; row++) {
            int remainingBytes = in.readUnsignedShort();

            int col = 0;
            for (int b = 0; b < remainingBytes; b++) {
                byte dataByte = in.readByte();
                int runLength = 0;
                if (packetId == DIGITAL_PRECIP_DATA17) {
                    runLength = 0x0F & (dataByte >> 8);
                } else if (packetId == PRECIP_RATE_DATA18) {
                    runLength = 0x0F & (dataByte >> 4);
                }
                byte level = (byte) (0x0F & dataByte);
                for (int i = 0; i < runLength; ++i) {
                    setPrecipDataValue(row, col++, level);
                }
            }
        }
    }

    protected void setPrecipDataValue(int row, int col, byte value) {
        precipData[row * numCols + col] = value;
    }

    public String toString() {
        String s = super.toString() + " Precip Data";
        s += "\n\t\tNum Rows: " + numRows;
        s += "\n\t\tNum Cols: " + numCols;

        return s;
    }

    public byte[] getPrecipData() {
        return precipData;
    }
}
