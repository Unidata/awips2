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
 * 10/25/2007   #465       randerso    initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class RasterPacket extends SymbologyPacket {

    public static final int RASTER_DATA_PACKET1 = 0xBA07;

    public static final int RASTER_DATA_PACKET2 = 0xBA0F;
    static {
        PacketFactory.registerPacketType(RasterPacket.class,
                RASTER_DATA_PACKET1, RASTER_DATA_PACKET2);
    }

    protected int iCenter;

    protected int jCenter;

    protected int numRows;

    protected int numCols;

    protected int xScale;

    protected int yScale;

    protected byte[] rasterData;

    /**
     * Construct takes a byte array containing a radial symbology layer.
     * 
     */
    public RasterPacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    /**
     * Returns the number of columns in the image
     * 
     * @return An int which will be from 1 to 460
     */
    public int getNumCols() {
        return numCols;
    }

    /**
     * Returns the I coordinate of the center of sweep
     * 
     * @return An int which will be from -2048 to 2048
     */
    public int getICenter() {
        return iCenter;
    }

    /**
     * Returns the J coordinate of the center of sweep
     * 
     * @return An int which will be from -2048 to 2048
     */
    public int getJCenter() {
        return jCenter;
    }

    /**
     * Returns the number of rows in the image.
     * 
     * @return An int which will be from 1 to 400
     */
    public int getNumRows() {
        return numRows;
    }

    public int getXScale() {
        return xScale;
    }

    public int getYScale() {
        return yScale;
    }

    /**
     * Parses the radial header
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        in.skip(4);
        iCenter = in.readUnsignedShort();
        jCenter = in.readUnsignedShort();
        xScale = in.readShort();
        in.skip(2);
        yScale = in.readShort();
        in.skip(2);
        numRows = numCols = in.readUnsignedShort();
        in.skip(2);

        readRasterData(in);
    }

    /**
     * @param in
     * @throws IOException
     */
    protected void readRasterData(DataInputStream in) throws IOException {
        // run through the first row to figure out the length of the rows
        if (in.markSupported()) {
            in.mark(Integer.MAX_VALUE);
        }
        int length = 0;

        int remainingBytes = in.readUnsignedShort();
        for (int b = 0; b < remainingBytes; b++) {
            byte dataByte = in.readByte();
            int runLength = 0x0F & (dataByte >> 4);
            length += runLength;
        }
        numCols = length;
        in.reset();

        rasterData = new byte[numCols * numRows];

        for (int row = 0; row < numRows; row++) {
            remainingBytes = in.readUnsignedShort();

            int col = 0;
            for (int b = 0; b < remainingBytes; b++) {
                byte dataByte = in.readByte();
                int runLength = 0x0F & (dataByte >> 4);
                byte value = (byte) (0x0F & dataByte);
                for (int i = 0; i < runLength; ++i) {
                    setRasterDataValue(row, col++, value);
                }
            }
        }
    }

    /**
     * @param row
     * @param col
     * @param value
     */
    protected void setRasterDataValue(int row, int col, byte value) {
        rasterData[row * numCols + col] = value;
    }

    @Override
    public String toString() {
        String s = super.toString() + " Raster Data";
        s += "\n\t\tNum Rows: " + numRows;
        s += "\n\t\tNum Cols: " + numCols;

        return s;
    }

    public byte[] getRasterData() {
        return rasterData;
    }
}
