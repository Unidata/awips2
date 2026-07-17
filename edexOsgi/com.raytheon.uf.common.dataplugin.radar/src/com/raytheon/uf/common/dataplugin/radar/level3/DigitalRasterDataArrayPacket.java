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
 * This class defines the Digital Raster Data Array packets
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/01/2023   2033911    jdynina     initial creation
 * 
 * </pre>
 * 
 * @author jdynina
 * @version 1.0
 */

public class DigitalRasterDataArrayPacket extends SymbologyPacket {

    protected int iCoordStart;

    protected int jCoordStart;

    protected int iScaleFactor;

    protected int jScaleFactor;

    protected int numCells;

    protected int numRows;

    protected int numCols;

    protected byte[] digitalRasterDataArray;

    /**
     * Construct takes a byte array containing a radial symbology layer.
     * 
     */
    public DigitalRasterDataArrayPacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    /**
     * Returns the I coordinate start in pixels
     * 
     * @return An int which will be from 0 to 511
     */
    public int getICoordStart() {
        return iCoordStart;
    }

    /**
     * Returns the J coordinate start in pixels
     * 
     * @return An int which will be from 0 to 511
     */
    public int getJCoordStart() {
        return jCoordStart;
    }

    /**
     * Returns the I (vertival) scale factor
     * 
     * @return An int which will be from 1 to 10
     */
    public int getIScaleFactor() {
        return iScaleFactor;
    }

    /**
     * Returns the J (horizontal) scale factor
     * 
     * @return An int which will be from 1 to 10
     */
    public int getJScaleFactor() {
        return jScaleFactor;
    }

    /**
     * Returns the number of cellss in the image.
     * 
     * @return An int which will be from 1 to 1840
     */
    public int getNumCells() {
        return numCells;
    }

    /**
     * Returns the number of rows in the image.
     * 
     * @return An int which will be from 1 to 464
     */
    public int getNumRows() {
        return numRows;
    }

    /**
     * Returns the number of columns in the image.
     * 
     * @return An int which will be from 1 to 46
     */
    public int getNumCols() {
        return numCols;
    }

    /**
     * Parses the digital raster array header
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        iCoordStart = in.readUnsignedShort();
        jCoordStart = in.readUnsignedShort();
        iScaleFactor = in.readShort();
        jScaleFactor = in.readShort();
        numCells = in.readUnsignedShort();
        numRows = in.readUnsignedShort();

        readDigitalRasterDataArrayData(in);
    }

    /**
     * @param in
     * @throws IOException
     */
    protected void readDigitalRasterDataArrayData(DataInputStream in) throws IOException {
        // run through the first row to figure out the length of the rows
        if (in.markSupported()) {
            in.mark(Integer.MAX_VALUE);
        }
        int length = 0;

        int remainingBytes = in.readUnsignedShort();
        for (int b = 0; b < remainingBytes; b++) {
            in.readByte();
            length++;
        }
        numCols = length;
        in.reset();

        digitalRasterDataArray = new byte[numCols * numRows];

        for (int row = 0; row < numRows; row++) {
            remainingBytes = in.readUnsignedShort();

            for (int b = 0; b < remainingBytes; b++) {
                byte dataByte = in.readByte();
                setDigitalRasterDataArrayValue(row, b, dataByte);
            }
        }
    }

    /**
     * @param row
     * @param col
     * @param value
     */
    protected void setDigitalRasterDataArrayValue(int row, int col, byte value) {
        digitalRasterDataArray[row * numCols + col] = value;
    }

    @Override
    public String toString() {
        String s = super.toString() + " Digital Raster Array Data";
        s += "\n\t\tNum Rows: " + numRows;
        s += "\n\t\tNum Cols: " + numCols;

        return s;
    }

    public byte[] getDigitalRasterDataArrayData() {
        return digitalRasterDataArray;
    }
}
