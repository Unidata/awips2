
package gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3;

import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.Level3Parser;
import java.io.DataInputStream;
import java.io.IOException;

/**
 * Abstract class that defines the binary values for the various raster
 * layers that are sent out in Level III format.
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 09/2009      143				L. Lin     	Initial coding
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

public class RasterPacket extends SymbologyPacket {

    public static final int RASTER_DATA_PACKET1 = 0xBA07;

    public static final int RASTER_DATA_PACKET2 = 0xBA0F;
    static {
        PacketFactory.registerPacketType(RasterPacket.class,
                RASTER_DATA_PACKET1, RASTER_DATA_PACKET2);
    }

    protected int theICenter;

    protected int theJCenter;

    protected int numRows;

    protected int numCols;

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
        return theICenter;
    }

    /**
     * Returns the J coordinate of the center of sweep
     * 
     * @return An int which will be from -2048 to 2048
     */
    public int getJCenter() {
        return theJCenter;
    }

    /**
     * Returns the number of rows in the image.
     * 
     * @return An int which will be from 1 to 400
     */
    public int getNumRows() {
        return numRows;
    }

    /**
     * Parses the raster header
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        in.skip(4);
        
        //Starts at 72th
        theICenter = in.readUnsignedShort();
        theJCenter = in.readUnsignedShort();
        in.skip(8);

        //Starts at 78th
        numRows = in.readUnsignedShort();
        numCols = Level3Parser.getNumberOfColumns();

        in.skip(2);
        rasterData = new byte[numRows * numCols];
        readRasterData(in);
    }

    /**
     * @param in
     * @throws IOException
     */
    protected void readRasterData(DataInputStream in) throws IOException {
    	
        for (int row = 0; row < numRows; row++) {
            int remainingBytes = in.readUnsignedShort();

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
