
package gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3;

import gov.noaa.nws.ncep.edex.plugin.mosaic.util.MosaicUtil;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Level3Parser is a class that will allow the user to do the following:
 * 1. Decode a compressed or uncompressed mosaic file
 * 2. Parse a standard mosaic file header
 * 3. Extract the symbology block
 * 4. Only decode raster packet for now
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

public class Level3Parser {

    private static final int Z_DEFLATED = 8;

    private static final int DEF_WBITS = 15;

    private static final Pattern WMO_PATTERN = Pattern
            .compile("([A-Z]{4}[0-9]{2} [A-Z]{4} [0-9]{6})\\x0D\\x0D\\x0A(\\w{6})\\x0D\\x0D\\x0A");

    /** The logger */
    private final transient Log logger = LogFactory.getLog(getClass());

    protected DataInputStream theMosaicData = null;

    private ByteArrayInputStream theRawMosaicData = null;

    protected byte[] theRawMosaicByteArray = null;

    // message header
    private int theMessageCode;

    private Calendar theMsgTimestamp;

    private int theMsgLength;

    private int theSourceId;

    private int theDestinationId;

    private int numberOfBlocks;

    // product description block
    private double theLatitude;

    private double theLongitude;

    private int theHeight;

    private int theProductCode;

    private int theOperationalMode;

    private int theVolumeCoveragePattern;

    private int theSequenceNumber;

    private int theVolumeScanNumber;

    private Calendar theVolScanTime;

    private Calendar theProdGenTime;

    private static short[] productDependentValues;

    private int elevationNumber;

    private short[] dataLevelThresholds;
    
    private int maximumDataLevel;
    
    // Use for raster display!
    private static int numberOfColumns;

    private SymbologyBlock symbologyBlock;

    private int numberOfMaps;

    /**
     * This level3Parser constructor accepts a mosaic file contained within a
     * java.io.File object.
     * 
     * @param aMosaic
     *            A java.io.File object containing a raw mosaic file
     * @throws IOException
     *             If the mosaic head parsing fails, an IO exception is thrown
     */
    public Level3Parser(File aMosaic) throws IOException {
        int fileSize = (int) aMosaic.length();
        byte[] tempRawMosaicByteArray = new byte[fileSize];

        FileInputStream mosaicFile = null;

        try {
            mosaicFile = new FileInputStream(aMosaic);
            mosaicFile.read(tempRawMosaicByteArray);
        } finally {
            if (mosaicFile != null) {
                mosaicFile.close();
            }
        }
        init(tempRawMosaicByteArray);
    }

    /**
     * This level3Parser constructor accepts a mosaic file already converted to a
     * byte array.
     * 
     * @param aMosaic
     *            A byte array containing a raw mosaic file
     * @throws IOException
     *             If the mosaic head parsing fails, an IO exception is thrown
     */
    public Level3Parser(byte[] aMosaic) throws IOException {
        init(aMosaic);
    }

    /**
     * @param aMosaic
     * @throws IOException
     */
    private void init(byte[] aMosaic) throws IOException {
        // skip the WMO header if any
        String headerSearch = new String(aMosaic, 0, 80);

    	// decide where the mosaic starts
        int wmoHeaderSize = findStartMosaicData(headerSearch);

    	//check if mosaic file compressed or not compressed
        if (isCompressed(aMosaic, wmoHeaderSize)) {
            theRawMosaicByteArray = decompressMosaic(aMosaic, wmoHeaderSize);
        } else {
            theRawMosaicByteArray = new byte[aMosaic.length - wmoHeaderSize];
            System.arraycopy(aMosaic, wmoHeaderSize, theRawMosaicByteArray, 0,
                    theRawMosaicByteArray.length);
        }

        //allocate the same length of buf
        theRawMosaicData = new ByteArrayInputStream(theRawMosaicByteArray);
        //create DataInputStream so that it may be accessible
        //to read by the object "theMosaicData" in parseMosaicHeader
        theMosaicData = new DataInputStream(theRawMosaicData);

        // parse the header to get constants in message header block
        this.parseMosaicHeader();
        if (this.theMessageCode != 2) {
            this.parseMosaicMessage();
        }
    }

    public int getNumberOfMaps() {
		return numberOfMaps;
	}

	public void setNumberOfMaps(int numberOfMaps) {
		this.numberOfMaps = numberOfMaps;
	}

	/**
     * Returns the NEXRAD message code from the level 3 header 
     * 
     * @return An integer that can range from -131 to -16 and 0 to 211
     */
    public int getMessageCode() {
        return theMessageCode;
    }

    /**
     * Returns a Date object which represents the time of the message. This is
     * for the time of the message only and does not reflect the time of the
     * scan or the time the product was generated.
     * 
     * @return A Date object for the time the NEXRAD message was generated
     */
    public Calendar getMessageTimestamp() {
        return theMsgTimestamp;
    }

    /**
     * Returns the total length of the message. This is the number of bytes for
     * the entire message including this header information being parsed right
     * now.
     * 
     * @return A long which will be from 18 to 409586
     */
    public long getMessageLength() {
        return theMsgLength;
    }

    /**
     * Returns an integer reflecting the the sender of the message.
     * 
     * @return A number from 0 to 999
     */
    public int getSourceId() {
        return theSourceId;
    }

    /**
     * Returns an integer reflecting the the destination of the message.
     * 
     * @return A number from 0 to 999
     */
    public int getDestinationId() {
        return theDestinationId;
    }

    /**
     * Returns the latitude of the mosaic site. Positive indicates north,
     * negative will indicate south.
     * 
     * @return A latitude in decimal degree between -90.0 to 90.0
     */
    public double getLatitude() {
        return theLatitude;
    }

    /**
     * Returns the longitude of the mosaic site. Positive indicates east,
     * negative will indicate west.
     * 
     * @return A longitude in decimal degrees between -180.0 to 180.0
     */
    public double getLongitude() {
        return theLongitude;
    }

    /**
     * Returns the height of the mosaic. This will be in the units feet.
     * 
     * @return An integer from -100 to 11,000
     */
    public int getHeight() {
        return theHeight;
    }

    /**
     * Returns the internal NEXRAD product code of the weather product being
     * transmitted.
     * 
     * @return A product code from -131 to -16 and 16 to 131
     */
    public int getProductCode() {
        return theProductCode;
    }

    /**
     * Returns the operational mode the mosaic is currently running in. 
     * 
     * @return A operational mode number from 0 to 2
     */
    public int getOperationalMode() {
        return theOperationalMode;
    }

    /**
     * Returns the RDA volum coverage pattern. This indicates the scan strategy
     * being used. Not entirely sure what this means in an operational setting.
     * 
     * @return A number 1 to 767 for the volume coverage pattern
     */
    public int getVolumeCoveragePattern() {
        return theVolumeCoveragePattern;
    }

    /**
     * Returns the sequence number of the request that generated the NEXRAD
     * product. In case the product was requested by an alert condition, it will
     * return a -13.
     * 
     * @return A sequence number either -13 or 0 to 32767
     */
    public int getSequenceNumber() {
        return theSequenceNumber;
    }

    /**
     * Returns the mosaic site's internal counter entry. This will start at 1 and
     * go to 80.
     * 
     * @return A counter number 1 to 80
     */
    public int getVolumeScanNumber() {
        return theVolumeScanNumber;
    }

    /**
     * Returns a Date object which represents the time the scan was done. This
     * is for the time of the volume scan only and does not reflect the time of
     * the message or the time the product was generated.
     * 
     * @return A Date object for the time of the volume scan
     */
    public Calendar getVolumeScanTime() {
        return theVolScanTime;
    }

    /**
     * Returns a Date object which represents the time the product was
     * generated. This is for the time of product generation only and does not
     * reflect the time of the message or the time the scan was done.
     * 
     * @return A Date object for the time the product was generated
     */
    public Calendar getProductGenerationTime() {
        return theProdGenTime;
    }

    /**
     * Returns the entire raw mosaic message.
     * 
     * @return The mosaic message
     */
    public byte[] getRawMosaicData() {
        return theRawMosaicByteArray;
    }

    public static short getProductDependentValue(int value) {
        return productDependentValues[value];
    }

    public short[] getProductDependentValue() {
        return productDependentValues;
    }

    public short getDataLevelThreshold(int code) {
        return dataLevelThresholds[code];
    }

    private SymbologyBlock readSymbologyBlock(int offset) throws IOException {

        SymbologyBlock symBlock = null;
        if (offset != 0) {
            theMosaicData.reset();
            theMosaicData.skip(offset);
            int divider = theMosaicData.readShort();
            int blockId = theMosaicData.readUnsignedShort();
            if ((divider != -1) || (blockId != SymbologyBlock.getBlockId())) {
                throw new IOException(
                        "This does not appear to be a symbology block");
            }
            int blockLen = theMosaicData.readInt();
            byte[] buf = MosaicUtil.subArray(theRawMosaicByteArray, offset,
                    blockLen);
            DataInputStream in = new DataInputStream(new ByteArrayInputStream(
                    buf));

            // Skipping the number of layers, layer divider,
            // and data layer length
            in.skip(8);
            symBlock = new SymbologyBlock(in);
        }
        return symBlock;
    }

    /**
     * Goes through the mosaic header and reads in all the important fields.
     * 
     * @throws IOException
     */
    private void parseMosaicHeader() throws IOException {

        // Message Header Block
        theMessageCode = theMosaicData.readUnsignedShort();

        theMsgTimestamp = Calendar.getInstance();
        theMsgTimestamp.setTimeInMillis(this.createTimestamp(theMosaicData
                .readUnsignedShort(), theMosaicData.readInt()));
        
        theMsgLength = theMosaicData.readInt();

        // TODO: validate message length here and not everywhere else

        theSourceId = theMosaicData.readShort();

        theDestinationId = theMosaicData.readShort();

        numberOfBlocks = theMosaicData.readShort();
    }

    private void parseMosaicMessage() throws IOException {

        // Product Description Block
    	// skip two bytes for the "BLock Divider" - always equals to -1
        theMosaicData.skip(2);
        
        theLatitude = theMosaicData.readInt() * 0.001;
        theLongitude = theMosaicData.readInt() * 0.001;
       
        theHeight = theMosaicData.readShort();
        theProductCode = theMosaicData.readShort();

        theOperationalMode = theMosaicData.readUnsignedShort();
        theVolumeCoveragePattern = theMosaicData.readUnsignedShort();
        theSequenceNumber = theMosaicData.readShort();
        theVolumeScanNumber = theMosaicData.readUnsignedShort();
        theVolScanTime = Calendar.getInstance();
        theVolScanTime.setTimeInMillis(this.createTimestamp(theMosaicData
                .readUnsignedShort(), theMosaicData.readInt()));
        theProdGenTime = Calendar.getInstance();
        theProdGenTime.setTimeInMillis(this.createTimestamp(theMosaicData
                .readUnsignedShort(), theMosaicData.readInt()));

        productDependentValues = new short[10];
        productDependentValues[0] = theMosaicData.readShort();
        productDependentValues[1] = theMosaicData.readShort();
        
        elevationNumber = theMosaicData.readUnsignedShort();
        productDependentValues[2] = theMosaicData.readShort();

        dataLevelThresholds = new short[16];
        for (int i = 0; i < dataLevelThresholds.length; i++) {
            dataLevelThresholds[i] = theMosaicData.readShort();
        }

        maximumDataLevel = theMosaicData.readShort();

        //for (int i = 3; i < productDependentValues.length; i++) {
        //In Mosaic, there are only 8 product dependent values
        for (int i = 3; i < productDependentValues.length -2; i++) {
            productDependentValues[i] = theMosaicData.readShort();
        }
        
        //Use for raster display!
        numberOfColumns = theMosaicData.readShort();
        
        //Use for raster display!
        numberOfMaps = theMosaicData.readShort();
        int symbologyBlockOffset = theMosaicData.readInt() * 2;
        symbologyBlock = readSymbologyBlock(symbologyBlockOffset);            
    }

    /**
     * Takes the various time and date fields from the mosaic data and creates
     * and Date object.
     * 
     * @param aDayCount
     * @param aSecondCount
     * @return
     */
    private long createTimestamp(long aDayCount, long aSecondCount) {
        aDayCount--;
        long timeStamp = ((aDayCount * 60 * 60 * 24) + aSecondCount) * 1000;
        return timeStamp;
    }

    /**
     * Takes a string from the beginning of the file and determines if the WMO
     * header is there.
     * 
     * @param headerInfo
     *            The string from the beginning of the file that might contain a
     *            wmo header
     * @return The count from the beginning of the file to the actual mosaic data
     */
    private int findStartMosaicData(String headerInfo) {
        int startOfMosaicData = 0;
        Matcher matcher = WMO_PATTERN.matcher(headerInfo);
        boolean foundHeader = matcher.find();
        String wmoHeader = "";
        String awipsID = "";

        if (foundHeader) {
            wmoHeader = matcher.group(1);
            awipsID = matcher.group(2);
            startOfMosaicData = matcher.end();
        }

        return startOfMosaicData;
    }

    /**
     * Checks to see if data at the specified offset of the buffer is the start
     * of a compressed block
     * 
     * @param inBuf
     *            the data buffer
     * @param inOff
     *            the offset into the buffer
     * @return true if data is compressed
     */
    private boolean isCompressed(byte[] inBuf, int inOff) {
        int b0 = inBuf[inOff] & 0xFF;
        int b1 = inBuf[inOff + 1] & 0xFF;
        if ((b0 & 0xf) == Z_DEFLATED) {
            if ((b0 >> 4) + 8 <= DEF_WBITS) {
                if ((((b0 << 8) + b1) % 31) == 0) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Method to handle compressed mosaic data. If data is not compressed it is
     * just copied to the output buffer.
     * 
     * @return The decompressed byte array for the mosaic data
     */
    private byte[] decompressMosaic(byte[] inBuf, int offset) {
        byte[] outBuf = new byte[4000];
        int inOff = offset;
        int outOff = 0;
        Inflater decompressor = new Inflater();
        int decompressedLen = 0;

        try {
            do {
                if (inBuf.length - inOff <= 0) {
                    logger
                            .error("An error occurred.  Apparently, the mosaic product expects more data.  Aborting decompress.");
                    break;
                }

                // if compressed then decompress this block
                if (isCompressed(inBuf, inOff)) {
                    decompressor.reset();
                    decompressor.setInput(inBuf, inOff, inBuf.length - inOff);
                    decompressor
                            .inflate(outBuf, outOff, outBuf.length - outOff);

                    inOff += decompressor.getTotalIn();
                    outOff += decompressor.getTotalOut();

                    // else just copy the remainder of the data
                } else {
                    int len = inBuf.length - inOff;
                    len = Math.min(len, outBuf.length - outOff);
                    System.arraycopy(inBuf, inOff, outBuf, outOff, len);
                    inOff += len;
                    outOff += len;
                }

                if (decompressedLen == 0) {
                    decompressedLen = (outBuf[62] << 24 & 0xFF000000)
                            | (outBuf[63] << 16 & 0x00FF0000)
                            | (outBuf[64] << 8 & 0x0000FF00)
                            | (outBuf[65] & 0x000000FF);

                    byte[] tmpBuf = outBuf;
                    outBuf = new byte[decompressedLen];
                    outOff = Math.min(decompressedLen, outOff - 54);
                    System.arraycopy(tmpBuf, 54, outBuf, 0, outOff);
                }

            } while (outOff < outBuf.length);

        } catch (DataFormatException e) {
            logger
                    .error("Invalid data format encountered during decompression");
        } finally {
            decompressor.end();
        }

        return outBuf;
    }

    public int getNumberOfBlocks() {
        return numberOfBlocks;
    }

    public int getElevationNumber() {
        return elevationNumber;
    }

    public int getTheSourceId() {
        return theSourceId;
    }

    public void setTheSourceId(int theSourceId) {
        this.theSourceId = theSourceId;
    }

    public int getTheDestinationId() {
        return theDestinationId;
    }

    public void setTheDestinationId(int theDestinationId) {
        this.theDestinationId = theDestinationId;
    }

    public SymbologyBlock getSymbologyBlock() {
        return symbologyBlock;
    }

	public int getMaximumDataLevel() {
		return maximumDataLevel;
	}

	public void setMaximumDataLevel(int maximumDataLevel) {
		this.maximumDataLevel = maximumDataLevel;
	}

	public static int getNumberOfColumns() {
		return numberOfColumns;
	}

	@SuppressWarnings("static-access")
	public void setNumberOfColumns(int numberOfColumns) {
		this.numberOfColumns = numberOfColumns;
	}

}
