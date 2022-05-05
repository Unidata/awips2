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

package com.raytheon.edex.plugin.radar.level3;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.itadaki.bzip2.BZip2InputStream;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.radar.util.RadarEdexTextProductUtil;
import com.raytheon.edex.plugin.radar.util.RadarSpatialUtil;
import com.raytheon.uf.common.dataplugin.exception.MalformedDataException;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.level3.CPMBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.GSMBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.GraphicBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.TabularBlock;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfo;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataplugin.radar.util.RadarTextProductUtil;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * BaseRadar is a class that will allow the user to do the following:
 * <ul>
 * <li>Decode a compressed or uncompressed radar file</li>
 * <li>Parse a standard radar file header</li>
 * <li>Extract the symbology block</li>
 * <li>Extract the graphic block</li>
 * <li>Extract the tabular block</li>
 * </ul>
 * <p>
 * This base radar class will parse the header common to all NEXRAD Level III
 * data files (based on the National Climatic Data Center Data Documentation
 * DSI-7000 for NEXRAD Level III data <a href="
 * http://www1.ncdc.noaa.gov/pub/data/documentlibrary/tddoc/td7000.pdf"> located
 * here</a>) and make it available via common methods as described in this
 * JavaDoc.
 * </p>
 * <p>
 * Additionally, the library allows the user to retrieve the three blocks that
 * might be encoded in the file. Those are the symbology block (typically
 * contains the radial data), the graphics block (a per-pixel graphic image),
 * and a tabular dataset (usually storm direction and vortex alert location).
 * </p>
 * <p>
 * All coordinates which appear as arguments to the methods of this Graphics
 * object are considered relative to the translation origin of this Graphics
 * object prior to the invocation of the method. All rendering operations modify
 * only pixels which lie within the area bounded by both the current clip of the
 * graphics context and the extents of the Component used to create the Graphics
 * object.
 *
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#   Engineer     Description
 * ------------- --------- ------------ ----------------------------------------
 * --/--/2006              brockwoo     Initial creation
 * Jan 21, 2014  2627      njensen      Changed offset errors to
 *                                      MalformedDataException
 * May 14, 2014  2536      bclement     moved WMO Header to common, removed
 *                                      TimeTools usage added storeTextProduct()
 * Jul 13, 2015  17672     D. Friedman  Only decompress products documented to
 *                                      support compression
 * Apr 14, 2016  18800     jdynina      Remove UAM and AM
 * Apr 25, 2016  18796     jdynina      Implemented SCL
 * May 09, 2016  DCS18795  jdynina      Implemented CPM
 * Sep 18, 2017  DCS19530  jdynina      Pad FTM text to use even-numbered char
 *                                      line counts per ICD
 * Mar 26, 2018  6711      randerso     Code cleanup
 * Apr 24, 2018  DCS20681  jdynina      Add MRLE to CPM processing
 * Jun 21, 2019  7629      mroos        Added setting of delta time and scan type
 *                                      for supporting product codes.
 * </pre>
 *
 * @author Bryan Rockwood
 */
public class Level3BaseRadar {
    /** The logger */
    private static final IUFStatusHandler theHandler = UFStatus
            .getHandler(Level3BaseRadar.class);

    private static final Pattern WMO_PATTERN = Pattern.compile(
            "([A-Z]{4}[0-9]{2} [A-Z]{4} [0-9]{6})\\x0D\\x0D\\x0A(\\w{6})\\x0D\\x0D\\x0A");

    private static final List<Integer> SPECIAL_PRODS = Arrays.asList(62, 75, 77,
            82);

    public static final int GENERAL_STATUS_MESSAGE = 2;

    public static final int PRODUCT_REQUEST_RESPONSE_MESSAGE = 3;

    public static final int PRODUCT_LIST = 8;

    public static final int COMMAND_PARAMETER_MESSAGE = 12;

    public static final int RADAR_CODED_MESSAGE = 74;

    public static final int FREE_TEXT_MESSAGE = 75;

    public static final int SHIFT_CHANGE_CHECKLIST = 202;

    // TODO - consider making this list into a config file if it needs modified
    /**
     * These values are the current product codes that support scan type and
     * delta time denotation within productDependentValues[6]
     */
    private static final List<Integer> SCAN_TYPE_PRODVALS = Arrays.asList(94,
            99, 153, 154, 155, 167, 168, 149, 143, 159, 161, 163, 165);

    protected DataInputStream theRadarData = null;

    protected byte[] theRawRadarByteArray = null;

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

    private short[] productDependentValues;

    private int elevationNumber;

    private short[] dataLevelThresholds;

    private SymbologyBlock symbologyBlock;

    private GraphicBlock graphicBlock;

    private TabularBlock tabularBlock;

    private CPMBlock cpmBlock;

    private GSMBlock gsmBlock;

    private String afosId = "";

    private String radarLoc = "";

    private int productVersion;

    private int productSpotBlank;

    private String rrmessage;

    private String wmoHeader = "";

    private RadarInfoDict dict = null;

    private short deltaTime = 0;

    private RadarRecord.ScanType scanType = RadarRecord.ScanType.NORMAL;

    /**
     * This baseradar constructor accepts a radar file contained within a
     * java.io.File object.
     *
     * @param aRadar
     *            A java.io.File object containing a raw radar file
     * @throws IOException
     *             If the radar head parsing fails, an IO exception is thrown
     * @throws MalformedDataException
     */
    public Level3BaseRadar(File aRadar, Headers headers)
            throws IOException, MalformedDataException {
        int fileSize = (int) aRadar.length();
        byte[] tempRawRadarByteArray = new byte[fileSize];

        try (FileInputStream radarFile = new FileInputStream(aRadar)) {
            radarFile.read(tempRawRadarByteArray);
        }

        init(tempRawRadarByteArray, headers);
    }

    /**
     * This baseradar constructor accepts a radar file already converted to a
     * byte array.
     *
     * @param aRadar
     *            A byte array containing a raw radar file
     * @throws IOException
     *             If the radar head parsing fails, an IO exception is thrown
     * @throws MalformedDataException
     */
    public Level3BaseRadar(byte[] aRadar, Headers headers)
            throws IOException, MalformedDataException {
        init(aRadar, headers);
    }

    public Level3BaseRadar(byte[] aRadar, Headers headers, RadarInfoDict dict)
            throws IOException, MalformedDataException {
        this.dict = dict;
        init(aRadar, headers);
    }

    /**
     * @param aRadar
     * @throws IOException
     * @throws MalformedDataException
     */
    private void init(byte[] aRadar, Headers headers)
            throws IOException, MalformedDataException {
        // printPacketContents(aRadar);

        int wmoHeaderSize;
        if (aRadar.length < 80) {
            wmoHeaderSize = 0;
        } else {
            // skip the WMO header if any
            String headerSearch = new String(aRadar, 0, 80);
            wmoHeaderSize = findStartRadarData(headerSearch);
        }

        theRawRadarByteArray = new byte[aRadar.length - wmoHeaderSize];
        System.arraycopy(aRadar, wmoHeaderSize, theRawRadarByteArray, 0,
                theRawRadarByteArray.length);

        theRadarData = new DataInputStream(
                new ByteArrayInputStream(theRawRadarByteArray));

        // parse the header
        this.parseRadarHeader();

        // Handle the message contents
        if (this.theMessageCode == PRODUCT_REQUEST_RESPONSE_MESSAGE) {
            this.parseRequestResponse();
        } else if (this.theMessageCode == PRODUCT_LIST) {
            this.parseProductList(headers);
        } else if (this.theMessageCode == GENERAL_STATUS_MESSAGE) {
            this.parseGeneralStatusMessage();
        } else if (this.theMessageCode == COMMAND_PARAMETER_MESSAGE) {
            this.parseCommandParameterMessage();
        } else {
            this.parseRadarMessage(headers);
        }
    }

    /**
     * @return the productVersion
     */
    public int getProductVersion() {
        return productVersion;
    }

    /**
     * @param productVersion
     *            the productVersion to set
     */
    public void setProductVersion(int productVersion) {
        this.productVersion = productVersion;
    }

    /**
     * @return the productSpotBlank
     */
    public int getProductSpotBlank() {
        return productSpotBlank;
    }

    /**
     * @param productSpotBlank
     *            the productSpotBlank to set
     */
    public void setProductSpotBlank(int productSpotBlank) {
        this.productSpotBlank = productSpotBlank;
    }

    /**
     * Returns the NEXRAD message code from the level 3 header defined in table
     * II of the <a href="
     * http://www1.ncdc.noaa.gov/pub/data/documentlibrary/tddoc/td7000.pdf">
     * DSI-7000</a>. For example, 19 will indicate a base reflectivity product
     * at 0.5 deg elevation.
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
     * Returns the latitude of the radar site. Positive indicates north,
     * negative will indicate south.
     *
     * @return A latitude in decimal degree between -90.0 to 90.0
     */
    public double getLatitude() {
        return theLatitude;
    }

    /**
     * Returns the longitude of the radar site. Positive indicates east,
     * negative will indicate west.
     *
     * @return A longitude in decimal degrees between -180.0 to 180.0
     */
    public double getLongitude() {
        return theLongitude;
    }

    /**
     * Returns the height of the radar. This will be in the units feet.
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
     * Returns the operational mode the radar is currently running in. These are
     * broken down by:
     * <ul>
     * <li>0 - Maintenance</li>
     * <li>1 - Clear Air</li>
     * <li>2 - Precipitation/Severe Weather</li>
     * </ul>
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
     * Returns the radar site's internal counter entry. This will start at 1 and
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
     * Returns the entire raw radar message.
     *
     * @return The radar message
     */
    public byte[] getRawRadarData() {
        return theRawRadarByteArray;
    }

    // /**
    // * Per the definition for the level three product (see Table V starting at
    // * page 29 in the DSI-7000 linked to above), each product type has various
    // * thresholds and product specific data. This block contains that
    // * information. Halfwords 5 though 20 (bytes 8 - 40) contain the data
    // * threshold values.
    // *
    // * @return A byte array containing all the product specific code
    // */
    // public byte[] getThresholds() {
    // return theThresholds;
    // }

    public short getProductDependentValue(int value) {
        return productDependentValues[value];
    }

    public short[] getProductDependentValue() {
        return productDependentValues;
    }

    public short getDataLevelThreshold(int code) {
        return dataLevelThresholds[code];
    }

    private SymbologyBlock readSymbologyBlock(int offset)
            throws IOException, MalformedDataException {
        SymbologyBlock symBlock = null;
        if (offset != 0) {
            theRadarData.reset();
            theRadarData.skip(offset);
            int divider = theRadarData.readShort();
            int blockId = theRadarData.readUnsignedShort();
            if ((divider != -1) || (blockId != SymbologyBlock.getBlockId())) {
                throw new MalformedDataException("Symbology block offset "
                        + offset + " does not point to a symbology block");
            }
            int blockLen = theRadarData.readInt();
            byte[] buf = RadarUtil.subArray(theRawRadarByteArray, offset,
                    blockLen);
            DataInputStream in = new DataInputStream(
                    new ByteArrayInputStream(buf));

            // Skipping the number of layers, layer divider,
            // and data layer length
            in.skip(8);
            symBlock = new SymbologyBlock(in);
        }
        return symBlock;
    }

    private GraphicBlock readGraphicBlock(int offset)
            throws IOException, MalformedDataException {
        GraphicBlock graphicBlock = null;
        if (offset != 0) {
            theRadarData.reset();
            theRadarData.skip(offset);
            int divider = theRadarData.readShort();
            int blockId = theRadarData.readUnsignedShort();
            if ((divider != -1) || (blockId != GraphicBlock.getBlockId())) {
                throw new MalformedDataException("Graphic block offset "
                        + offset + " does not point to a graphic block");
            }
            int blockLen = theRadarData.readInt();
            byte[] buf = RadarUtil.subArray(theRawRadarByteArray, offset,
                    blockLen);
            DataInputStream in = new DataInputStream(
                    new ByteArrayInputStream(buf));
            in.skip(8);
            graphicBlock = new GraphicBlock(in);
        }
        return graphicBlock;
    }

    private TabularBlock readTabularBlock(int offset)
            throws IOException, MalformedDataException {
        TabularBlock tabBlock = null;
        if (offset != 0) {
            theRadarData.reset();
            theRadarData.skip(offset);
            int divider = theRadarData.readShort();
            int blockId = theRadarData.readUnsignedShort();
            if ((divider != -1) || (blockId != TabularBlock.getBlockId())) {
                throw new MalformedDataException("Tabular block offset "
                        + offset + " does not point to a tabular block");
            }
            int blockLen = theRadarData.readInt();
            byte[] buf = RadarUtil.subArray(theRawRadarByteArray, offset,
                    blockLen);
            DataInputStream in = new DataInputStream(
                    new ByteArrayInputStream(buf));
            in.skip(8);
            tabBlock = new TabularBlock(in);
        }

        return tabBlock;
    }

    /**
     *
     * @return
     * @throws IOException
     */
    private TabularBlock readStandaloneTabular(int offset)
            throws IOException, MalformedDataException {
        int divider = theRadarData.readShort();
        TabularBlock tabBlock = new TabularBlock();
        int numPages = theRadarData.readUnsignedShort();
        if ((divider != -1)) {
            throw new MalformedDataException("Standalone tabular block offset "
                    + offset + " does not point to a standalone tabular block");
        }
        List<List<String>> pages = new ArrayList<>();
        for (int p = 0; p < numPages; p++) {
            List<String> page = new ArrayList<>();
            int lineLen = theRadarData.readUnsignedShort();
            while (lineLen != 0xFFFF) {
                if (this.theMessageCode == FREE_TEXT_MESSAGE) {
                    /*
                     * FTM pads lines to even number of characters
                     */
                    if ((lineLen % 2) > 0) {
                        lineLen = lineLen + 1;
                    }
                }
                byte[] buf = new byte[lineLen];
                theRadarData.readFully(buf);
                page.add(new String(buf));
                lineLen = theRadarData.readUnsignedShort();
            }
            pages.add(page);
        }
        tabBlock.setPages(pages);
        return tabBlock;
    }

    /**
     *
     * @return
     * @throws IOException
     */
    private TabularBlock readRadarCodedMessage(int offset) throws IOException {
        TabularBlock tabBlock = new TabularBlock();
        int temp = theRadarData.read();
        List<Byte> array = new ArrayList<>();
        while (temp != -1) {
            array.add((byte) temp);
            temp = theRadarData.read();
        }
        byte[] theTemp = new byte[array.size()];
        for (int i = 0; i < theTemp.length; i++) {
            theTemp[i] = array.get(i).byteValue();
        }
        String string = new String(theTemp);
        String[] strings = string.split("\\s\\s\\s*");
        StringBuilder fullString = new StringBuilder();
        for (String strng : strings) {
            fullString.append(strng.trim()).append("\n");
        }
        tabBlock.setString(fullString.toString());
        return tabBlock;
    }

    /**
     * Returns the graphic block. <b>DO NOT USE</b> This is a placeholder method
     * that will be implemented at a later date.
     */
    public GraphicBlock getGraphicBlock() {
        return graphicBlock;
    }

    /**
     * Returns the tabular block. <b>DO NOT USE</b> This is a placeholder method
     * that will be implemented at a later date.
     */
    public TabularBlock getTabularBlock() {
        return tabularBlock;
    }

    /**
     * Goes through the radar header and reads in all the important fields.
     *
     * @throws IOException
     */
    private void parseRadarHeader() throws IOException {
        // Message Header Block
        theMessageCode = theRadarData.readUnsignedShort();
        theMsgTimestamp = Calendar.getInstance();
        theMsgTimestamp.setTimeInMillis(this.createTimestamp(
                theRadarData.readUnsignedShort(), theRadarData.readInt()));

        theMsgLength = theRadarData.readInt();

        // TODO: validate message length here and not everywhere else

        theSourceId = theRadarData.readShort();
        theDestinationId = theRadarData.readShort();
        numberOfBlocks = theRadarData.readShort();

    }

    private void parseRadarMessage(Headers headers)
            throws IOException, MalformedDataException {
        // Product Description Block
        theRadarData.skip(2);
        theLatitude = theRadarData.readInt() * 0.001;
        theLongitude = theRadarData.readInt() * 0.001;
        theHeight = theRadarData.readShort();
        theProductCode = theRadarData.readShort();
        theOperationalMode = theRadarData.readUnsignedShort();
        theVolumeCoveragePattern = theRadarData.readUnsignedShort();
        theSequenceNumber = theRadarData.readShort();
        theVolumeScanNumber = theRadarData.readUnsignedShort();
        theVolScanTime = Calendar.getInstance();
        theVolScanTime.setTimeInMillis(this.createTimestamp(
                theRadarData.readUnsignedShort(), theRadarData.readInt()));
        theProdGenTime = Calendar.getInstance();
        theProdGenTime.setTimeInMillis(this.createTimestamp(
                theRadarData.readUnsignedShort(), theRadarData.readInt()));

        productDependentValues = new short[10];
        productDependentValues[0] = theRadarData.readShort();
        productDependentValues[1] = theRadarData.readShort();

        elevationNumber = theRadarData.readUnsignedShort();

        productDependentValues[2] = theRadarData.readShort();

        dataLevelThresholds = new short[16];
        for (int i = 0; i < dataLevelThresholds.length; i++) {
            dataLevelThresholds[i] = theRadarData.readShort();
        }

        for (int i = 3; i < productDependentValues.length; i++) {
            productDependentValues[i] = theRadarData.readShort();
        }

        productVersion = theRadarData.readByte();

        productSpotBlank = theRadarData.readByte();
        lookupAfosId();
        int symbologyBlockOffset = theRadarData.readInt() * 2;
        int graphicBlockOffset = theRadarData.readInt() * 2;
        int tabularBlockOffset = theRadarData.readInt() * 2;

        RadarInfo radarInfo = dict.getInfo(theProductCode);

        if (productDependentValues[7] == 1 && radarInfo != null
                && radarInfo.isCompressionAllowed()) {
            int uncompressedSize = ((productDependentValues[8] & 0xffff) << 16)
                    | (productDependentValues[9] & 0xffff);
            byte[] uncompressed = null;
            byte[] msg = new byte[120];
            InputStream byt;
            if (uncompressedSize + msg.length != theRawRadarByteArray.length) {
                try {
                    theRadarData.reset();
                    theRadarData.readFully(msg);
                    try (DataInputStream di = new DataInputStream(
                            new BZip2InputStream(theRadarData, false))) {
                        uncompressed = new byte[uncompressedSize];
                        di.readFully(uncompressed);
                    }
                } catch (IOException e) {
                    theHandler.handle(Priority.ERROR,
                            "Error decompressing product: ", e);
                    return;
                }
                theRawRadarByteArray = new byte[120 + uncompressed.length];
                System.arraycopy(msg, 0, theRawRadarByteArray, 0, 120);
                System.arraycopy(uncompressed, 0, theRawRadarByteArray, 120,
                        uncompressed.length);
                byt = new ByteArrayInputStream(theRawRadarByteArray);
                theRadarData = new DataInputStream(byt);
            }
        }

        if (SPECIAL_PRODS.contains(this.theProductCode)) {
            // The first offset will be to the tabular block
            tabularBlock = readStandaloneTabular(symbologyBlockOffset);
            // The second offset will be to a symbology block with no header
            symbologyBlock = readPseudoSymbologyBlock(graphicBlockOffset);
            // tabularBlock.getPages().toString();
        } else if (this.theProductCode == RADAR_CODED_MESSAGE) {
            tabularBlock = readRadarCodedMessage(symbologyBlockOffset);
        } else if (this.theProductCode == SHIFT_CHANGE_CHECKLIST) {
            symbologyBlock = readSymbologyBlock(symbologyBlockOffset);

            if (RadarTextProductUtil.radarTable.keySet()
                    .contains(theProductCode)) {
                storeTextProduct(headers);
            }
        } else {
            symbologyBlock = readSymbologyBlock(symbologyBlockOffset);
            graphicBlock = readGraphicBlock(graphicBlockOffset);
            tabularBlock = readTabularBlock(tabularBlockOffset);
        }

        if (tabularBlock != null) {
            if (tabularBlock.getString() == null
                    || tabularBlock.getString().isEmpty()) {
                tabularBlock.setString(tabularBlock.toString());
            }
            if (RadarTextProductUtil.radarTable.keySet()
                    .contains(theProductCode)) {
                storeTextProduct(headers);
            }
        }

        // sets the delta time of the cut if it is one of the products that
        // supports delta time in productDependentValues[6]
        if (SCAN_TYPE_PRODVALS.contains(this.theProductCode)) {
            short status = productDependentValues[6];
            // create a mask representing 11 bits
            int mask = ((1 << 11) - 1);
            // bitwise 'and' the status value and the mask to get the integer
            // value of bits 5-15 of 'status'
            int cutTime = (status >> 5) & mask;
            this.deltaTime = (short) cutTime;

            // sets the scan type of the cut if it is one of the products that
            // supports scan type in productDependentValues[6]
            if (status != 0) {
                // Grab the first two bits of the short in
                // productDependentValues[6]
                int statFlag = status & 0b11;
                if (statFlag == 1) {
                    // value of first two bits == 1: MRLE cut
                    this.scanType = RadarRecord.ScanType.MRLE;
                } else if (statFlag == 2) {
                    // value of first two bits == 2: SAILS cut
                    this.scanType = RadarRecord.ScanType.SAILS;
                }
            }
        }
        // If no value for ProductDependentValue[6], then it is a normal cut
    }

    private void parseProductList(Headers headers) throws IOException {
        tabularBlock = new TabularBlock();
        DecimalFormat format = new DecimalFormat("#.#");
        theRadarData.skip(4);
        int numProducts = theRadarData.readShort();
        theRadarData.skip(2);
        StringBuilder builder = new StringBuilder();
        builder.append("\t\t\tRPG PRODUCTS AVAILABLE (" + numProducts
                + " AVAILABLE)\n\n");
        builder.append(
                "\tPROD\tPROD\tDATA\n\tID#\t\tNAME\tLVL\t\tRES\t\tSLICE\tPARAM1\tPARAM2\tPARAM3\tPARAM4\n\t----\t----\t---\t\t---\t\t-----\t------\t------\t------\t------\n");

        for (int i = 0; i < numProducts; i++) {
            builder.append("");
            int productCode = theRadarData.readShort();
            if (dict.getInfo(productCode) == null) {
                // radarInfo.txt does not contain this product code
                continue;
            }
            double elevation = (theRadarData.readShort() * .1);
            String elev = format.format(elevation);
            if (elevation == 0) {
                elev = "";
            }

            int param1 = theRadarData.readShort();
            int param2 = theRadarData.readShort();
            int param3 = theRadarData.readShort();
            int param4 = theRadarData.readShort();
            if (param1 == 0) {

            }
            String params = String.valueOf(param1 == 0 ? "" : param1) + "\t\t"
                    + String.valueOf(param2 == 0 ? "" : param2) + "\t\t"
                    + String.valueOf(param3 == 0 ? "" : param3) + "\t\t"
                    + String.valueOf(param4 == 0 ? "" : param4);
            theRadarData.skip(2);
            try {
                builder.append("\t" + productCode + "\t\t"
                        + dict.getInfo(productCode).getMnemonic() + "\t\t"
                        + (dict.getInfo(productCode).getNumLevels() == 0 ? ""
                                : dict.getInfo(productCode).getNumLevels())
                        + "\t\t"
                        + (dict.getInfo(productCode).getResolution() == 0 ? ""
                                : format.format(
                                        (double) dict.getInfo(productCode)
                                                .getResolution() / 1000))
                        + "\t\t" + elev + "\t\t" + params + "\n");
            } catch (Exception e) {
                theHandler.warn("Error building string for Tabular Block", e);
            }
        }

        tabularBlock.setString(builder.toString());

        lookupAfosId();

        if (RadarTextProductUtil.radarTable.keySet().contains(theMessageCode)) {
            storeTextProduct(headers);
        }
    }

    /**
     * Stores text from tabular block if AFOS product id is filled.
     *
     * @see RadarEdexTextProductUtil#storeTextProduct(AFOSProductId, WMOHeader,
     *      String, boolean, Calendar)
     * @see AFOSProductId#isFilled()
     * @param headers
     */
    private void storeTextProduct(Headers headers) {
        byte[] wmoid = wmoHeader.getBytes();
        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        WMOHeader header = new WMOHeader(wmoid, fileName);
        AFOSProductId afos = new AFOSProductId(afosId);
        if (afos.isFilled()) {
            try {
                Calendar cal = (WMOTimeParser.allowArchive() ? theMsgTimestamp
                        : Calendar.getInstance());

                if (this.theProductCode == SHIFT_CHANGE_CHECKLIST) {
                    RadarEdexTextProductUtil.storeTextProduct(afos, header,
                            symbologyBlock.getString(), true, cal);
                } else {
                    RadarEdexTextProductUtil.storeTextProduct(afos, header,
                            tabularBlock.getString(), true, cal);
                }

            } catch (Exception e) {
                theHandler.handle(Priority.ERROR,
                        "Could not store text product", e);
            }
        }
    }

    /**
     * Set the afosId and radarLoc based off the sourceId.
     */
    private void lookupAfosId() {
        if (afosId.isEmpty()) {
            if (radarLoc.isEmpty()) {
                try {
                    RadarStation loc = RadarSpatialUtil
                            .getRadarStationByRpgIdDec(theSourceId);
                    if (loc != null) {
                        radarLoc = loc.getRdaId();
                        radarLoc = radarLoc.substring(1);
                    }
                } catch (DataAccessLayerException e) {
                    theHandler.handle(Priority.ERROR,
                            "Unable to query database for radar location", e);
                }
            }
            afosId = RadarTextProductUtil.createAfosId(theMessageCode,
                    radarLoc);
        }
    }

    private void parseRequestResponse() throws IOException {
        String temp = new String();
        theRadarData.skip(4);
        int errorCode = theRadarData.readInt();
        int sequenceNumber = theRadarData.readShort();
        int prodCode = theRadarData.readShort();
        int elevAngle = theRadarData.readShort();
        int volScanDate = theRadarData.readShort();
        int volScanTime = theRadarData.readInt();
        theRadarData.skip(8);
        long tmp = ((volScanDate - 1) * 24l * 60l * 60l * 1000l
                + volScanTime * 1000l);
        Date date = new Date(tmp);
        DataTime time = new DataTime(date);
        temp += RadarUtil.formatBits(errorCode,
                RadarConstants.requestReponseErrorCode) + "\n\n"
                + "Product Code : " + prodCode + "\nDate : " + time
                + "\nElevation : " + elevAngle + "\nSequence Number : "
                + sequenceNumber;
        setRequestResponseMessage(temp);
    }

    private void parseGeneralStatusMessage()
            throws IOException, MalformedDataException {
        int divider = theRadarData.readShort();
        if ((divider != -1)) {
            throw new MalformedDataException("This is not a gsm block");
        }
        int blockLen = theRadarData.readShort();
        byte[] buf = RadarUtil.subArray(theRawRadarByteArray, 22, blockLen);
        DataInputStream in = new DataInputStream(new ByteArrayInputStream(buf));
        gsmBlock = new GSMBlock(in);
    }

    private void parseCommandParameterMessage()
            throws IOException, MalformedDataException {
        int divider = theRadarData.readShort();
        if ((divider != -1)) {
            throw new MalformedDataException("This is not a cpm block");
        }

        // skip version number
        theRadarData.readShort();

        // skip block length
        theRadarData.readShort();

        byte[] buf = RadarUtil.subArray(theRawRadarByteArray, 24,
                theRawRadarByteArray.length - 24);
        DataInputStream in = new DataInputStream(new ByteArrayInputStream(buf));
        cpmBlock = new CPMBlock(in);

        in.close();
    }

    private SymbologyBlock readPseudoSymbologyBlock(int offset)
            throws IOException {
        SymbologyBlock symBlock = null;

        /*
         * This pseudo block is missing this header information:
         *
         * 0xFFFF - Block divider
         *
         * 0x0001 - Symbology Block ID
         *
         * 0x????
         *
         * 0x???? - The block length
         *
         * 0x0001 - Number of layers <=== Where SymbologyBlock starts to read
         *
         * 0xFFFF - Layer divider
         *
         * 0x????
         *
         * 0x???? - The Layer length
         */

        if (offset != 0) {
            int layerLength = theMsgLength - offset;
            // Block length = total Msg Length - offset + Missing Header Size
            int blockLen = layerLength + 8;

            // Generate pertinent missing header information
            byte[] symbologyBlock = new byte[blockLen];

            // Set the number of layers
            symbologyBlock[0] = 0x00;
            symbologyBlock[1] = 0x01;

            // Set layer divider
            symbologyBlock[2] = (byte) 0xFF;
            symbologyBlock[3] = (byte) 0xFF;

            // Set the layer length a byte at a time
            symbologyBlock[4] = (byte) (0xFF & (layerLength >> 24));
            symbologyBlock[5] = (byte) (0xFF & (layerLength >> 16));
            symbologyBlock[6] = (byte) (0xFF & (layerLength >> 8));
            symbologyBlock[7] = (byte) (0xFF & layerLength);

            // copy out the Symbology Data
            System.arraycopy(theRawRadarByteArray, offset, symbologyBlock, 8,
                    layerLength);
            DataInputStream in = new DataInputStream(
                    new ByteArrayInputStream(symbologyBlock));

            symBlock = new SymbologyBlock(in);
        }

        return symBlock;
    }

    /**
     * Takes the various time and date fields from the radar data and creates
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
     * @return The count from the beginning of the file to the actual radar data
     */
    private int findStartRadarData(String headerInfo) {
        int startOfRadarData = 0;
        Matcher matcher = WMO_PATTERN.matcher(headerInfo);
        boolean foundHeader = matcher.find();
        String awipsID = "";
        if (foundHeader) {
            wmoHeader = matcher.group(1);
            awipsID = matcher.group(2);
            radarLoc = awipsID.substring(awipsID.length() - 3,
                    awipsID.length());
            // afosId = wmoHeader + awipsID;
            startOfRadarData = matcher.end();
        } else {
            wmoHeader = "";
        }

        return startOfRadarData;
    }

    public int getNumberOfBlocks() {
        return numberOfBlocks;
    }

    public int getElevationNumber() {
        return elevationNumber;
    }

    public SymbologyBlock getSymbologyBlock() {
        return symbologyBlock;
    }

    /**
     * @return the gsmBlock
     */
    public GSMBlock getGsmBlock() {
        return gsmBlock;
    }

    /**
     * @return the cpmBlock
     */
    public CPMBlock getCpmBlock() {
        return cpmBlock;
    }

    public String getRequestResponseMessage() {
        return this.rrmessage;
    }

    public void setRequestResponseMessage(String msg) {
        this.rrmessage = msg;
    }

    /**
     * @return the dataLevelThresholds
     */
    public short[] getDataLevelThresholds() {
        return dataLevelThresholds;
    }

    /**
     * @return the deltaTime
     */
    public short getDeltaTime() {
        return deltaTime;
    }

    /**
     * @return the scan type
     */
    public RadarRecord.ScanType getScanType() {
        return scanType;
    }

}
