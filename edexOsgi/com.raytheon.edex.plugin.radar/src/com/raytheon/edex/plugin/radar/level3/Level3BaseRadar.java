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
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.level3.AlertAdaptationParameters;
import com.raytheon.uf.common.dataplugin.radar.level3.AlertMessage;
import com.raytheon.uf.common.dataplugin.radar.level3.GSMBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.GraphicBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.TabularBlock;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataplugin.radar.util.RadarTextProductUtil;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.AFOSProductId;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

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
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * --/--/2006             brockwoo    Initial creation
 * Jan 21, 2014  2627     njensen     Changed offset errors to MalformedDataException
 * 
 * </pre>
 * 
 * 
 * @author Bryan Rockwood
 * @version 1.0
 */
public class Level3BaseRadar {

    private static final Pattern WMO_PATTERN = Pattern
            .compile("([A-Z]{4}[0-9]{2} [A-Z]{4} [0-9]{6})\\x0D\\x0D\\x0A(\\w{6})\\x0D\\x0D\\x0A");

    /** The logger */
    private static final transient IUFStatusHandler theHandler = UFStatus
            .getHandler(Level3BaseRadar.class);

    protected DataInputStream theRadarData = null;

    private ByteArrayInputStream theRawRadarData = null;

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

    private GSMBlock gsmBlock;

    private AlertAdaptationParameters aapMessage;

    private String afosId = "";

    private String radarLoc = "";

    private int productVersion;

    private int productSpotBlank;

    private String rrmessage;

    private String wmoHeader = "";

    private AlertMessage alertMessage;

    private RadarInfoDict dict = null;

    private final List<Integer> SPECIAL_PRODS = new ArrayList<Integer>(
            Arrays.asList(73, 62, 75, 77, 82));

    public static final int GSM_MESSAGE = 2;

    public final int PRODUCT_REQUEST_RESPONSE_MESSAGE = 3;

    public final int ALERT_ADAPTATION_PARAMETERS = 6;

    public final int PRODUCT_LIST = 8;

    public final int RADAR_CODED_MESSAGE = 74;

    public final int ALERT_MESSAGE = 9;

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
    public Level3BaseRadar(File aRadar, Headers headers) throws IOException,
            MalformedDataException {
        int fileSize = (int) aRadar.length();
        byte[] tempRawRadarByteArray = new byte[fileSize];

        FileInputStream radarFile = null;

        try {
            radarFile = new FileInputStream(aRadar);
            radarFile.read(tempRawRadarByteArray);
        } finally {
            if (radarFile != null) {
                radarFile.close();
            }
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
    public Level3BaseRadar(byte[] aRadar, Headers headers) throws IOException,
            MalformedDataException {
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
    private void init(byte[] aRadar, Headers headers) throws IOException,
            MalformedDataException {
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

        theRawRadarData = new ByteArrayInputStream(theRawRadarByteArray);
        theRadarData = new DataInputStream(theRawRadarData);

        // parse the header
        this.parseRadarHeader();

        // Handle the message contents
        if (this.theMessageCode == ALERT_ADAPTATION_PARAMETERS) {
            // Alert Adaptation Params
            this.parseAAP();
        } else if (this.theMessageCode == PRODUCT_REQUEST_RESPONSE_MESSAGE) {
            this.parseRequestResponse();
        } else if (this.theMessageCode == PRODUCT_LIST) {
            this.parseProductList(headers);
        } else if (this.theMessageCode == GSM_MESSAGE) {
            this.parseGeneralStatusMessage();
        } else if (this.theMessageCode == ALERT_MESSAGE) {
            this.parseAlertMessage(headers);
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
     * @return the aapMessage
     */
    public AlertAdaptationParameters getAapMessage() {
        return aapMessage;
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

    private SymbologyBlock readSymbologyBlock(int offset) throws IOException,
            MalformedDataException {
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
            DataInputStream in = new DataInputStream(new ByteArrayInputStream(
                    buf));

            // Skipping the number of layers, layer divider,
            // and data layer length
            in.skip(8);
            symBlock = new SymbologyBlock(in);
        }
        return symBlock;
    }

    private GraphicBlock readGraphicBlock(int offset) throws IOException,
            MalformedDataException {
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
            DataInputStream in = new DataInputStream(new ByteArrayInputStream(
                    buf));
            in.skip(8);
            graphicBlock = new GraphicBlock(in);
        }
        return graphicBlock;
    }

    private TabularBlock readTabularBlock(int offset) throws IOException,
            MalformedDataException {
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
            DataInputStream in = new DataInputStream(new ByteArrayInputStream(
                    buf));
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
    private TabularBlock readStandaloneTabular(int offset) throws IOException,
            MalformedDataException {
        int divider = theRadarData.readShort();
        TabularBlock tabBlock = new TabularBlock();
        int numPages = theRadarData.readUnsignedShort();
        if ((divider != -1)) {
            throw new MalformedDataException("Standalone tabular block offset "
                    + offset + " does not point to a standalone tabular block");
        }
        List<List<String>> pages = new ArrayList<List<String>>();
        for (int p = 0; p < numPages; p++) {
            List<String> page = new ArrayList<String>();
            int lineLen = theRadarData.readUnsignedShort();
            while (lineLen != 0xFFFF) {
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
        ArrayList<Byte> array = new ArrayList<Byte>();
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
            strng.trim();
            fullString.append(strng);
            fullString.append("\n");
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

    private void parseRadarMessage(Headers headers) throws IOException,
            MalformedDataException {
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

        if (productDependentValues[7] == 1) {
            int uncompressedSize = ((productDependentValues[8] & 0xffff) << 16)
                    | (productDependentValues[9] & 0xffff);
            byte[] uncompressed = null;
            byte[] msg = new byte[120];
            InputStream byt;
            if (uncompressedSize + msg.length != theRawRadarByteArray.length) {
                InputStream ins = null;
                try {
                    theRadarData.reset();
                    theRadarData.readFully(msg);
                    ins = new BZip2InputStream(theRadarData, false);
                    uncompressed = new byte[uncompressedSize];
                    ins.read(uncompressed);
                } catch (IOException e) {
                    theHandler.handle(Priority.ERROR,
                            "Error decompressing product: ", e);
                    return;
                } finally {
                    if (ins != null) {
                        ins.close();
                    }
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
            if (RadarTextProductUtil.radarTable.keySet().contains(
                    theProductCode)) {
                byte[] wmoid = wmoHeader.getBytes();
                WMOHeader header = new WMOHeader(wmoid, headers);
                AFOSProductId afos = new AFOSProductId(afosId);
                if (afos.isFilled()) {
                    try {
                        Calendar cal = (TimeTools.allowArchive() ? theMsgTimestamp
                                : Calendar.getInstance());
                        RadarEdexTextProductUtil.storeTextProduct(afos, header,
                                tabularBlock.getString(), true, cal);
                    } catch (Exception e) {
                        theHandler.handle(Priority.ERROR,
                                "Could not store text product", e);
                    }
                }
            }
        }
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
        builder.append("\tPROD\tPROD\tDATA\n\tID#\t\tNAME\tLVL\t\tRES\t\tSLICE\tPARAM1\tPARAM2\tPARAM3\tPARAM4\n\t----\t----\t---\t\t---\t\t-----\t------\t------\t------\t------\n");

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
                builder.append("\t"
                        + productCode
                        + "\t\t"
                        + dict.getInfo(productCode).getMnemonic()
                        + "\t\t"
                        + (dict.getInfo(productCode).getNumLevels() == 0 ? ""
                                : dict.getInfo(productCode).getNumLevels())
                        + "\t\t"
                        + (dict.getInfo(productCode).getResolution() == 0 ? ""
                                : format.format((double) dict.getInfo(
                                        productCode).getResolution() / 1000))
                        + "\t\t" + elev + "\t\t" + params + "\n");
            } catch (Exception e) {
                System.out.println("Uh oh");
            }
        }

        tabularBlock.setString(builder.toString());

        lookupAfosId();

        if (RadarTextProductUtil.radarTable.keySet().contains(theMessageCode)) {
            byte[] wmoid = wmoHeader.getBytes();
            WMOHeader header = new WMOHeader(wmoid, headers);
            AFOSProductId afos = new AFOSProductId(afosId);
            if (afos.isFilled()) {
                try {
                    Calendar cal = (TimeTools.allowArchive() ? theMsgTimestamp
                            : Calendar.getInstance());
                    RadarEdexTextProductUtil.storeTextProduct(afos, header,
                            tabularBlock.getString(), true, cal);
                } catch (Exception e) {
                    theHandler.handle(Priority.ERROR,
                            "Could not store text product", e);
                }
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
            afosId = RadarTextProductUtil
                    .createAfosId(theMessageCode, radarLoc);
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
        long tmp = ((volScanDate - 1) * 24l * 60l * 60l * 1000l + volScanTime * 1000l);
        Date date = new Date(tmp);
        DataTime time = new DataTime(date);
        temp += RadarUtil.formatBits((short) errorCode,
                RadarConstants.requestReponseErrorCode)
                + "\n\n"
                + "Product Code : "
                + prodCode
                + "\nDate : "
                + time
                + "\nElevation : "
                + elevAngle
                + "\nSequence Number : "
                + sequenceNumber;
        setRequestResponseMessage(temp);
    }

    private void parseAlertMessage(Headers headers) throws IOException {
        int divider = theRadarData.readShort();
        if (divider != -1) {
            theHandler.handle(Priority.ERROR,
                    "This does not appear to be an alert message");
        } else {
            alertMessage = new AlertMessage();
            alertMessage.setStatus(theRadarData.readShort());
            alertMessage.setAlertAreaNum(theRadarData.readShort());
            alertMessage.setAlertCategory(theRadarData.readShort());
            alertMessage.setThresholdCode(theRadarData.readShort());
            alertMessage.setThresholdValue(theRadarData.readInt());
            alertMessage.setExceedingValue(theRadarData.readInt());
            alertMessage.setGridBoxAz(theRadarData.readShort() / 10);
            alertMessage.setGridBoxRange(theRadarData.readShort() / 10);

            // put together the storm id, date, and time
            char[] ids = new char[2];
            ids[0] = (char) theRadarData.readByte();
            ids[1] = (char) theRadarData.readByte();
            alertMessage.setStormId(String.valueOf(ids));
            int volScan = theRadarData.readShort();
            int volScanDate = theRadarData.readUnsignedShort();
            int volScanTime = theRadarData.readInt();
            Date date = new Date((volScanDate - 1) * 24 * 60 * 60 * 1000l
                    + (volScanTime * 1000l));
            alertMessage.setVolScan(volScan);
            alertMessage.setVolScanDate(date);
        }
    }

    private void parseGeneralStatusMessage() throws IOException,
            MalformedDataException {
        int divider = theRadarData.readShort();
        if ((divider != -1)) {
            throw new MalformedDataException("This is not a gsm block");
        }
        int blockLen = theRadarData.readShort();
        byte[] buf = RadarUtil.subArray(theRawRadarByteArray, 22, blockLen);
        DataInputStream in = new DataInputStream(new ByteArrayInputStream(buf));
        gsmBlock = new GSMBlock(in);
    }

    /**
     * Parses the Alert Adaptation Param product
     * 
     * @throws IOException
     */
    private void parseAAP() throws IOException {
        // Skip block divider, -1
        theRadarData.skip(2);
        this.aapMessage = new AlertAdaptationParameters(theRadarData);
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
            DataInputStream in = new DataInputStream(new ByteArrayInputStream(
                    symbologyBlock));

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
            radarLoc = awipsID
                    .substring(awipsID.length() - 3, awipsID.length());
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
     * @return the alertMessage
     */
    public AlertMessage getAlertMessage() {
        return alertMessage;
    }

    /**
     * @param alertMessage
     *            the alertMessage to set
     */
    public void setAlertMessage(AlertMessage alertMessage) {
        this.alertMessage = alertMessage;
    }
}
