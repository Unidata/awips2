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

package com.raytheon.edex.plugin.satellite;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.satellite.dao.SatelliteDao;
import com.raytheon.edex.plugin.satellite.gini.SatelliteCreatingEntity;
import com.raytheon.edex.plugin.satellite.gini.SatellitePhysicalElement;
import com.raytheon.edex.plugin.satellite.gini.SatellitePosition;
import com.raytheon.edex.plugin.satellite.gini.SatelliteSectorId;
import com.raytheon.edex.plugin.satellite.gini.SatelliteSource;
import com.raytheon.edex.plugin.satellite.gini.SatelliteUnit;
import com.raytheon.edex.util.satellite.SatSpatialFactory;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.exception.UnrecognizedDataException;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteMessageData;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.ArraysUtil;
import com.raytheon.uf.common.util.header.WMOHeaderFinder;

/**
 * Decoder implementation for satellite plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 006                      garmenda    Initial Creation
 * /14/2007     139         Phillippe   Modified to follow refactored plugin pattern
 * 8/30/07                  njensen     Added units, commented out data that
 *                                      is currently decoded but not used.
 * 12/01/07     555         garmendariz Modified decompress method.
 * 12/06/07     555         garmendariz Modifed start point to remove satellite header
 * Dec 17, 2007 600         bphillip    Added dao pool usage
 * 04Apr2008    1068        MW Fegan    Modified decompression routine to prevent
 *                                       process hang-up.
 * 11/11/2008               chammack    Refactored to be thread safe in camel
 * 02/05/2010   4120        jkorman     Modified removeWmoHeader to handle WMOHeader in
 *                                      various start locations.
 * 04/17/2012  14724        kshresth    This is a temporary workaround - Projection off CONUS
 * - AWIPS2 Baseline Repository --------
 * 06/27/2012    798        jkorman     Using SatelliteMessageData to "carry" the decoded image.
 * 01/03/2013  15294        D. Friedman Start with File instead of byte[] to
 *                                      reduce memory usage.
 * Feb 15, 2013 1638        mschenke    Moved array based utilities from Util into ArraysUtil
 * 
 * Mar 19, 2013 1785        bgonzale    Added performance status handler and added status
 *                                      to decode.
 * Jan 20, 2014             njensen     Better error handling when fields are not recognized
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class SatelliteDecoder {

    private IUFStatusHandler statusHandler = UFStatus.getHandler(getClass());

    private final String traceId = "";

    private static final int MAX_IMAGE_SIZE = 30000000;

    private static final String SAT_HDR_TT = "TI";

    private static final int GINI_HEADER_SIZE = 512;

    private static final int INITIAL_READ = GINI_HEADER_SIZE + 128;

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("Satellite:");

    private SatelliteDao dao;

    public PluginDataObject[] decode(File file) throws Exception {

        PluginDataObject[] retData = null;

        SatelliteRecord record = null;

        if ((file == null) || (file.length() < 1)) {
            return new PluginDataObject[0];
        }
        RandomAccessFile f = new RandomAccessFile(file, "r");
        try {
            ITimer timer = TimeUtil.getTimer();
            timer.start();
            // Read in enough data to cover the WMO heading and GINI header.
            ByteBuffer byteBuffer = ByteBuffer.allocate(INITIAL_READ);
            f.getChannel().read(byteBuffer);
            byteBuffer.flip();

            try {
                removeWmoHeader(byteBuffer);
            } catch (DecoderException e) {
                statusHandler.error("Error removing WMO header", e);
                byteBuffer = null;
            }
            if (byteBuffer != null) {
                int offsetOfDataInFile = byteBuffer.position()
                        + GINI_HEADER_SIZE;
                Calendar calendar = Calendar.getInstance(TimeZone
                        .getTimeZone("GMT"));
                int intValue = 0;
                byte byteValue = 0;
                byte[] tempBytes = null;
                byte threeBytesArray[] = new byte[3];

                record = new SatelliteRecord();

                if (isCompressed(byteBuffer)) {
                    /*
                     * If the data is compressed, we assume it came from the SBN
                     * and will have a reasonable size such that we can have two
                     * copies of the data in memory at the same time. Ideally,
                     * SBN decompression should be performed upstream from EDEX
                     * and this code would be removed.
                     */
                    byte[] data = new byte[(int) file.length()
                            - byteBuffer.position()];
                    f.seek(byteBuffer.position());
                    f.readFully(data);
                    byte[][] retVal = decompressSatellite(data);
                    byteBuffer = ByteBuffer.wrap(retVal[0]);
                    tempBytes = retVal[1];
                } else {
                    /*
                     * The code bellow performs absolute gets on the buffer, so
                     * it needs to be compacted.
                     */
                    byteBuffer.compact();
                    byteBuffer.flip();
                }

                // get the scanning mode
                int scanMode = byteBuffer.get(37);

                // read the source
                byte sourceByte = byteBuffer.get(0);
                SatelliteSource source = dao.getSource(sourceByte);
                if (source == null) {
                    throw new UnrecognizedDataException(
                            "Unknown satellite source id: " + sourceByte);
                }
                record.setSource(source.getSourceName());

                // read the creating entity
                byte entityByte = byteBuffer.get(1);
                SatelliteCreatingEntity entity = dao
                        .getCreatingEntity(entityByte);
                if (entity == null) {
                    throw new UnrecognizedDataException(
                            "Unknown satellite entity id: " + entityByte);
                }
                record.setCreatingEntity(entity.getEntityName());

                // read the sector ID
                byte sectorByte = byteBuffer.get(2);
                SatelliteSectorId sector = dao.getSectorId(sectorByte);
                if (sector == null) {
                    throw new UnrecognizedDataException(
                            "Unknown satellite sector id: " + sectorByte);
                }
                record.setSectorID(sector.getSectorName());

                // read the physical element
                byte physByte = byteBuffer.get(3);
                SatellitePhysicalElement physElem = dao
                        .getPhysicalElement(physByte);
                if (physElem == null) {
                    throw new UnrecognizedDataException(
                            "Unknown satellite physical element id: "
                                    + physByte);
                }
                record.setPhysicalElement(physElem.getElementName());

                // read the units
                SatelliteUnit unit = dao.getUnit(byteBuffer.get(3));
                if (unit != null) {
                    record.setUnits(unit.getUnitName());
                }

                // read the century
                intValue = 1900 + byteBuffer.get(8);
                calendar.set(Calendar.YEAR, intValue);

                // read the month of the year
                // Calendar months = 0 - 11, so subtract 1
                byteValue = byteBuffer.get(9);
                calendar.set(Calendar.MONTH, byteValue - 1);

                // read the day of the month
                byteValue = byteBuffer.get(10);
                calendar.set(Calendar.DAY_OF_MONTH, byteValue);

                // read the hour of the day
                byteValue = byteBuffer.get(11);
                calendar.set(Calendar.HOUR_OF_DAY, byteValue);

                // read the minute of the hour
                byteValue = byteBuffer.get(12);
                calendar.set(Calendar.MINUTE, byteValue);

                // read the second of the minute
                byteValue = byteBuffer.get(13);
                calendar.set(Calendar.SECOND, byteValue);

                // read the hundredths of a second
                byteValue = byteBuffer.get(14);
                calendar.set(Calendar.MILLISECOND, byteValue * 10);

                record.setDataTime(new DataTime(calendar));

                // read the projection
                byteValue = byteBuffer.get(15);
                int mapProjection = byteBuffer.get(15);

                // get the image resolution
                // imageResolution = (int) byteBuffer.get(41);

                // get the data compression
                // if (byteBuffer.get(42) == 0) {
                // compression = false;
                // }

                // get the version number
                // int pdbVersion = (int) byteBuffer.get(43);

                // get the number of octects in the PDB
                // int pdbBytes = (int) byteBuffer.getShort(44);

                // get the navigation/calibration indicator
                int navCalIndicator = byteBuffer.get(46);

                // Get latitude of satellite sub point
                byteBuffer.position(47);
                byteBuffer.get(threeBytesArray, 0, 3);
                float latSub = transformLatitude(threeBytesArray);

                // Get longitude of satellite sub point
                byteBuffer.position(50);
                byteBuffer.get(threeBytesArray, 0, 3);
                float lonSub = transformLongitude(threeBytesArray);

                // Get the Satellite Height
                int satHeight = byteBuffer.getShort(53);

                if ((latSub != 0) || (lonSub != 0) || (satHeight != 0)) {
                    // Correct the longitude so negative is west
                    lonSub *= -1;
                    // Correct the height to be height above ground
                    satHeight = Math.abs(satHeight);

                    record.setSatSubPointLat(latSub);
                    record.setSatSubPointLon(lonSub);
                    record.setSatHeight(satHeight);
                } else {
                    SatellitePosition position = dao
                            .getSatellitePosition(record.getCreatingEntity());
                    if (position == null) {
                        statusHandler
                                .info("Unable to determine geostationary location of ["
                                        + record.getCreatingEntity()
                                        + "].  Zeroing out fields.");
                    } else {
                        record.setSatSubPointLat(position.getLatitude());
                        record.setSatSubPointLon(position.getLongitude());
                        record.setSatHeight(position.getHeight());
                    }
                }

                if (navCalIndicator != 0) {
                    statusHandler
                            .info("Nav/Cal info provided.  Currently unused.");
                }

                // get number of points along x-axis
                int nx = byteBuffer.getShort(16);
                // get number of points along y-axis
                int ny = byteBuffer.getShort(18);

                /*
                 * If input was SBN-compressed, we already have the data loaded.
                 * If not, load it now.
                 */
                if (tempBytes == null) {
                    tempBytes = new byte[nx * ny];
                    f.seek(offsetOfDataInFile);
                    f.readFully(tempBytes, 0, tempBytes.length);
                }

                /*
                 * Rotate image if necessary
                 */
                // TODO: Can these numbers be an enum or constants?
                switch (scanMode) {
                case 1:
                    ArraysUtil.flipHoriz(tempBytes, ny, nx);
                    break;
                case 2:
                    ArraysUtil.flipVert(tempBytes, ny, nx);
                    break;
                case 3:
                    ArraysUtil.flipVert(tempBytes, ny, nx);
                    ArraysUtil.flipVert(tempBytes, ny, nx);
                    break;
                default:
                    break;
                }
                SatelliteMessageData messageData = new SatelliteMessageData(
                        tempBytes, nx, ny);

                // get the latitude of the first point
                byteBuffer.position(20);
                byteBuffer.get(threeBytesArray, 0, 3);
                float la1 = transformLatitude(threeBytesArray);

                // get longitude of the first point
                byteBuffer.position(23);
                byteBuffer.get(threeBytesArray, 0, 3);
                float lo1 = transformLongitude(threeBytesArray);

                // bytes are received with the first bit set to 1 to indicate
                // South
                byteBuffer.position(38);
                byteBuffer.get(threeBytesArray, 0, 3);
                float latin = transformLatitude(threeBytesArray);

                // get the scanning mode
                scanMode = byteBuffer.get(37);

                float dx = 0.0f, dy = 0.0f, lov = 0.0f, lo2 = 0.0f, la2 = 0.0f;
                // Do specialized decoding and retrieve spatial data for Lambert
                // Conformal and Polar Stereographic projections
                if ((mapProjection == SatSpatialFactory.PROJ_LAMBERT)
                        || (mapProjection == SatSpatialFactory.PROJ_POLAR)) {
                    byteBuffer.position(30);
                    byteBuffer.get(threeBytesArray, 0, 3);
                    dx = byteArrayToFloat(threeBytesArray) / 10;

                    byteBuffer.position(33);
                    byteBuffer.get(threeBytesArray, 0, 3);
                    dy = byteArrayToFloat(threeBytesArray) / 10;

                    byteBuffer.position(27);
                    byteBuffer.get(threeBytesArray, 0, 3);
                    lov = transformLongitude(threeBytesArray);
                }
                // Do specialized decoding and retrieve spatial data for
                // Mercator projection
                else if (mapProjection == SatSpatialFactory.PROJ_MERCATOR) {
                    dx = byteBuffer.getShort(33);
                    dy = byteBuffer.getShort(35);

                    byteBuffer.position(27);
                    byteBuffer.get(threeBytesArray, 0, 3);
                    la2 = transformLatitude(threeBytesArray);

                    byteBuffer.position(30);
                    byteBuffer.get(threeBytesArray, 0, 3);
                    lo2 = transformLongitude(threeBytesArray);

                } else {
                    throw new DecoderException(
                            "Unable to decode GINI Satellite: Encountered Unknown projection");
                }

                SatMapCoverage mapCoverage = null;

                try {
                    /**
                     * This is a temporary workaround for DR14724, hopefully to
                     * be removed after NESDIS changes the product header
                     */
                    if ((mapProjection == SatSpatialFactory.PROJ_LAMBERT)
                            && (record.getPhysicalElement()
                                    .equalsIgnoreCase("Imager 13 micron (IR)"))
                            && (record.getSectorID()
                                    .equalsIgnoreCase("West CONUS"))) {
                        nx = 1100;
                        ny = 1280;
                        dx = 4063.5f;
                        dy = 4063.5f;
                        la1 = 12.19f;
                        lo1 = -133.4588f;
                    }
                    /**
                     * End of DR14724
                     */
                    mapCoverage = SatSpatialFactory.getInstance()
                            .getMapCoverage(mapProjection, nx, ny, dx, dy, lov,
                                    latin, la1, lo1, la2, lo2);
                } catch (Exception e) {
                    StringBuffer buf = new StringBuffer();
                    buf.append(
                            "Error getting or constructing SatMapCoverage for values: ")
                            .append("\n\t");
                    buf.append("mapProjection=" + mapProjection).append("\n\t");
                    buf.append("nx=" + nx).append("\n\t");
                    buf.append("ny=" + ny).append("\n\t");
                    buf.append("dx=" + dx).append("\n\t");
                    buf.append("dy=" + dy).append("\n\t");
                    buf.append("lov=" + lov).append("\n\t");
                    buf.append("latin=" + latin).append("\n\t");
                    buf.append("la1=" + la1).append("\n\t");
                    buf.append("lo1=" + lo1).append("\n\t");
                    buf.append("la2=" + la2).append("\n\t");
                    buf.append("lo2=" + lo2).append("\n");
                    throw new DecoderException(buf.toString(), e);
                }

                if (record != null) {
                    record.setTraceId(traceId);
                    record.setCoverage(mapCoverage);
                    // Create the data record.
                    IDataRecord dataRec = messageData.getStorageRecord(record,
                            SatelliteRecord.SAT_DATASET_NAME);
                    record.setMessageData(dataRec);
                }
            }
            timer.stop();
            perfLog.logDuration("Time to Decode", timer.getElapsedTime());
        } catch (Exception e) {
            statusHandler.error("Error decoding satellite", e);
        } finally {
            try {
                f.close();
            } catch (IOException e) {
                // ignore
            }
        }
        if (record == null) {
            retData = new PluginDataObject[0];
        } else {
            retData = new PluginDataObject[] { record };
        }
        return retData;
    }

    /**
     * Verifies that this data is satellite imager and removes the WMO header
     * from the data and extracts the data from the file. Method expects that
     * the input data has been null checked prior to invocation.
     * 
     * @throws DecoderException
     *             If WMO header is not found, or is incorrect.
     * @param messageData
     *            Contains the start of the satellite data file. On return, the
     *            position is set the beginning of the GINI header.
     */
    private void removeWmoHeader(ByteBuffer messageData)
            throws DecoderException {

        // Copy to a char [], carefully, as creating a string from
        // a byte [] with binary data can create erroneous data
        char[] message = new char[messageData.remaining()];
        for (int i = 0; i < message.length; i++) {
            message[i] = (char) (messageData.get() & 0xFF);
        }
        String msgStr = new String(message);
        Matcher matcher = WMOHeaderFinder.WMO_PATTERN.matcher(msgStr);
        if (matcher.find()) {
            int headerStart = matcher.start();
            if (SAT_HDR_TT.equals(msgStr
                    .substring(headerStart, headerStart + 2))) {
                int startOfSatellite = matcher.end();
                messageData.position(startOfSatellite);
                messageData.limit(messageData.capacity());
            } else {
                throw new DecoderException(
                        "First character of the WMO header must be 'T'");
            }
        } else {
            throw new DecoderException("Cannot decode an empty WMO header");
        }
    }

    /**
     * Checks to see if the current satellite product is compressed.
     * 
     * Assumes messageData is a byte[]-backed ByteBuffer.
     * 
     * @return A boolean indicating if the file is compressed or not
     */
    private boolean isCompressed(ByteBuffer messageData) {
        boolean compressed = true;
        byte[] placeholder = new byte[10];
        Inflater decompressor = new Inflater();
        try {
            decompressor.setInput(messageData.array(), messageData.position(),
                    messageData.remaining());
            decompressor.inflate(placeholder);
        } catch (DataFormatException e) {
            compressed = false;
        } finally {
            decompressor.end();
        }
        return compressed;
    }

    /**
     * Method to handle compressed satellite data.
     * 
     * @param messageData
     * @return
     * @throws DecoderException
     */
    private byte[][] decompressSatellite(byte[] messageData)
            throws DecoderException {
        byte[] retVal = null;

        boolean firstCall = true;
        byte[] zSatellite = messageData;
        byte[] header = new byte[512];

        int inflatedBytes = 0;
        byte[] inflateArray = new byte[1024 * 10];
        // Allocate 30MB for a possible max size
        ByteArrayOutputStream bos = new ByteArrayOutputStream(MAX_IMAGE_SIZE);
        int totalBytesDecomp = 0;
        byte[] inputArray = new byte[1024 * 10];
        Inflater decompressor = new Inflater();
        int index = -1;
        try {
            while (totalBytesDecomp < zSatellite.length) {

                int compChunkSize = (zSatellite.length - totalBytesDecomp) > 10240 ? 10240
                        : zSatellite.length - totalBytesDecomp;

                // copy compChunkSize compressed data from zSatellite, offset by
                // compByteCounter to inputArray
                System.arraycopy(zSatellite, totalBytesDecomp, inputArray, 0,
                        compChunkSize);

                // set the data to the decompressor
                decompressor.setInput(inputArray, 0, compChunkSize);

                // reset the total bytes decompressed
                inflatedBytes = 0;
                while (!decompressor.finished()) {

                    // inflate the compressed data and get total inflated size
                    inflatedBytes = decompressor.inflate(inflateArray);

                    // check to see if the decompression used the buffer w/o
                    // finishing - this indicates a truncated file
                    if (inflatedBytes == 0) {
                        throw new DecoderException(
                                "Unable to decompress satellite data - input data appears to be truncated");
                    }
                    // retrieve the total compressed bytes input so far
                    totalBytesDecomp += decompressor.getTotalIn();

                    // check for first decoded row
                    if (firstCall) {
                        // copy header to a separate array,
                        // start at the end of the WMO header
                        System.arraycopy(inflateArray, 21, header, 0, 512);

                        // reflect the total bytes
                        inflatedBytes = 0;

                        // set sentinel
                        firstCall = false;
                    } else if (totalBytesDecomp == zSatellite.length) {
                        // check for the last decoded row
                        // search for the index
                        index = getIndex(inflateArray, 0);
                    }

                    if (index == -1) {
                        // did not search, or search failed, write all data
                        bos.write(inflateArray, 0, inflatedBytes);
                    } else {
                        // found starting point, writing to it
                        bos.write(inflateArray, 0, index);
                    }

                }

                // reset the decompressor to set additional input
                decompressor.reset();
            }
        } catch (DataFormatException e) {
            throw new DecoderException("Unable to decompress satellite data", e);
        } finally {
            decompressor.end();
            inputArray = null;
            inflateArray = null;
        }

        retVal = bos.toByteArray();
        bos = null;
        return new byte[][] { header, retVal };
    }

    /**
     * Retrieve the starting point for -1,0,-1, 0 in the array. Data after this
     * token is filler and may be eliminated from the buffer.
     * 
     * @param inflateArray
     *            buffer containing inflated data
     * @param startingIndex
     *            buffer point to start the search
     * 
     * @return The index to the invalid data
     */
    private int getIndex(byte[] inflateArray, int startingIndex) {

        int index = -1;
        for (int i = startingIndex; i < inflateArray.length; i++) {
            if (inflateArray[i] == -1) {
                index = i;
                break;
            }

        }

        if ((index != -1) && ((index + 3) <= (inflateArray.length - 1))) {
            if (!((inflateArray[index] == -1) && (inflateArray[index + 1] == 0)
                    && (inflateArray[index + 2] == -1) && (inflateArray[index + 3] == 0))) {
                index = getIndex(inflateArray, index + 1);
            }
        } else {
            index = -1;
        }

        return index;

    }

    /**
     * Converts a 3 element byte array into a float
     * 
     * @param b
     *            the byte array
     * @return The byte array represented as a float
     */
    private float byteArrayToFloat(byte[] b) {
        int i = 0;
        i |= b[0] & 0xFF;
        i <<= 8;
        i |= b[1] & 0xFF;
        i <<= 8;
        i |= b[2] & 0xFF;
        return i;
    }

    /**
     * Transforms a latitude in the form of 3 element byte array into the
     * corresponding float value.
     * 
     * @param byteArray
     *            The latitude as a byte array
     * @return The float value of the latitude. A negative return value
     *         indicates south latitude
     */
    private float transformLatitude(byte[] byteArray) {
        float latitude;

        if (byteArray[0] < 0) {
            // remove the negative value
            byteArray[0] &= 127;
            latitude = (byteArrayToFloat(byteArray) / 10000) * -1;
        } else {
            latitude = byteArrayToFloat(byteArray) / 10000;
        }
        return latitude;
    }

    /**
     * Transforms a longitude in the form of a 3 element byte array into the
     * corresponding float value
     * 
     * @param byteArray
     *            The longitude as a byte array
     * @return The float value of the longitude. A negative return value
     *         indicates west longitude
     */
    private float transformLongitude(byte[] byteArray) {

        float longitude;
        if (byteArray[0] < 0) {
            // west longitude
            // remove the negative value
            byteArray[0] &= 127;
            longitude = byteArrayToFloat(byteArray);

            if (longitude <= 1800000) {
                longitude *= -1;
            } else {
                longitude = 3600000 - longitude;
            }

        } else {
            // east longitude
            longitude = byteArrayToFloat(byteArray);
            if (longitude > 1800000) {
                longitude = longitude - 3600000;
            }
        }
        return longitude / 10000;
    }

    public SatelliteDao getDao() {
        return dao;
    }

    public void setDao(SatelliteDao dao) {
        this.dao = dao;
    }
}
