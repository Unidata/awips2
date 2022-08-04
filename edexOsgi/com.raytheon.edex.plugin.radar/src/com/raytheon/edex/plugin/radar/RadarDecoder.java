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

package com.raytheon.edex.plugin.radar;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.radar.dao.RadarStationDao;
import com.raytheon.edex.plugin.radar.level2.Level2BaseRadar;
import com.raytheon.edex.plugin.radar.level3.Level3BaseRadar;
import com.raytheon.edex.plugin.radar.util.RadarEdexTextProductUtil;
import com.raytheon.edex.plugin.radar.util.RadarSpatialUtil;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.exception.MalformedDataException;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.level3.GenericDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.GraphicBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.Layer;
import com.raytheon.uf.common.dataplugin.radar.level3.PrecipDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.RadialPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.RasterPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.TabularBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent.ComponentType;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.RadialComponent;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfo;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataplugin.radar.util.RadarTabularBlockParser;
import com.raytheon.uf.common.dataplugin.radar.util.RadarTextProductUtil;
import com.raytheon.uf.common.dataplugin.radar.util.TerminalRadarUtils;
import com.raytheon.uf.common.dataplugin.radar.util.TiltAngleBin;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Decoder implementation for radar plugin
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Feb 14, 2007  139      Phillippe  Initial check-in. Refactor of initial
 *                                   implementation.
 * Dec 17, 2007  600      bphillip   Added dao pool usage
 * Dec 03, 2010  2235     cjeanbap   EDEXUtility.sendMessageAlertViz() signature
 *                                   changed.
 * Mar 19, 2013  1804     bsteffen   Optimize decoder performance.
 * Mar 19, 2013  1785     bgonzale   Added performance status handler and added
 *                                   status  to decode.
 * Aug 30, 2013  2298     rjpeter    Make getPluginName abstract
 * Oct 09, 2013  2457     bsteffen   Improve error message for missing icao.
 * Jan 21, 2014  2627     njensen    Removed decode()'s try/catch, camel route
 *                                   will do try/catch
 * May 14, 2014  2536     bclement   moved WMO Header to common, removed
 *                                   TimeTools usage
 * Dec 26, 2014  632      dhuffman   Added AlertMessageSanityCheck() for this
 *                                   DR.
 * Feb 27, 2015  17086    zwang      Corrected the elevation of volume based
 *                                   TDWR products
 * Mar 25, 2015  4319     bsteffen   Save the volume scan number.
 * Dec 16, 2015  5166     kbisanz    Update logging to use SLF4J by adding
 *                                   private logger and stop extending
 *                                   AbstractDecoder. finalizeRecord(..) does
 *                                   not throw PluginException.
 * Apr 14, 2016  18800    jdynina    Removed alerting
 * Feb 02, 2018  14381    kshresth    Updated alarm messages
 * Mar 26, 2018  6711     randerso   Moved AlertViz notifications for GSMs to
 *                                   GSMNotifier. Code cleanup.
 * May 31, 2018  6724     bsteffen   Set primaryelevationangle for ULR to
 *                                   differentiate different layers.
 * Jun 04, 2018  6725     bsteffen   Set primaryelevationangle for DUA to
 *                                   differentiate different durations.
 * Jun 21, 2019  7629     mroos      Added calls for setting delta time and scan type.
 * Sep 23, 2021  8608     mapeters   Handle PDO.traceId changes
 * </pre>
 *
 * @author bphillip
 */
public class RadarDecoder {

    private static final Logger logger = LoggerFactory
            .getLogger(RadarDecoder.class);

    // radar server sends messages from EDEX to CAVE, handle that here
    private static final String EDEX = "EDEX";

    private static final String RADAR = "RADAR";

    /*
     * Constants having to do with certain products
     */

    private static final List<String> LEVEL_TWO_IDENTS = Arrays.asList("ARCH",
            "AR2V");

    private static final String NOUS = "NOUS";

    private static final int USER_LAYER_REFL = 137;

    private static final int USER_SELECT_ACCUM = 173;

    private static final int CLUTTER_FILTER_CONTROL = 34;

    /*
     * End constants
     */

    private String traceId = "";

    private final RadarInfoDict infoDict;

    private RadarStationDao radarStationDao = new RadarStationDao();

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("Radar:");

    /**
     * Construct an instance of the RadarDecoder
     *
     * @throws DecoderException
     */
    public RadarDecoder() throws DecoderException {

        String dir = "";

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        try {
            dir = pathMgr.getFile(commonStaticBase, ".").getCanonicalPath();
        } catch (IOException e) {
            logger.error("Failed to get localization directory", e);
        }

        infoDict = RadarInfoDict.getInstance(dir);
    }

    /**
     * Actual decode of the product.
     *
     * @param messageData
     * @param headers
     * @return the decoded RadarRecords
     * @throws IOException
     * @throws MalformedDataException
     * @throws DataAccessLayerException
     */
    public PluginDataObject[] decode(byte[] messageData, Headers headers)
            throws IOException, MalformedDataException,
            DataAccessLayerException {
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        List<RadarRecord> recordList = new ArrayList<>();

        // decode the product
        String arch = new String(messageData, 0, 4);

        ITimer timer = TimeUtil.getTimer();

        timer.start();
        // for level2 data, this does not happen very often
        if (LEVEL_TWO_IDENTS.contains(arch)) {
            decodeLevelTwoData(messageData, recordList);
        }
        // for free text messages, which come in with the following wmo
        else if (NOUS.equals(arch)) {
            decodeFreeTextMessage(messageData, headers);
        } else {
            if (headers.get("header") != null) {
                // handle an interesting special case
                String wmoHeader = headers.get("header").toString();
                if (wmoHeader.contains("SDUS4")) {
                    String fileName = (String) headers
                            .get(WMOHeader.INGEST_FILE_NAME);
                    WMOHeader header = new WMOHeader(wmoHeader.getBytes(),
                            fileName);
                    String dataString = new String(messageData, 0,
                            messageData.length).substring(1,
                                    messageData.length - 1);
                    String siteId = dataString.substring(0, 3);
                    AFOSProductId afos = new AFOSProductId("WSR", "ROB",
                            siteId);
                    // store the product ROB that is barely do-able
                    Calendar cal = (WMOTimeParser.allowArchive()
                            ? header.getHeaderDate()
                            : Calendar.getInstance());
                    RadarEdexTextProductUtil.storeTextProduct(afos, header,
                            dataString, true, cal);
                    return new PluginDataObject[0];
                }
            }
            Level3BaseRadar l3Radar = new Level3BaseRadar(messageData, headers,
                    infoDict);
            RadarRecord record = new RadarRecord();
            record.setProductCode(l3Radar.getMessageCode());
            record.setDataTime(new DataTime(l3Radar.getMessageTimestamp()));
            RadarStation station = RadarSpatialUtil
                    .getRadarStationByRpgIdDec(l3Radar.getSourceId());
            if (station == null) {
                record.setIcao("unkn");
                logger.error(headers.get("ingestfilename")
                        + " contains an rpg id(" + l3Radar.getSourceId()
                        + ") that is not in the radar_spatial table.");
            } else {
                record.setIcao(station.getRdaId().toLowerCase());
            }

            record.setLocation(station);

            RadarInfo info = infoDict.getInfo(record.getProductCode());
            if (info == null) {
                logger.error(
                        "Unknown radar product code: " + record.getProductCode()
                                + " for " + headers.get("ingestfilename"));
                return new PluginDataObject[0];
            }
            record.setFormat(info.getFormat());
            record.setNumLevels(info.getNumLevels());
            record.setGateResolution(info.getResolution());
            record.setMnemonic(info.getMnemonic());
            record.setDisplayModes(info.getDisplayModes());
            record.setUnit(info.getUnit());

            // -- some product specific decode functionality --
            // the general status message product
            if (l3Radar
                    .getMessageCode() == Level3BaseRadar.GENERAL_STATUS_MESSAGE) {
                record.setGsmMessage(l3Radar.getGsmBlock().getMessage());

                record.setPrimaryElevationAngle(0.0);
                record.setTrueElevationAngle(0.0f);
            }

            // the command parameter message product
            if (l3Radar
                    .getMessageCode() == Level3BaseRadar.COMMAND_PARAMETER_MESSAGE) {
                record.setCpmMessage(l3Radar.getCpmBlock().getMessage());
                record.setPrimaryElevationAngle(0.0);
                record.setTrueElevationAngle(0.0f);
                EDEXUtil.sendMessageAlertViz(Priority.VERBOSE,
                        RadarConstants.PLUGIN_ID, EDEX, RADAR,
                        record.getIcao()
                                + ": Command Parameter Message Received",
                        l3Radar.getCpmBlock().getMessage().toString(), null);
            }
            // the product request response product
            else if (l3Radar
                    .getMessageCode() == Level3BaseRadar.PRODUCT_REQUEST_RESPONSE_MESSAGE) {
                // do nothing with this, it will get excessive otherwise!
                return new PluginDataObject[0];
            }
            // handle the other case for free text message
            else if (l3Radar
                    .getMessageCode() == Level3BaseRadar.FREE_TEXT_MESSAGE) {
                // product already stored to the text database, so just send
                // to alertviz
                String formattedMsg = l3Radar.getTabularBlock().toString()
                        .replace("Page 1\n\t", "");
                EDEXUtil.sendMessageAlertViz(Priority.SIGNIFICANT,
                        RadarConstants.PLUGIN_ID, EDEX, RADAR,
                        record.getIcao() + ": Free Text Message Received",
                        formattedMsg, null);
                return new PluginDataObject[0];

            } else {
                record.setLatitude((float) l3Radar.getLatitude());
                record.setLongitude((float) l3Radar.getLongitude());
                record.setElevation((float) l3Radar.getHeight());
                record.setVolumeCoveragePattern(
                        l3Radar.getVolumeCoveragePattern());
                record.setOperationalMode(l3Radar.getOperationalMode());

                record.setElevationNumber(l3Radar.getElevationNumber());
                record.setVolumeScanNumber(l3Radar.getVolumeScanNumber());
                // some products don't have real elevation angles, 0 is a
                // default value
                if (record.getElevationNumber() == 0) {
                    record.setTrueElevationAngle(0f);
                } else {
                    record.setTrueElevationAngle(
                            l3Radar.getProductDependentValue(2) * 0.1f);
                }

                // determine to use the primary elevations or the elevation
                // in the terminal radar configuration file
                if (TerminalRadarUtils.isTerminalRadar(record.getIcao())
                        && info.isElevation()) {
                    Double elevation = TerminalRadarUtils
                            .getPrimarysMap(record.getIcao())
                            .get(TiltAngleBin.getPrimaryElevationAngle(
                                    record.getTrueElevationAngle()));
                    if (elevation != null) {
                        record.setPrimaryElevationAngle(
                                elevation.doubleValue());
                    } else {
                        // fall back
                        record.setPrimaryElevationAngle(
                                record.getTrueElevationAngle().doubleValue());
                    }
                } else {
                    record.setPrimaryElevationAngle(
                            TiltAngleBin.getPrimaryElevationAngle(
                                    record.getTrueElevationAngle()));
                }

                // code specific for clutter filter control
                if (record.getProductCode() == CLUTTER_FILTER_CONTROL) {
                    int segment = ((int) (Math
                            .log(l3Radar.getProductDependentValue(0))
                            / Math.log(2)));
                    record.setLayer((double) segment);
                }
                // code specific for user select accum
                else if (record.getProductCode() == USER_SELECT_ACCUM) {
                    // Default to zero
                    int layer = 0;

                    int timeSpan = l3Radar.getProductDependentValue(1);
                    if (timeSpan == 60) {
                        layer = 1;
                    } else if (timeSpan == 120) {
                        layer = 2;
                    } else if (timeSpan == 180) {
                        layer = 3;
                    } else if (timeSpan == 360) {
                        layer = 4;
                    } else if (timeSpan == 720) {
                        layer = 5;
                    } else if (timeSpan == 1440) {
                        layer = 6;
                    } else {
                        layer = 0;
                    }
                    record.setLayer((double) layer);
                    /*
                     * Need to ensure that products with different duration are
                     * distinct. Since primary elevation angle is not applicable
                     * to this product it is used to store a the duration
                     * converted to hours
                     */
                    record.setPrimaryElevationAngle(timeSpan / 60.0);
                } else if (record.getProductCode() == USER_LAYER_REFL) {
                    double bottom = l3Radar.getProductDependentValue(0);
                    double top = l3Radar.getProductDependentValue(1);
                    /*
                     * Need to ensure that products with different layer
                     * boundaries are distinct. Since primary elevation angle is
                     * not applicable to this product it is used to store a
                     * unique number for this layer.
                     */
                    record.setPrimaryElevationAngle(bottom / 100 + top);
                }

                // handle times because radar times are sent out in batches
                // (a volume scan) and we want the volume scan time to be
                // part of the data uri
                if (l3Radar.getVolumeScanTime() != null) {
                    record.setDataTime(new DataTime(
                            l3Radar.getVolumeScanTime().getTime()));
                    record.setVolScanTime(
                            l3Radar.getProductGenerationTime().getTime());
                } else {
                    record.setDataTime(new DataTime(
                            l3Radar.getMessageTimestamp().getTime()));
                    record.setVolScanTime(
                            l3Radar.getMessageTimestamp().getTime());
                }

                // thresholds specific per product and site
                if (l3Radar.getDataLevelThresholds() != null) {
                    for (int i = 0; i < 16; ++i) {
                        record.setThreshold(i,
                                l3Radar.getDataLevelThreshold(i));
                    }
                }
                // values that are product dependent as defined in the ICD
                record.setProductDependentValues(
                        l3Radar.getProductDependentValue());

                processSymbologyBlock(record, l3Radar.getSymbologyBlock());

                GraphicBlock gb = l3Radar.getGraphicBlock();
                record.setGraphicBlock(gb);

                // Tabular block is where most of the text values are...
                TabularBlock tb = l3Radar.getTabularBlock();
                if (tb != null) {
                    // complicated, but makes for logical access to needed
                    // elements
                    Map<RadarConstants.MapValues, Map<String, Map<RadarConstants.MapValues, String>>> map = new HashMap<>();
                    Map<RadarConstants.MapValues, Map<RadarConstants.MapValues, String>> recordVals = new HashMap<>();
                    RadarTabularBlockParser.parseTabularBlock(tb,
                            record.getProductCode(), map, recordVals);
                    record.setProductVals(map);
                    record.setMapRecordVals(recordVals);
                    record.setTabularBlock(tb);
                }
            }
            record.setDeltaTime(l3Radar.getDeltaTime());
            record.setScanType(l3Radar.getScanType());

            finalizeRecord(record);

            timer.stop();
            perfLog.logDuration("Time to Decode", timer.getElapsedTime());

            recordList.add(record);
        }

        return recordList.toArray(new PluginDataObject[recordList.size()]);
    }

    /**
     * Decode the level 2 data that comes in infrequently
     *
     * @param messageData
     * @param recordList
     * @throws IOException
     */
    private void decodeLevelTwoData(byte[] messageData,
            List<RadarRecord> recordList) throws IOException {
        Level2BaseRadar l2Radar = new Level2BaseRadar(messageData);

        for (RadarRecord record : l2Radar.getRecords()) {
            RadarStation radarStation = getStationByName(l2Radar.getIcao());

            record.setIcao(l2Radar.getIcao().toLowerCase());
            record.setLatitude(radarStation.getLat());
            record.setLongitude(radarStation.getLon());
            record.setElevation(radarStation.getEqpElv());

            int prodCode = record.getProductCode();
            RadarInfo info = infoDict.getInfo(prodCode);
            if (info == null) {
                logger.error(record.getIcao() + "-Unknown radar product code: "
                        + prodCode);
                continue;
            }
            record.setNumLevels(info.getNumLevels());
            record.setMnemonic(info.getMnemonic());
            record.setFormat(info.getFormat());
            record.setDisplayModes(info.getDisplayModes());
            record.setUnit(info.getUnit());
            record.setLocation(radarStation);
            finalizeRecord(record);
            recordList.add(record);
        }
    }

    /**
     * Decodes the free text messages that come infrequently
     *
     * @param messageData
     * @param headers
     */
    private void decodeFreeTextMessage(byte[] messageData, Headers headers) {
        String temp = new String(messageData);
        temp = temp.substring(0, temp.length() - 4);
        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        WMOHeader header = new WMOHeader(messageData, fileName);
        temp = temp.replace(header.toString(), "");

        String[] splits = temp.split(" ");
        AFOSProductId afos = new AFOSProductId(RadarTextProductUtil
                .createAfosId(Level3BaseRadar.FREE_TEXT_MESSAGE,
                        splits[1].substring(1)));

        // store the product to the text database
        Calendar cal = (WMOTimeParser.allowArchive() ? header.getHeaderDate()
                : Calendar.getInstance());
        RadarEdexTextProductUtil.storeTextProduct(afos, header, temp, true,
                cal);

        // send message to alertviz
        EDEXUtil.sendMessageAlertViz(Priority.VERBOSE, RadarConstants.PLUGIN_ID,
                EDEX, RADAR, "Free text message received", temp, null);
    }

    private void finalizeRecord(RadarRecord record) {
        record.setSourceTraceId(traceId);
        record.setInsertTime(TimeUtil.newGmtCalendar());
        // for GSM/CPM, we want all the messages as they have the possibility of
        // being different
        if (record.getProductCode() == Level3BaseRadar.GENERAL_STATUS_MESSAGE
                || record
                        .getProductCode() == Level3BaseRadar.COMMAND_PARAMETER_MESSAGE) {
            record.setOverwriteAllowed(true);
        } else {
            record.setOverwriteAllowed(false);
        }
    }

    /**
     * @param record
     * @param symbologyBlock
     */
    private void processSymbologyBlock(RadarRecord record,
            SymbologyBlock symbologyBlock) {
        if (symbologyBlock == null) {
            return;
        }

        int packetsKept = 0;

        List<Layer> packetsInLyrs = new ArrayList<>();
        for (int layer = 0; layer < symbologyBlock.getNumLayers(); ++layer) {
            Layer lyr = new Layer();
            lyr.setLayerId(layer);

            List<SymbologyPacket> packets = new ArrayList<>();
            SymbologyPacket[] inPackets = symbologyBlock.getPackets(layer);
            if (inPackets != null) {
                for (SymbologyPacket packet : inPackets) {
                    if (packet instanceof RadialPacket) {
                        RadialPacket radialPacket = (RadialPacket) packet;
                        processRadialPacket(record, radialPacket);
                    } else if (packet instanceof RasterPacket) {
                        RasterPacket rasterPacket = (RasterPacket) packet;
                        processRasterPacket(record, rasterPacket);
                    } else if (packet instanceof PrecipDataPacket) {
                        PrecipDataPacket precipPacket = (PrecipDataPacket) packet;
                        processPrecipPacket(record, precipPacket);
                    } else if (packet instanceof GenericDataPacket) {
                        GenericDataPacket genericPacket = (GenericDataPacket) packet;
                        List<GenericDataComponent> components = genericPacket
                                .getComponents();
                        if ((components != null) && (components.size() == 1)
                                && (components.get(0)
                                        .getComponentType() == ComponentType.RADIAL)) {
                            processRadialComponent(record,
                                    (RadialComponent) components.get(0));
                        } else {
                            packets.add(packet);
                        }
                    } else {
                        packets.add(packet);
                    }
                }
            }
            packetsKept += packets.size();
            lyr.setPackets(
                    packets.toArray(new SymbologyPacket[packets.size()]));
            packetsInLyrs.add(lyr);

        }

        // remove the radial and raster from the symb block, only keep it if
        // there are other packets.
        if (packetsKept > 0) {
            symbologyBlock.setLayers(
                    packetsInLyrs.toArray(new Layer[packetsInLyrs.size()]));
            record.setSymbologyBlock(symbologyBlock);
            record.correlateSymbologyPackets();
        }
    }

    /**
     * @param record
     * @param radialComponent
     */
    private void processRadialComponent(RadarRecord record,
            RadialComponent radialComponent) {
        record.setNumRadials(radialComponent.getNumRadials());
        record.setNumBins(radialComponent.getNumBins());
        record.setRawShortData(radialComponent.getData());
        record.setAngleData(radialComponent.getAngleData());
    }

    /**
     * @param l3Radar
     * @param radialPacket
     */
    private void processRadialPacket(RadarRecord record,
            RadialPacket radialPacket) {
        record.setNumRadials(radialPacket.getNumRadials());
        record.setNumBins(radialPacket.getNumBins());
        record.setRawData(radialPacket.getRadialData());
        record.setAngleData(radialPacket.getAngleData());
        record.setJstart(radialPacket.getFirstBinIndex());
    }

    /**
     *
     * @param record
     * @param rasterPacket
     */
    private void processRasterPacket(RadarRecord record,
            RasterPacket rasterPacket) {
        record.setNumRadials(rasterPacket.getNumRows());
        record.setNumBins(rasterPacket.getNumCols());
        record.setRawData(rasterPacket.getRasterData());
        record.setXscale(rasterPacket.getXScale());
        record.setYscale(rasterPacket.getYScale());
        record.setIstart(rasterPacket.getICenter());
        record.setJstart(rasterPacket.getJCenter());
    }

    /**
     *
     * @param record
     * @param precipPacket
     */
    private void processPrecipPacket(RadarRecord record,
            PrecipDataPacket precipPacket) {
        record.setNumRadials(precipPacket.getNumRows());
        record.setNumBins(precipPacket.getNumCols());
        record.setRawData(precipPacket.getPrecipData());
    }

    /**
     * Retrieve the radar station from the dao for the name given
     *
     * @param name
     * @return
     */
    private RadarStation getStationByName(String name) {
        RadarStation station = null;
        try {
            station = radarStationDao.queryByRdaId(name);
            if (station == null) {
                throw new IOException(
                        "No station ID found for rda_id = " + name);
            }

        } catch (Exception e) {
            logger.error("Unable to query for the radar station", e);
        }

        return station;
    }

    /**
     * @return the radarStationDao
     */
    public RadarStationDao getRadarStationDao() {
        return radarStationDao;
    }

    /**
     * @param radarStationDao
     *            the radarStationDao to set
     */
    public void setRadarStationDao(RadarStationDao radarStationDao) {
        this.radarStationDao = radarStationDao;
    }
}
