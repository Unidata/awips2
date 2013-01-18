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

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.radar.dao.RadarStationDao;
import com.raytheon.edex.plugin.radar.level2.Level2BaseRadar;
import com.raytheon.edex.plugin.radar.level3.Level3BaseRadar;
import com.raytheon.edex.plugin.radar.util.RadarEdexTextProductUtil;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.level3.AlertMessage;
import com.raytheon.uf.common.dataplugin.radar.level3.AlertMessage.AlertCategory;
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
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.dataplugin.radar.util.TerminalRadarUtils;
import com.raytheon.uf.common.dataplugin.radar.util.TiltAngleBin;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.AFOSProductId;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Decoder implementation for radar plugin
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/14/2007    139         Phillippe   Initial check-in. Refactor of initial implementation.
 * Dec 17, 2007 600         bphillip    Added dao pool usage
 * Dec 03, 2010 2235        cjeanbap    EDEXUtility.sendMessageAlertViz() signature changed.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

public class RadarDecoder extends AbstractDecoder {

    private static final IUFStatusHandler theHandler = UFStatus
            .getHandler(RadarDecoder.class);

    // radar server sends messages from edex to cave, handle that here
    private final String EDEX = "EDEX";

    /*
     * Constants having to do with certain products
     */

    private final List<String> LEVEL_TWO_IDENTS = new ArrayList<String>(
            Arrays.asList("ARCH", "AR2V"));

    private final String NOUS = "NOUS";

    private final int USER_ALERT_MESSAGE = 73;

    private final int FREE_TEXT_MESSAGE = 75;

    private final int USER_SELECT_ACCUM = 173;

    private final int CLUTTER_FILTER_CONTROL = 34;

    /*
     * End constants
     */

    private String traceId = "";

    private RadarInfoDict infoDict;

    private RadarStationDao radarStationDao = new RadarStationDao();

    private final String RADAR = "RADAR";

    public RadarDecoder() throws DecoderException {

        String dir = "";

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        try {
            dir = pathMgr.getFile(commonStaticBase, ".").getCanonicalPath();
        } catch (IOException e) {
            theHandler.handle(Priority.ERROR,
                    "Failed to get localization directory", e);
        }

        infoDict = RadarInfoDict.getInstance(dir);

    }

    /**
     * Actual decode of the product.
     * 
     * @param messageData
     * @param headers
     * @return
     * @throws DecoderException
     */
    public PluginDataObject[] decode(byte[] messageData, Headers headers)
            throws DecoderException {
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        List<RadarRecord> recordList = new ArrayList<RadarRecord>();

        // decode the product
        String arch = new String(messageData, 0, 4);
        try {
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
                        WMOHeader header = new WMOHeader(wmoHeader.getBytes(),
                                headers);
                        String dataString = new String(messageData, 0,
                                messageData.length).substring(1,
                                messageData.length - 1);
                        String siteId = dataString.substring(0, 3);
                        AFOSProductId afos = new AFOSProductId("WSR", "ROB",
                                siteId);
                        // store the product ROB that is barely do-able
                        Calendar cal = (TimeTools.allowArchive() ? header
                                .getHeaderDate() : Calendar.getInstance());
                        RadarEdexTextProductUtil.storeTextProduct(afos, header,
                                dataString, true, cal);
                        return new PluginDataObject[0];
                    }
                }
                Level3BaseRadar l3Radar = new Level3BaseRadar(messageData,
                        headers, infoDict);
                RadarRecord record = new RadarRecord();
                record.setProductCode(l3Radar.getMessageCode());
                record.setDataTime(new DataTime(l3Radar.getMessageTimestamp()));
                RadarStation station = getStationById(l3Radar.getSourceId());
                if (station == null) {
                    record.setIcao("unkn");
                    logger.error(headers.get("ingestfilename")
                            + "-Unknown radar station id: "
                            + l3Radar.getSourceId());
                } else {
                    record.setIcao(station.getRdaId().toLowerCase());
                }

                record.setLocation(station);

                RadarInfo info = infoDict.getInfo(record.getProductCode());
                if (info == null) {
                    theHandler.handle(
                            Priority.ERROR,
                            "Unknown radar product code: "
                                    + record.getProductCode() + " for "
                                    + headers.get("ingestfilename"));
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
                if (l3Radar.getMessageCode() == l3Radar.GSM_MESSAGE) {
                    record.setGsmMessage(l3Radar.getGsmBlock().getMessage());
                    record.setPrimaryElevationAngle(0.0);
                    record.setTrueElevationAngle(0.0f);
                    handleRadarStatus(record);
                }
                // the product request response product
                else if (l3Radar.getMessageCode() == l3Radar.PRODUCT_REQUEST_RESPONSE_MESSAGE) {
                    // do nothing with this, it will get excessive otherwise!
                    return new PluginDataObject[0];
                }
                // the user alert message product
                else if (l3Radar.getMessageCode() == USER_ALERT_MESSAGE) {
                    EDEXUtil.sendMessageAlertViz(Priority.VERBOSE,
                            RadarConstants.PLUGIN_ID, EDEX, RADAR,
                            record.getIcao() + ": User Alert Message Received",
                            l3Radar.getTabularBlock().toString(), null);
                    return new PluginDataObject[0];
                }
                // handle the other case for free text message
                else if (l3Radar.getMessageCode() == FREE_TEXT_MESSAGE) {
                    // product already stored to the text database, so just send
                    // to alertviz
                    String formattedMsg = l3Radar.getTabularBlock().toString()
                            .replace("Page 1\n\t", "");
                    EDEXUtil.sendMessageAlertViz(Priority.SIGNIFICANT,
                            RadarConstants.PLUGIN_ID, EDEX, RADAR,
                            record.getIcao() + ": Free Text Message Received",
                            formattedMsg, null);
                    return new PluginDataObject[0];
                }
                // the alert adaptations parameters product
                else if (l3Radar.getMessageCode() == l3Radar.ALERT_ADAPTATION_PARAMETERS) {
                    record.setAapMessage(l3Radar.getAapMessage());
                    record.setPrimaryElevationAngle(0.0);
                    record.setTrueElevationAngle(0.0f);
                    EDEXUtil.sendMessageAlertViz(
                            Priority.VERBOSE,
                            RadarConstants.PLUGIN_ID,
                            EDEX,
                            RADAR,
                            record.getIcao()
                                    + ": Alert Adapation Parameter Message Received",
                            l3Radar.getAapMessage().toString(), null);
                }
                // the alert message product
                else if (l3Radar.getMessageCode() == l3Radar.ALERT_MESSAGE) {
                    record.setPrimaryElevationAngle(0.0);
                    record.setTrueElevationAngle(0.0f);
                    AlertMessage msg = l3Radar.getAlertMessage();
                    String details = "Alert Area : " + msg.getAlertAreaNum()
                            + "\n";
                    details += "Position : " + msg.getGridBoxAz()
                            + " deg\nRange : " + msg.getGridBoxRange() + "nm\n";
                    String category = AlertCategory.values()[msg
                            .getAlertCategory()].toString();
                    category = category.substring(category.indexOf("_"));
                    category = category.replaceAll("_", " ");
                    details += "Alert Category : " + category + "\n";
                    details += "Threshold : " + msg.getThresholdValue() + "\n";
                    details += "Exceeding : " + msg.getExceedingValue() + "\n";
                    details += "Storm Cell ID :" + msg.getStormId() + "\n";
                    record.setAlertMessage(msg);
                    EDEXUtil.sendMessageAlertViz(Priority.SIGNIFICANT,
                            RadarConstants.PLUGIN_ID, EDEX, RADAR,
                            record.getIcao() + ": Alert Message Received",
                            details, null);
                } else {
                    record.setLatitude((float) l3Radar.getLatitude());
                    record.setLongitude((float) l3Radar.getLongitude());
                    record.setElevation((float) l3Radar.getHeight());
                    record.setVolumeCoveragePattern(l3Radar
                            .getVolumeCoveragePattern());
                    record.setOperationalMode(l3Radar.getOperationalMode());

                    record.setElevationNumber(l3Radar.getElevationNumber());
                    // some products don't have real elevation angles, 0 is a
                    // default value
                    if (record.getElevationNumber() == 0) {
                        record.setTrueElevationAngle(0f);
                    } else {
                        record.setTrueElevationAngle(l3Radar
                                .getProductDependentValue(2) * 0.1f);
                    }

                    // determine to use the primary elevations or the elevation
                    // in the terminal radar configuration file
                    if (TerminalRadarUtils.isTerminalRadar(record.getIcao())) {
                        Double elevation = TerminalRadarUtils.getPrimarysMap(
                                record.getIcao()).get(
                                TiltAngleBin.getPrimaryElevationAngle(record
                                        .getTrueElevationAngle()));
                        if (elevation != null) {
                            record.setPrimaryElevationAngle(elevation
                                    .doubleValue());
                        } else {
                            // fall back
                            record.setPrimaryElevationAngle(record
                                    .getTrueElevationAngle().doubleValue());
                        }
                    } else {
                        record.setPrimaryElevationAngle(TiltAngleBin
                                .getPrimaryElevationAngle(record
                                        .getTrueElevationAngle()));
                    }

                    // code specific for clutter filter control
                    if (record.getProductCode() == CLUTTER_FILTER_CONTROL) {
                        int segment = ((int) (Math.log(l3Radar
                                .getProductDependentValue(0)) / Math.log(2)));
                        record.setLayer((double) segment);
                    }
                    // code specific for user select accum
                    else if (record.getProductCode() == USER_SELECT_ACCUM) {
                        int layer = 0; // Default to zero

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
                    }

                    // handle times because radar times are sent out in batches
                    // (a volume scan) and we want the volume scan time to be
                    // part of the data uri
                    if (l3Radar.getVolumeScanTime() != null) {
                        record.setDataTime(new DataTime(l3Radar
                                .getVolumeScanTime().getTime()));
                        record.setVolScanTime(l3Radar
                                .getProductGenerationTime().getTime());
                    } else {
                        record.setDataTime(new DataTime(l3Radar
                                .getMessageTimestamp().getTime()));
                        record.setVolScanTime(l3Radar.getMessageTimestamp()
                                .getTime());
                    }

                    // thresholds specific per product and site
                    if (l3Radar.getDataLevelThresholds() != null) {
                        for (int i = 0; i < 16; ++i) {
                            record.setThreshold(i,
                                    l3Radar.getDataLevelThreshold(i));
                        }
                    }
                    // values that are product dependent as defined in the ICD
                    record.setProductDependentValues(l3Radar
                            .getProductDependentValue());

                    processSymbologyBlock(record, l3Radar.getSymbologyBlock());

                    GraphicBlock gb = l3Radar.getGraphicBlock();
                    record.setGraphicBlock(gb);

                    // Tabular block is where most of the text values are...
                    TabularBlock tb = l3Radar.getTabularBlock();
                    if (tb != null) {
                        // complicated, but makes for logical access to needed
                        // elements
                        HashMap<RadarConstants.MapValues, Map<String, Map<RadarConstants.MapValues, String>>> map = new HashMap<RadarConstants.MapValues, Map<String, Map<RadarConstants.MapValues, String>>>();
                        HashMap<RadarConstants.MapValues, Map<RadarConstants.MapValues, String>> recordVals = new HashMap<RadarConstants.MapValues, Map<RadarConstants.MapValues, String>>();
                        RadarTabularBlockParser.parseTabularBlock(tb,
                                record.getProductCode(), map, recordVals);
                        record.setProductVals(map);
                        record.setMapRecordVals(recordVals);
                        record.setTabularBlock(tb);
                    }
                    record.setAlphanumericValues(l3Radar
                            .getAlphanumericValues());
                }

                try {
                    finalizeRecord(record);
                } catch (PluginException e) {
                    logger.error(e);
                    return new PluginDataObject[0];
                }
                recordList.add(record);

            }
        } catch (Exception e) {
            theHandler.handle(Priority.ERROR, "Couldn't properly handle "
                    + headers.get("ingestfilename"), e);
        }

        return recordList.toArray(new PluginDataObject[recordList.size()]);
    }

    /**
     * Checks the ClusterLock table to see the current status of the radar,
     * sends message to AlertViz if status has changed
     * 
     * @param record
     */
    private void handleRadarStatus(RadarRecord record) {
        // ensure that this doesn't display every time it comes in
        lookupRadarStatus("rdaStatus", record, (short) record.getGsmMessage()
                .getRdaStatus(), RadarConstants.rdaStatusStr);
        lookupRadarStatus("rpgStatus", record, (short) record.getGsmMessage()
                .getRpgStatus(), RadarConstants.rpgStatusStr);
        lookupRadarStatus("rdaOperabilityStatus", record, (short) record
                .getGsmMessage().getOperabilityStatus(),
                RadarConstants.rdaOpStatusStr);
        lookupRadarStatus("rpgOperabilityStatus", record, (short) record
                .getGsmMessage().getRpgOperability(), RadarConstants.rpgOpStr);
        lookupRadarStatus("rpgNarrowbandStatus", record, (short) record
                .getGsmMessage().getRpgNarrowbandStatus(),
                RadarConstants.rpgNarrowbandStatus);
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
                theHandler.handle(Priority.ERROR, record.getIcao()
                        + "-Unknown radar product code: " + prodCode);
                continue;
            }
            record.setNumLevels(info.getNumLevels());
            record.setMnemonic(info.getMnemonic());
            record.setFormat(info.getFormat());
            record.setDisplayModes(info.getDisplayModes());
            record.setUnit(info.getUnit());
            record.setLocation(radarStation);
            try {
                finalizeRecord(record);
            } catch (PluginException e) {
                logger.error(e);
                continue;
            }
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
        WMOHeader header = new WMOHeader(messageData, headers);
        temp = temp.replace(header.toString(), "");

        String[] splits = temp.split(" ");
        AFOSProductId afos = new AFOSProductId(
                RadarTextProductUtil.createAfosId(FREE_TEXT_MESSAGE,
                        splits[1].substring(1)));

        // store the product to the text database
        Calendar cal = (TimeTools.allowArchive() ? header.getHeaderDate()
                : Calendar.getInstance());
        RadarEdexTextProductUtil
                .storeTextProduct(afos, header, temp, true, cal);

        // send message to alertviz
        EDEXUtil.sendMessageAlertViz(Priority.VERBOSE,
                RadarConstants.PLUGIN_ID, EDEX, RADAR,
                "Free text message received", temp, null);
    }

    private void finalizeRecord(RadarRecord record) throws PluginException {
        record.setTraceId(traceId);
        record.setPluginName("radar");
        record.constructDataURI();
        record.setInsertTime(TimeTools.getSystemCalendar());
        // for GSM, we want all the messages as they have the possibility of
        // being different
        if (record.getProductCode() == Level3BaseRadar.GSM_MESSAGE) {
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

        int errorCount = 0;

        if (symbologyBlock == null) {
            return;
        }

        List<Layer> packetsInLyrs = new ArrayList<Layer>();
        for (int layer = 0; layer < symbologyBlock.getNumLayers(); ++layer) {
            Layer lyr = new Layer();
            lyr.setLayerId(layer);

            List<SymbologyPacket> packets = new ArrayList<SymbologyPacket>();
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
                        if (components != null
                                && components.size() == 1
                                && components.get(0).getComponentType() == ComponentType.RADIAL) {
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
            lyr.setPackets(packets.toArray(new SymbologyPacket[packets.size()]));
            packetsInLyrs.add(lyr);

        }

        // remove the radial and raster from the symb block
        symbologyBlock.setLayers(packetsInLyrs.toArray(new Layer[packetsInLyrs
                .size()]));
        record.setSymbologyBlock(symbologyBlock);
        record.correlateSymbologyPackets();

        if (errorCount > 0) {
            logger.error("Radar file contains " + errorCount
                    + " unrecognized symbology packet types.");
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
     * @return
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
     * Retrieve the radar station from the dao for the rpg id given
     * 
     * @param rpg_id
     * @return
     */
    private RadarStation getStationById(int rpg_id) {
        try {
            RadarStation station = radarStationDao.queryByRpgIdDec(String
                    .format("%03d", rpg_id));
            return station;
        } catch (Exception e) {
            logger.error("Error retrieving RadarStation for id: " + rpg_id, e);
        }
        return null;
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
                throw new IOException("No station ID found for rda_id = "
                        + name);
            }

        } catch (Exception e) {
            theHandler.handle(Priority.ERROR,
                    "Unable to query for the radar station", e);
        }

        return station;
    }

    private void lookupRadarStatus(String lockname, RadarRecord record,
            short messagePart, String[] constants) {
        ClusterTask task = ClusterLockUtils.lookupLock(lockname,
                record.getIcao());
        String formatStatus = RadarUtil.formatBits(messagePart, constants);
        if (task == null || task.getExtraInfo() == null) {
            ClusterLockUtils.lock(lockname, record.getIcao(), formatStatus, 30,
                    true);
            EDEXUtil.sendMessageAlertViz(Priority.INFO,
                    RadarConstants.PLUGIN_ID, EDEX, RADAR, record.getIcao()
                            + ": General Status Message Received",
                    "Adding new entry to table : " + lockname + "="
                            + formatStatus, null);
            ClusterLockUtils.unlock(lockname, record.getIcao());
            return;
        }

        if (task.getExtraInfo() != null) {
            if (formatStatus != null
                    && !formatStatus.equals(task.getExtraInfo().trim())) {
                String details = "";
                String temp = "";
                switch (record.getGsmMessage().getMode()) {
                case 0:
                    temp = " Maintenance Mode |";
                    break;
                case 1:
                    temp = " Clear Air Mode |";
                    break;
                case 2:
                    temp = " Precipitation/Severe Weather Mode |";
                    break;
                }
                details += "Op Mode/VCP =" + temp + " VCP"
                        + record.getGsmMessage().getVolumeCoveragePattern()
                        + "\n";
                details += "New Product Status = "
                        + RadarUtil.formatBits((short) record.getGsmMessage()
                                .getProductAvail(),
                                RadarConstants.productAvailStr) + "\n";
                details += "RPG Availability = "
                        + RadarUtil.formatBits((short) record.getGsmMessage()
                                .getRpgOperability(), RadarConstants.rpgOpStr)
                        + "\n";
                details += "RDA Availability = "
                        + RadarUtil.formatBits((short) record.getGsmMessage()
                                .getOperabilityStatus(),
                                RadarConstants.rdaOpStatusStr) + "\n";
                ClusterLockUtils.lock(lockname, record.getIcao(), formatStatus,
                        30, true);
                EDEXUtil.sendMessageAlertViz(Priority.SIGNIFICANT,
                        RadarConstants.PLUGIN_ID, EDEX, RADAR, record.getIcao()
                                + ": General Status Message Received", lockname
                                + " has Changed : " + formatStatus + "\n\n"
                                + details, null);
                ClusterLockUtils.unlock(lockname, record.getIcao());
            }
        }
    }

    public RadarStationDao getRadarStationDao() {
        return radarStationDao;
    }

    public void setRadarStationDao(RadarStationDao radarStationDao) {
        this.radarStationDao = radarStationDao;
    }
}
