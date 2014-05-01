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
package com.raytheon.edex.plugin.goessounding.decoder;

import static com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes.RepSubList;

import java.io.File;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.goessounding.dao.GOESSoundingDAO;
import com.raytheon.uf.common.dataplugin.goessounding.GOESSounding;
import com.raytheon.uf.common.geospatial.spi.SPIContainer;
import com.raytheon.uf.common.geospatial.spi.SPIEntry;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * This class contains several utility methods that construct a GOESSounding
 * instance from the BUFR decoded data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2008 1077       jkorman     Initial implementation.
 * May 09, 2013 1869       bsteffen    Modified D2D time series of point data to
 *                                     work without dataURI.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class GOESSoundingDataAdapter {

    private static Log logger = LogFactory
            .getLog(GOESSoundingDataAdapter.class);

    private static final String SPI_FILE = "goesBufr.spi";

    private static final int MAX_DISTANCE = 50;

    private static final int SAT_ID_POS = 5;

    private static final int SAT_PROC_INSTR = 6;

    // Use this position, instead of position 0, per the GOES Code.
    private static final int SAT_YEAR_POS = 15;

    private static final int SAT_QUAL_POS = 22;

    private static final int SAT_CHANNELS_POS = 23;

    private static final int SAT_SOUNDERDATA_POS = 45;

    private static final SPIContainer SPI_DATA = populateSPIData();

    /**
     * Construct a ProfilerObs instance from the BUFR decoded data contained in
     * the specified separator.
     * 
     * @param separator
     *            A separator containing decoded BUFR data.
     * @return A ProfilerObs instance, or null in the event of an error.
     */
    public static GOESSounding createSoundingData(BUFRDataDocument dataDoc,
            WMOHeader wmoHeader, Map<File, PointDataContainer> containerMap,
            PointDataDescription pdd, GOESSoundingDAO dao) {

        GOESSounding obsData = null;
        if (dataDoc != null) {

            // Get the primary data list.
            List<IBUFRDataPacket> dataList = dataDoc.getList();
            // Extract the header data.
            obsData = getHeaderData(dataList);

            if (obsData != null) {
                // Have to do this to make sure data gets in the right file
                // as the bufr files come in across hours. Eventually
                // may want to solve this in a different way so that this
                // knowledge
                // of dao at this point is not necessary.
                File file = dao.getFullFilePath(obsData);
                PointDataContainer container = containerMap.get(file);
                if (container == null) {
                    container = PointDataContainer.build(pdd);
                    containerMap.put(file, container);
                }
                obsData.setWmoHeader(wmoHeader.getWmoHeader());
                obsData = getGOESData(dataList, obsData, container);

            }
        }
        return obsData;
    }

    /**
     * Extract all header data from the "main" observation list.
     * 
     * @param dataList
     *            List of data packets to get data from.
     * @return The ProfilerObs with primary data populated.
     */
    private static GOESSounding getHeaderData(List<IBUFRDataPacket> dataList) {

        Integer year = IDecoderConstants.VAL_MISSING;
        Integer day = IDecoderConstants.VAL_MISSING;
        Integer hour = IDecoderConstants.VAL_MISSING;
        Integer minute = IDecoderConstants.VAL_MISSING;
        Integer seconds = IDecoderConstants.VAL_MISSING;

        Double lat = null;
        Double lon = null;

        IBUFRDataPacket dp = null;
        GOESSounding obsData = null;

        int index = 0;

        if (dataList != null) {

            obsData = new GOESSounding();

            index = SAT_YEAR_POS;
            dp = dataList.get(index++);
            year = getInt(dp,null);
            
//            year = (dp.getValue() != null) ? ((Number) dp.getValue())
//                    .intValue() : null;
            dp = dataList.get(index++);
            day = getInt(dp,null);

//            day = (dp.getValue() != null) ? ((Number) dp.getValue()).intValue()
//                    : null;

            dp = dataList.get(index++);
            lat = getDouble(dp,null);
//            lat = ((Number) dp.getValue()).doubleValue();
            dp = dataList.get(index++);
            lon = getDouble(dp,null);
//            lon = ((Number) dp.getValue()).doubleValue();

            // If we have lat/lon data, set it into the obs object. Otherwise
            // no need to go further, set the obs object to null and quit!
            if ((lat != null) && (lon != null)) {
                if ((lat <= IDecoderConstants.VAL_MISSING)
                        || (lon <= IDecoderConstants.VAL_MISSING)) {
                    obsData = null;
                } else {
                    SPIEntry s = SPI_DATA.nearest(lat, lon, MAX_DISTANCE);
                    if (s != null) {
                        SurfaceObsLocation location = new SurfaceObsLocation();
                        location.assignLocation(lat, lon);
                        location.setStationId(s.getId());
                        obsData.setLocation(location);
                    } else {
                        obsData = null;
                    }
                }
            } else {
                obsData = null;
            }
            if (obsData != null) {
                dp = dataList.get(index++);
                hour = getInt(dp,null);
//                hour = (dp.getValue() != null) ? ((Double) dp.getValue())
//                        .intValue() : null;

                dp = dataList.get(index++);
                minute = getInt(dp,null);
//                minute = (dp.getValue() != null) ? ((Double) dp.getValue())
//                        .intValue() : null;

                dp = dataList.get(index++);
                seconds = getInt(dp,null);
//                seconds = (dp.getValue() != null) ? ((Double) dp.getValue())
//                        .intValue() : null;

                Calendar baseTime = TimeTools.getBaseCalendar(year, 1, 1);
                baseTime.set(Calendar.DAY_OF_YEAR, day);
                baseTime.set(Calendar.HOUR_OF_DAY, hour);
                baseTime.set(Calendar.MINUTE, minute);
                baseTime.set(Calendar.SECOND, seconds);
                baseTime.set(Calendar.MILLISECOND, 0);
                obsData.setTimeObs(TimeTools.copy(baseTime));
                DataTime dt = new DataTime(TimeTools.copy(baseTime));
                obsData.setDataTime(dt);
            }
        }
        return obsData;
    }

    /**
     * Extract all header data from the "main" observation list.
     * 
     * @param obsData
     *            List of data packets to get data from.
     * @return The ProfilerObs with primary data populated.
     */
    @SuppressWarnings("unchecked")
    private static GOESSounding getGOESData(List<IBUFRDataPacket> dataList,
            GOESSounding obsData, PointDataContainer container) {
        if ((dataList != null) && (obsData != null)) {

            PointDataView view = container.append();

            int n;

            IBUFRDataPacket dp = dataList.get(SAT_ID_POS);
            n = getInt(dp,-9999);
//            n = (dp.getValue() != null) ? ((Long) dp.getValue()).intValue()
//                    : -9999;
            view.setInt("satid", n);

            dp = dataList.get(SAT_PROC_INSTR);
            n = getInt(dp,-9999);
//            n = (dp.getValue() != null) ? ((Long) dp.getValue()).intValue()
//                    : -9999;
            view.setInt("satinstrument", n);

            dp = dataList.get(SAT_QUAL_POS);
            n = getInt(dp,-9999);
//            n = (dp.getValue() != null) ? ((Long) dp.getValue()).intValue()
//                    : -9999;
            view.setInt("qualityinfo", n);

            dp = dataList.get(SAT_CHANNELS_POS);
            n = getInt(dp,-9999);
//            n = (dp.getValue() != null) ? ((Long) dp.getValue()).intValue()
//                    : -9999;
            view.setInt("sounderchannels", n);

            // TODO: need elevation
            // dp = dataList.get(SAT_CHANNELS_POS);
            // TODO: need solar elevation
            // TODO: need cloudAmt, cloudTopPressure, precipWater and
            // skinTemp

            view.setString("wmoHeader", obsData.getWmoHeader());

            // get the replication sublist for the sounding data
            IBUFRDataPacket p = dataList.get(SAT_SOUNDERDATA_POS);
            if ((p instanceof BUFRSublistPacket)
                    && (RepSubList.getPacketType().equals(p.getUnits()))) {

                List<IBUFRDataPacket> subList = (List<IBUFRDataPacket>) p
                        .getValue();
                int levelIndex = 0;
                for (IBUFRDataPacket pList : subList) {
                    List<IBUFRDataPacket> sList = (List<IBUFRDataPacket>) pList
                            .getValue();

                    int levels = createLevel(sList, view, levelIndex);
                    levelIndex += levels;
                } // for

                view.setInt("numLevels", levelIndex);
            }
            obsData.setPointDataView(view);
        }
        return obsData;
    }

    /**
     * Creates individual level data from level "sublists" that were decoded.
     * 
     * @param levelList
     *            A sublist containing level data.
     * @param view
     *            the view to populate
     * @return number of levels populated
     */
    private static int createLevel(List<IBUFRDataPacket> levelList,
            PointDataView view, int levelIndex) {
        final int PRESSURE_POS = 0;
        final int TEMPERATURE_POS = 1;
        final int DEWPOINT_POS = 2;
        final int HEIGHT_POS = 3;

        IBUFRDataPacket packet = null;

        if (levelList != null) {

            packet = levelList.get(PRESSURE_POS);
            Double pressureVal = (Double) packet.getValue();

            packet = levelList.get(TEMPERATURE_POS);
            Double temperatureVal = (Double) packet.getValue();

            packet = levelList.get(DEWPOINT_POS);
            Double dewpoint = (Double) packet.getValue();

            packet = levelList.get(HEIGHT_POS);
            Double height = (Double) packet.getValue();

            if (pressureVal != null) {
                if ((temperatureVal == null) && (dewpoint == null)) {
                    return 0;
                }
            } else {
                // No pressure data
                return 0;
            }

            view.setInt(
                    "pressure",
                    ((pressureVal != null) && (pressureVal > 0)) ? pressureVal
                            .intValue() : -9999, levelIndex);

            view.setFloat(
                    "temperature",
                    ((temperatureVal != null) && (temperatureVal > 0)) ? temperatureVal
                            .floatValue() : -9999.0f, levelIndex);

            view.setFloat(
                    "dewPoint",
                    ((dewpoint != null) && (dewpoint > 0)) ? dewpoint
                            .floatValue() : -9999.0f, levelIndex);

            view.setFloat("height",
                    ((height != null) && (height > -400)) ? height.floatValue()
                            : -9999.0f, levelIndex);

        }

        return 1;
    }

    /**
     * 
     * @param packet
     * @param defaultValue
     * @return
     */
    private static Double getDouble(IBUFRDataPacket packet, Double defaultValue) {
        Double retValue = defaultValue;
        if(packet != null) {
            Object o = packet.getValue();
            if(o instanceof Double) {
                retValue = ((Double) o).doubleValue();
            } else if(o instanceof Long) {
                retValue = ((Long) o).doubleValue();
            } 
        }
        return retValue;
    }

    /**
     * 
     * @param packet
     * @param defaultValue
     * @return
     */
    private static Integer getInt(IBUFRDataPacket packet, Integer defaultValue) {
        Integer retValue = defaultValue;
        if(packet != null) {
            Object o = packet.getValue();
            if(o instanceof Double) {
                retValue = ((Double) o).intValue();
            } else if(o instanceof Long) {
                retValue = ((Long) o).intValue();
            } 
        }
        return retValue;
    }
    
    
    
    /**
     * 
     * @return
     */
    private static SPIContainer populateSPIData() {
        SPIContainer container = null;

        PathManager pathMgr = (PathManager) PathManagerFactory.getPathManager();

        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        String site = ctx.getContextName();

        logger.info("Loading " + SPI_FILE + " for site [" + site + "]");

        File siteDir = pathMgr.getFile(ctx, "basemaps");

        File srcFile = new File(siteDir, SPI_FILE);

        container = new SPIContainer(srcFile);
        if (container.isLoaded()) {
            logger.info("Loading " + SPI_FILE + " for site [" + site
                    + "] Successful");
        } else {
            logger.error("Loading " + SPI_FILE + " for site [" + site
                    + "] failed");
        }

        return container;
    }
}
