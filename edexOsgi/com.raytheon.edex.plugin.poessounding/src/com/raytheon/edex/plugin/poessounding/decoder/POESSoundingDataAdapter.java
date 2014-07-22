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
package com.raytheon.edex.plugin.poessounding.decoder;

import java.io.File;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.poessounding.dao.POESSoundingDAO;
import com.raytheon.uf.common.dataplugin.poessounding.POESSounding;
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
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * This class contains several utility methods that construct a ProfilerObs
 * instance from the BUFR decoded data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2008 1026       jkorman     Initial implementation.
 * Jul 17, 2013 2112       bsteffen    Split poes data so it gets stored in
 *                                     correct file.
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed TimeTools usage
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class POESSoundingDataAdapter {

    private static Log logger = LogFactory
            .getLog(POESSoundingDataAdapter.class);

    private static final String SPI_FILE = "poesBufr.spi";

    private static final SPIContainer SPI_DATA = populateSPIData();

    private static final int MAX_DISTANCE = 50;

    private static final int SNDG_HGT = 30;

    private static final int SNDG_DATA_START = 105;

    /**
     * Construct a ProfilerObs instance from the BUFR decoded data contained in
     * the specified separator.
     * 
     * @param iterator
     *            An iterator containing decoded BUFR data.
     * @param whoHeader
     *            the wmo header
     * @return A ProfilerObs instance, or null in the event of an error.
     */
    public static POESSounding createSoundingData(
            Iterator<BUFRDataDocument> iterator, WMOHeader wmoHeader,
            Map<File, PointDataContainer> containerMap,
            PointDataDescription pdd, POESSoundingDAO dao) {

        POESSounding obsData = null;

        BUFRDataDocument dataDoc = iterator.next();
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
                PointDataView pdv = container.append();

                getSoundingData(dataList, obsData, pdv);

                obsData.setWmoHeader(wmoHeader.getWmoHeader());

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
    private static POESSounding getHeaderData(List<IBUFRDataPacket> dataList) {

        int year = IDecoderConstants.VAL_MISSING;
        int month = IDecoderConstants.VAL_MISSING;
        int day = IDecoderConstants.VAL_MISSING;
        int hour = IDecoderConstants.VAL_MISSING;
        int minute = IDecoderConstants.VAL_MISSING;
        int seconds = IDecoderConstants.VAL_MISSING;

        POESSounding obsData = null;

        if (dataList != null) {

            obsData = new POESSounding();

            int index = 10;

            IBUFRDataPacket dp = dataList.get(index++);
            
            Double lat = getDouble(dp,null); // (Double) dp.getValue();
            dp = dataList.get(index++);
            Double lon = getDouble(dp,null); // (Double) dp.getValue();

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
                        location.assignLocation(lat.floatValue(),
                                lon.floatValue());
                        location.setStationId(s.getId());
                        obsData.setLocation(location);

                        dp = dataList.get(SNDG_HGT);
                        Double elev = getDouble(dp,-9999.0); // (Double) dp.getValue();
                        location.setElevation(elev.intValue());

                        index = 0;

                        dp = dataList.get(index++);
                        year = (dp.getValue() != null) ? ((Double) dp
                                .getValue()).intValue()
                                : IDecoderConstants.VAL_MISSING;
                        dp = dataList.get(index++);
                        month = (dp.getValue() != null) ? ((Double) dp
                                .getValue()).intValue()
                                : IDecoderConstants.VAL_MISSING;
                        dp = dataList.get(index++);
                        day = (dp.getValue() != null) ? ((Double) dp.getValue())
                                .intValue() : IDecoderConstants.VAL_MISSING;
                        dp = dataList.get(index++);
                        hour = (dp.getValue() != null) ? ((Double) dp
                                .getValue()).intValue()
                                : IDecoderConstants.VAL_MISSING;
                        dp = dataList.get(index++);
                        minute = (dp.getValue() != null) ? ((Double) dp
                                .getValue()).intValue()
                                : IDecoderConstants.VAL_MISSING;
                        dp = dataList.get(index++);
                        seconds = (dp.getValue() != null) ? ((Double) dp
                                .getValue()).intValue()
                                : IDecoderConstants.VAL_MISSING;

                        // Ensure that we have all of the time info and create
                        // the
                        // date-time and datatime info.
                        if ((year > 0) && (month > 0) && (day > 0)
                                && (hour >= 0)) {
                            Calendar baseTime = TimeUtil.newGmtCalendar(year,
                                    month, day);
                            baseTime.set(Calendar.HOUR_OF_DAY, hour);
                            baseTime.set(Calendar.MINUTE, minute);
                            baseTime.set(Calendar.SECOND, seconds);

                            DataTime dt = new DataTime(
                                    (Calendar) baseTime.clone());
                            obsData.setDataTime(dt);

                        }

                    } else {
                        obsData = null;
                    }
                }
            } else {
                obsData = null;
            }
        }

        return obsData;
    }

    /**
     * Extract all header data from the "main" observation list.
     * 
     * @param siteData
     *            List of data packets to get data from.
     * @return The ProfilerObs with primary data populated.
     */
    private static void getSoundingData(List<IBUFRDataPacket> dataList,
            POESSounding sndgData, PointDataView pdv) {
        if ((dataList != null) && (sndgData != null)) {

            int index = SNDG_DATA_START;
            int levelIndex = 0;
            while (index < dataList.size()) {

                IBUFRDataPacket packet = dataList.get(index++);
                Double pressure = (Double) packet.getValue();
                packet = dataList.get(index++);
                Double temperature = (Double) packet.getValue();
                packet = dataList.get(index++);
                Double mixingRatio = (Double) packet.getValue();
                if (pressure == null || temperature == null)
                    continue;

                pdv.setInt(
                        "pressure",
                        ((pressure != null) && (pressure > 0)) ? pressure
                                .intValue() : -9999, levelIndex);

                pdv.setFloat(
                        "temperature",
                        ((temperature != null) && (temperature > 0)) ? temperature
                                .floatValue() : -9999.0f, levelIndex);

                pdv.setFloat(
                        "mixingRatio",
                        ((mixingRatio != null) && (mixingRatio > 0)) ? mixingRatio
                                .floatValue() : -9999.0f, levelIndex);

                levelIndex++;
            }

            pdv.setInt("numLevels", levelIndex);

            sndgData.setPointDataView(pdv);
        }
        return;
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
