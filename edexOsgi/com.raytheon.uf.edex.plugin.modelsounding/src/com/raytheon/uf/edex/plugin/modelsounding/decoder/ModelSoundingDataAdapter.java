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
package com.raytheon.uf.edex.plugin.modelsounding.decoder;

import java.io.File;
import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.dataplugin.modelsounding.SoundingLevel;
import com.raytheon.uf.common.dataplugin.modelsounding.SoundingSite;
import com.raytheon.uf.common.geospatial.spi.SPIContainer;
import com.raytheon.uf.common.geospatial.spi.SPIEntry;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.bufrtools.BUFRDataDocument;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.bufrtools.packets.BUFRStringPacket;
import com.raytheon.uf.edex.bufrtools.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.bufrtools.packets.DataPacketTypes;
import com.raytheon.uf.edex.bufrtools.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.plugin.modelsounding.SoundingModelTemporalData;
import com.raytheon.uf.edex.plugin.modelsounding.common.SoundingModels;

/**
 * This class contains several utility methods that construct a ProfilerObs
 * instance from the BUFR decoded data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 17, 2008  1026     jkorman     Initial implementation.
 * May 09, 2013  1869     bsteffen    Modified D2D time series of point data to
 *                                    work without dataURI.
 * Jul 03, 2013  2161     bkowal      Relocated the logic used to retrieve
 *                                    temporal information into its own function.
 * Dec 02, 2013  2537     bsteffen    Use SoundingSite setters instead of view.
 * Jul 23, 2014  3410     bclement    location changed to floats
 * Sep 16, 2014  3628     mapeters    Replaced static imports.
 * Jul 12, 2016  5744     mapeters    SoundingStations constructor no longer takes
 *                                    path parameter
 * Sep 05, 2018           mjames@ucar Remove modelBufr filter.
 * 
 * May 19, 2021 22629  mgamazaychikov Fixed the header issue for GFS v16 update

 * 
 * </pre>
 * 
 * @author jkorman
 */
public class ModelSoundingDataAdapter {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(ModelSoundingDataAdapter.class);

    private static final Object LOCK = new Object();

    public static final String MODEL_STATION_LIST = "modelBufrStationList.txt";

    private static SoundingStations stationsList = new SoundingStations();

    /**
     * Get the temporal and model information.
     * 
     * @param dataDoc
     * @param wmoHeader
     * @return
     */
    public static SoundingModelTemporalData getSoundingTemporalInformation(
            BUFRDataDocument dataDoc, WMOHeader wmoHeader) {
        Calendar obsTime = dataDoc.getEnclosingDocument().getSection1()
                .getSectionDate();
        if (obsTime == null) {
            return null;
        }

        // Get the primary data list.
        List<IBUFRDataPacket> dataList = dataDoc.getList();
        IBUFRDataPacket dp = dataList.get(0);
        int d = dp.getReferencingDescriptor().getDescriptor();
        // retrieve the forecast seconds
        Long forecastSeconds = null;
        if (d == BUFRDescriptor.createDescriptor(0, 4, 194)) {
            forecastSeconds = (dp.getValue() != null) ? ((Double) dp.getValue())
                    .longValue() : null;
        }

        SoundingModelTemporalData soundingTemporalData = new SoundingModelTemporalData();

        DataTime dt = new DataTime(obsTime, forecastSeconds.intValue());
        soundingTemporalData.setDt(dt);

        Calendar baseTime = dt.getRefTimeAsCalendar();
        soundingTemporalData.setRefTime(baseTime.getTimeInMillis() / 1000L);

        Calendar validTime = dt.getValidTime();
        soundingTemporalData.setValidTime(validTime.getTimeInMillis() / 1000L);

        soundingTemporalData.setForecastHr((int) (forecastSeconds / 3600));

        soundingTemporalData.setModel(SoundingModels.getModel(wmoHeader
                .getCccc()));

        return soundingTemporalData;
    }

    /**
     * Construct a ProfilerObs instance from the BUFR decoded data contained in
     * the specified separator.
     * 
     * @param iterator
     *            A iterator containing decoded BUFR data.
     * @param wmoHeader
     *            the wmo header
     * @return A ProfilerObs instance, or null in the event of an error.
     */
    public static SoundingSite createSoundingData(BUFRDataDocument dataDoc,
            WMOHeader wmoHeader, PointDataContainer container,
            SoundingModelTemporalData soundingTemporalData) {

        SoundingSite obsData = null;

        synchronized (LOCK) {
            try {
                SoundingModels model = soundingTemporalData.getModel();
                // Get the primary data list.
                List<IBUFRDataPacket> dataList = dataDoc.getList();
                // Extract the header data.
                PointDataView view = container.append();
                obsData = getHeaderData(dataList, model, view);

                if (obsData != null) {
                    switch (model) {
                    case MODEL_GFS: {
                        obsData.setReportType(model.getReportType());
                        obsData = getGFSSiteData(dataList, obsData);
                        break;
                    }
                    case MODEL_ETA: {
                        obsData.setReportType(model.getReportType());
                        obsData = getETASiteData(dataList, obsData);
                        break;
                    }
                    }

                    obsData.setWmoHeader(wmoHeader.getWmoHeader());

                    obsData.setDataTime(soundingTemporalData.getDt());

                    view.setLong("validTime",
                            soundingTemporalData.getValidTime());

                }
            } catch (Throwable t) {
                logger.error("Error decoding BUFR data", t);
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
    private static SoundingSite getHeaderData(List<IBUFRDataPacket> dataList,
            SoundingModels model, PointDataView view) {

        int index = 0;
        int stationHeight = IDecoderConstants.VAL_MISSING;

        SoundingSite obsData = null;

        if (dataList != null) {

            obsData = new SoundingSite();
            SurfaceObsLocation location = new SurfaceObsLocation();

            IBUFRDataPacket dp = dataList.get(index++);
            /*
             * dp is forecastHr packet, already handled in
             * SoundingModelTemporalData
             */

            int wmoStaNum = getInt(dataList.get(index++), -9999);
            view.setInt("wmoStaNum", wmoStaNum);
            // Map the WMO station number to a station Id
            String stationId = stationsList.mapId(String.format("%010d",
                    wmoStaNum));
            // Now determine if the station Id is in this localization list.
            if (stationId != null) {
                location.setStationId(stationId);
                obsData.setSiteId(String.format("%06d", wmoStaNum));
            }

            if (model.equals(SoundingModels.MODEL_ETA)) {
                index++;
            } else if (model.equals(SoundingModels.MODEL_GFS)) {
                // GFS v16 data headers have station ID just like MODEL_ETA headers
                // For that type of data index needs to be incremented so the
                // reading of station ID is skipped.
                dp = dataList.get(index);
                if (dp instanceof BUFRStringPacket) {
                    index++;
                }
            }

            Double lat = null;
            dp = dataList.get(index++);
            int d = dp.getReferencingDescriptor().getDescriptor();
            if (d == BUFRDescriptor.createDescriptor(0, 5, 2)) {
                lat = (Double) dp.getValue();
            }
            Double lon = null;
            dp = dataList.get(index++);
            d = dp.getReferencingDescriptor().getDescriptor();
            if (d == BUFRDescriptor.createDescriptor(0, 6, 2)) {
                lon = (Double) dp.getValue();
            }
            location.assignLocation(lat.floatValue(), lon.floatValue());
            dp = dataList.get(index);
            d = dp.getReferencingDescriptor().getDescriptor();
            if (d == BUFRDescriptor.createDescriptor(0, 10, 194)) {
                stationHeight = (dp.getValue() != null) ? ((Double) dp
                        .getValue()).intValue() : null;
                location.setElevation(stationHeight);
            }
            obsData.setLocation(location);
            obsData.setPointDataView(view);
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
    @SuppressWarnings("unchecked")
    private static SoundingSite getGFSSiteData(List<IBUFRDataPacket> dataList,
            SoundingSite siteData) {
        if ((dataList != null) && (siteData != null)) {

            // get the replication sublist for the sounding data
            // istart is the number of header fields in the packet
            // in the pre v16 GFS the header has 5 fields
            // in the v16 upgrade the header has 6 fields
            // use 3rd IBUFRDataPacket to determine the version
            IBUFRDataPacket dp = dataList.get(2);
            int istart = 6;
            if (!(dp instanceof BUFRStringPacket)) {
                istart = 5;
            }
            IBUFRDataPacket p = dataList.get(istart);
            if ((p instanceof BUFRSublistPacket)
                    && (DataPacketTypes.RepSubList.getPacketType().equals(p
                            .getUnits()))) {

                List<IBUFRDataPacket> subList = (List<IBUFRDataPacket>) p
                        .getValue();

                for (IBUFRDataPacket pList : subList) {
                    List<IBUFRDataPacket> sList = (List<IBUFRDataPacket>) pList
                            .getValue();

                    createGFSLevel(sList, siteData.addLevel());
                } // for
            }

            siteData.setPressSLP(extractFloat(dataList.get(6)));
            siteData.setPressSfc(extractFloat(dataList.get(7)));

            // [0 12 061] Skin temperature
            siteData.setSkinTemp(extractFloat(dataList.get(8)));
            // *************************************************************
            siteData.setTotPrecip(extractFloat(dataList.get(11)));
            siteData.setPrecipConv(extractFloat(dataList.get(12)));
            siteData.setSnowFall(extractFloat(dataList.get(13)));
            // *************************************************************
            siteData.setCldAmtLo(extractFloat(dataList.get(14)));
            siteData.setCldAmtLo(extractFloat(dataList.get(15)));
            siteData.setCldAmtLo(extractFloat(dataList.get(16)));
            // *************************************************************
            siteData.setUc10M(extractFloat(dataList.get(17)));
            siteData.setVc10M(extractFloat(dataList.get(18)));
            siteData.setTemp2M(extractFloat(dataList.get(19)));
            siteData.setSpecHum2M(extractFloat(dataList.get(20)));
            // *************************************************************
            // Snow precipitation type
            siteData.setSnowType(extractInteger(dataList.get(21)));
            // Ice pellet precipitation type
            siteData.setIceType(extractInteger(dataList.get(21)));
            // Freezing rain precipitation type
            siteData.setFzRainType(extractInteger(dataList.get(23)));
            // Rain precipitation type
            siteData.setRainType(extractInteger(dataList.get(24)));
        }
        return siteData;
    }

    /**
     * Creates individual level data from level "sublists" that were decoded.
     * 
     * @param levelList
     *            A sublist containing level data.
     * @param level
     *            The current level to populate.
     */
    private static void createGFSLevel(List<IBUFRDataPacket> levelList,
            SoundingLevel level) {
        if (levelList != null) {

            level.setPressure(extractFloat(levelList.get(0)));
            level.setTemperature(extractFloat(levelList.get(1)));
            level.setUcWind(extractFloat(levelList.get(2)));
            level.setVcWind(extractFloat(levelList.get(3)));
            level.setSpecificHumidity(extractFloat(levelList.get(4)));
            level.setOmega(extractFloat(levelList.get(5)));
        }
    }

    private static void createETALevel(List<IBUFRDataPacket> levelList,
            SoundingLevel level) {
        // go get the common data.
        createGFSLevel(levelList, level);
        level.setLyrCldCvr(extractFloat(levelList.get(11)));
    }

    /**
     * Extract all header data from the "main" observation list.
     * 
     * @param siteData
     *            List of data packets to get data from.
     * @return The ProfilerObs with primary data populated.
     */
    @SuppressWarnings("unchecked")
    private static SoundingSite getETASiteData(List<IBUFRDataPacket> dataList,
            SoundingSite siteData) {
        if ((dataList != null) && (siteData != null)) {

            // get the replication sublist for the sounding data
            IBUFRDataPacket p = dataList.get(7);
            if ((p instanceof BUFRSublistPacket)
                    && (DataPacketTypes.RepSubList.getPacketType().equals(p
                            .getUnits()))) {

                List<IBUFRDataPacket> subList = (List<IBUFRDataPacket>) p
                        .getValue();

                for (IBUFRDataPacket pList : subList) {
                    List<IBUFRDataPacket> sList = (List<IBUFRDataPacket>) pList
                            .getValue();

                    createETALevel(sList, siteData.addLevel());
                } // for
            }

            // *************************************************************
            // [0 10 051] Pressure reduced to mean sea level
            siteData.setPressSLP(extractFloat(dataList.get(8)));
            // [0 10 195] Surface pressure
            siteData.setPressSfc(extractFloat(dataList.get(9)));

            // [0 12 061] Skin temperature
            siteData.setSkinTemp(extractFloat(dataList.get(10)));
            // [0 12 196] 1-hour minimum temperature at lowest model level
            siteData.setMinTemp(extractFloat(dataList.get(11)));
            // [0 12 197] 1-hour maximum temperature at lowest model level
            siteData.setMaxTemp(extractFloat(dataList.get(12)));
            // *************************************************************
            // [0 13 019] Total precipitation past 1 hour
            siteData.setTotPrecip(extractFloat(dataList.get(14)));
            // [0 13 208] Convective precipitation in past 1 hour
            siteData.setPrecipConv(extractFloat(dataList.get(15)));
            // *************************************************************
            // [0 12 201] 1-hour average sensible heat flux
            siteData.setSensHeat(extractFloat(dataList.get(18)));
            // [0 12 202] 1-hour average sub-surface heat flux
            siteData.setSubSfcHeat(extractFloat(dataList.get(19)));
            // [0 12 203] 1-hour average snow phase change heat flux
            siteData.setSnowFlux(extractFloat(dataList.get(20)));
            // *************************************************************
            // [0 13 216] 1-hour accumulated snowfall
            siteData.setSnowWaterEquiv(extractFloat(dataList.get(29)));
            // [0 13 210] Snow water equivalent
            siteData.setSnowFall(extractFloat(dataList.get(27)));
            // [0 13 218] 1-hour accumulated snow melt
            siteData.setSnowMelt(extractFloat(dataList.get(30)));
            // *************************************************************
            // % Amount of low clouds
            siteData.setCldAmtLo(extractFloat(dataList.get(46)));
            // % Amount of mid clouds
            siteData.setCldAmtMd(extractFloat(dataList.get(47)));
            // % Amount of high clouds
            siteData.setCldAmtHi(extractFloat(dataList.get(48)));
            // *************************************************************
            // u component at 10 meters
            siteData.setUc10M(extractFloat(dataList.get(35)));
            // v component at 10 meters
            siteData.setVc10M(extractFloat(dataList.get(36)));
            // Potential temperature at 10 m
            siteData.setTheta10M(extractFloat(dataList.get(37)));
            // Specific humidity at 10 m
            siteData.setSpecHum10M(extractFloat(dataList.get(38)));
            // *************************************************************
            // Dry-bulb temperature at 2 m
            siteData.setTemp2M(extractFloat(dataList.get(39)));
            // Specific humidity at 2 m
            siteData.setSpecHum2M(extractFloat(dataList.get(40)));
            // *************************************************************
            // Snow precipitation type
            siteData.setSnowType(extractInteger(dataList.get(50)));
            // Ice pellet precipitation type
            siteData.setIceType(extractInteger(dataList.get(51)));
            // Freezing rain precipitation type
            siteData.setFzRainType(extractInteger(dataList.get(52)));
            // Rain precipitation type
            siteData.setRainType(extractInteger(dataList.get(53)));

            siteData.setStormUComp(extractFloat(dataList.get(54)));
            siteData.setStormVComp(extractFloat(dataList.get(55)));
            // Storm relative helicity
            siteData.setStormRelHeli(extractFloat(dataList.get(56)));
            siteData.setPressCldBase(extractFloat(dataList.get(57)));
            siteData.setHorzVis(extractFloat(dataList.get(58)));

        }
        return siteData;
    }

    private static int extractInteger(IBUFRDataPacket packet) {
        if (packet == null) {
            return PointDataDescription.FILL_VALUE_INT;
        } else {
            Object val = packet.getValue();
            if (val instanceof Number) {
                return ((Number) val).intValue();
            } else {
                return PointDataDescription.FILL_VALUE_INT;
            }
        }
    }

    private static float extractFloat(IBUFRDataPacket packet) {
        if (packet == null) {
            return PointDataDescription.FILL_VALUE_INT;
        } else {
            Object val = packet.getValue();
            if (val instanceof Number) {
                return ((Number) val).floatValue();
            } else {
                return PointDataDescription.FILL_VALUE_INT;
            }
        }
    }

    public static int getInt(IBUFRDataPacket packet, int defaultValue) {
        int retValue = defaultValue;
        if (packet != null) {
            Object o = packet.getValue();
            if (o instanceof Double) {
                retValue = ((Double) o).intValue();
            } else if (o instanceof Long) {
                retValue = ((Long) o).intValue();
            }
        }
        return retValue;
    }

}
