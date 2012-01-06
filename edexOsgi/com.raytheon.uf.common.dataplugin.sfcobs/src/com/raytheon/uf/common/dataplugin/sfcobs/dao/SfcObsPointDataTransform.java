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
package com.raytheon.uf.common.dataplugin.sfcobs.dao;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.sfcobs.AncPrecip;
import com.raytheon.uf.common.dataplugin.sfcobs.InterWinds;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2009             jkorman     Initial creation
 * Feb 15,2011  5705       cjeanbap    Added wmoHeader to HDR_PARAMS_LIST.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class SfcObsPointDataTransform {

    /** The logger */
    private static Log logger = LogFactory
            .getLog(SfcObsPointDataTransform.class);

    private static final int INT_DEFAULT = -9999;

    private static final float FLOAT_DEFAULT = -9999.0f;

    // /pluginName/dataTime/reportType/corIndicator/latitude/longitude
    // /sfcobs/2009-10-12_12:00:00.0/1001/null/71905/55.28/-77.76

    // ** DataUri
    private static final String STATION_ID = "stationId";

    // ** DataUri
    private static final String LATITUDE = "latitude";

    // ** DataUri
    private static final String LONGITUDE = "longitude";

    private static final String ELEVATION = "elevation";

    private static final String TIME_OBS = "timeObs";

    private static final String TIME_NOMINAL = "timeNominal";

    // ** DataUri
    private static final String REPORT_TYPE = "reportType";

    // ** DataUri
    private static final String COR_INDICATOR = "corIndicator";

    private static final String DATA_URI = "dataURI";

    private static final String WMO_HEADER = "wmoHeader";

    private static final String RAW_REPORT = "rawReport";

    private static final String PRESS_CHANGE_3HR = "pressChange3Hour";

    private static final String PRESS_CHANGE_CHAR = "pressChangeChar";

    private static final String PRECIP1_HOUR = "precip1Hour";

    private static final String PRECIP6_HOUR = "precip6Hour";

    private static final String PRECIP12_HOUR = "precip12Hour";

    private static final String PRECIP18_HOUR = "precip18Hour";

    private static final String PRECIP24_HOUR = "precip24Hour";

    private static final String TEMPERATURE = "temperature";

    private static final String DEWPOINT = "dewpoint";

    private static final String WIND_DIR = "windDir";

    private static final String WIND_GUST = "windGust";

    private static final String WIND_SPEED = "windSpeed";

    private static final String PEAK_WIND_DIR = "peakWindDir";

    private static final String PEAK_WIND_SPEED = "peakWindSpeed";

    private static final String PEAK_WIND_SPEED_TIME = "peakWindSpeedTime";

    private static final String SEA_LEVEL_PRESS = "seaLevelPress";

    private static final String ALTIMETER = "altimeter";

    private static final String STATION_PRESS = "stationPress";

    private static final String VISIBILITY = "visibility";

    private static final String PRES_WEATHER = "presWeather";

    private static final String WX_PRESENT = "wx_present";

    private static final String WX_PAST_1 = "wx_past_1";

    private static final String WX_PAST_2 = "wx_past_2";

    private static final String CLOUD_AMOUNT_TOT = "totCloudAmount";

    private static final String CLOUD_HGT_LOW = "lowCloudHeight";

    private static final String CLOUD_AMOUNT_LOW = "lowCloudAmount";

    private static final String CLOUD_TYPE_LOW = "lowCloudType";

    private static final String CLOUD_TYPE_MID = "midCloudType";

    private static final String CLOUD_TYPE_HI = "hiCloudType";

    private static final String SEA_SFC_TEMP = "seaSurfaceTemp";

    private static final String ICE_CODE = "iceCode";

    private static final String WET_BULB = "wetBulb";

    private static final String PLATFORM_DIR = "platformTrueDirection";

    private static final String PLATFORM_SPD = "platformTrueSpeed";

    private static final String WIND_SPD_EQUIV_10M = "equivWindSpeed10m";

    private static final String WIND_SPD_EQUIV_20M = "equivWindSpeed20m";

    private static final String WIND_WV_HGT = "windWaveHeight";

    private static final String WIND_WV_PD = "windWavePeriod";

    private static final String WV_HGT = "waveHeight";

    private static final String WV_PD = "wavePeriod";

    private static final String WV_STEEPNESS = "waveSteepness";

    private static final String HI_RES_WV_HGT = "highResWaveHeight";

    private static final String PRI_SWELL_WV_DIR = "primarySwellWaveDir";

    private static final String PRI_SWELL_WV_PD = "primarySwellWavePeriod";

    private static final String PRI_SWELL_WV_HGT = "primarySwellWaveHeight";

    private static final String SEC_SWELL_WV_DIR = "secondarySwellWaveDir";

    private static final String SEC_SWELL_WV_PD = "secondarySwellWavePeriod";

    private static final String SEC_SWELL_WV_HGT = "secondarySwellWaveHeight";

    private static final String NUM_INTER_WINDS = "numInterWinds";

    private static final String INTER_WIND_TIME = "interWindTime";

    private static final String INTER_WIND_DIR = "interWindDir";

    private static final String INTER_WIND_SPD = "interWindSpeed";

    public static final String HDR_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();

        sb.append("stationId,");
        sb.append("latitude,");
        sb.append("longitude,");
        sb.append("elevation,");
        sb.append("timeObs,");
        sb.append("timeNominal,");
        sb.append("reportType,");
        sb.append("dataURI,");
        sb.append("rawReport,");
        sb.append("wmoHeader,");

        HDR_PARAMS_LIST = sb.toString();
    }

    public static final String MAN_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        sb.append(HDR_PARAMS_LIST);

        sb.append("temperature,");
        sb.append("dewpoint,");

        sb.append("windSpeed,");
        sb.append("windDir,");
        sb.append("windGust,");

        sb.append("peakWindSpeedTime,");
        sb.append("peakWindDir,");
        sb.append("peakWindSpeed,");

        sb.append("seaLevelPress,");
        sb.append("altimeter,");
        sb.append("stationPress,");
        sb.append("pressChangeChar,");
        sb.append("pressChange3Hour,");

        sb.append("visibility,");
        sb.append("wx_past_1,");
        sb.append("wx_past_2,");
        sb.append("wx_present,");
        sb.append("presWeather,");

        sb.append("totCloudAmount,");
        sb.append("lowCloudHeight,");
        sb.append("lowCloudAmount,");
        sb.append("lowCloudType,");
        sb.append("midCloudType,");
        sb.append("hiCloudType,");

        sb.append("precip1Hour,");
        sb.append("precip6Hour,");
        sb.append("precip12Hour,");
        sb.append("precip18Hour,");
        sb.append("precip24Hour,");

        sb.append("equivWindSpeed10m,");
        sb.append("equivWindSpeed20m,");

        sb.append("iceCode,");
        sb.append("wetBulb,");

        sb.append("seaSurfaceTemp,");
        sb.append("platformTrueDirection,");
        sb.append("platformTrueSpeed,");

        sb.append("windWaveHeight,");
        sb.append("windWavePeriod,");

        sb.append("waveHeight,");
        sb.append("wavePeriod,");
        sb.append("waveSteepness,");

        sb.append("highResWaveHeight,");

        sb.append("primarySwellWaveDir,");
        sb.append("primarySwellWavePeriod,");
        sb.append("primarySwellWaveHeight,");

        sb.append("secondarySwellWaveDir,");
        sb.append("secondarySwellWavePeriod,");
        sb.append("secondarySwellWaveHeight,");

        sb.append("numInterWinds,");
        sb.append("interWindTime,");
        sb.append("interWindDir,");
        sb.append("interWindSpeed");

        MAN_PARAMS_LIST = sb.toString();
    }

    private SfcObsDao dao;

    private PointDataDescription description;

    /**
     * 
     */
    public SfcObsPointDataTransform() {

    }

    /**
     * 
     * @param pluginName
     */
    public SfcObsPointDataTransform(String pluginName) {
        try {
            description = getDescription(pluginName);

        } catch (Exception e) {
            logger.error("Error creating PointDataDescription", e);
        }
        try {
            dao = new SfcObsDao(pluginName);
        } catch (Exception e) {
            logger.error("", e);
        }
    }

    /**
     * 
     * @param pdo
     * @return
     */
    public PluginDataObject[] toPointData(PluginDataObject[] pdo) {

        if (pdo.length > 0) {
            Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();

            for (PluginDataObject p : pdo) {
                if (!(p instanceof ObsCommon))
                    continue;

                File f = this.dao.getFullFilePath(p);
                PointDataContainer pdc = pointMap.get(f);
                if (pdc == null) {
                    pdc = PointDataContainer.build(this.description);
                    pointMap.put(f, pdc);
                }

                ObsCommon obs = (ObsCommon) p;
                PointDataView pdv = buildView(pdc, obs);
                if (pdv != null) {
                    obs.setPointDataView(pdv);
                } else {
                    // indicate the error here
                }
            }
        }
        return pdo;
    }

    /**
     * 
     * @param container
     * @param record
     * @return
     */
    private PointDataView buildView(PointDataContainer container,
            ObsCommon record) {

        PointDataView pdv = container.append();
        if (pdv != null) {
            pdv.setLong(TIME_OBS, record.getTimeObs().getTimeInMillis());
            pdv.setLong(TIME_NOMINAL, record.getRefHour().getTimeInMillis());

            pdv.setString(RAW_REPORT, record.getObsText());
            pdv.setString(WMO_HEADER, record.getWmoHeader());

            pdv.setFloat(TEMPERATURE, getFloat(record.getTemp()));
            pdv.setFloat(DEWPOINT, getFloat(record.getDwpt()));
            pdv.setFloat(WIND_SPEED, getFloat(record.getWindSpeed()));
            pdv.setFloat(WIND_DIR, getFloat(record.getWindDirection()));
            pdv.setFloat(WIND_GUST, getFloat(record.getWindGust().floatValue()));
            pdv.setFloat(PEAK_WIND_SPEED, getFloat(record.getPeakWindSpeed()));
            pdv.setLong(PEAK_WIND_SPEED_TIME, record.getPeakWindTime());
            pdv.setFloat(PEAK_WIND_DIR, getFloat(record.getPeakWindDir()));

            pdv.setFloat(SEA_LEVEL_PRESS,
                    getFloat(record.getPressureSealevel()));
            pdv.setFloat(ALTIMETER, getFloat(record.getPressureAltimeter()));
            pdv.setFloat(STATION_PRESS, getFloat(record.getPressureStation()));
            pdv.setFloat(PRESS_CHANGE_3HR, getFloat(record.getPressChange3Hr()));
            pdv.setInt(PRESS_CHANGE_CHAR, getInt(record.getPressChangeChar()));

            pdv.setInt(VISIBILITY, getInt(record.getHorzVisibility()));
            pdv.setInt(WX_PRESENT, getInt(record.getWx_present()));
            pdv.setString(PRES_WEATHER, record.getPresWeather());
            pdv.setInt(WX_PAST_1, getInt(record.getWx_past_1()));
            pdv.setInt(WX_PAST_2, getInt(record.getWx_past_2()));

            pdv.setInt(CLOUD_AMOUNT_TOT, record.getTotalCloudCover());
            pdv.setInt(CLOUD_HGT_LOW, record.getCloudBaseHeight());

            pdv.setInt(CLOUD_TYPE_LOW, record.getLowCloudType());
            pdv.setInt(CLOUD_TYPE_MID, record.getMidCloudType());
            pdv.setInt(CLOUD_TYPE_HI, record.getHighCloudType());

            pdv.setFloat(WIND_SPD_EQUIV_10M, getFloat(record.getWind10mSpeed()));
            pdv.setFloat(WIND_SPD_EQUIV_20M, getFloat(record.getWind20mSpeed()));

            // pdv.setInt(ICE_CODE,record.getIceCode);
            pdv.setFloat(SEA_SFC_TEMP, getFloat(record.getSeaTemp()));
            pdv.setFloat(WET_BULB, getFloat(record.getWetBulb()));

            pdv.setFloat(PLATFORM_DIR, getFloat(record.getPlatformDirection()));
            pdv.setFloat(PLATFORM_SPD, getFloat(record.getPlatformMovement()));

            pdv.setFloat(WIND_WV_HGT, getFloat(record.getWindWaveHeight()));
            pdv.setInt(WIND_WV_PD, getInt(record.getWindWavePeriod()));

            pdv.setFloat(WV_HGT, getFloat(record.getWaveHeight()));
            pdv.setInt(WV_PD, getInt(record.getWavePeriod()));
            pdv.setFloat(WV_STEEPNESS, getFloat(record.getWaveSteepness()));

            pdv.setFloat(HI_RES_WV_HGT, getFloat(record.getHighResWaveHeight()));

            pdv.setFloat(PRI_SWELL_WV_DIR,
                    getFloat(record.getPrimarySwellWaveDir()));
            pdv.setInt(PRI_SWELL_WV_PD,
                    getInt(record.getPrimarySwellWavePeriod()));
            pdv.setFloat(PRI_SWELL_WV_HGT,
                    getFloat(record.getPrimarySwellWaveHeight()));

            pdv.setFloat(SEC_SWELL_WV_DIR,
                    getFloat(record.getSecondarySwellWaveDir()));
            pdv.setInt(SEC_SWELL_WV_PD,
                    getInt(record.getSecondarySwellWavePeriod()));
            pdv.setFloat(SEC_SWELL_WV_HGT,
                    getFloat(record.getSecondarySwellWaveHeight()));

            List<AncPrecip> precip = record.getAncPrecip();
            if ((precip != null) && (precip.size() > 0)) {
                for (AncPrecip p : record.getAncPrecip()) {
                    float v = p.getPrecipAmount().floatValue();
                    switch (p.getTimePeriod()) {
                    case 3600: {
                        pdv.setFloat(PRECIP1_HOUR, v);
                        break;
                    }
                    case 21600: {
                        pdv.setFloat(PRECIP6_HOUR, v);
                        break;
                    }
                    case 43200: {
                        pdv.setFloat(PRECIP12_HOUR, v);
                        break;
                    }
                    case 64800: {
                        pdv.setFloat(PRECIP18_HOUR, v);
                        break;
                    }
                    case 86400: {
                        pdv.setFloat(PRECIP24_HOUR, v);
                        break;
                    }
                    } // switch
                } // for
            }
        }

        int idx = 0;
        List<InterWinds> winds = record.getInterWinds();
        if ((winds != null) && (winds.size() > 0)) {
            for (InterWinds w : record.getInterWinds()) {
                if (w.getObsTime() != null) {
                    pdv.setLong(INTER_WIND_TIME, w.getObsTime()
                            .getTimeInMillis(), idx);
                    pdv.setFloat(INTER_WIND_DIR, getFloat(w.getWindDir()), idx);
                    pdv.setFloat(INTER_WIND_SPD, getFloat(w.getWindSpeed()),
                            idx);
                    idx++;
                }
            }
            pdv.setInt(NUM_INTER_WINDS, idx);
        }

        return pdv;
    }

    /**
     * 
     * @param type
     * @return
     * @throws JAXBException
     */
    private PointDataDescription getDescription(String type)
            throws JAXBException {
        InputStream is = this.getClass().getResourceAsStream(
                "/res/pointdata/" + type + ".xml");
        if (is == null) {
            throw new RuntimeException("Cannot find descriptor for: " + type);
        }
        PointDataDescription d = PointDataDescription.fromStream(is);

        return d;
    }

    public static ObsCommon toSfcObsRecord(PointDataView pdv) {
        ObsCommon obs = getObsHdr(pdv);
        if (obs != null) {

            obs.setTemp(pdv.getNumber(TEMPERATURE).doubleValue());
            obs.setDwpt(pdv.getNumber(DEWPOINT).doubleValue());
            obs.setWindSpeed(pdv.getNumber(WIND_SPEED).doubleValue());
            obs.setWindDirection(pdv.getNumber(WIND_DIR).intValue());
            obs.setWindGust(pdv.getNumber(WIND_GUST).doubleValue());
            obs.setPeakWindSpeed(pdv.getNumber(PEAK_WIND_SPEED).doubleValue());
            obs.setPeakWindTime(pdv.getNumber(PEAK_WIND_SPEED_TIME).longValue());
            obs.setPeakWindDir(pdv.getNumber(PEAK_WIND_DIR).intValue());

            obs.setPressureSealevel(pdv.getNumber(SEA_LEVEL_PRESS).intValue());
            obs.setPressureAltimeter(pdv.getNumber(ALTIMETER).intValue());
            obs.setPressureStation(pdv.getNumber(STATION_PRESS).intValue());
            obs.setPressChange3Hr(pdv.getNumber(PRESS_CHANGE_3HR).doubleValue());
            obs.setPressChangeChar(pdv.getNumber(PRESS_CHANGE_CHAR).intValue());

            obs.setHorzVisibility(pdv.getNumber(VISIBILITY).intValue());
            obs.setWx_present(pdv.getNumber(WX_PRESENT).intValue());
            obs.setPresWeather(pdv.getString(PRES_WEATHER));
            obs.setWx_past_1(pdv.getNumber(WX_PAST_1).intValue());
            obs.setWx_past_2(pdv.getNumber(WX_PAST_2).intValue());

            obs.setTotalCloudCover(pdv.getNumber(CLOUD_AMOUNT_TOT).intValue());
            obs.setCloudBaseHeight(pdv.getNumber(CLOUD_HGT_LOW).intValue());
            obs.setLowCloudType(pdv.getNumber(CLOUD_TYPE_LOW).intValue());
            obs.setMidCloudType(pdv.getNumber(CLOUD_TYPE_MID).intValue());
            obs.setHighCloudType(pdv.getNumber(CLOUD_TYPE_HI).intValue());

            obs.setWind10mSpeed(pdv.getNumber(WIND_SPD_EQUIV_10M).doubleValue());
            obs.setWind20mSpeed(pdv.getNumber(WIND_SPD_EQUIV_20M).doubleValue());

            // pdv.setInt(ICE_CODE,record.getIceCode);
            obs.setSeaTemp(pdv.getNumber(SEA_SFC_TEMP).doubleValue());
            obs.setWetBulb(pdv.getNumber(WET_BULB).doubleValue());

            obs.setPlatformDirection(pdv.getNumber(PLATFORM_DIR).intValue());
            obs.setPlatformMovement(pdv.getNumber(PLATFORM_SPD).doubleValue());

            obs.setWindWaveHeight(pdv.getNumber(WIND_WV_HGT).doubleValue());
            obs.setWindWavePeriod(pdv.getNumber(WIND_WV_PD).intValue());

            obs.setWaveHeight(pdv.getNumber(WV_HGT).doubleValue());
            obs.setWavePeriod(pdv.getNumber(WV_PD).intValue());
            obs.setWaveSteepness(pdv.getNumber(WV_STEEPNESS).doubleValue());

            obs.setHighResWaveHeight(pdv.getNumber(HI_RES_WV_HGT).doubleValue());

            obs.setPrimarySwellWaveDir(pdv.getNumber(PRI_SWELL_WV_DIR)
                    .doubleValue());
            obs.setPrimarySwellWavePeriod(pdv.getNumber(PRI_SWELL_WV_PD)
                    .intValue());
            obs.setPrimarySwellWaveHeight(pdv.getNumber(PRI_SWELL_WV_HGT)
                    .doubleValue());

            obs.setSecondarySwellWaveDir(pdv.getNumber(SEC_SWELL_WV_DIR)
                    .doubleValue());
            obs.setSecondarySwellWavePeriod(pdv.getNumber(SEC_SWELL_WV_PD)
                    .intValue());
            obs.setSecondarySwellWaveHeight(pdv.getNumber(SEC_SWELL_WV_HGT)
                    .doubleValue());

            // TODO : Intermediate winds
        }
        return obs;
    }

    /**
     * 
     * @param container
     * @return
     */
    public static ObsCommon[] toSfcObsRecords(PointDataContainer container) {
        List<ObsCommon> records = new ArrayList<ObsCommon>();
        if (container != null) {
            container.setAllocatedSz(container.getAllocatedSz());
            for (int i = 0; i < container.getCurrentSz(); i++) {
                PointDataView pdv = container.readRandom(i);
                ObsCommon obs = toSfcObsRecord(pdv);
                if (obs != null) {
                    records.add(obs);
                }
            }
        }
        return records.toArray(new ObsCommon[records.size()]);
    }

    /**
     * 
     * @param pdv
     * @return
     */
    private static ObsCommon getObsHdr(PointDataView pdv) {
        ObsCommon obs = null;
        if (pdv != null) {
            String uri = pdv.getString(DATA_URI);
            logger.debug("URI = " + uri);
            // This sets
            // pluginName/dataTime/reportType/corIndicator/latitude/longitude
            // from the datauri
            obs = new ObsCommon(uri);

            int elev = pdv.getNumber(ELEVATION).intValue();
            SurfaceObsLocation loc = obs.getLocation();
            loc.setElevation(elev);

            long tt = pdv.getNumber(TIME_OBS).longValue();
            obs.setTimeObs(TimeTools.newCalendar(tt));

            tt = pdv.getNumber(TIME_NOMINAL).longValue();
            obs.setRefHour(TimeTools.newCalendar(tt));

            obs.setObsText(pdv.getString(RAW_REPORT));
            obs.setWmoHeader(pdv.getString(WMO_HEADER));
        }
        return obs;
    }

    /**
     * 
     * @param value
     * @return
     */
    private static int getInt(Double value) {
        int retValue = INT_DEFAULT;
        if (value != null) {
            retValue = value.intValue();
        }
        return retValue;
    }

    private static int getInt(Integer value) {
        int retValue = INT_DEFAULT;
        if (value != null) {
            retValue = value.intValue();
        }
        return retValue;
    }

    private static int getInt(Float value) {
        int retValue = INT_DEFAULT;
        if (value != null) {
            retValue = value.intValue();
        }
        return retValue;
    }

    private static float getFloat(Double value) {
        float retValue = FLOAT_DEFAULT;
        if (value != null) {
            retValue = value.floatValue();
        }
        return retValue;
    }

    private static float getFloat(Integer value) {
        float retValue = FLOAT_DEFAULT;
        if (value != null) {
            retValue = value.floatValue();
        }
        return retValue;
    }

    private static float getFloat(Float value) {
        float retValue = FLOAT_DEFAULT;
        if (value != null) {
            retValue = value.floatValue();
        }
        return retValue;
    }

}
