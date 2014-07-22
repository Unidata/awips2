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
package com.raytheon.uf.common.dataplugin.ldadmesonet;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.ldadmesonet.dao.LdadMesonetDao;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;

/**
 * Transform LDAD MESONET records into Point Data Model.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Oct 09, 2009 DR2814      vkorolev    Initial creation
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * Jul 23, 2014 3410        bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author vkorolev
 * @version 1
 */

public class LdadmesonetPointDataTransform {

    private LdadMesonetDao dao;

    private PointDataDescription description;

    // ------------------Common params (From OBS
    // plugin)----------------------------
    private static final String PRESS_CHANGE3_HOUR = "pressChange3Hour";

    private static final String PRESS_CHANGE_CHAR = "pressChangeChar";

    private static final String ALTIMETER = "altimeter";

    private static final String WIND_GUST = "windGust";

    private static final String WIND_SPEED = "windSpeed";

    private static final String DEWPOINT = "dewpoint";

    private static final String TEMPERATURE = "temperature";

    private static final String PRES_WEATHER = "presWeather";

    private static final String VISIBILITY = "visibility";

    private static final String LONGITUDE = "longitude";

    private static final String LATITUDE = "latitude";

    private static final String ELEVATION = "elevation";

    private static final String STATION_NAME = "stationName";

    private static final String DATAURI = "dataURI";

    // ------------------From LDAD mesonet netCDF------------------------
    private static final String STORAGE_TYPE = "storageType";

    private static final String STATION_ID = "stationId";

    private static final String DATA_PROVIDER = "dataProvider";

    private static final String HOME_WFO = "homeWFO";

    private static final String OBSERVATION_TIME = "observationTime";

    private static final String PROVIDER_ID = "providerId";

    private static final String HANDBOOK5_ID = "handbook5Id";

    private static final String STATION_TYPE = "stationType";

    private static final String REPORT_TIME = "reportTime";

    private static final String RECEIVED_TIME = "receivedTime";

    private static final String NUMERICAL_WMO_ID = "numericWMOid";

    private static final String DATA_PLATFORM_TYPE = "dataPlatformType";

    private static final String PLATFORM_TRUE_DIRECTION = "platformTrueDirection";

    private static final String PLARFORM_TRUE_SPEED = "platformTrueSpeed";

    private static final String TEMP_CHANGE_TIME = "tempChangeTime";

    private static final String WET_BULB_TEMPERATURE = "wetBulbTemperature";

    private static final String RH_CHANGE_TIME = "rhChangeTime";

    private static final String STATION_PRESSURE = "stationPressure";

    private static final String STATION_PRESS_CHANGE_TIME = "stationPressChangeTime";

    private static final String WIND_DIR_CHANGE_TIME = "windDirChangeTime";

    private static final String WIND_SPEED_CHANGE_TIME = "windSpeedChangeTime";

    private static final String WIND_GUST_CHANGE_TIME = "windGustChangeTime";

    private static final String WIND_DIR_MIN = "windDirMin";

    private static final String WIND_DIR_MAX = "windDirMax";

    private static final String VISIBILITY_STATUS = "visibilityStatus";

    private static final String TOTAL_CLOUD_COVER = "totalCloudCover";

    private static final String CLOUD_BASE_HEIGHT = "cloudBaseHeight";

    private static final String LOW_LEVEL_CLOUD_TYPE = "lowLevelCloudType";

    private static final String MID_LEVEL_CLOUD_TYPE = "midLevelCloudType";

    private static final String HIGH_LEVEL_CLOUD_TYPE = "highLevelCloudType";

    private static final String MAX_TEMP_RECORD_PERIOD = "maxTempRecordPeriod";

    private static final String MAXIMUM_TEMPERATURE = "maximumTemperature";

    private static final String MIN_TEMP_RECORD_PERIOD = "minTempRecordPeriod";

    private static final String MINIMUM_TEMPERATURE = "minimumTemperature";

    private static final String PRECIP_ACCUM = "precipAccum";

    private static final String PRECIP_TYPE = "precipType";

    private static final String PRECIP_INTENSITY = "precipIntensity";

    private static final String TIME_SINCE_LAST_PCP = "timeSinceLastPcp";

    private static final String SOLAR_RADIATION = "solarRadiation";

    private static final String SOLAR_RAD_CHANGE_TIME = "solarRadChangeTime";

    private static final String SEA_SURFACE_TEMP = "seaSurfaceTemp";

    private static final String WAVE_PERIOD = "wavePeriod";

    private static final String WAVE_HEIGHT = "waveHeight";

    private static final String RAW_MESONET = "rawMessage";

    private static final String REL_HUMIDITY = "relHumidity";

    private static final String WIND_DIR = "windDir";

    private static final String PRESSURE = "pressure";

    private static final String SEA_LEVEL_PRESSURE = "seaLevelPressure";

    private static final String PRECIP_RATE = "precipRate";

    private static final String FUEL_TEMPERATURE = "fuelTemperature";

    private static final String FUEL_MOISTURE = "fuelMoisture";

    private static final String SOIL_TEMPERATURE = "soilTemperature";

    private static final String SOIL_MOISTURE = "soilMoisture";

    /**
     * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! It is important to
     * keep this up to date or risk breaking backwards compatibility
     * 
     */
    private static final String[] ALL_PARAMS = { PRESS_CHANGE3_HOUR,
            PRESS_CHANGE_CHAR, ALTIMETER, WIND_GUST, WIND_SPEED, DEWPOINT,
            TEMPERATURE, PRES_WEATHER, VISIBILITY, LONGITUDE, LATITUDE,
            SEA_LEVEL_PRESSURE, STATION_NAME, DATAURI, STORAGE_TYPE, ELEVATION,
            STATION_ID, DATA_PROVIDER, HOME_WFO, OBSERVATION_TIME, PROVIDER_ID,
            HANDBOOK5_ID, STATION_TYPE, REPORT_TIME, RECEIVED_TIME,
            NUMERICAL_WMO_ID, DATA_PLATFORM_TYPE, PLATFORM_TRUE_DIRECTION,
            PLARFORM_TRUE_SPEED, TEMP_CHANGE_TIME, WET_BULB_TEMPERATURE,
            RH_CHANGE_TIME, STATION_PRESSURE, STATION_PRESS_CHANGE_TIME,
            WIND_DIR_CHANGE_TIME, WIND_SPEED_CHANGE_TIME,
            WIND_GUST_CHANGE_TIME, WIND_DIR_MIN, WIND_DIR_MAX,
            VISIBILITY_STATUS, TOTAL_CLOUD_COVER, CLOUD_BASE_HEIGHT,
            LOW_LEVEL_CLOUD_TYPE, MID_LEVEL_CLOUD_TYPE, HIGH_LEVEL_CLOUD_TYPE,
            MAX_TEMP_RECORD_PERIOD, MAXIMUM_TEMPERATURE,
            MIN_TEMP_RECORD_PERIOD, MINIMUM_TEMPERATURE, PRECIP_ACCUM,
            PRECIP_TYPE, PRECIP_INTENSITY, TIME_SINCE_LAST_PCP,
            SOLAR_RADIATION, SOLAR_RAD_CHANGE_TIME, SEA_SURFACE_TEMP,
            WAVE_PERIOD, WAVE_HEIGHT, RAW_MESONET, REL_HUMIDITY, WIND_DIR,
            PRESSURE, SEA_LEVEL_PRESSURE, PRECIP_RATE, FUEL_TEMPERATURE,
            FUEL_MOISTURE, SOIL_TEMPERATURE, SOIL_MOISTURE };

    public static final String ALL_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (String s : ALL_PARAMS) {
            if (!first) {
                sb.append(", ");
            } else {
                first = false;
            }
            sb.append(s);
        }
        ALL_PARAMS_LIST = sb.toString();
    }

    // public LdadmesonetPointDataTransform() throws JAXBException,
    // PluginException {
    // this.description = getDescription("ldadmesonet");
    // logger.info("=============PointDataDescription loaded==============");
    // this.dao = new LdadMesonetDao("ldadmesonet");
    // }

    public LdadmesonetPointDataTransform() {
        try {
            this.dao = new LdadMesonetDao("ldadmesonet");
            this.description = dao.getPointDataDescription(null);
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public PluginDataObject[] toPointData(PluginDataObject[] pdo) {

        if (pdo.length > 0) {
            Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();

            for (PluginDataObject p : pdo) {
                if (!(p instanceof MesonetLdadRecord)) {
                    continue;
                }

                File f = this.dao.getFullFilePath(p);

                PointDataContainer pdc = pointMap.get(f);
                if (pdc == null) {
                    pdc = PointDataContainer.build(this.description);
                    pointMap.put(f, pdc);
                }

                MesonetLdadRecord mesor = (MesonetLdadRecord) p;
                PointDataView pdv = buildView(pdc, mesor);
                mesor.setPointDataView(pdv);
            }
        }
        return pdo;
    }

    private PointDataView buildView(PointDataContainer container,
            MesonetLdadRecord record) {
        PointDataView pdv = container.append();

        if (record.getRawMessage() != null) {
            pdv.setString(RAW_MESONET, record.getRawMessage());
        }
        if (record.getSeaLevelPressure() != null) {
            pdv.setFloat(SEA_LEVEL_PRESSURE, record.getSeaLevelPressure());
        }

        if (record.getObservationTime() != null) {
            pdv.setLong(OBSERVATION_TIME, record.getDataTime().getRefTime()
                    .getTime());
        }
        if (record.getVisibility() != null) {
            pdv.setFloat(VISIBILITY, record.getVisibility());
        }
        if (record.getTemperature() != null) {
            pdv.setFloat(TEMPERATURE, record.getTemperature());
        }
        if (record.getDewpoint() != null) {
            pdv.setFloat(DEWPOINT, record.getDewpoint());
        }
        if (record.getWindSpeed() != null) {
            pdv.setFloat(WIND_SPEED, record.getWindSpeed());
        }
        if (record.getWindGust() != null) {
            pdv.setFloat(WIND_GUST, record.getWindGust());
        }
        if (record.getAltimeter() != null) {
            pdv.setFloat(ALTIMETER, record.getAltimeter());
        }
        if (record.getPressChangeChar() != null) {
            pdv.setInt(PRESS_CHANGE_CHAR, record.getPressChangeChar()
                    .intValue());
        }
        if (record.getPressChange3Hour() != null) {
            pdv.setFloat(PRESS_CHANGE3_HOUR, record.getPressChange3Hour());
        }

        // --------------------------------------LDAD mesonet
        // specific------------------------

        if (record.getReportTime() != null) {
            pdv.setLong(REPORT_TIME, record.getReportTime());
        }
        if (record.getReceivedTime() != null) {
            pdv.setLong(RECEIVED_TIME, record.getReceivedTime().longValue());
        }
        if (record.getNumericWMOid() != null) {
            pdv.setLong(NUMERICAL_WMO_ID, record.getNumericWMOid());
        }
        if (record.getDataPlatformType() != null) {
            pdv.setInt(DATA_PLATFORM_TYPE, record.getDataPlatformType()
                    .intValue());
        }
        if (record.getPlatformTrueDirection() != null) {
            pdv.setFloat(PLATFORM_TRUE_DIRECTION,
                    record.getPlatformTrueDirection());
        }
        if (record.getPlatformTrueSpeed() != null) {
            pdv.setFloat(PLARFORM_TRUE_SPEED, record.getPlatformTrueSpeed());
        }
        if (record.getTempChangeTime() != null) {
            pdv.setLong(TEMP_CHANGE_TIME, record.getTempChangeTime()
                    .longValue());
        }
        if (record.getWetBulbTemperature() != null) {
            pdv.setFloat(WET_BULB_TEMPERATURE, record.getWetBulbTemperature());
        }
        if (record.getRhChangeTime() != null) {
            pdv.setLong(RH_CHANGE_TIME, record.getRhChangeTime().longValue());
        }
        if (record.getStationPressure() != null) {
            pdv.setFloat(STATION_PRESSURE, record.getStationPressure());
        }
        if (record.getStationPressChangeTime() != null) {
            pdv.setLong(STATION_PRESS_CHANGE_TIME, record
                    .getStationPressChangeTime().longValue());
        }
        if (record.getWindDirChangeTime() != null) {
            pdv.setLong(WIND_DIR_CHANGE_TIME, record.getWindDirChangeTime()
                    .longValue());
        }
        if (record.getWindSpeedChangeTime() != null) {
            pdv.setLong(WIND_SPEED_CHANGE_TIME, record.getWindSpeedChangeTime()
                    .longValue());
        }
        if (record.getWindGustChangeTime() != null) {
            pdv.setLong(WIND_GUST_CHANGE_TIME, record.getWindGustChangeTime()
                    .longValue());
        }
        if (record.getWindDirMin() != null) {
            pdv.setFloat(WIND_DIR_MIN, record.getWindDirMin());
        }
        if (record.getWindDirMax() != null) {
            pdv.setFloat(WIND_DIR_MAX, record.getWindDirMax());
        }
        if (record.getVisibilityStatus() != null) {
            pdv.setString(VISIBILITY_STATUS, record.getVisibilityStatus());
        }
        if (record.getTotalCloudCover() != null) {
            pdv.setFloat(TOTAL_CLOUD_COVER, record.getTotalCloudCover());
        }
        if (record.getCloudBaseHeight() != null) {
            pdv.setInt(CLOUD_BASE_HEIGHT, record.getCloudBaseHeight()
                    .intValue());
        }
        if (record.getLowLevelCloudType() != null) {
            pdv.setInt(LOW_LEVEL_CLOUD_TYPE, record.getLowLevelCloudType()
                    .intValue());
        }
        if (record.getMidLevelCloudType() != null) {
            pdv.setInt(MID_LEVEL_CLOUD_TYPE, record.getMidLevelCloudType()
                    .intValue());
        }
        if (record.getHighLevelCloudType() != null) {
            pdv.setInt(HIGH_LEVEL_CLOUD_TYPE, record.getHighLevelCloudType()
                    .intValue());
        }
        if (record.getMinTempRecordPeriod() != null) {
            pdv.setInt(MAX_TEMP_RECORD_PERIOD, record.getMinTempRecordPeriod()
                    .intValue());
        }
        if (record.getMaximumTemperature() != null) {
            pdv.setFloat(MAXIMUM_TEMPERATURE, record.getMaximumTemperature());
        }
        if (record.getMinTempRecordPeriod() != null) {
            pdv.setInt(MIN_TEMP_RECORD_PERIOD, record.getMinTempRecordPeriod()
                    .intValue());
        }
        if (record.getMinimumTemperature() != null) {
            pdv.setFloat(MINIMUM_TEMPERATURE, record.getMinimumTemperature());
        }
        if (record.getPrecipAccum() != null) {
            pdv.setFloat(PRECIP_ACCUM, record.getPrecipAccum());
        }
        if (record.getPrecipType() != null) {
            pdv.setInt(PRECIP_TYPE, record.getPrecipType().intValue());
        }
        if (record.getPrecipIntensity() != null) {
            pdv.setInt(PRECIP_INTENSITY, record.getPrecipIntensity().intValue());
        }
        if (record.getTimeSinceLastPcp() != null) {
            pdv.setLong(TIME_SINCE_LAST_PCP, record.getTimeSinceLastPcp()
                    .longValue());
        }
        if (record.getSolarRadiation() != null) {
            pdv.setFloat(SOLAR_RADIATION, record.getSolarRadiation());
        }
        if (record.getSolarRadChangeTime() != null) {
            pdv.setLong(SOLAR_RAD_CHANGE_TIME, record.getSolarRadChangeTime()
                    .longValue());
        }
        if (record.getSeaSurfaceTemp() != null) {
            pdv.setFloat(SEA_SURFACE_TEMP, record.getSeaSurfaceTemp());
        }
        if (record.getWavePeriod() != null) {
            pdv.setFloat(WAVE_PERIOD, record.getWavePeriod());
        }
        if (record.getWaveHeight() != null) {
            pdv.setFloat(WAVE_HEIGHT, record.getWaveHeight());
        }
        if (record.getRelHumidity() != null) {
            pdv.setFloat(REL_HUMIDITY, record.getRelHumidity());
        }
        if (record.getWindDir() != null) {
            pdv.setFloat(WIND_DIR, record.getWindDir());
        }
        if (record.getPressure() != null) {
            pdv.setFloat(PRESSURE, record.getPressure());
        }
        if (record.getSeaLevelPressure() != null) {
            pdv.setFloat(SEA_LEVEL_PRESSURE, record.getSeaLevelPressure());
        }
        if (record.getPrecipRate() != null) {
            pdv.setFloat(PRECIP_RATE, record.getPrecipRate());
        }
        if (record.getFuelTemperature() != null) {
            pdv.setFloat(FUEL_TEMPERATURE, record.getFuelTemperature());
        }
        if (record.getFuelMoisture() != null) {
            pdv.setFloat(FUEL_MOISTURE, record.getFuelMoisture());
        }
        if (record.getSoilTemperature() != null) {
            pdv.setFloat(SOIL_TEMPERATURE, record.getSoilTemperature());
        }
        if (record.getSoilMoisture() != null) {
            pdv.setFloat(SOIL_MOISTURE, record.getSoilMoisture());
        }
        return pdv;
    }

    public static MesonetLdadRecord toMesonetLdadRecord(PointDataView pdv) {
        MesonetLdadRecord mr = new MesonetLdadRecord();
        mr.setAltimeter(pdv.getNumber(ALTIMETER).floatValue());
        mr.setDewpoint(pdv.getNumber(DEWPOINT).floatValue());
        SurfaceObsLocation loc = new SurfaceObsLocation(
                pdv.getString(STATION_ID));
        float lat = pdv.getNumber(LATITUDE).floatValue();
        float lon = pdv.getNumber(LONGITUDE).floatValue();
        loc.assignLocation(lat, lon);
        loc.setElevation(pdv.getNumber(ELEVATION).intValue());
        mr.setLocation(loc);
        mr.setReportType(pdv.getString(STORAGE_TYPE));
        mr.setProviderId(pdv.getString(PROVIDER_ID));
        mr.setPressChange3Hour(pdv.getNumber(PRESS_CHANGE3_HOUR).floatValue());
        mr.setPressChangeChar((short) pdv.getInt(PRESS_CHANGE_CHAR));
        mr.setSeaLevelPressure(pdv.getNumber(SEA_LEVEL_PRESSURE).floatValue());
        mr.setTemperature(pdv.getNumber(TEMPERATURE).floatValue());
        mr.setVisibility(pdv.getNumber(VISIBILITY).floatValue());
        mr.setWindDir(pdv.getFloat(WIND_DIR));
        mr.setWindGust(pdv.getFloat(WIND_GUST));
        mr.setWindSpeed(pdv.getFloat(WIND_SPEED));
        mr.setDataURI(pdv.getString(DATAURI));
        mr.setReceivedTime(pdv.getNumber(RECEIVED_TIME).doubleValue());
        // ----------------------------------------------------------------------------------
        mr.setTempChangeTime(pdv.getNumber(TEMP_CHANGE_TIME).doubleValue());
        mr.setWetBulbTemperature(pdv.getFloat(WET_BULB_TEMPERATURE));
        mr.setRhChangeTime((Double) pdv.getNumber(RH_CHANGE_TIME));
        mr.setStationPressure(pdv.getFloat(STATION_PRESSURE));
        mr.setStationPressChangeTime((Double) pdv
                .getNumber(STATION_PRESS_CHANGE_TIME));
        mr.setWindDirChangeTime((Double) pdv.getNumber(WIND_DIR_CHANGE_TIME));
        mr.setWindSpeedChangeTime((Double) pdv
                .getNumber(WIND_SPEED_CHANGE_TIME));
        mr.setWindGustChangeTime((Double) pdv.getNumber(WIND_GUST_CHANGE_TIME));
        mr.setWindDirMin(pdv.getFloat(WIND_DIR_MIN));
        mr.setWindDirMax(pdv.getFloat(WIND_DIR_MAX));
        mr.setVisibilityStatus(pdv.getString(VISIBILITY_STATUS));
        mr.setTotalCloudCover(pdv.getFloat(TOTAL_CLOUD_COVER));
        mr.setCloudBaseHeight((Short) pdv.getNumber(CLOUD_BASE_HEIGHT));
        mr.setLowLevelCloudType((Short) pdv.getNumber(LOW_LEVEL_CLOUD_TYPE));
        mr.setMidLevelCloudType((Short) pdv.getNumber(MID_LEVEL_CLOUD_TYPE));
        mr.setHighLevelCloudType((Short) pdv.getNumber(HIGH_LEVEL_CLOUD_TYPE));
        mr.setMaxTempRecordPeriod((Short) pdv.getNumber(MAX_TEMP_RECORD_PERIOD));
        mr.setMaximumTemperature(pdv.getFloat(MAXIMUM_TEMPERATURE));
        mr.setMinTempRecordPeriod((Short) pdv.getNumber(MIN_TEMP_RECORD_PERIOD));
        mr.setMaximumTemperature(pdv.getFloat(MINIMUM_TEMPERATURE));
        mr.setPrecipAccum(pdv.getFloat(PRECIP_ACCUM));
        mr.setPrecipType((Short) pdv.getNumber(PRECIP_TYPE));
        mr.setPrecipIntensity((Short) pdv.getNumber(PRECIP_INTENSITY));
        mr.setTimeSinceLastPcp((Double) pdv.getNumber(TIME_SINCE_LAST_PCP));
        mr.setSolarRadiation(pdv.getFloat(SOLAR_RADIATION));
        mr.setSolarRadChangeTime((Double) pdv.getNumber(SOLAR_RAD_CHANGE_TIME));
        mr.setSeaSurfaceTemp(pdv.getFloat(SEA_SURFACE_TEMP));
        mr.setWavePeriod(pdv.getFloat(WAVE_PERIOD));
        mr.setWaveHeight(pdv.getFloat(WAVE_HEIGHT));
        mr.setRawMessage(pdv.getString(RAW_MESONET));
        mr.setRelHumidity(pdv.getFloat(REL_HUMIDITY));
        mr.setWindDir(pdv.getFloat(WIND_DIR));
        mr.setPressure(pdv.getFloat(PRESSURE));
        mr.setSeaLevelPressure(pdv.getFloat(SEA_LEVEL_PRESSURE));
        mr.setPrecipRate(pdv.getFloat(PRECIP_RATE));
        mr.setFuelTemperature(pdv.getFloat(FUEL_TEMPERATURE));
        mr.setFuelMoisture(pdv.getFloat(FUEL_MOISTURE));
        mr.setSoilTemperature(pdv.getFloat(SOIL_TEMPERATURE));
        mr.setSoilMoisture(pdv.getFloat(SOIL_MOISTURE));
        return mr;
    }

    public static MesonetLdadRecord[] toMesonetLdadRecords(
            PointDataContainer container) {
        List<MesonetLdadRecord> records = new ArrayList<MesonetLdadRecord>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
            PointDataView pdv = container.readRandom(i);
            records.add(toMesonetLdadRecord(pdv));
        }
        return records.toArray(new MesonetLdadRecord[records.size()]);

    }

}
