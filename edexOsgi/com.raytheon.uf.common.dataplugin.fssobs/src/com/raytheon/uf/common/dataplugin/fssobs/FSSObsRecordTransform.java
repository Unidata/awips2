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
package com.raytheon.uf.common.dataplugin.fssobs;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Provides a transform from PointDataContainer to FSSObsRecord.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 8, 2011            skorolev     Initial creation
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */

public class FSSObsRecordTransform {
    private static final String DATA_URI = "dataURI";

    private static final String LONGITUDE = "longitude";

    private static final String LATITUDE = "latitude";

    private static final String ELEVATION = "elevation";

    private static final String CEILING = "ceiling";

    private static final String DEWPOINT = "dewpoint";

    private static final String DEWPOINT_DEPR = "dewpointDepr";

    private static final String FROSTBITE_TIME = "frostbiteTime";

    private static final String HORIZONTAL_VIS = "horzVisibility";

    private static final String HOURLY_PRECIP = "hourlyPrecip";

    private static final String MAX_WIND_SPEED = "maxWindSpeed";

    private static final String PLATFORM_ID = "platformId";

    private static final String PRES_WEATHER = "presWeather";

    private static final String PRESS_CHANGE3_HOUR = "pressChange3Hour";

    private static final String PRESS_CHANGE_CHAR = "pressChangeChar";

    private static final String PRESS_ALTIMETER = "pressureAltimeter";

    private static final String PRI_SWELL_WV_DIR = "primarySwellWaveDir";

    private static final String PRI_SWELL_WV_HGT = "primarySwellWaveHeight";

    private static final String PRI_SWELL_WV_PD = "primarySwellWavePeriod";

    private static final String RAW_MESSAGE = "rawMessage";

    private static final String REL_HUMIDITY = "relativeHumidity";

    private static final String SEA_LEVEL_PRESS = "seaLevelPress";

    private static final String SEA_SFC_TEMP = "seaSurfaceTemp";

    private static final String SEC_SWELL_WV_DIR = "secondarySwellWaveDir";

    private static final String SEC_SWELL_WV_HGT = "secondarySwellWaveHeight";

    private static final String SEC_SWELL_WV_PD = "secondarySwellWavePeriod";

    private static final String SKY_COVER = "skyCover";

    private static final String SNOW_INC_HOURLY = "snincrHourly";

    private static final String SNOW_INC_TOTAL = "snincrTotal";

    private static final String SNOW_DEPTH = "snowDepth";

    private static final String STATION_NAME = "stnName";

    private static final String TEMPERATURE = "temperature";

    private static final String TIME_OBS = "timeObs";

    private static final String REF_HOUR = "refHour";

    private static final String CLOUD_AMOUNT_TOT = "totCloudAmount";

    private static final String VISIBILITY = "visibility";

    private static final String WAVE_HEIGHT = "highResWaveHeight";

    private static final String WV_HGT = "waveHeight";

    private static final String WV_PD = "wavePeriod";

    private static final String WV_STEEPNESS = "waveSteepness";

    private static final String WIND_DIR = "windDir";

    private static final String WIND_GUST = "windGust";

    private static final String WIND_SPEED = "windSpeed";

    public static final String[] FSSOBS_PARAMS = { DATA_URI, LONGITUDE,
            LATITUDE, ELEVATION, CEILING, DEWPOINT, DEWPOINT_DEPR,
            FROSTBITE_TIME, HORIZONTAL_VIS, HOURLY_PRECIP, MAX_WIND_SPEED,
            PLATFORM_ID, PRES_WEATHER, PRESS_CHANGE3_HOUR, PRESS_CHANGE_CHAR,
            PRESS_ALTIMETER, PRI_SWELL_WV_DIR, PRI_SWELL_WV_HGT,
            PRI_SWELL_WV_PD, RAW_MESSAGE, REL_HUMIDITY, SEA_LEVEL_PRESS,
            SEA_SFC_TEMP, SEC_SWELL_WV_DIR, SEC_SWELL_WV_HGT, SEC_SWELL_WV_PD,
            SKY_COVER, SNOW_INC_HOURLY, SNOW_INC_TOTAL, SNOW_DEPTH,
            STATION_NAME, TEMPERATURE, TIME_OBS, REF_HOUR, CLOUD_AMOUNT_TOT,
            VISIBILITY, WAVE_HEIGHT, WV_HGT, WV_PD, WV_STEEPNESS, WIND_DIR,
            WIND_GUST, WIND_SPEED };

    public static final String FSSOBS_PARAMS_LIST;

    static {
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (String s : FSSOBS_PARAMS) {
            if (!first) {
                sb.append(", ");
            } else {
                first = false;
            }
            sb.append(s);
        }
        FSSOBS_PARAMS_LIST = sb.toString();
    }

    /**
     * 
     * @param container
     * @return
     */
    public static FSSObsRecord[] toFSSObsRecords(PointDataContainer container) {
        List<FSSObsRecord> records = new ArrayList<FSSObsRecord>();
        if (container != null) {
            container.setAllocatedSz(container.getAllocatedSz());
            for (int i = 0; i < container.getCurrentSz(); i++) {
                PointDataView pdv = container.readRandom(i);
                FSSObsRecord obs = toFSSObsRecord(pdv);
                if (obs != null) {
                    records.add(obs);
                }
            }
        }
        return records.toArray(new FSSObsRecord[records.size()]);
    }

    private static FSSObsRecord toFSSObsRecord(PointDataView pdv) {
        FSSObsRecord obs = null;
        if (pdv != null) {
            // String uri = pdv.getString(DATA_URI);
            obs = new FSSObsRecord();
            obs.setDataURI(pdv.getString(DATA_URI));
            SurfaceObsLocation loc = new SurfaceObsLocation(
                    pdv.getString(PLATFORM_ID));
            float lat = pdv.getNumber(LATITUDE).floatValue();
            float lon = pdv.getNumber(LONGITUDE).floatValue();
            loc.assignLocation(lat, lon);
            loc.setElevation(pdv.getNumber(ELEVATION).intValue());
            obs.setLocation(loc);
            obs.setCeiling(pdv.getFloat(CEILING));
            obs.setDewpoint(pdv.getNumber(DEWPOINT).floatValue());
            obs.setDewpointDepr(pdv.getFloat(DEWPOINT_DEPR));
            obs.setFrostbiteTime(pdv.getFloat(FROSTBITE_TIME));
            obs.setHorzVisibility(pdv.getFloat(HORIZONTAL_VIS));
            obs.setHourlyPrecip(pdv.getFloat(HOURLY_PRECIP));
            obs.setMaxWindSpeed(pdv.getFloat(MAX_WIND_SPEED));
            obs.setPlatformId(pdv.getString(PLATFORM_ID));
            obs.setPresWeather(pdv.getStringAllLevels(PRES_WEATHER));
            obs.setPressChange3Hour(pdv.getNumber(PRESS_CHANGE3_HOUR)
                    .floatValue());
            obs.setPressChangeChar(pdv.getString(PRESS_CHANGE_CHAR));
            obs.setPressureAltimeter(pdv.getFloat(PRESS_ALTIMETER));
            obs.setPrimarySwellWaveDir(pdv.getNumber(PRI_SWELL_WV_DIR)
                    .doubleValue());
            obs.setPrimarySwellWaveHeight(pdv.getNumber(PRI_SWELL_WV_HGT)
                    .doubleValue());
            obs.setPrimarySwellWavePeriod(pdv.getNumber(PRI_SWELL_WV_PD)
                    .intValue());
            obs.setRawMessage(pdv.getString(RAW_MESSAGE));
            obs.setRelativeHumidity(pdv.getFloat(REL_HUMIDITY));
            obs.setSeaLevelPress(pdv.getFloat(SEA_LEVEL_PRESS));
            obs.setSeaSurfaceTemp(pdv.getFloat(SEA_SFC_TEMP));
            obs.setSecondarySwellWaveDir(pdv.getNumber(SEC_SWELL_WV_DIR)
                    .doubleValue());
            obs.setSecondarySwellWaveHeight(pdv.getNumber(SEC_SWELL_WV_HGT)
                    .doubleValue());
            obs.setSecondarySwellWavePeriod(pdv.getNumber(SEC_SWELL_WV_PD)
                    .intValue());
            obs.setSkyCover(pdv.getStringAllLevels(SKY_COVER));
            obs.setSnincrHourly(pdv.getFloat(SNOW_INC_HOURLY));
            obs.setSnincrTotal(pdv.getFloat(SNOW_INC_TOTAL));
            obs.setSnowDepth(pdv.getFloat(SNOW_DEPTH));
            obs.setStnName(pdv.getString(STATION_NAME));
            obs.setTemperature(pdv.getFloat(TEMPERATURE));
            long to = pdv.getLong(TIME_OBS);
            obs.setTimeObs(TimeUtil.newGmtCalendar(new Date(to)));
            long rh = pdv.getLong(REF_HOUR);
            obs.setRefHour(TimeUtil.newGmtCalendar(new Date(rh)));
            obs.setTotCloudAmount(pdv.getNumber(CLOUD_AMOUNT_TOT).intValue());
            obs.setVisibility(pdv.getNumber(VISIBILITY).floatValue());
            obs.setHighResWaveHeight(pdv.getFloat(WAVE_HEIGHT));
            obs.setWaveHeight(pdv.getNumber(WV_HGT).doubleValue());
            obs.setWavePeriod(pdv.getNumber(WV_PD).intValue());
            obs.setWindDir(pdv.getNumber(WIND_DIR).floatValue());
            obs.setWindGust(pdv.getNumber(WIND_GUST).floatValue());
            obs.setWindSpeed(pdv.getNumber(WIND_SPEED).floatValue());

        }
        return obs;
    }
}
