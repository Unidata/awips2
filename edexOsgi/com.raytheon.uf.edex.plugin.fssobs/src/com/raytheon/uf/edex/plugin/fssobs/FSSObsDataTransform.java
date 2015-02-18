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
package com.raytheon.uf.edex.plugin.fssobs;

import java.util.Date;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
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
 * Dec 3, 2010            skorolev     Initial creation
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */

public class FSSObsDataTransform {

	private FSSObsDAO dao;

	private static PointDataDescription fsspdd;

	public static PointDataContainer pdc;

	public FSSObsDataTransform() throws JAXBException, PluginException {
		this.dao = new FSSObsDAO("fssobs");
		fsspdd = dao.getPointDataDescription(null);
		this.setPdc(PointDataContainer.build(fsspdd));
	}

	public static final float MISSING = -9999.0f;

	// ------------------ params in the FSSObsRecord----------------------------

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
	private static final String WV_HGT = "waveHeight";
	private static final String WV_PD = "wavePeriod";
	private static final String WV_STEEPNESS = "waveSteepness";
	private static final String WIND_DIR = "windDir";
	private static final String WIND_GUST = "windGust";
	private static final String WIND_SPEED = "windSpeed";

	// ------------------Common params (From OBS
	// plugin)----------------------------
	private static final String ALTIMETER = "altimeter";

	// private static final String WIND_DIR_STR = "windDirStr";

	// private static final String DP_FROM_TENTHS = "dpFromTenths";

	// private static final String TEMP_FROM_TENTHS = "tempFromTenths";

	private static final String VERT_VISIBILITY = "vertVisibility";

	private static final String LONGITUDE = "longitude";

	private static final String LATITUDE = "latitude";

	private static final String ELEVATION = "elevation";

	private static final String STATION_ID = "stationId";

	private static final String REPORT_TYPE = "reportType";

	private static final String RAW_METAR = "rawMETAR";

	private static final String SKY_COVER_TYPE = "skyCoverType";

	private static final String SKY_LAYER_BASE = "skyLayerBase";

	// ---------------From SFCOBS -------------------------------------

	private static final String RAW_REPORT = "rawReport";

	private static final String HI_RES_WV_HGT = "highResWaveHeight";

	private static final String PEAK_WIND_SPEED = "peakWindSpeed";

	private static final String PRECIP1_HOUR = "precip1Hour";

	public static final String OBS_PARAMS_LIST;

	// ----------------From LDADMESOWEST-------------------------------

	private static final String STORAGE_TYPE = "storageType";

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

	private static final String PRESSURE = "pressure";

	private static final String SEA_LEVEL_PRESSURE = "seaLevelPressure";

	private static final String PRECIP_RATE = "precipRate";

	private static final String FUEL_TEMPERATURE = "fuelTemperature";

	private static final String FUEL_MOISTURE = "fuelMoisture";

	private static final String SOIL_TEMPERATURE = "soilTemperature";

	private static final String SOIL_MOISTURE = "soilMoisture";

	static {
		StringBuffer sb = new StringBuffer();
		sb.append("altimeter,");
		sb.append("windGust,");
		sb.append("windSpeed,");
		sb.append("windDir,");
		sb.append("dewpoint,");
		sb.append("temperature,");
		sb.append("precip1Hour,");
		sb.append("seaLevelPress,");
		sb.append("pressChange3Hour,");
		sb.append("pressChangeChar,");
		sb.append("visibility,");
		sb.append("vertVisibility,");
		sb.append("longitude,");
		sb.append("latitude,");
		sb.append("elevation,");
		sb.append("stationId,");
		sb.append("reportType,");
		sb.append("skyCover,");
		sb.append("skyCoverType,");
		sb.append("skyLayerBase,");
		sb.append("presWeather,");
		sb.append("rawMETAR,");
		sb.append("timeObs ");

		OBS_PARAMS_LIST = sb.toString();
	}

	public static FSSObsRecord fromMetarRecord(PointDataContainer container)
			throws JAXBException {
		container.setCurrentSz(container.getAllocatedSz());
		PointDataView pdv = container.readRandom(0);
		FSSObsRecord fssr = new FSSObsRecord();
		fssr.setReportType(pdv.getString(REPORT_TYPE));
		SurfaceObsLocation loc = new SurfaceObsLocation();
		loc.setStationId(pdv.getString(STATION_ID));
        float lat = pdv.getNumber(LATITUDE).floatValue();
        float lon = pdv.getNumber(LONGITUDE).floatValue();
		loc.assignLocation(lat, lon);
		loc.setElevation(pdv.getNumber(ELEVATION).intValue());
		fssr.setLocation(loc);
		String stId = FSSObsUtils.getStationName(loc.getStationId());
		fssr.setPlatformId(loc.getStationId());
		fssr.setStnName(stId);
		fssr.setRawMessage(pdv.getString(RAW_METAR));
		fssr.setDataTime(new DataTime(new Date(pdv.getNumber(TIME_OBS)
				.longValue())));
		long to = pdv.getLong(TIME_OBS);
        fssr.setTimeObs(TimeUtil.newGmtCalendar(new Date(to)));
		// nominalTime
		fssr.setRefHour(TimeTools.roundToNearestHour(fssr.getTimeObs()));
		// in mbar
		fssr.setSeaLevelPress(pdv.getNumber(SEA_LEVEL_PRESS).floatValue());
		// in mmHg
		fssr.setPressureAltimeter(pdv.getNumber(ALTIMETER).floatValue());
		// Double pa =
		// DecoderTools.inToPa(pdv.getNumber(ALTIMETER).doubleValue());
		// fssr.setPressureAltimeter(pa.floatValue());
		fssr.setPressChange3Hour(pdv.getNumber(PRESS_CHANGE3_HOUR).floatValue());
		fssr.setPressChangeChar(pdv.getString(PRESS_CHANGE_CHAR));
		// in Fahrenheit
		if (pdv.getFloat(DEWPOINT) != MISSING) {
			fssr.setDewpoint(1.8f * pdv.getFloat(DEWPOINT) + 32.0f);
		}
		if (pdv.getFloat(TEMPERATURE) != MISSING) {
			fssr.setTemperature(1.8f * pdv.getFloat(TEMPERATURE) + 32.0f);
		}
		// in meters
		fssr.setVisibility(pdv.getNumber(VISIBILITY).floatValue());
		Number[] levels = pdv.getNumberAllLevels(SKY_LAYER_BASE);
		String[] skyCov = pdv.getStringAllLevels(SKY_COVER_TYPE);
		if (pdv.getNumber(VERT_VISIBILITY).floatValue() >= 0
				&& pdv.getNumber(VERT_VISIBILITY).floatValue() < 1e20f
				&& pdv.getNumber(VERT_VISIBILITY).floatValue() != MISSING) {
			fssr.setCeiling(pdv.getNumber(VERT_VISIBILITY).floatValue() / 100f);
		} else {
			// in feet
			float ceiling = FSSObsUtils.findMetarCeilingFromLayers(skyCov,
					levels);
			fssr.setCeiling(ceiling);
		}
		fssr.setSkyCover(pdv.getStringAllLevels(SKY_COVER));
		// in inch
		fssr.setHourlyPrecip(pdv.getFloat(PRECIP1_HOUR));

		// fssr.setWindDir(Float.valueOf(pdv.getString(WIND_DIR_STR)));
		fssr.setWindDir(pdv.getFloat(WIND_DIR));
		// in knotes
		if (pdv.getNumber(WIND_GUST).floatValue() != MISSING) {
			fssr.setWindGust(pdv.getNumber(WIND_GUST).floatValue());
		}
		if (pdv.getNumber(WIND_SPEED).floatValue() != MISSING) {
			fssr.setWindSpeed(pdv.getNumber(WIND_SPEED).floatValue());
		}

		fssr.setPresWeather(pdv.getStringAllLevels(PRES_WEATHER));

		return fssr;
	}

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
		sb.append("rawReport,");
		sb.append("wmoHeader,");

		HDR_PARAMS_LIST = sb.toString();
	}

	public static String SFCOBS_PARAMS_LIST = null;
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
		sb.append("pressChangeChar,");
		sb.append("pressChange3Hour,");

		sb.append("visibility,");
		sb.append("presWeather,");

		sb.append("totCloudAmount,");
		sb.append("precip1Hour,");
		sb.append("seaSurfaceTemp,");
		sb.append("waveHeight,");
		sb.append("wavePeriod,");
		sb.append("waveSteepness,");

		sb.append("highResWaveHeight,");

		sb.append("primarySwellWaveDir,");
		sb.append("primarySwellWavePeriod,");
		sb.append("primarySwellWaveHeight,");

		sb.append("secondarySwellWaveDir,");
		sb.append("secondarySwellWavePeriod,");
		sb.append("secondarySwellWaveHeight ");

		SFCOBS_PARAMS_LIST = sb.toString();
	}

	private static final String[] MESOWEST_PARAMS = { PRESS_CHANGE3_HOUR,
			PRESS_CHANGE_CHAR, ALTIMETER, WIND_GUST, WIND_SPEED, DEWPOINT,
			TEMPERATURE, PRES_WEATHER, VISIBILITY, LONGITUDE, LATITUDE,
			STATION_NAME, STORAGE_TYPE, ELEVATION, STATION_ID, DATA_PROVIDER,
			HOME_WFO, OBSERVATION_TIME, PROVIDER_ID, HANDBOOK5_ID,
			STATION_TYPE, REPORT_TIME, RECEIVED_TIME, NUMERICAL_WMO_ID,
			DATA_PLATFORM_TYPE, PLATFORM_TRUE_DIRECTION, PLARFORM_TRUE_SPEED,
			TEMP_CHANGE_TIME, WET_BULB_TEMPERATURE, RH_CHANGE_TIME,
			STATION_PRESSURE, STATION_PRESS_CHANGE_TIME, WIND_DIR_CHANGE_TIME,
			WIND_SPEED_CHANGE_TIME, WIND_GUST_CHANGE_TIME, WIND_DIR_MIN,
			WIND_DIR_MAX, VISIBILITY_STATUS, TOTAL_CLOUD_COVER,
			CLOUD_BASE_HEIGHT, LOW_LEVEL_CLOUD_TYPE, MID_LEVEL_CLOUD_TYPE,
			HIGH_LEVEL_CLOUD_TYPE, MAX_TEMP_RECORD_PERIOD, MAXIMUM_TEMPERATURE,
			MIN_TEMP_RECORD_PERIOD, MINIMUM_TEMPERATURE, PRECIP_ACCUM,
			PRECIP_TYPE, PRECIP_INTENSITY, TIME_SINCE_LAST_PCP,
			SOLAR_RADIATION, SOLAR_RAD_CHANGE_TIME, SEA_SURFACE_TEMP,
			WAVE_PERIOD, WAVE_HEIGHT, RAW_MESONET, "relHumidity", WIND_DIR,
			PRESSURE, SEA_LEVEL_PRESSURE, PRECIP_RATE, FUEL_TEMPERATURE,
			FUEL_MOISTURE, SOIL_TEMPERATURE, SOIL_MOISTURE, REPORT_TYPE };

	public static final String MESOWEST_PARAMS_LIST;
	static {
		StringBuffer sb = new StringBuffer();
		boolean first = true;
		for (String s : MESOWEST_PARAMS) {
			if (!first) {
				sb.append(", ");
			} else {
				first = false;
			}
			sb.append(s);
		}
		MESOWEST_PARAMS_LIST = sb.toString();
	}

	public static FSSObsRecord fromMaritimeRecord(PointDataContainer container)
			throws JAXBException {
		container.setCurrentSz(container.getAllocatedSz());
		PointDataView pdv = container.readRandom(0);

		FSSObsRecord fssr = new FSSObsRecord();
		SurfaceObsLocation loc = new SurfaceObsLocation();
		loc.setStationId(pdv.getString(STATION_ID));
        float lat = pdv.getNumber(LATITUDE).floatValue();
        float lon = pdv.getNumber(LONGITUDE).floatValue();
		loc.assignLocation(lat, lon);
		loc.setElevation(pdv.getNumber(ELEVATION).intValue());
		fssr.setLocation(loc);
		String stId = FSSObsUtils.getStationName(loc.getStationId());
		fssr.setPlatformId(loc.getStationId());
		fssr.setStnName(stId);

		fssr.setRawMessage(pdv.getString(RAW_REPORT));
		fssr.setDataTime(new DataTime(new Date(pdv.getNumber(TIME_OBS)
				.longValue())));
		long to = pdv.getLong(TIME_OBS);
        fssr.setTimeObs(TimeUtil.newGmtCalendar(new Date(to)));
		// TODO: check nominalTime
		fssr.setRefHour(TimeTools.roundToNearestHour(fssr.getTimeObs()));

		// in mbar
		if (pdv.getFloat(SEA_LEVEL_PRESS) != MISSING) {
			fssr.setSeaLevelPress(pdv.getFloat(SEA_LEVEL_PRESS) / 100);
		}
		fssr.setPressureAltimeter(pdv.getNumber(ALTIMETER).floatValue());
		// Double pa =
		// DecoderTools.inToPa(pdv.getNumber(ALTIMETER).doubleValue());
		// if (pdv.getNumber(ALTIMETER).doubleValue() != MISSING) {
		// fssr.setPressure(pa.floatValue());
		// }
		fssr.setPressChange3Hour(pdv.getFloat(PRESS_CHANGE3_HOUR));
		fssr.setPressChangeChar(String.valueOf(pdv.getInt(PRESS_CHANGE_CHAR)));

		// http://www.hpc.ncep.noaa.gov/html/tempconversion.shtml
		// in Fahrenheit
		if (pdv.getFloat(DEWPOINT) != MISSING) {
			fssr.setDewpoint(1.8f * (pdv.getFloat(DEWPOINT) - 273.15f) + 32.0f);
		}
		if (pdv.getFloat(TEMPERATURE) != MISSING) {
			fssr.setTemperature(1.8f * (pdv.getFloat(TEMPERATURE) - 273.15f) + 32.0f);
		}
		if (pdv.getNumber(SEA_SFC_TEMP).floatValue() != MISSING) {
			fssr.setSeaSurfaceTemp(1.8f * (pdv.getNumber(SEA_SFC_TEMP)
					.floatValue() - 273.15f) + 32.0f);
		}
		// in nautical miles
		fssr.setHorzVisibility(pdv.getNumber(VISIBILITY).floatValue());

		fssr.setWindDir(pdv.getNumber(WIND_DIR).floatValue());
		// in knotes
		if (pdv.getNumber(WIND_GUST).floatValue() != MISSING) {
			fssr.setWindGust(pdv.getNumber(WIND_GUST).floatValue());
		}
		if (pdv.getNumber(WIND_SPEED).floatValue() != MISSING) {
			fssr.setWindSpeed(pdv.getNumber(WIND_SPEED).floatValue());
		}
		if (pdv.getFloat(PEAK_WIND_SPEED) != MISSING) {
			fssr.setMaxWindSpeed(pdv.getFloat(PEAK_WIND_SPEED));
		}
		fssr.setReportType(String.valueOf(pdv.getInt(REPORT_TYPE)));
		String[] pw = new String[25];
		pw[0] = pdv.getString(PRES_WEATHER);
		fssr.setPresWeather(pw);

		fssr.setHighResWaveHeight(pdv.getFloat(HI_RES_WV_HGT));
		fssr.setHourlyPrecip(pdv.getFloat(PRECIP1_HOUR));

		fssr.setPrimarySwellWaveDir(pdv.getNumber(PRI_SWELL_WV_DIR)
				.doubleValue());
		fssr.setPrimarySwellWaveHeight(pdv.getNumber(PRI_SWELL_WV_HGT)
				.doubleValue());
		fssr.setPrimarySwellWavePeriod(pdv.getNumber(PRI_SWELL_WV_PD)
				.intValue());
		fssr.setSecondarySwellWaveDir(pdv.getNumber(SEC_SWELL_WV_DIR)
				.doubleValue());
		fssr.setSecondarySwellWaveHeight(pdv.getNumber(SEC_SWELL_WV_HGT)
				.doubleValue());
		fssr.setSecondarySwellWavePeriod(pdv.getNumber(SEC_SWELL_WV_PD)
				.intValue());
		fssr.setWaveHeight(pdv.getNumber(WV_HGT).doubleValue());
		fssr.setWavePeriod(pdv.getNumber(WV_PD).intValue());
		fssr.setWaveSteepness(pdv.getNumber(WV_STEEPNESS).floatValue());
		fssr.setTotCloudAmount(pdv.getInt(CLOUD_AMOUNT_TOT));

		return fssr;
		// TODO : Intermediate winds
	}

	/**
	 * @param result
	 * @return
	 */
	public static FSSObsRecord fromLdadmesowestRecord(
			PointDataContainer container) throws JAXBException {
		container.setCurrentSz(container.getAllocatedSz());
		PointDataView pdv = container.readRandom(0);
		FSSObsRecord fssr = new FSSObsRecord();
		fssr.setReportType(pdv.getString(REPORT_TYPE));
		SurfaceObsLocation loc = new SurfaceObsLocation();
		loc.setStationId(pdv.getString(STATION_ID));
        float lat = pdv.getNumber(LATITUDE).floatValue();
        float lon = pdv.getNumber(LONGITUDE).floatValue();
		loc.assignLocation(lat, lon);
		loc.setElevation(pdv.getNumber(ELEVATION).intValue());
		fssr.setLocation(loc);
		String stId = FSSObsUtils.getStationName(loc.getStationId());
		fssr.setPlatformId(loc.getStationId());
		fssr.setStnName(stId + pdv.getString(DATA_PROVIDER));
		fssr.setRawMessage(pdv.getString(RAW_MESSAGE));
		fssr.setDataTime(new DataTime(new Date(pdv.getNumber(OBSERVATION_TIME)
				.longValue())));
		long to = pdv.getLong(OBSERVATION_TIME);
        fssr.setTimeObs(TimeUtil.newGmtCalendar(new Date(to)));
		fssr.setRefHour(TimeTools.roundToNearestHour(fssr.getTimeObs()));
		// fssr.setCeiling(pdv.getNumber(CLOUD_BASE_HEIGHT).floatValue());
		if (pdv.getFloat(DEWPOINT) != MISSING) {
			fssr.setDewpoint(1.8f * (pdv.getFloat(DEWPOINT) - 273.15f) + 32.0f);
		}
		fssr.setRelativeHumidity(pdv.getFloat("relHumidity"));
		if (pdv.getFloat(TEMPERATURE) != MISSING) {
			fssr.setTemperature(1.8f * (pdv.getFloat(TEMPERATURE) - 273.15f) + 32.0f);
		}
		// fssr.setHighResWaveHeight(highResWaveHeight);
		// fssr.setHorzVisibility(horzVisibility);
		// fssr.setHourlyPrecip(pdv.getFloat(HOURLY_PRECIP));
		// fssr.setMaxWindSpeed(maxWindSpeed);
		fssr.setMessageData(pdv.getString(RAW_MESSAGE));
		fssr.setPressChange3Hour(pdv.getFloat(PRESS_CHANGE3_HOUR));
		fssr.setPressChangeChar(String.valueOf(pdv.getInt(PRESS_CHANGE_CHAR)));
		fssr.setPressureAltimeter(pdv.getFloat(ALTIMETER)); // in Pascal
		// fssr.setPresWeather(pdv.getStringAllLevels(PRES_WEATHER));
		// fssr.setPrimarySwellWaveDir(primarySwellWaveDir);
		// fssr.setPrimarySwellWaveHeight(primarySwellWaveHeight);
		// fssr.setPrimarySwellWavePeriod(primarySwellWavePeriod);
		if (pdv.getFloat(SEA_LEVEL_PRESSURE) != MISSING) {
			fssr.setSeaLevelPress(pdv.getFloat(SEA_LEVEL_PRESSURE) / 100);
		}
		if (pdv.getFloat(SEA_SURFACE_TEMP) != MISSING) {
			fssr.setSeaSurfaceTemp(1.8f * (pdv.getFloat(SEA_SURFACE_TEMP) - 273.15f) + 32.0f);
		}
		// fssr.setSecondarySwellWaveDir(secondarySwellWaveDir);
		// fssr.setSecondarySwellWaveHeight(secondarySwellWaveHeight);
		// fssr.setSecondarySwellWavePeriod(secondarySwellWavePeriod);
		// fssr.setSkyCover(pdv.getStringAllLevels(SKY_COVER));
		// fssr.setTotCloudAmount(pdv.getInt(TOTAL_CLOUD_COVER));
		fssr.setVisibility(pdv.getFloat(VISIBILITY));
		fssr.setWaveHeight(pdv.getNumber(WAVE_HEIGHT).doubleValue());
		fssr.setWindDir(pdv.getFloat(WIND_DIR));
		fssr.setWindGust(pdv.getFloat(WIND_GUST));
		fssr.setWindSpeed(pdv.getFloat(WIND_SPEED));

		return fssr;
	}

	public static PointDataView buildView(FSSObsRecord record) {
		pdc = PointDataContainer.build(fsspdd);
		PointDataView pdv = pdc.append();

		pdv.setFloat(CEILING, record.getCeiling());
		pdv.setFloat(DEWPOINT, record.getDewpoint());
		pdv.setFloat(DEWPOINT_DEPR, record.getDewpointDepr());
		pdv.setFloat(FROSTBITE_TIME, record.getFrostbiteTime());
		pdv.setFloat(HORIZONTAL_VIS, record.getHorzVisibility());
		pdv.setFloat(HOURLY_PRECIP, record.getHourlyPrecip());
		pdv.setFloat(MAX_WIND_SPEED, record.getMaxWindSpeed());
		pdv.setString(PLATFORM_ID, record.getPlatformId());
		String[] weatherCondition = record.getPresWeather();
		if (weatherCondition != null) {
			for (int i = 0; i < weatherCondition.length; i++) {
				if (weatherCondition[i] != null) {
					pdv.setString(PRES_WEATHER, weatherCondition[i], i);
				}
			}
		}
		pdv.setFloat(PRESS_CHANGE3_HOUR, record.getPressChange3Hour());
		pdv.setString(PRESS_CHANGE_CHAR, record.getPressChangeChar());
		pdv.setFloat(PRESS_ALTIMETER, record.getPressureAltimeter());
		pdv.setFloat(PRI_SWELL_WV_DIR, record.getPrimarySwellWaveDir()
				.floatValue());
		pdv.setFloat(PRI_SWELL_WV_HGT, record.getPrimarySwellWaveHeight()
				.floatValue());
		pdv.setInt(PRI_SWELL_WV_PD, record.getPrimarySwellWavePeriod());
		pdv.setString(RAW_MESSAGE, record.getRawMessage());
		pdv.setFloat(REL_HUMIDITY, record.getRelativeHumidity());
		pdv.setFloat(SEA_LEVEL_PRESS, record.getSeaLevelPress());
		pdv.setFloat(SEA_SFC_TEMP, record.getSeaSurfaceTemp());
		pdv.setFloat(SEC_SWELL_WV_DIR, record.getSecondarySwellWaveDir()
				.floatValue());
		pdv.setFloat(SEC_SWELL_WV_HGT, record.getSecondarySwellWaveHeight()
				.floatValue());
		pdv.setInt(SEC_SWELL_WV_PD, record.getSecondarySwellWavePeriod());
		if (record.getSkyCover() != null) {
			String[] skyList = record.getSkyCover();
			for (int i = 0; i < skyList.length; i++) {
				if (skyList[i] != null) {
					pdv.setString(SKY_COVER, skyList[i], i);
				}
			}
		}
		pdv.setFloat(SNOW_INC_HOURLY, record.getSnincrHourly());
		pdv.setFloat(SNOW_INC_TOTAL, record.getSnincrTotal());
		pdv.setFloat(SNOW_DEPTH, record.getSnowDepth());
		pdv.setString(STATION_NAME, record.getStnName());
		pdv.setFloat(TEMPERATURE, record.getTemperature());
		pdv.setLong(TIME_OBS, record.getTimeObs().getTimeInMillis());
		pdv.setLong(REF_HOUR, record.getRefHour().getTimeInMillis());
		pdv.setInt(CLOUD_AMOUNT_TOT, record.getTotCloudAmount());
		pdv.setFloat(VISIBILITY, record.getVisibility());
		pdv.setFloat(WV_HGT, record.getWaveHeight().floatValue());
		pdv.setInt(WV_PD, record.getWavePeriod());
		pdv.setFloat(WV_STEEPNESS, record.getWaveSteepness());
		pdv.setFloat(WIND_DIR, record.getWindDir());
		pdv.setFloat(WIND_SPEED, record.getWindSpeed());
		pdv.setFloat(WIND_GUST, record.getWindGust());
		pdv.setFloat(HI_RES_WV_HGT, record.getHighResWaveHeight());

		// pdv.setLong(TIME_NOMINAL, record.getRefHour().getTimeInMillis());

		record.setPointDataView(pdv);
		return pdv;
	}

	/**
	 * @param pdc
	 *            the pdc to set
	 */
	public void setPdc(PointDataContainer pdc) {
        FSSObsDataTransform.pdc = pdc;
	}

	/**
	 * @return the pdc
	 */
	public PointDataContainer getPdc() {
		return pdc;
	}

}
