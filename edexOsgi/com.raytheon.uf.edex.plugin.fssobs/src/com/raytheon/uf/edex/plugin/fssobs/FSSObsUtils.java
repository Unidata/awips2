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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Scanner;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.pointdata.PointDataQuery;

/**
 * Get METAR and Maritime data records
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2010            skorolev    Initial creation
 * Nov 26, 2012 1297       skorolev    Changed ArrayList to List.Clean code
 * May 15, 2013 1869       bsteffen    Remove DataURI column from ldadmesonet.
 * May 16, 2013 1869       bsteffen    Rewrite dataURI property mappings.
 * Jan 02, 2014 2580       skorolev    Fixed FSSObs error.
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */

public class FSSObsUtils {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FSSObsUtils.class);

    /**
     * Value of missed data.
     */
    public static final float MISSING = -9999.0f;

    /**
     * The constant representing the sky condition for sky clear
     */
    private static final int SKC_SKY_CONDITION = 9999999;

    /**
     * The constant representing the sky condition for clear sky
     */
    private static final int CLR_SKY_CONDITION = 8888888;

    /** Monitor ID **/
    private enum monID {
        ss, fog, snow
    };

    /** Plug-in name **/
    private enum plgn {
        obs, sfcobs, ldadmesonet
    };

    /** Selected column in database **/
    private static String slct = "dataURI";

    /** Equal sign **/
    private static String equ = "=";

    /** Database **/
    private static String db = "metadata";

    /** SQL expression **/
    private static String sqlexp = "select name from common_obs_spatial where ( catalogtype=1 or catalogtype=33 or catalogtype = 32 or catalogtype = 1000) and stationid = '";

    /**
     * Constructor
     */
    private FSSObsUtils() {
    }

    /**
     * Gets METAR records from Obs.
     * 
     * @param uri
     * @return Metar record
     * @throws PluginException
     */
    public static FSSObsRecord getRecordFromMetar(String uri)
            throws PluginException {

        FSSObsRecord recFromMetar = null;
        PointDataQuery request = null;
        PointDataContainer result = null;
        try {
            request = new PointDataQuery(plgn.obs.toString());
            request.requestAllLevels();
            request.addParameter(slct, uri, equ);
            request.setParameters(FSSObsDataTransform.OBS_PARAMS_LIST);
            result = request.execute();
            if (result != null) {
                recFromMetar = FSSObsDataTransform.fromMetarRecord(result);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return recFromMetar;
    }

    /**
     * Gets station name from database.
     * 
     * @param stnId
     * @return station name
     */
    public static String getStationName(String stnId) {
        String retVal = null;
        ISpatialQuery sq = null;
        String sql = sqlexp + stnId + "'";
        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, db);
            retVal = (String) results[0];
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getMessage());
        }
        return retVal;
    }

    /**
     * Gets Maritime record from sfcobs.
     * 
     * @param uri
     * @return maritaime record
     * @throws PluginException
     */
    public static FSSObsRecord getRecordFromMaritime(String uri)
            throws PluginException {
        FSSObsRecord recFromMaritime = null;
        PointDataQuery request = null;
        PointDataContainer result = null;
        try {
            request = new PointDataQuery(plgn.sfcobs.toString());
            request.addParameter(slct, uri, equ);
            request.setParameters(FSSObsDataTransform.SFCOBS_PARAMS_LIST);
            result = request.execute();
            if (result != null) {
                recFromMaritime = FSSObsDataTransform
                        .fromMaritimeRecord(result);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }
        return recFromMaritime;
    }

    /**
     * Gets Mesowest record from ldadmesonet.
     * 
     * @param uri
     * @return mesowest record
     * @throws PluginException
     */
    public static FSSObsRecord getRecordFromMesowest(String uri)
            throws PluginException {

        FSSObsRecord recFromMesowest = null;
        PointDataQuery request = null;
        PointDataContainer result = null;
        try {
            Map<String, RequestConstraint> rcMap = RequestConstraint
                    .toConstraintMapping(DataURIUtil.createDataURIMap(uri));
            // Not actually in db
            rcMap.remove("pluginName");
            request = new PointDataQuery(plgn.ldadmesonet.toString());
            for (Entry<String, RequestConstraint> entry : rcMap.entrySet()) {
                RequestConstraint rc = entry.getValue();
                String value = rc.getConstraintValue();
                String type = rc.getConstraintType().getOperand();
                request.addParameter(entry.getKey(), value, type);
            }
            request.setParameters(FSSObsDataTransform.MESOWEST_PARAMS_LIST);
            result = request.execute();
            if (result != null) {
                recFromMesowest = FSSObsDataTransform
                        .fromLdadmesowestRecord(result);

            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return recFromMesowest;
    }

    /**
     * This method calculates the amount of time needed for frost bite to occur
     * on exposed skin.
     * 
     * @param windspeedKPH
     *            -- wind speed in kilometers per hour
     * @param temperatureC
     *            -- temperature in degrees Celsius
     * @return -- time in minutes
     */
    public static float calcFrostbiteTime(float windspeedKPH, float temperatureC) {
        float fbMinutes = MISSING;

        // Temperature must be lower than -4.8C (23F) to avoid a calculation
        // error (a negative number to -1.668 power is NAN)
        if (temperatureC < -4.8)
            fbMinutes = ((-24.5f * ((0.667f * windspeedKPH) + 4.8f)) + 2111f)
                    * (float) Math.pow((-4.8 - temperatureC), -1.668);
        else
            return MISSING;
        // Check for frost bite boundaries
        if (!(fbMinutes <= 30 && windspeedKPH > 25.0 && windspeedKPH <= 80.5))
            fbMinutes = MISSING;
        return fbMinutes;
    }

    /**
     * This method calculates the windChill from temperature and windSpeed.
     * 
     * @param temp
     *            -- temperature in degrees Celsius
     * @param windSpd
     *            -- wind speed in kilometers per hour
     * @return -- wind chill in degrees Celsius
     */
    public static float calcWindChill(float temp, float windSpd) {
        float spd;
        /* arbitrarily do the calculation only for temps at or below 60F */
        if (temp > 16.)
            return 1e37f;
        /* no chilling if speed < 4 mph = 6.44km/h */
        if (windSpd < 6.4)
            return temp;
        /* peg speed at 80 mph (= 128.75 km/h) */
        if (windSpd > 128.75)
            spd = 128.75f;
        else
            spd = windSpd;
        spd = (float) Math.pow(spd, 0.16);
        float windChillTemp = 13.12f + 0.6215f * temp - 11.37f * spd + 0.3965f
                * temp * spd;
        return windChillTemp;
    }

    /**
     * This method calculates a floating point number representing the ceiling.
     * By definition, the ceiling is the lowest overcast or broken cloud layer,
     * so the method looks for the lowest layer that matches a BKN or OVC
     * condition, and returns that layer.
     * 
     * @param skyCov
     *            -- the set of sky coverage data
     * @return -- the ceiling
     */
    public static float findMetarCeilingFromLayers(String[] skyCov,
            Number[] levels) {
        float ceiling = 1e20f;
        // Find a ceiling in a METAR report.
        try {
            for (int i = 0; i < skyCov.length; i++) {
                String sc = skyCov[i];
                // SCT ???
                if (sc.equals("CLR")) {
                    ceiling = CLR_SKY_CONDITION;
                    break;
                } else if (sc.equals("SKC")) {
                    ceiling = SKC_SKY_CONDITION;
                    break;
                } else if ((sc.equals("BKN")) || (sc.equals("OVC"))) {
                    if (levels[i] != null) {
                        ceiling = levels[i].floatValue() / 100f;
                        break;
                    }
                }
            }
        } catch (RuntimeException e) {
            // ignore cloud cover that is null
        }
        return ceiling >= 1e20f ? MISSING : ceiling;
    }

    /**
     * This method determines the RH from temperature and dew point in degrees
     * Fahrenheit.
     * 
     * @param dewpoint
     *            in F
     * @param temperature
     *            in F
     * @return -- calculated Relative Humidity in %
     */
    public static Float getRH(float dewpoint, float temperature) {
        float retVal = MISSING;
        // From http://www.hpc.ncep.noaa.gov/html/dewrh.shtml
        // to Celsius
        if (dewpoint != MISSING && temperature != MISSING) {
            float temp = (100f / (212f - 32f)) * (temperature - 32f);
            float dwpt = (100f / (212f - 32f)) * (dewpoint - 32f);
            // saturation vapor pressure
            float c = (float) (6.11 * Math.pow(10,
                    ((7.5 * temp / (237.7 + temp)))));
            // actual vapor pressure
            float d = (float) (6.11 * Math.pow(10,
                    ((7.5 * dwpt / (237.7 + dwpt)))));
            // relative humidity
            retVal = (d / c) * 100;
        }
        return retVal;
    }

    /**
     * Gets snow data.
     * 
     * @param tableRow
     * @return -- Snow data from METAR
     */
    public static float[] getSnowData(FSSObsRecord tableRow) {
        // Check parameters for wind chill in K
        // frost bite in minutes
        // snow increase and depth in inches
        // time calculation (upper limit for wind chill
        // is set at 40F and wind speed between 14 and 43 knts) :
        float[] retVal = new float[5];
        for (int i = 0; i < 5; i++) {
            retVal[i] = MISSING;
        }
        Scanner sc = new Scanner(tableRow.getRawMessage());
        String whatMatched;
        whatMatched = sc.findWithinHorizon("RMK", 0);
        if (whatMatched != null) {
            whatMatched = sc.findWithinHorizon("SNINCR", 0);
            if (whatMatched != null) {
                sc.useDelimiter("/");
                if (sc.hasNextInt()) {
                    // last hour snow in inches
                    retVal[0] = sc.nextInt();
                }
                sc.reset();
                if (sc.hasNextInt()) {
                    // total snow in inches
                    retVal[1] = sc.nextInt();
                }
            }
            whatMatched = sc.findWithinHorizon("4/", 0);
            if (whatMatched != null) {
                // snow depth on ground in inches
                if (retVal.length >= 5) {
                    retVal[2] = sc.nextInt();
                }
            }
        }
        sc.close();
        if ((tableRow.getTemperature() != MISSING)
                && (tableRow.getTemperature() < 4.4f)
                // 277.6 K = 40 F = 4.44444 C
                && (tableRow.getWindSpeed() != MISSING)
                && (tableRow.getWindSpeed() <= 43.0f && tableRow.getWindSpeed() >= 14.0f)) {
            float speedKPH = tableRow.getWindSpeed() * 1.6f;
            float t = tableRow.getTemperature();
            // in Kelvin
            retVal[3] = calcWindChill(t, speedKPH) + 273.15f;
            // in minutes
            retVal[4] = calcFrostbiteTime(speedKPH, t);
        }
        return retVal;
    }

    /**
     * Routine to calculate dewpoint depression from temperature and relative
     * humidity.
     * 
     * @param TK
     *            - temperature in K
     * @param RH
     *            - relative humidity in %
     * @return dewpoint depression in C
     */
    public static float getDpDepression(float TK, float RH) {
        float retVal = MISSING;
        if (RH != MISSING && TK != MISSING) {
            float rhqc = Math.min(100.0f, Math.max(1.0f, RH));
            float b = (float) (0.0091379024f * TK + 6106.396f / TK - Math
                    .log(rhqc / 100.0f));
            retVal = (float) (TK - (b - Math.sqrt((b * b - 223.1986)) / 0.0182758048f));
        }
        return retVal;
    }

    /**
     * Gets stations which FSS monitor is using.
     * 
     * @param monitor
     * @return stations
     */
    public static List<String> getStations(String monitor) {
        String currentSite = SiteUtil.getSite();

        List<String> stations = new ArrayList<String>();
        // Which monitor should use this station: fog, ss or snow
        if (monitor.equals(monID.fog.name())) {
            FogMonitorConfigurationManager fogConfigManager = FogMonitorConfigurationManager
                    .getInstance();
            fogConfigManager.readConfigXml(currentSite);
            List<String> fogStations = fogConfigManager.getStations();
            stations.addAll(fogStations);
        }
        if (monitor.equals(monID.ss.name())) {
            SSMonitorConfigurationManager ssConfigManger = SSMonitorConfigurationManager
                    .getInstance();
            ssConfigManger.readConfigXml(currentSite);
            List<String> ssStaitions = ssConfigManger.getStations();
            stations.addAll(ssStaitions);
        }
        if (monitor.equals(monID.snow.name())) {
            SnowMonitorConfigurationManager snowConfigManager = SnowMonitorConfigurationManager
                    .getInstance();
            snowConfigManager.readConfigXml(currentSite);
            List<String> snowStations = snowConfigManager.getStations();
            stations.addAll(snowStations);
        }
        return stations;
    }
}
