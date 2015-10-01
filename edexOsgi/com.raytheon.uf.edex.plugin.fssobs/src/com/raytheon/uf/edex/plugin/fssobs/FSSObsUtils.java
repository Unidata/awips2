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

import gov.noaa.nws.ncep.edex.common.metparameters.Amount;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Scanner;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.pointdata.PointDataQuery;

/**
 * Utilities for FSSObs data records.
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
 * Jan 06, 2014 2653       skorolev    Corrected decoding of snincrHourly and snincrTotal.
 * Apr 28, 2014 3086       skorolev    Updated getStations method.
 * Sep 04, 2014 3220       skorolev    Removed getStations method.
 * Sep 18, 2015 3873       skorolev    Removed identical constant definitions.
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */

public class FSSObsUtils {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FSSObsUtils.class);

    /** FAHRENHEIT -> CELSIUS */
    private static final UnitConverter fToC = NonSI.FAHRENHEIT
            .getConverterTo(SI.CELSIUS);

    /** CELSIUS --> KELVIN */
    private static final UnitConverter cToK = SI.CELSIUS
            .getConverterTo(SI.KELVIN);

    /** Knots --> kilometers per hour */
    private static final UnitConverter knotToKph = NonSI.KNOT
            .getConverterTo(NonSI.KILOMETERS_PER_HOUR);

    private static final float defaultCeiling = 1e20f;

    // ----------- Constants required for Frostbite time calculation.---------

    // Temperature must be lower than -4.8C (23F) to avoid a calculation error
    private static final float frostbiteTempMax = -4.8f;

    private static final float f1 = -24.5f;

    private static final float f2 = 0.667f;

    private static final float f3 = 2111f;

    private static final float f4 = -1.668f;

    /** Plug-in name **/
    private enum Plgn {
        obs, sfcobs, ldadmesonet
    };

    /** Selected column in database **/
    private static String slct = "dataURI";

    /** Equal sign **/
    private static String equ = "=";

    /** Database **/
    private static String db = "metadata";

    /**
     * SQL expression METAR CAT_TYPE_ICAO = 1; Known ship identifications -
     * Mobile no lat/lon CAT_TYPE_SHIP_MOB = 30 Drifting buoy locations
     * CAT_TYPE_BUOY_MOB = 31; Moored (Fixed) buoy locations CAT_TYPE_BUOY_FXD =
     * 32; Coastal Marine (CMAN) locations CAT_TYPE_CMAN = 33; CAT_TYPE_MESONET
     * = 1000; MESONET_NWSFAA = 1001;
     */
    private static String sqlexp = "select name from common_obs_spatial where ( catalogtype=1 or catalogtype=30 or catalogtype=31 or catalogtype=32 or catalogtype = 33 or catalogtype = 1000 or catalogtype = 1001) and stationid = '";

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
            request = new PointDataQuery(Plgn.obs.toString());
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
     * Gets descriptive station name from database.
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
            statusHandler.handle(Priority.ERROR,
                    "Could not to get a station name: " + e.getMessage());
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
            request = new PointDataQuery(Plgn.sfcobs.toString());
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
            request = new PointDataQuery(Plgn.ldadmesonet.toString());
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
        float fbMinutes = ObConst.MISSING;

        // Temperature must be lower than -4.8C (23F) to avoid a calculation
        // error (a negative number to -1.668 power is NAN)
        if (temperatureC < frostbiteTempMax)
            fbMinutes = ((f1 * ((f2 * windspeedKPH) + frostbiteTempMax)) + f3)
                    * (float) Math.pow((frostbiteTempMax - temperatureC), f4);
        else
            return ObConst.MISSING;
        // Check for frost bite boundaries
        if (!(fbMinutes <= 30 && windspeedKPH > 25.0 && windspeedKPH <= 80.5))
            fbMinutes = ObConst.MISSING;
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
        float retVal = ObConst.MISSING;
        Amount t = new Amount(temp, SI.CELSIUS);
        Amount s = new Amount(windSpd, NonSI.KNOT);
        try {
            Amount wcht = PRLibrary.prWcht(t, s);
            retVal = (float) wcht.doubleValue();
        } catch (NullPointerException | InvalidValueException e) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Could not get a WindChill temperature: "
                            + e.getLocalizedMessage(), e);
        }
        return retVal;
    }

    /**
     * This method calculates a floating point number representing the ceiling.
     * By definition, the ceiling is the lowest overcast or broken cloud layer,
     * so the method looks for the lowest layer that matches a BKN or OVC
     * condition, and returns that
     * layer(http://www.srh.noaa.gov/srh/dad/sfc/chapter5.pdf).
     * 
     * @param skyCov
     *            -- the set of sky coverage data
     * @return -- the ceiling
     */
    public static float findMetarCeilingFromLayers(String[] skyCov,
            Number[] levels) {
        float ceiling = defaultCeiling;
        for (int i = 0; i < skyCov.length; i++) {
            String sc = skyCov[i];
            // SCT = scattered ???
            switch (sc) {
            case "CLR":
                ceiling = ObConst.CLR_SKY_CONDITION;
                break;
            case "SKC":
                ceiling = ObConst.SKC_SKY_CONDITION;
                break;
            case "BKN":
            case "OVC":
                if (levels[i] != null) {
                    ceiling = levels[i].floatValue() / 100f;
                }
                break;
            default:
                statusHandler.error("Get unkown sky cover " + sc);
                break;
            }
        }
        return ceiling >= defaultCeiling ? ObConst.MISSING : ceiling;
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
        float retVal = ObConst.MISSING;
        Amount t = new Amount(fToC.convert(temperature), SI.CELSIUS);
        Amount d = new Amount(fToC.convert(dewpoint), SI.CELSIUS);
        Amount rh;
        try {
            rh = PRLibrary.prRelh(t, d);
            retVal = (float) rh.doubleValue();
        } catch (NullPointerException | InvalidValueException e) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Error to get Relative Humidity: "
                            + e.getLocalizedMessage(), e);
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
            retVal[i] = ObConst.MISSING;
        }
        float temp = tableRow.getTemperature();
        float windspd = tableRow.getWindSpeed();

        Scanner sc = new Scanner(tableRow.getRawMessage());
        String whatMatched;
        whatMatched = sc.findWithinHorizon("RMK", 0);
        if (whatMatched != null) {
            whatMatched = sc.findWithinHorizon("SNINCR", 0);
            if (whatMatched != null) {
                sc.useDelimiter("/");
                if (sc.hasNext()) {
                    // last hour snow in inches
                    retVal[0] = Float.parseFloat(sc.next());
                }
                sc.reset();
                sc.findWithinHorizon("/", 0);
                if (sc.hasNextInt()) {
                    // total snow in inches
                    retVal[1] = sc.nextInt();
                }
            }
            whatMatched = sc.findWithinHorizon("4/", 0);
            if (whatMatched != null) {
                // snow depth on ground in inches
                retVal[2] = sc.nextInt();
            }
        }
        sc.close();
        if (temp != ObConst.MISSING && windspd != ObConst.MISSING) {
            float speedKPH = (float) knotToKph.convert(windspd);
            // in Kelvin
            retVal[3] = (float) cToK.convert(calcWindChill(
                    (float) fToC.convert(temp), speedKPH));
            // in minutes
            retVal[4] = calcFrostbiteTime(speedKPH, (float) fToC.convert(temp));
        }
        return retVal;
    }
}
