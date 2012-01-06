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
package com.raytheon.edex.plugin.modelsounding.common;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.pointdata.PointDataQuery;

/**
 * 
 * A class for converting point data into sounding sites.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ModelSoundingPointDataTransform {

    // Record parameters
    public static final String P_WMO_HEADER = "wmoHeader";

    public static final String P_DATAURI = "dataURI";

    public static final String P_STATION_NUMBER = "wmoStaNum";

    public static final String P_LATITUDE = "latitude";

    public static final String P_LONGITUDE = "longitude";

    public static final String P_ELEVATION = "elevation";

    public static final String P_STATION_ID = "stationId";

    public static final String P_REF_TIME = "refTime";

    public static final String P_FORECAST_HOUR = "forecastHr";

    public static final String P_NUM_LEVELS = "numProfLvls";

    // Level parameters
    public static final String P_LVL_PRESS = "pressure";

    public static final String P_LVL_TEMP = "temperature";

    public static final String P_LVL_SPEC_HUM = "specHum";

    public static final String P_LVL_OMEGA = "omega";

    public static final String P_LVL_U_COMP = "uComp";

    public static final String P_LVL_V_COMP = "vComp";

    public static final String P_LVL_CLOUD_COVER = "cldCvr";

    // Surface parameters
    public static final String P_SFC_PRESS = "sfcPress";

    public static final String P_SEA_LEVEL_PRESS = "seaLvlPress";

    public static final String P_LOW_CLOUD = "lowCld";

    public static final String P_MID_CLOUD = "midCld";

    public static final String P_HIGH_CLOUD = "hiCld";

    public static final String P_CLOUD_PRESS = "prCloud";

    public static final String P_VISIBILITY = "vsby";

    public static final String P_U_STORM = "uStorm";

    public static final String P_V_STORM = "vStorm";

    public static final String P_STORM_REL_HELI = "srHel";

    public static final String P_TOTAL_PRECIP = "totPrecip";

    public static final String P_CONV_PRECIP = "convPrecip";

    public static final String P_SNOW_FALL = "snowFall";

    public static final String P_U_COMP_10M = "u10";

    public static final String P_V_COMP_10M = "v10";

    public static final String P_TEMP_2M = "temp2";

    public static final String P_SPEC_HUM_2M = "q2";

    public static final String P_SNOW_TYPE = "snowTyp";

    public static final String P_ICE_TYPE = "iceTyp";

    public static final String P_FREEZING_RAIN_TYPE = "frzgRainTyp";

    public static final String P_RAIN_TYPE = "rainType";

    // This list deliberately omits data which can be pulled from the dataURI
    // including location and time information
    public static final List<String> ALL_DATA = Arrays.asList(P_WMO_HEADER,
            P_DATAURI, P_ELEVATION, P_STATION_NUMBER, P_NUM_LEVELS, P_LVL_PRESS, P_LVL_TEMP,
            P_LVL_SPEC_HUM, P_LVL_OMEGA, P_LVL_U_COMP, P_LVL_V_COMP,
            P_LVL_CLOUD_COVER, P_SFC_PRESS, P_SEA_LEVEL_PRESS, P_LOW_CLOUD,
            P_MID_CLOUD, P_HIGH_CLOUD, P_CLOUD_PRESS, P_VISIBILITY, P_U_STORM,
            P_V_STORM, P_STORM_REL_HELI, P_TOTAL_PRECIP, P_CONV_PRECIP,
            P_SNOW_FALL, P_U_COMP_10M, P_V_COMP_10M, P_TEMP_2M, P_SPEC_HUM_2M,
            P_SNOW_TYPE, P_ICE_TYPE, P_FREEZING_RAIN_TYPE, P_RAIN_TYPE);

    public static final List<String> LVL_PARAMETERS = Arrays.asList(
            P_LVL_PRESS, P_LVL_TEMP, P_LVL_SPEC_HUM, P_LVL_OMEGA, P_LVL_U_COMP,
            P_LVL_V_COMP, P_LVL_CLOUD_COVER);

    /**
     * Use all point data parameters to build sounding sites for all sites which
     * match the query defined by fields, values, and operands.
     * 
     */
    public static List<SoundingSite> getSoundingSites(List<String> fields,
            List<Object> values, List<String> operands) throws Exception {
        return getSoundingSites(fields, values, operands, ALL_DATA);
    }

    /**
     * Use the specified point data parameters to build sounding sites for all
     * sites which match the query defined by fields, values, and operands.
     * 
     */
    public static List<SoundingSite> getSoundingSites(List<String> fields,
            List<Object> values, List<String> operands, List<String> parameters)
            throws Exception {
        StringBuilder parametersString = new StringBuilder();
        for (String parameter : parameters) {
            if (parametersString.length() > 0) {
                parametersString.append(",");
            }
            parametersString.append(parameter);
        }
        if (!parameters.contains(P_NUM_LEVELS)) {
            // if you have any level based parameters you must include num
            // levels
            for (String lvlParam : LVL_PARAMETERS) {
                if (parameters.contains(lvlParam)) {
                    parametersString.append(",");
                    parametersString.append(P_NUM_LEVELS);
                    break;
                }
            }
        }
        PointDataQuery pdq = new PointDataQuery("modelsounding");
        pdq.setParameters(parametersString.toString());
        for (int i = 0; i < fields.size(); i++) {
            Object value = values.get(i);
            String valueString = String.valueOf(value);
            // TODO more generic support of different objects and/or allow
            // PointDataQuery to handle Objects instead of just String
            if (value instanceof Date) {
                valueString = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
                        .format(value);
            }
            pdq.addParameter(fields.get(i), valueString, operands.get(i));

        }
        pdq.requestAllLevels();
        PointDataContainer pdc = pdq.execute();
        if (pdc == null) {
            return Collections.emptyList();
        }
        return getSoundingSites(pdc);
    }

    /**
     * Build sounding sites from the data in the container.
     * 
     * @param pdc
     * @return
     */
    public static List<SoundingSite> getSoundingSites(PointDataContainer pdc) {
        List<SoundingSite> sites = new ArrayList<SoundingSite>(
                pdc.getCurrentSz());
        for (int i = 0; i < pdc.getCurrentSz(); i++) {
            sites.add(getSoundingSite(pdc.readRandom(i)));
        }
        return sites;
    }

    /**
     * Build a single sounding site from the data in the view.
     * 
     * @param pdv
     * @return
     */
    public static SoundingSite getSoundingSite(PointDataView pdv) {
        // All the code from here on is boilerplate code for determining what
        // parameters are in the view and setting the appropriate field in the
        // Sounding Site.
        Set<String> parameters = pdv.getContainer().getParameters();
        SoundingSite site = null;
        if (parameters.contains(P_DATAURI)) {
            // Parsing from the dataURI gets several fields for us.
            site = new SoundingSite(pdv.getString(P_DATAURI));
            site.setFcstSeconds((long) site.getDataTime().getFcstTime());
        } else {
            site = new SoundingSite();
            site.setLocation(new SurfaceObsLocation());
            // All of these things would have been in dataURI
            if (parameters.contains(P_LATITUDE)) {
                site.getLocation().setLatitude(
                        pdv.getNumber(P_LATITUDE).doubleValue());
            }
            if (parameters.contains(P_LONGITUDE)) {
                site.getLocation().setLongitude(
                        pdv.getNumber(P_LONGITUDE).doubleValue());
            }
            if (parameters.contains(P_STATION_ID)) {
                site.getLocation().setStationId(pdv.getString(P_STATION_ID));
            }
            if (parameters.contains(P_FORECAST_HOUR)) {
                if (parameters.contains(P_REF_TIME)) {
                    Date refTime = new Date(pdv.getNumber(P_REF_TIME)
                            .longValue() * 1000);
                    int fcstTime = pdv.getNumber(P_FORECAST_HOUR).intValue() * 3600;
                    site.setDataTime(new DataTime(refTime, fcstTime));
                }
                site.setFcstSeconds(pdv.getNumber(P_FORECAST_HOUR).longValue() * 3600);
            } else if (parameters.contains(P_REF_TIME)) {
                // This might not be the best idea most people will want
                // forecast time also
                site.setDataTime(new DataTime(new Date(pdv
                        .getNumber(P_REF_TIME).longValue() * 1000)));
            }
        }
        // Record parameters
        if (parameters.contains(P_WMO_HEADER)) {
            site.setWmoHeader(pdv.getString(P_WMO_HEADER));
        }
        if (parameters.contains(P_STATION_NUMBER)) {
            site.setSiteId(String.format("%06d", pdv
                    .getNumber(P_STATION_NUMBER).intValue()));
        }
        populateLevels(site, pdv);

        // Surface parameters
        if (parameters.contains(P_ELEVATION)) {
            site.getLocation().setElevation(pdv.getNumber(P_ELEVATION).intValue());
        }
        if (parameters.contains(P_SFC_PRESS)) {
            site.setPressSfc(pdv.getNumber(P_SFC_PRESS).intValue());
        }
        if (parameters.contains(P_SEA_LEVEL_PRESS)) {
            site.setPressSLP(pdv.getNumber(P_SEA_LEVEL_PRESS).intValue());
        }
        if (parameters.contains(P_LOW_CLOUD)) {
            site.setCldAmtLo(pdv.getNumber(P_LOW_CLOUD).intValue());
        }
        if (parameters.contains(P_MID_CLOUD)) {
            site.setCldAmtMd(pdv.getNumber(P_MID_CLOUD).intValue());
        }
        if (parameters.contains(P_HIGH_CLOUD)) {
            site.setCldAmtHi(pdv.getNumber(P_HIGH_CLOUD).intValue());
        }
        if (parameters.contains(P_CLOUD_PRESS)) {
            site.setPressCldBase(pdv.getNumber(P_CLOUD_PRESS).intValue());
        }
        if (parameters.contains(P_VISIBILITY)) {
            site.setHorzVis(pdv.getNumber(P_VISIBILITY).doubleValue());
        }
        if (parameters.contains(P_U_STORM)) {
            site.setStormUComp(pdv.getNumber(P_U_STORM).doubleValue());
        }
        if (parameters.contains(P_V_STORM)) {
            site.setStormVComp(pdv.getNumber(P_V_STORM).doubleValue());
        }
        if (parameters.contains(P_STORM_REL_HELI)) {
            site.setStormRelHeli(pdv.getNumber(P_STORM_REL_HELI).doubleValue());
        }
        if (parameters.contains(P_TOTAL_PRECIP)) {
            site.setTotPrecip1Hr(pdv.getNumber(P_TOTAL_PRECIP).doubleValue());
        }
        if (parameters.contains(P_CONV_PRECIP)) {
            site.setPrecipConv1Hr(pdv.getNumber(P_CONV_PRECIP).doubleValue());
        }
        if (parameters.contains(P_SNOW_FALL)) {
            site.setSnowWaterEquiv(pdv.getNumber(P_SNOW_FALL).doubleValue());
        }
        if (parameters.contains(P_U_COMP_10M)) {
            site.setUc10M(pdv.getNumber(P_U_COMP_10M).doubleValue());
        }
        if (parameters.contains(P_V_COMP_10M)) {
            site.setVc10M(pdv.getNumber(P_V_COMP_10M).doubleValue());
        }
        if (parameters.contains(P_TEMP_2M)) {
            site.setTemp2M(pdv.getNumber(P_TEMP_2M).doubleValue());
        }
        if (parameters.contains(P_SPEC_HUM_2M)) {
            site.setSpecHum2M(pdv.getNumber(P_SPEC_HUM_2M).doubleValue());
        }
        if (parameters.contains(P_SNOW_TYPE)) {
            site.setSnowType(pdv.getNumber(P_SNOW_TYPE).intValue());
        }
        if (parameters.contains(P_ICE_TYPE)) {
            site.setIceType(pdv.getNumber(P_ICE_TYPE).intValue());
        }
        if (parameters.contains(P_FREEZING_RAIN_TYPE)) {
            site.setFzRainType(pdv.getNumber(P_FREEZING_RAIN_TYPE).intValue());
        }
        if (parameters.contains(P_RAIN_TYPE)) {
            site.setRainType(pdv.getNumber(P_RAIN_TYPE).intValue());
        }
        return site;
    }

    private static void populateLevels(SoundingSite site, PointDataView pdv) {
        // Level Parameters
        Set<String> parameters = pdv.getContainer().getParameters();
        if (parameters.contains(P_NUM_LEVELS)) {
            int numLevels = pdv.getInt(P_NUM_LEVELS);
            Number[] pressure = null;
            Number[] temperature = null;
            Number[] specHum = null;
            Number[] omega = null;
            Number[] uComp = null;
            Number[] vComp = null;
            Number[] cloudCover = null;
            if (parameters.contains(P_LVL_PRESS)) {
                pressure = pdv.getNumberAllLevels(P_LVL_PRESS);
            }
            if (parameters.contains(P_LVL_TEMP)) {
                temperature = pdv.getNumberAllLevels(P_LVL_TEMP);
            }
            if (parameters.contains(P_LVL_SPEC_HUM)) {
                specHum = pdv.getNumberAllLevels(P_LVL_SPEC_HUM);
            }
            if (parameters.contains(P_LVL_OMEGA)) {
                omega = pdv.getNumberAllLevels(P_LVL_OMEGA);
            }
            if (parameters.contains(P_LVL_U_COMP)) {
                uComp = pdv.getNumberAllLevels(P_LVL_U_COMP);
            }
            if (parameters.contains(P_LVL_V_COMP)) {
                vComp = pdv.getNumberAllLevels(P_LVL_V_COMP);
            }
            if (parameters.contains(P_LVL_CLOUD_COVER)) {
                cloudCover = pdv.getNumberAllLevels(P_LVL_CLOUD_COVER);
            }
            for (int j = 0; j < numLevels; j++) {
                SoundingLevel level = new SoundingLevel();
                if (pressure != null) {
                    level.setPressure(pressure[j].intValue());
                }
                if (temperature != null) {
                    level.setTemperature(temperature[j].doubleValue());
                }
                if (specHum != null) {
                    level.setSpecificHumidity(specHum[j].doubleValue());
                }
                if (omega != null) {
                    level.setOmega(omega[j].doubleValue());
                }
                if (uComp != null) {
                    level.setUcWind(uComp[j].doubleValue());
                }
                if (vComp != null) {
                    level.setVcWind(vComp[j].doubleValue());
                }
                if (cloudCover != null) {
                    level.setLyrCldCvr(cloudCover[j].intValue());
                }
                site.addLevel(level);
            }
        }
    }
}
