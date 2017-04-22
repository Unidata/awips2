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
package com.raytheon.uf.common.dataplugin.modelsounding;

import java.util.Arrays;
import java.util.List;

/**
 * This class holds constants for all the point data parameters for model
 * sounding.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 18, 2013  2537     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ModelSoundingParameters {

    /* Record parameters */
    @Deprecated
    public static final String DATAURI = "dataURI";

    public static final String WMO_HEADER = "wmoHeader";

    public static final String STATION_NUMBER = "wmoStaNum";

    public static final String LATITUDE = "latitude";

    public static final String LONGITUDE = "longitude";

    public static final String ELEVATION = "elevation";

    public static final String STATION_ID = "stationId";

    public static final String REF_TIME = "refTime";

    public static final String FORECAST_HOUR = "forecastHr";

    /* Surface parameters */
    public static final String SEA_LEVEL_PRESS = "seaLvlPress";

    public static final String SFC_PRESS = "sfcPress";

    public static final String LOW_CLOUD = "lowCld";

    public static final String MID_CLOUD = "midCld";

    public static final String HIGH_CLOUD = "hiCld";

    public static final String CLOUD_PRESS = "prCloud";

    public static final String VISIBILITY = "vsby";

    public static final String U_STORM = "uStorm";

    public static final String V_STORM = "vStorm";

    public static final String STORM_REL_HELI = "srHel";

    public static final String TOTAL_PRECIP = "totPrecip";

    public static final String CONV_PRECIP = "convPrecip";

    public static final String SNOW_WATER = "snowWater";

    public static final String SNOW_FALL = "snowFall";

    public static final String SNOW_FLUX = "snowFlux";

    public static final String SNOW_MELT = "snowMelt";

    public static final String U_COMP_10M = "u10";

    public static final String V_COMP_10M = "v10";

    public static final String SENS_HEAT = "sensHeat";

    public static final String SUB_SFC_HEAT = "subSfcHeat";

    public static final String SKIN_TEMP = "skinTemp";

    public static final String MIN_TEMP = "minTemp";

    public static final String MAX_TEMP = "maxTemp";

    public static final String TEMP_2M = "temp2";

    public static final String SPEC_HUM_2M = "q2";

    public static final String THETA_10M = "Theta10";

    public static final String SPEC_HUM_10M = "q10";

    public static final String SNOW_TYPE = "snowTyp";

    public static final String ICE_TYPE = "iceTyp";

    public static final String FREEZING_RAIN_TYPE = "frzgRainTyp";

    public static final String RAIN_TYPE = "rainType";

    /* Level parameters */
    public static final String NUM_LEVELS = "numProfLvls";

    public static final String LVL_PRESS = "pressure";

    public static final String LVL_TEMP = "temperature";

    public static final String LVL_SPEC_HUM = "specHum";

    public static final String LVL_OMEGA = "omega";

    public static final String LVL_U_COMP = "uComp";

    public static final String LVL_V_COMP = "vComp";

    public static final String LVL_CLOUD_COVER = "cldCvr";

    public static final List<String> ALL_DATA = Arrays.asList(WMO_HEADER,
            STATION_NUMBER, LATITUDE, LONGITUDE, ELEVATION, STATION_ID,
            REF_TIME, FORECAST_HOUR, SEA_LEVEL_PRESS, SFC_PRESS, LOW_CLOUD,
            MID_CLOUD, HIGH_CLOUD, CLOUD_PRESS, VISIBILITY, U_STORM, V_STORM,
            STORM_REL_HELI, TOTAL_PRECIP, CONV_PRECIP, SNOW_WATER, SNOW_FALL,
            SNOW_FLUX, SNOW_MELT, U_COMP_10M, V_COMP_10M, SENS_HEAT,
            SUB_SFC_HEAT, SKIN_TEMP, MIN_TEMP, MAX_TEMP, TEMP_2M, SPEC_HUM_2M,
            THETA_10M, SPEC_HUM_10M, SNOW_TYPE, ICE_TYPE, FREEZING_RAIN_TYPE,
            RAIN_TYPE, NUM_LEVELS, LVL_PRESS, LVL_TEMP, LVL_SPEC_HUM,
            LVL_OMEGA, LVL_U_COMP, LVL_V_COMP, LVL_CLOUD_COVER);

    public static final List<String> LVL_PARAMETERS = Arrays.asList(LVL_PRESS,
            LVL_TEMP, LVL_SPEC_HUM, LVL_OMEGA, LVL_U_COMP, LVL_V_COMP,
            LVL_CLOUD_COVER);

}
