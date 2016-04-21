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
package com.raytheon.uf.viz.monitor.data;

import java.util.Date;
import java.util.SortedMap;
import java.util.TreeMap;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.data.ObConst.ProductName;
import com.raytheon.uf.common.monitor.data.ObConst.VarName;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr.ThresholdKey;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants;
/**
 * This class models data for a SNOW/FOG/SAFESEAS trending plot.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec. 23, 2009  3424       zhao       Initial creation.
 * 
 * </pre>
 * 
 * @author zhao
 * @version 1.0
 */

public class ObTrendDataSet {

    /**
     * application name (snow, fog, safeseas, etc)
     */
    private final CommonConfig.AppName appName;

    /**
     * the zone ID
     */
    private final String zone;

    /**
     * the enumerated type representation of the trending variable
     */
    private final ObConst.VarName varName;
    
    /**
     * the enumerated type representation of product name for a trending variable
     */
    private final ObConst.ProductName productName; 

    /**
     * the red threshold
     */
    private float redThreshold = Float.NaN;

    /**
     * the second value in the case of dual-valued thresholds
     */
    private float redThreshold_2 = Float.NaN;

    /**
     * the yellow threshold
     */
    private float yellowThreshold = Float.NaN;

    /**
     * the second value in the case of dual-valued thresholds
     */
    private float yellowThreshold_2 = Float.NaN;

    /**
     * the lowest value of the variable in the trending plot
     */
    private float minValue = ObConst.MISSING;

    /**
     * the highest value of the variable in the trending plot
     */
    private float maxValue = ObConst.MISSING;

    /**
     * the trending data set key is observation time; value is Float object of
     * the variable's observation value
     */
    private final SortedMap<Date, Float> dataset;

    private final AbstractThresholdMgr thresholdMgr;

    /**
     * 'CLR' if ceiling value = 88888.88f
     */
    public String ceilingCond = "";

    /**
     * Constructor
     * 
     * @param zone
     *            zone ID
     * @param station
     *            station ID
     * @param varName
     *            variable name of enumerated-type
     * @param appName
     *            application name (snow, fog, safeseas) of enumerated-type
     */
    public ObTrendDataSet(String zone, ObConst.VarName varName, ObConst.ProductName productName, 
            CommonConfig.AppName appName, AbstractThresholdMgr thresholdMgr) {

        this.zone = zone;
        this.varName = varName;
        this.productName = productName;
        this.appName = appName;
        this.thresholdMgr = thresholdMgr;

        dataset = new TreeMap<Date, Float>();

        setThresholds();
    }

    private void setThresholds() {

        if (appName == AppName.FOG) {
            if (varName == VarName.VISIBILITY) {
                redThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.RED, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_VIS
                        .getXmlKey()) / 16;
                yellowThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.YELLOW, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_VIS
                        .getXmlKey()) / 16;
            } else if (varName == VarName.CEILING) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_CEILING
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_CEILING
                        .getXmlKey());
            } else if (varName == VarName.WIND_DIR) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_DIR_FROM
                        .getXmlKey());
                redThreshold_2 = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.RED, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_DIR_TO
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_DIR_FROM
                        .getXmlKey());
                yellowThreshold_2 = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.YELLOW, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_DIR_TO
                        .getXmlKey());
            } else if (varName == VarName.WIND_SPEED) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_WIND_SPEED
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_WIND_SPEED
                        .getXmlKey());
            } else if (varName == VarName.MAX_WIND_SPEED) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_PEAK_WIND
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_PEAK_WIND
                        .getXmlKey());
            } else if (varName == VarName.GUST_SPEED) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_GUST_SPEED
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_GUST_SPEED
                        .getXmlKey());
            } else if (varName == VarName.TEMPERATURE) {
                redThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.RED, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_TEMP
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.YELLOW, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_TEMP
                        .getXmlKey());
            } else if (varName == VarName.DEWPOINT) {
                redThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.RED, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_DEWPT
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.YELLOW, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_DEWPT
                        .getXmlKey());
            } else if (varName == VarName.DEWPOINT_DEPR) {
                redThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.RED, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_T_TD
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.YELLOW, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_T_TD
                        .getXmlKey());
            } else if (varName == VarName.RELATIVE_HUMIDITY) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_REL_HUMIDITY
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_REL_HUMIDITY
                        .getXmlKey());
            } else {
                System.err.println("Unknow variable name = " + varName
                        + " for FOG trend plot");
            }

        } else if (appName == AppName.SNOW) {

            if (varName == VarName.WIND_DIR) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_DIR_FROM
                        .getXmlKey());
                redThreshold_2 = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_DIR_TO
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_DIR_FROM
                        .getXmlKey());
                yellowThreshold_2 = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_DIR_TO
                        .getXmlKey());
            } else if (varName == VarName.WIND_SPEED) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_WIND_SPEED
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_WIND_SPEED
                        .getXmlKey());
            } else if (varName == VarName.MAX_WIND_SPEED) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_PEAK_WIND
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_PEAK_WIND
                        .getXmlKey());
            } else if (varName == VarName.GUST_SPEED) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_GUST_SPEED
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_GUST_SPEED
                        .getXmlKey());
            } else if (varName == VarName.TEMPERATURE) {
                redThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.RED, zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_TEMP
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.YELLOW, zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_TEMP
                        .getXmlKey());
            } else if (varName == VarName.DEWPOINT) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_DEWPT
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_DEWPT
                        .getXmlKey());
            } else if (varName == VarName.VISIBILITY) {
                redThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.RED, zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_VIS
                        .getXmlKey()) / 16;
                yellowThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.YELLOW, zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_VIS
                        .getXmlKey()) / 16;
            } else if (varName == VarName.SEA_LEVEL_PRESS) {
                redThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.RED, zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SLP
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr.getThresholdValue(
                        DataUsageKey.DISPLAY, ThresholdKey.YELLOW, zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SLP
                        .getXmlKey());
            } else if (varName == VarName.WIND_CHILL) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_WIND_CHILL
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_WIND_CHILL
                        .getXmlKey());
            } else if (varName == VarName.FROSTBITE_TIME) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_FROSTBITE
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_FROSTBITE
                        .getXmlKey());
            } else if (varName == VarName.HOURLY_PRECIP) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_HOURLY_PRECIP
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_HOURLY_PRECIP
                        .getXmlKey());
            } else if (varName == VarName.SNOW_DEPTH) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SNOW_DEPTH
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SNOW_DEPTH
                        .getXmlKey());
            } else if (varName == VarName.SNINCR_HOURLY) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SNINCR_HOURLY
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SNINCR_HOURLY
                        .getXmlKey());
            } else if (varName == VarName.SNINCR_TOTAL) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SNINCR_TOTAL
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SNINCR_TOTAL
                        .getXmlKey());
            } else {
                System.err.println("Unknow variable name = " + varName
                        + " for SNOW trend plot");
            }

        } else if (appName == AppName.SAFESEAS) {

            if (varName == VarName.WIND_DIR) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_DIR_FROM
                        .getXmlKey());
                redThreshold_2 = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_DIR_TO
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_DIR_FROM
                        .getXmlKey());
                yellowThreshold_2 = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_DIR_TO
                        .getXmlKey());
            } else if (varName == VarName.WIND_SPEED) {
            	if ( productName == ProductName.SCA ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_SCA_WIND_SPEED.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_SCA_WIND_SPEED.getXmlKey());
            	} else if ( productName == ProductName.GALE_WARNING ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_GALE_WIND_SPEED.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_GALE_WIND_SPEED.getXmlKey());
            	} else if ( productName == ProductName.STORM_WARNING ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_STORM_WIND_SPEED.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_STORM_WIND_SPEED.getXmlKey());
            	} else if ( productName == ProductName.HFWW ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_HFWW_WIND_SPEED.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_HFWW_WIND_SPEED.getXmlKey());
            	} else {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_WIND_SPEED.getXmlKey());
            		yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_WIND_SPEED.getXmlKey());
            	}
            } else if (varName == VarName.MAX_WIND_SPEED) {
            	if ( productName == ProductName.SCA ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_SCA_PEAK_WIND.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_SCA_PEAK_WIND.getXmlKey());
            	} else if ( productName == ProductName.GALE_WARNING ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_GALE_PEAK_WIND.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_GALE_PEAK_WIND.getXmlKey());
            	} else if ( productName == ProductName.STORM_WARNING ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_STORM_PEAK_WIND.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_STORM_PEAK_WIND.getXmlKey());
            	} else if ( productName == ProductName.HFWW ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_HFWW_PEAK_WIND.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_HFWW_PEAK_WIND.getXmlKey());
            	} else {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_PEAK_WIND.getXmlKey());
            		yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_PEAK_WIND.getXmlKey());
            	}
            } else if (varName == VarName.GUST_SPEED) {
            	if ( productName == ProductName.SCA ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_SCA_GUST_SPEED.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_SCA_GUST_SPEED.getXmlKey());
            	} else if ( productName == ProductName.GALE_WARNING ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_GALE_GUST_SPEED.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_GALE_GUST_SPEED.getXmlKey());
            	} else if ( productName == ProductName.STORM_WARNING ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_STORM_GUST_SPEED.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_STORM_GUST_SPEED.getXmlKey());
            	} else if ( productName == ProductName.HFWW ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_HFWW_GUST_SPEED.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_HFWW_GUST_SPEED.getXmlKey());
            	} else {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_GUST_SPEED.getXmlKey());
            		yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_GUST_SPEED.getXmlKey());
            	}
            } else if (varName == VarName.VISIBILITY) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_VIS
                        .getXmlKey()) / 16;
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_VIS
                        .getXmlKey()) / 16;
            } else if (varName == VarName.TEMPERATURE) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_TEMP
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_TEMP
                        .getXmlKey());
            } else if (varName == VarName.DEWPOINT) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_DEWPT
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_DEWPT
                        .getXmlKey());
            } else if (varName == VarName.SEA_LEVEL_PRESS) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_SLP
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_SLP
                        .getXmlKey());
            } else if (varName == VarName.SEA_SURFACE_TEMPERATURE) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_SST
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_SST
                        .getXmlKey());
            } else if (varName == VarName.WAVE_HEIGHT) {
            	if ( productName == ProductName.SCA ) {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_SCA_WAVE_HT.getXmlKey());
                    yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_SCA_WAVE_HT.getXmlKey());
            	} else {
            		redThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.RED,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_WAVE_HT.getXmlKey());
            		yellowThreshold = (float) thresholdMgr.getThresholdValue(DataUsageKey.DISPLAY,ThresholdKey.YELLOW,zone,MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_WAVE_HT.getXmlKey());
            	}
            } else if (varName == VarName.WAVE_STEEPNESS) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_WAVE_STEEP
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_WAVE_STEEP
                        .getXmlKey());
            } else if (varName == VarName.PRIM_SWELL_HT) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_HT
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_HT
                        .getXmlKey());
            } else if (varName == VarName.PRIM_SWELL_PD) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD
                        .getXmlKey());
            } else if (varName == VarName.PRIM_SWELL_DIR) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_FROM
                        .getXmlKey());
                redThreshold_2 = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_TO
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_FROM
                        .getXmlKey());
                yellowThreshold_2 = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_TO
                        .getXmlKey());
            } else if (varName == VarName.SEC_SWELL_HT) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_HT
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_HT
                        .getXmlKey());
            } else if (varName == VarName.SEC_SWELL_PD) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_PD
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_PD
                        .getXmlKey());
            } else if (varName == VarName.SEC_SWELL_DIR) {
                redThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_FROM
                        .getXmlKey());
                redThreshold_2 = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.RED,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_TO
                        .getXmlKey());
                yellowThreshold = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_FROM
                        .getXmlKey());
                yellowThreshold_2 = (float) thresholdMgr
                .getThresholdValue(
                        DataUsageKey.DISPLAY,
                        ThresholdKey.YELLOW,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_TO
                        .getXmlKey());
            } else {
                System.err.println("Unknow variable name = " + varName
                        + " for SAFESEAS trend plot");
            }

        }
    }

    /**
     * add a data point (a pair of observation time and observation value of the
     * trend variable)
     * 
     * @param obsTime
     *            observation time
     * @param varValue
     *            observation value of the variable
     */
    public void addDataPoint(Date obsTime, Float varValue) {
        if (varName == VarName.VISIBILITY && appName == AppName.SAFESEAS) {
			if (varValue != ObConst.MISSING) {
				varValue = varValue / TableUtil.milesPerNauticalMile;
			} else {
				varValue = ObConst.MISSING;
			}
        }
        // miss CLR and SKC in the Trend plot.
        if (varName == VarName.CEILING) {
            if (varValue == ObConst.CLR_SKY_FLOAT) {
                varValue = ObConst.MISSING;
                ceilingCond = this.setCeilingCond(ObConst.CLR_SKY_STRING);
            } else if (varValue == ObConst.SKC_SKY_FLOAT) {
                varValue = ObConst.MISSING;
                ceilingCond = this.setCeilingCond(ObConst.SKC_SKY_STRING);
            }
        }
        dataset.put(obsTime, varValue);
        updateMinMaxValues(varValue.floatValue());
    }

    private void updateMinMaxValues(float newValue) {
        if (varName == VarName.WIND_DIR || varName == VarName.PRIM_SWELL_DIR
                || varName == VarName.SEC_SWELL_DIR) {
            // nothing to do
            return;
        }
        if (newValue != ObConst.MISSING) {
            minValue = minValue == ObConst.MISSING ? newValue : Math.min(
                    minValue, newValue);
            maxValue = maxValue == ObConst.MISSING ? newValue : Math.max(
                    maxValue, newValue);
        }
    }

    /**
     * determine range of the y-axis of the trend plot based on min and max
     * values and that at least two threat levels must be shown on a trend plot
     * 
     * @return the lower and upper range limits of the y-axis of the trend plot;
     *         return null if no data available [0] = lower limit; [1] = upper
     *         limit [obsolete: use getYAxisMinMaxIncrement() Jan 8, 2010, zhao]
     */
    public float[] getRange() {
        float[] range = new float[2];
        if (varName == ObConst.VarName.WIND_DIR
                || varName == VarName.PRIM_SWELL_DIR
                || varName == VarName.SEC_SWELL_DIR) {
            // this module has nothing to do with wind direction
            return null;
        }
        if (minValue == ObConst.MISSING || maxValue == ObConst.MISSING
                || dataset.isEmpty()) {
            // no data yet
            return null;
        }

        // in case thresholds are not set (due to incompleteness of or bugs in
        // the code)
        if (Float.isNaN(redThreshold) || Float.isNaN(yellowThreshold)) {
            return new float[] { minValue - 0.2f * Math.abs(minValue),
                    maxValue + 0.2f * Math.abs(maxValue) };
        }

        // data is available, and minValue and maxValue have been set
        // (the thresholds have been set)

        float upperThreshold = Math.max(redThreshold, yellowThreshold);
        float lowerThreshold = Math.min(redThreshold, yellowThreshold);

        if (minValue < lowerThreshold && lowerThreshold < maxValue
                || minValue < upperThreshold && upperThreshold < maxValue
                || minValue < lowerThreshold && upperThreshold < maxValue) {
            // ( minValue < lowerThreshold < maxValue ) || ( minValue <
            // upperThreshold < maxValue) ||
            // ( minValue < the two thresholds < maxValue )
            // i.e., this is the case that there is at least one threshold
            // between the min and max values
            range[0] = minValue - 0.2f * Math.abs(minValue);
            range[1] = maxValue + 0.2f * Math.abs(maxValue);
        } else if (upperThreshold < minValue) {
            // both thresholds are below min value
            range[0] = upperThreshold - 0.2f * Math.abs(upperThreshold);
            range[1] = maxValue + 0.2f * Math.abs(maxValue);
        } else if (maxValue < lowerThreshold) {
            // max value is below both threshold
            range[0] = minValue - 0.2f * Math.abs(minValue);
            range[1] = lowerThreshold + 0.2f * Math.abs(lowerThreshold);
        } else if (lowerThreshold < minValue && upperThreshold < maxValue) {
            // min and max values are between the two threshold
            range[0] = lowerThreshold - 0.2f * Math.abs(lowerThreshold);
            range[1] = upperThreshold + 0.2f * Math.abs(upperThreshold);
        } else {
            // any other case? no!
        }

        return range;
    }

    /**
     * Determine the min and max values and the increment of the y-axis of the
     * trend plot based on (1) the min and max values of the observation data
     * and (2) at least two threat levels are shown on a trend plot
     * 
     * @return the min and max values and the increment of the y-axis of the
     *         trend plot; return null if no data available [0] = min; [1] =
     *         max; [2] = increment
     */
    public float[] getYAxisMinMaxIncrement() {
        if (varName == ObConst.VarName.WIND_DIR
                || varName == VarName.PRIM_SWELL_DIR
                || varName == VarName.SEC_SWELL_DIR) {
            // this module has nothing to do with wind direction
            return null;
        }
        if (minValue == ObConst.MISSING || maxValue == ObConst.MISSING
                || dataset.isEmpty()) {
            // no data yet
            return null;
        }

        float lowerThreshold;
        float upperThreshold;

        if (Float.isNaN(redThreshold) || Float.isNaN(yellowThreshold)) {
            // thresholds are not set (due to incompleteness of or bugs in the
            // code)
            lowerThreshold = minValue - 0.2f * Math.abs(minValue);
            upperThreshold = maxValue + 0.2f * Math.abs(maxValue);
        } else {
            // the thresholds have been set
            upperThreshold = Math.max(redThreshold, yellowThreshold);
            lowerThreshold = Math.min(redThreshold, yellowThreshold);
        }

        // Here, "lowerlimit", "upperLimit", and "increment" are the min, max
        // and increment that we want to calculate
        // "min" and "max" are temporary guesses for "lowerLimit" and
        // "upperLimit"
        // "minValue" and "maxValue" are the min and max values of the
        // observation data

        float lowerLimit = minValue, upperLimit = maxValue;
        float increment = (maxValue - minValue) / 5;

        float min = minValue;
        float max = maxValue;

        //Adjust the min/max value, so it will at least cover two colors (red/yellow/green)
        //there are six different cases: 1 above + 5 below
        if (maxValue <= lowerThreshold) //values are below the lower threshold
        	max = lowerThreshold * 1.1f;
        else if (minValue >= upperThreshold) //values are above upper threshold
        	min = upperThreshold * .9f;
        else if (maxValue < upperThreshold) {
        	max = upperThreshold ; //values covers across the lower threshold
        	if (minValue >= lowerThreshold)
        		min = lowerThreshold * .9f; //values are within the thresholds
        } else if (minValue > lowerThreshold)
        	min = lowerThreshold;  //values cover across the upper threshold

        float range = max - min;
        if (range > 5) {
            if (range > 1000) {
                increment = 200;
            } else if (range > 500) {
            	increment = 100;
            } else if (range > 200) {
            	increment = 50;
            } else if (range > 100) {
                increment = 20;
            } else if (range > 50) {
                increment = 10;
            } else if (range > 10) {
                increment = 5;
            } else {
                increment = 2;
            }

            lowerLimit = (int)min;
            upperLimit = (int)max;
            if (lowerLimit > min) {
                lowerLimit -= increment;
            }
            if (upperLimit < max) {
                upperLimit += increment;
            }
        } else {
            if (varName == ObConst.VarName.VISIBILITY) {
                if (range < 2) {
                    if (range < 0.5) {
                        increment = 0.125f;
                    } else {
                        increment = 0.25f;
                    }
                } else if (range <= 3) {
                    increment = 0.5f;
                } else {
                    increment = 1;
                }
                increment = 1000 * increment;
                lowerLimit = Float.valueOf(min * 1000) / increment * increment
                * 0.001f;
                upperLimit = Float.valueOf(max * 1000) / increment * increment
                * 0.001f;
                increment = increment / 1000;
                if (lowerLimit > min) {
                    lowerLimit -= increment;
                }
                if (upperLimit < max) {
                    upperLimit += increment;
                }
            } else {
                if (varName == ObConst.VarName.WAVE_HEIGHT) {
                    increment = 0.5f;
                } else {
                    increment = 1;
                }
                increment = 10 * increment;
                lowerLimit = Float.valueOf(min * 10) / increment * increment
                * 0.1f;
                upperLimit = Float.valueOf(max * 10) / increment * increment
                * 0.1f;
                increment = increment / 10;
                if (lowerLimit > min) {
                    lowerLimit -= increment;
                }
                if (upperLimit < max) {
                    upperLimit += increment;
                }
            }
        }

        // allow lowerLimit go below zero for temperature-type variables;
        // perform eror check otherwise
        if (varName != ObConst.VarName.TEMPERATURE
                && varName != ObConst.VarName.WIND_CHILL
                && varName != ObConst.VarName.DEWPOINT) {
            if (lowerLimit < 0) {
                lowerLimit = 0;
            }
        }
        // (SK) SLP limits (20% too much for SLP range)
        if (varName == ObConst.VarName.SEA_LEVEL_PRESS) {
            increment = (float) 1.0;
            lowerLimit = minValue - increment;
            upperLimit = maxValue;
        }
        // (SK) Rel. Humidity limits
        if (varName == ObConst.VarName.RELATIVE_HUMIDITY) {
            if (maxValue >= 100.0) {
                upperLimit = (float) 100.0;
            }
        }
        return new float[] { lowerLimit, upperLimit, increment };
    }

    /**
     * 
     * @return the min and max values of the trend variable [0] = min; [1] = max
     */
    public float[] getMinMaxValues() {
        return new float[] { minValue, maxValue };
    }

    /**
     * 
     * @return thresholds ([0]=red, [1]=yellow)
     */
    public float[] getSingleValuedThresholds() {
        return new float[] { redThreshold, yellowThreshold };
    }

    /**
     * 
     * @return thresholds ([0]=red, [1]=red_2, [2]=yellow, [3]=yellow_2)
     */
    public float[] getDualValuedThresholds() {
        return new float[] { redThreshold, redThreshold_2, yellowThreshold,
                yellowThreshold_2 };
    }

    /**
     * 
     * @return the trend data set in the form of a SortedMap<Date,Float> object
     */
    public SortedMap<Date, Float> getDataSet() {
        return dataset;
    }

    /**
     * 
     * @return true if no data availe
     */
    public boolean isEmpty() {
        return dataset.isEmpty();
    }

    /**
     * 
     * @return number of data points (obsTime-value mappings) contained in
     *         dataset
     */
    public int numberOfDataPoints() {
        return dataset.size();
    }

    /**
     * @return ceilingCond: "" or "CLR"
     */
    public String getCeilingCond() {
        return ceilingCond;
    }

    /**
     * @param ceilingCond
     * @return
     */
    public String setCeilingCond(String ceilingCond) {
        return this.ceilingCond = ceilingCond;
    }

}
