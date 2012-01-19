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
package com.raytheon.uf.viz.monitor;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.dataplugin.obs.metar.util.WeatherCondition;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.ChosenAppKey;
import com.raytheon.uf.common.monitor.data.ObConst.ReportType;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.util.ObUtil;

/**
 * This class generates an observation report from the actual observation
 * (metar)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 2, 2009  2047       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class GenerateMetarObReport implements IObReportable {

    /**
     * This method generates an observation report from the actual observation
     * (metar)
     * 
     * @param metar
     *            -- the metar record
     * @param obReport
     *            -- the observation report
     * @param chosenAppKey
     *            -- the chosen application key
     * @return -- the generated observation report
     */
    @Override
    public ObReport generateObReport(PluginDataObject report, ObReport obReport,
            ChosenAppKey chosenAppKey) {
        // Generate the observation report.
        if(report instanceof MetarRecord) {
            MetarRecord metar = (MetarRecord) report;
            obReport.setObservationTime(metar.getTimeObs()
                    .getTime());
            obReport.setPlatformId(metar.getStationId());
            obReport.setStationary(true);
            obReport.setLatitude((float) metar.getLatitude());
            obReport.setLongitude((float) metar.getLongitude());
            obReport.setWindSpeed(metar.getWindSpeed());
            
            try {
                obReport.setWindDir(Float.parseFloat(metar
                        .getWindDir()));
            } catch (Exception e) {
                obReport.setWindDir(0);
            }
            
            obReport.setHighResWaveHeight(ObConst.MISSING);
            obReport.setWaveSteepness(ObConst.MISSING);
            
            try {
                obReport.setVisibility(metar.getVisibility());
            } catch (Exception e) {
                obReport.setVisibility(0);
            }
            
            obReport.setSeaLevelPress(metar.getSeaLevelPress());
            obReport.setWavePeriod(ObConst.MISSING);
            obReport.setWindGust(metar.getWindGust());
            obReport.setPSwellHeight(ObConst.MISSING);
            obReport.setPSwellPeriod(ObConst.MISSING);
            obReport.setPSwellDir(ObConst.MISSING);
            obReport.setSSwellHeight(ObConst.MISSING);
            obReport.setSSwellPeriod(ObConst.MISSING);
            obReport.setSSwellDir(ObConst.MISSING);
            obReport.setPressure(metar.getAltimeter());
            obReport.setPressureChange(metar.getPressChange3Hour());
            
            try {
                obReport.setPressChangeChar(Short.parseShort(metar
                        .getPressChangeChar()));
            } catch (NumberFormatException e) {
                obReport.setPressChangeChar((short) 0);
            }
            
            obReport.setDewpoint(metar.getDewPoint());
            obReport.setTemperature(metar.getTemperature());
            obReport.setMaxWindSpeed(ObConst.MISSING);
           
            obReport.setHourlyPrecip(metar.getPrecip1Hour());
            obReport.setPresentWx(WeatherCondition.toCanonicalForm(metar.getWeatherCondition()));
            obReport.setRawMessage(metar.getMessageData());
            obReport.setReportType(ReportType.METAR);
            
            if (chosenAppKey == ChosenAppKey.SNOW) {
                obReport = ObUtil.generateObReportSnow(obReport,
                        metar.getMessageData());
            } else if (chosenAppKey == ChosenAppKey.FOG) {
                obReport = ObUtil.generateObReportFog(obReport,
                        metar.getSkyCoverage(),
                        metar.getVertVisibility());
            }
            
            // conversions
            if (obReport.getCeiling() != ObConst.MISSING) {
                obReport.setCeiling(obReport.getCeiling()/100);
            }
            if (metar.getDewPoint() != ObConst.MISSING) {
                //convert to fahrenheit
                obReport.setDewpoint((metar.getDewPoint()*(9f/5f))+32.0f);
            }
            if (metar.getTemperature() != ObConst.MISSING) {
                //convert to fahrenheit
               obReport.setTemperature((metar.getTemperature()*(9f/5f))+32.0f);
            }
            // set the depression
            if (obReport.getTemperature() != ObConst.MISSING &&
                obReport.getDewpoint() != ObConst.MISSING) {
                obReport.setDewpointDepr(obReport.getTemperature()-obReport.getDewpoint());
            }
        }
        
        return obReport;
    }
}
