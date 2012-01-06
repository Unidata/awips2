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
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.ChosenAppKey;
import com.raytheon.uf.common.monitor.data.ObConst.ReportType;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.util.ConfigUtil;
import com.raytheon.uf.viz.monitor.util.ObUtil;

/**
 * This class generates an observation report from the actual observation
 * (sfcobs)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2009  2047       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class GenerateMarineObReport implements IObReportable {

    /**
     * This method generates an observation report from the actual observation
     * (sfcobs)
     * 
     * @param marine
     *            -- the marine record
     * @param obReport
     *            -- the observation report
     * @param chosenAppKey
     *            -- the chosen application key
     * @return -- the generated observation report
     */
    @Override
    public ObReport generateObReport(PluginDataObject marine,
            ObReport obReport, ChosenAppKey chosenAppKey) {
        // Generate the observation report.
        obReport
                .setObservationTime(((ObsCommon) marine).getTimeObs().getTime());
        obReport.setPlatformId(((ObsCommon) marine).getStationId());
        obReport.setStationary(true);
        obReport.setLatitude((float) ((ObsCommon) marine).getLatitude());
        obReport.setLongitude((float) ((ObsCommon) marine).getLongitude());
        try {
            obReport.setWindSpeed(((ObsCommon) marine).getWindSpeed()
                    .floatValue());
        } catch (Exception e1) {
            obReport.setWindSpeed(0);
        }
        try {
            obReport.setWindDir(((ObsCommon) marine).getWindDirection());
        } catch (Exception e) {
            obReport.setWindDir(0);
        }
        obReport.setHighResWaveHeight(ObConst.MISSING);
        obReport.setWaveSteepness(ObConst.MISSING);
        try {
            obReport.setVisibility(ConfigUtil
                    .lookupVisibility(((ObsCommon) marine).getHorzVisibility()
                            .floatValue()));
        } catch (Exception e) {
            obReport.setVisibility(0);
        }
        try {
            obReport
                    .setTemperature(((ObsCommon) marine).getTemp().floatValue());
        } catch (Exception e) {
            obReport.setTemperature(ObConst.MISSING);
        }
        obReport.setSeaLevelPress(ObConst.MISSING);
        obReport.setWavePeriod(ObConst.MISSING);
        obReport.setWindGust(ObConst.MISSING);
        obReport.setPSwellHeight(ObConst.MISSING);
        obReport.setPSwellPeriod(ObConst.MISSING);
        obReport.setPSwellDir(ObConst.MISSING);
        obReport.setSSwellHeight(ObConst.MISSING);
        obReport.setSSwellPeriod(ObConst.MISSING);
        obReport.setSSwellDir(ObConst.MISSING);
        try {
            obReport.setPressure(((ObsCommon) marine).getPressureAltimeter());
        } catch (Exception e) {
            obReport.setPressure(ObConst.MISSING);
        }
        try {
            obReport.setPressureChange(((ObsCommon) marine)
                    .getPressureAltimeter());
        } catch (Exception e) {
            obReport.setPressureChange(ObConst.MISSING);
        }
        obReport.setPressChangeChar((short) 0);
        try {
            obReport.setDewpoint(((ObsCommon) marine).getDwpt().floatValue());
        } catch (Exception e) {
            obReport.setDewpoint(ObConst.MISSING);
        }
        obReport.setMaxWindSpeed(ObConst.MISSING);
        try {
            obReport.setHourlyPrecip(((ObsCommon) marine).getWx_past_1());
        } catch (Exception e) {
            obReport.setHourlyPrecip(ObConst.MISSING);
        }
        try {
            obReport.setPresentWx(((ObsCommon) marine).getWx_past_1()
                    .toString());
        } catch (Exception e) {
            obReport.setPresentWx("");
        }
        try {
            obReport.setRawMessage(((ObsCommon) marine).getMessageData()
                    .toString());
        } catch (Exception e) {
            obReport.setRawMessage("");
        }
        obReport.setReportType(findReportType(((ObsCommon) marine)
                .getReportType()));
        if (chosenAppKey == ChosenAppKey.SNOW) {
            obReport = ObUtil.generateObReportSnow(obReport,
                    ((ObsCommon) marine).getMessageData().toString());
        } else if (chosenAppKey == ChosenAppKey.FOG) {
            try {
                obReport.setRelativeHumidity(((ObsCommon) marine)
                        .getTotalCloudCover());
            } catch (Exception e) {
                obReport.setRelativeHumidity(ObConst.MISSING);
            }
            try {
                obReport.setCeiling(((ObsCommon) marine).getVertVisibility());
            } catch (Exception e) {
                obReport.setCeiling(ObConst.MISSING);
            }
        }

        return obReport;
    }

    // Method that finds the report type enumeration of a marine report
    private ReportType findReportType(int rt) {
        ReportType reportType = ReportType.MARINE;
        switch (rt) {
        case IDecoderConstants.SYNOPTIC_FIXED_LAND:
            reportType = ReportType.SYNOPTIC_FIXED_LAND;
            break;
        case IDecoderConstants.SYNOPTIC_MOBILE_LAND:
            reportType = ReportType.SYNOPTIC_MOBILE_LAND;
            break;
        case IDecoderConstants.SYNOPTIC_SHIP:
            reportType = ReportType.SYNOPTIC_SHIP;
            break;
        case IDecoderConstants.SYNOPTIC_CMAN:
            reportType = ReportType.SYNOPTIC_CMAN;
            break;
        case IDecoderConstants.SYNOPTIC_MOORED_BUOY:
            reportType = ReportType.SYNOPTIC_MOORED_BUOY;
            break;
        case IDecoderConstants.DRIFTING_BUOY:
            reportType = ReportType.DRIFTING_BUOY;
            break;
        case IDecoderConstants.SYNOPTIC_MAROB:
            reportType = ReportType.SYNOPTIC_MAROB;
            break;
        }

        return reportType;
    }
}
