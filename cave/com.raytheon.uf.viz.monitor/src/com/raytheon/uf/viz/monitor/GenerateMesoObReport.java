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
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.ChosenAppKey;
import com.raytheon.uf.common.monitor.data.ObConst.ReportType;
import com.raytheon.uf.edex.plugin.mesowest.common.MESOWestRecord;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.util.ObUtil;

/**
 * This class generates an observation report from the actual observation
 * (mesowest)
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

public class GenerateMesoObReport implements IObReportable {

    /**
     * This method generates an observation report from the actual observation
     * (mesowest)
     * 
     * @param meso
     *            -- the mesonet record
     * @param obReport
     *            -- the observation report
     * @param chosenAppKey
     *            -- the chosen application key
     * @return -- the generated observation report
     */
    @Override
    public ObReport generateObReport(PluginDataObject meso, ObReport obReport,
            ChosenAppKey chosenAppKey) {
        // Generate the observation report.
        obReport.setObservationTime(((MESOWestRecord) meso).getTimeObs()
                .getTime());
        obReport.setPlatformId(((MESOWestRecord) meso).getStationId());
        obReport.setStationary(true);
        obReport.setLatitude((float) ((MESOWestRecord) meso).getLatitude());
        obReport.setLongitude((float) ((MESOWestRecord) meso).getLongitude());
        obReport.setWindSpeed(((MESOWestRecord) meso).getWindSpeed()
                .floatValue());
        try {
            obReport.setWindDir(((MESOWestRecord) meso).getWindDirection()
                    .floatValue());
        } catch (Exception e) {
            obReport.setWindDir(0);
        }
        obReport.setHighResWaveHeight(ObConst.MISSING);
        obReport.setWaveSteepness(ObConst.MISSING);
        obReport.setVisibility(0);

        obReport.setSeaLevelPress(((MESOWestRecord) meso).getSeaLevelPressure().floatValue());
        obReport.setTemperature(((MESOWestRecord) meso).getTemp().floatValue());
        obReport.setWavePeriod(ObConst.MISSING);
        obReport
                .setWindGust(((MESOWestRecord) meso).getWindGust().floatValue());
        obReport.setPSwellHeight(ObConst.MISSING);
        obReport.setPSwellPeriod(ObConst.MISSING);
        obReport.setPSwellDir(ObConst.MISSING);
        obReport.setSSwellHeight(ObConst.MISSING);
        obReport.setSSwellPeriod(ObConst.MISSING);
        obReport.setSSwellDir(ObConst.MISSING);
        obReport.setPressure(((MESOWestRecord) meso).getAltimeter()
                .floatValue());
        obReport.setPressureChange(((MESOWestRecord) meso).getPressure()
                .floatValue());

        obReport.setPressChangeChar((short) 0);

        obReport.setDewpoint(((MESOWestRecord) meso).getDwpt().floatValue());
        obReport.setMaxWindSpeed(ObConst.MISSING);
        obReport.setHourlyPrecip(((MESOWestRecord) meso).getPrecip_01H()
                .floatValue());

        obReport.setRawMessage(((MESOWestRecord) meso).getMessageData()
                .toString());
        obReport.setReportType(ReportType.MESONET);
        if (chosenAppKey == ChosenAppKey.SNOW) {
            obReport = ObUtil.generateObReportSnow(obReport,
                    ((MESOWestRecord) meso).getMessageData().toString());
        } else if (chosenAppKey == ChosenAppKey.FOG) {
            obReport = ObUtil.generateObReportFog(obReport, null, 0);

        }

        return obReport;
    }

}
