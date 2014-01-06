/**
 * 
 */
package com.raytheon.uf.viz.monitor;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.ReportType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.monitor.data.ObReport;

/**
 * Generates FSSObs Report for tables.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         skorolev     Initial creation
 * May 15, 2012 14510      zhao         Modified generateObReport()
 * Jan 06, 2014  2653      skorolev     Included SNOW data into ObReport.
 * 
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */
public class GenerateFSSObReport {
    private static IUFStatusHandler statusHandler = UFStatus
            .getHandler(GenerateFSSObReport.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.IObReportable#generateObReport(com.raytheon
     * .uf.common.dataplugin.PluginDataObject,
     * com.raytheon.uf.viz.monitor.data.ObReport,
     * com.raytheon.uf.common.monitor.data.ObConst.ChosenAppKey)
     */
    public static ObReport generateObReport(PluginDataObject report) {
        // Generate the observation report.
        ObReport obReport = new ObReport();
        FSSObsRecord metar = (FSSObsRecord) report;
        try {
            obReport.setObservationTime(metar.getTimeObs().getTime());
            obReport.setRefHour(metar.getRefHour().getTime());
        } catch (Exception e) {
            statusHandler
                    .error("Warning: missing obsTime or refHour at getTimeObs() when processing obs data; "
                            + e
                            + "; trying to set obsTime and refHour from dataURI");
            obReport.setTimesFromFssobDataURI(report.getDataURI());
        }
        obReport.setPlatformId(metar.getPlatformId());
        obReport.setStationary(true);
        obReport.setLatitude((float) metar.getLatitude());
        obReport.setLongitude((float) metar.getLongitude());
        // Table data:
        obReport.setCeiling(metar.getCeiling());
        obReport.setWindDir(metar.getWindDir());
        obReport.setWindSpeed(metar.getWindSpeed());
        obReport.setMaxWindSpeed(metar.getMaxWindSpeed());
        obReport.setWindGust(metar.getWindGust());
        obReport.setRelativeHumidity(metar.getRelativeHumidity());
        try {
            obReport.setVisibility(metar.getVisibility());
        } catch (Exception e) {
            obReport.setVisibility(0);
        }
        obReport.setDewpoint(metar.getDewpoint());
        obReport.setTemperature(metar.getTemperature());
        obReport.setDewpointDepr(metar.getDewpointDepr());

        obReport.setPresentWx(getPrWX(metar.getPresWeather()));

        obReport.setHighResWaveHeight(ObConst.MISSING);
        obReport.setWaveSteepness(metar.getWaveSteepness());

        obReport.setSeaLevelPress(metar.getSeaLevelPress());
        obReport.setWavePeriod(metar.getWavePeriod());
        obReport.setWindGust(metar.getWindGust());
        obReport.setPSwellHeight(metar.getPrimarySwellWaveHeight().floatValue());
        obReport.setPSwellPeriod(metar.getPrimarySwellWavePeriod());
        obReport.setPSwellDir(metar.getPrimarySwellWaveDir().floatValue());
        obReport.setSSwellHeight(metar.getSecondarySwellWaveHeight()
                .floatValue());
        obReport.setSSwellPeriod(metar.getSecondarySwellWavePeriod());
        obReport.setSSwellDir(metar.getSecondarySwellWaveDir().floatValue());
        obReport.setPressure(metar.getPressureAltimeter());
        obReport.setPressureChange(metar.getPressChange3Hour());
        try {
            obReport.setPressChangeChar(Short.parseShort(metar
                    .getPressChangeChar()));
        } catch (NumberFormatException e) {
            obReport.setPressChangeChar((short) 0);
        }

        obReport.setHourlyPrecip(metar.getHourlyPrecip());

        obReport.setRawMessage(metar.getRawMessage());
        obReport.setReportType(ReportType.METAR);
        obReport.setSnincrHourly(metar.getSnincrHourly());
        obReport.setSnincrTotal(metar.getSnincrTotal());
        obReport.setFrostbiteTime(metar.getFrostbiteTime());
        obReport.setWindChill(metar.getWindChill());
        obReport.setSnowDepth(metar.getSnowDepth());
        obReport.setSeaSurfaceTemp(metar.getSeaSurfaceTemp());
        return obReport;
    }

    /**
     * Construct Present Weather string.
     * 
     * @param presWeather
     * @return
     */
    private static String getPrWX(String[] presWeather) {
        StringBuffer prWx = new StringBuffer();
        for (int i = presWeather.length - 1; i >= 0; i--) {
            if (presWeather[i] != "") {
                prWx.append(presWeather[i]);
            }
        }

        return prWx.toString();
    }

}
