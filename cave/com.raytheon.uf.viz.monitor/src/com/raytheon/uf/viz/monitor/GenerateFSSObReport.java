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
 * Sep 20, 2015  3873      skorolev     Added IsStationary and getReportType.
 * Dec 02, 2015  3873      dhladky      Fixed missing parameters.
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
        FSSObsRecord fssData = (FSSObsRecord) report;

        try {
            obReport.setObservationTime(fssData.getTimeObs().getTime());
            obReport.setRefHour(fssData.getRefHour().getTime());
        } catch (Exception e) {
            statusHandler
                    .error("Warning: missing obsTime or refHour at getTimeObs() when processing obs data; "
                            + e
                            + "; trying to set obsTime and refHour from dataURI");
            obReport.setTimesFromFssobDataURI(report.getDataURI());
        }
        obReport.setPlatformId(fssData.getPlatformId());
        obReport.setStationary(fssData.isStationary());
        obReport.setLatitude((float) fssData.getLatitude());
        obReport.setLongitude((float) fssData.getLongitude());
        // Table data:
        obReport.setCeiling(fssData.getCeiling());
        obReport.setWindDir(fssData.getWindDir());
        obReport.setWindSpeed(fssData.getWindSpeed());
        obReport.setMaxWindSpeed(fssData.getMaxWindSpeed());
        obReport.setWindGust(fssData.getWindGust());
        obReport.setRelativeHumidity(fssData.getRelativeHumidity());
        try {
            obReport.setVisibility(fssData.getVisibility());
        } catch (Exception e) {
            obReport.setVisibility(0);
        }
        obReport.setDewpoint(fssData.getDewpoint());
        obReport.setTemperature(fssData.getTemperature());
        obReport.setDewpointDepr(fssData.getDewpointDepr());

        obReport.setPresentWx(getPrWX(fssData.getPresWeather()));

        obReport.setHighResWaveHeight(ObConst.MISSING);
        obReport.setWaveSteepness(fssData.getWaveSteepness());

        obReport.setSeaLevelPress(fssData.getSeaLevelPress());
        obReport.setWavePeriod(fssData.getWavePeriod());
        obReport.setWindGust(fssData.getWindGust());
        obReport.setPSwellHeight(fssData.getPrimarySwellWaveHeight()
                .floatValue());
        obReport.setPSwellPeriod(fssData.getPrimarySwellWavePeriod());
        obReport.setPSwellDir(fssData.getPrimarySwellWaveDir().floatValue());
        obReport.setSSwellHeight(fssData.getSecondarySwellWaveHeight()
                .floatValue());
        obReport.setSSwellPeriod(fssData.getSecondarySwellWavePeriod());
        obReport.setSSwellDir(fssData.getSecondarySwellWaveDir().floatValue());
        obReport.setPressure(fssData.getPressureAltimeter());
        obReport.setPressureChange(fssData.getPressChange3Hour());
        try {
            obReport.setPressChangeChar(Short.parseShort(fssData
                    .getPressChangeChar()));
        } catch (NumberFormatException e) {
            obReport.setPressChangeChar((short) 0);
        }

        obReport.setHourlyPrecip(fssData.getHourlyPrecip());

        obReport.setRawMessage(fssData.getRawMessage());
        ReportType reportType = getReportType(fssData.getReportType());
        obReport.setReportType(reportType);
        obReport.setSnincrHourly(fssData.getSnincrHourly());
        obReport.setSnincrTotal(fssData.getSnincrTotal());
        obReport.setFrostbiteTime(fssData.getFrostbiteTime());
        obReport.setWindChill(fssData.getWindChill());
        obReport.setSnowDepth(fssData.getSnowDepth());
        obReport.setSeaSurfaceTemp(fssData.getSeaSurfaceTemp());
        return obReport;
    }

    /**
     * Retrieve the type of the report.  ReportType Enumeration
     */
    private static ReportType getReportType(String reportType) {
        if (reportType == null) {
            reportType = "";
        }
        switch (reportType) {
        case "1003": 
            return ReportType.SYNOPTIC_SHIP;
        case "1004": 
            return ReportType.SYNOPTIC_CMAN;
        case "1005": 
            return ReportType.SYNOPTIC_MOORED_BUOY;
        case "1006": 
            return ReportType.DRIFTING_BUOY;
        case "1007": 
            return ReportType.MARITIME;
        case "SPECI": 
            return ReportType.SPECI;
        case "METAR": 
            return ReportType.METAR;
        case "MESONET": 
            return ReportType.MESONET;
        default:  
            return ReportType.METAR;
        }
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
