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

import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.xml.AreaIdXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.CellType;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.ObsHistType;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;

/**
 * Moved some common utility methods related to
 * TableData/TableRowData/TableCellData in FogMonitor/SowMonitor/SafeseasMonitor
 * to this place.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec. 3, 2009  3424       zhao       Initial creation.
 * Jan 25, 2010  4281       wkwock/Zhao Added history-table-row-data related modules
 * May 23, 2012  14410      zhao       Modified getCellTypeForBlizWarn and getCellTypeForHsnowWarn modules
 * Feb 28, 2013  14410      zhao       Modified getCellTypeForBlizWarn
 * May 23, 2014  3086       skorolev   Corrected ObsHistType. Cleaned code.
 * Sep 18, 2015  3873       skorolev   Added coordinates in the hover text for a newly added zones.Corrected code for Fog and SNOW table data.
 * Oct 22, 2015  3873       dhladky    Cached off the zone/station hover texts.
 * Dec 26, 2015  5114       skorolev   Replaced MonitorConfigConstants import.
 * 
 * </pre>
 * 
 * @author zhao
 * @version 1.0
 */

public final class TableUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TableUtil.class);

    /** Conversion coefficient of nautical miles to statute miles. */
    public static final float milesPerNauticalMile = 1.15078f;

    /** Singleton instance of this class */
    private static TableUtil tableUtil = new TableUtil();

    /** Hover text lookup map **/
    public Map<String, String> zoneHoverTextMap = null;

    /** station text lookup map **/
    public Map<String, String> stationHoverTextMap = null;

    /**
     * Actual initialization if necessary
     * 
     * @return
     */
    public static synchronized TableUtil getInstance() {
        if (tableUtil == null) {
            tableUtil = new TableUtil();
        }

        return tableUtil;
    }

    private TableUtil() {
        zoneHoverTextMap = new ConcurrentHashMap<String, String>();
        stationHoverTextMap = new ConcurrentHashMap<String, String>();
    }

    /**
     * Returns the nominal time for a caller-specified time (to be consistent
     * with D2D, here "Nominal time" is defined as the hour of an one-hour
     * interval from 30 minutes before the hour to 30 minutes after the hour)
     * 
     * @param time
     * @return
     */
    public static Date getNominalTime(Date time) {
        Calendar cal = Calendar.getInstance();
        cal.setTime(time);
        int hour = cal.get(Calendar.HOUR_OF_DAY);
        int min = cal.get(Calendar.MINUTE);
        if (min > 30 && min < 60) {
            cal.set(Calendar.HOUR_OF_DAY, hour + 1);
        }
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        return cal.getTime();
    }

    /**
     * Gets Fog Table Row Data.
     * 
     * @param areaId
     * @param zone
     * @param report
     * @param tm
     *            Abstract Threshold Manager
     * @param algCellType
     * @return
     */
    public static TableRowData getFogTableRowData(String areaId, String zone,
            ObReport report, AbstractThresholdMgr tm, CellType algCellType) {

        TableRowData tblRowData = new TableRowData(CommonConfig.AppName.FOG);

        boolean isZone = false;
        if (areaId.equals(zone)) {
            isZone = true;
        }

        DataUsageKey dataUsageKey;
        if (tm.getDataUsageKey() != null) {
            dataUsageKey = tm.getDataUsageKey();
        } else {
            dataUsageKey = DataUsageKey.DISPLAY;
        }

        String hoverText = "";
        if (isZone) {
            AreaIdXML zoneXML = tm.areaConfigMgr.getAreaXml(zone);
            if (zoneXML != null) {
                hoverText = getZoneHoverText(zoneXML);
            }
        } else {
            hoverText = getStationHoverText(areaId);
        }
        // zone or station ID
        tblRowData.setTableCellData(0, new TableCellData(areaId, hoverText,
                CellType.AreaId, false));

        // visibility
        float visValue = report.getVisibility(); // vis value in miles
        if (visValue == ObConst.MISSING) {
            tblRowData.setTableCellData(1, new TableCellData("",
                    CellType.NotAvailable, true));
        } else {
            RangesUtil util = RangesUtil.getInstance();
            String visString = util.getVisStringFromMiles(visValue);
            visValue = visValue * 16.0f; // vis in units of "miles/16"; this is
                                         // used to compare with Red/Yellow
                                         // threshold values
            String mccVIS = "";
            if (dataUsageKey == DataUsageKey.DISPLAY) {
                mccVIS = MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_VIS
                        .getXmlKey();
            } else {
                mccVIS = MonitorConfigConstants.FogMonitor.FOG_MONITOR_METEO_VIS
                        .getXmlKey();
            }

            TableCellData visCellData = new TableCellData(visString,
                    tm.getThresholdValueCellType(dataUsageKey, zone, mccVIS,
                            visValue), true);

            visCellData.setValue(visValue); // visValue, instead of visString,
                                            // will be used for sorting

            tblRowData.setTableCellData(1, visCellData);
        }

        // PresWx
        if (isZone) {
            // zone table: PreWx not monitored at zone/county level
            tblRowData.setTableCellData(2, new TableCellData("",
                    CellType.NotMonitored, true));
        } else {
            // station table
            if (report.getPresentWx().equals("")
                    || report.getPresentWx() == null) {
                tblRowData.setTableCellData(2, new TableCellData("",
                        CellType.NotAvailable, true));
            } else {
                tblRowData.setTableCellData(2,
                        new TableCellData(report.getPresentWx(),
                                CellType.PresWx, false));
            }
        }
        // ceiling
        float seilingVal = report.getCeiling();
        if (seilingVal == ObConst.MISSING) {
            tblRowData.setTableCellData(3, new TableCellData("",
                    CellType.NotAvailable, true));
        } else if (seilingVal == ObConst.CLR_SKY_CONDITION) {
            tblRowData.setTableCellData(3, new TableCellData("CLR", CellType.G,
                    true));
        } else if (seilingVal == ObConst.SKC_SKY_CONDITION) {
            tblRowData.setTableCellData(3, new TableCellData("SKC", CellType.G,
                    true));
        } else {
            tblRowData
                    .setTableCellData(
                            3,
                            new TableCellData(
                                    Math.round(new Float(seilingVal)),
                                    tm.getThresholdValueCellType(
                                            DataUsageKey.DISPLAY,
                                            zone,
                                            MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_CEILING
                                                    .getXmlKey(), seilingVal),
                                    true));
        }
        // wind direction
        if (isZone) { // zone table
            tblRowData
                    .setTableCellData(
                            4,
                            new TableCellData(
                                    "",
                                    tm.getDirectionalThresholdValueCellType(
                                            DataUsageKey.DISPLAY,
                                            zone,
                                            MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_DIR_FROM
                                                    .getXmlKey(),
                                            MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_DIR_TO
                                                    .getXmlKey(), report
                                                    .getWindDir()), true));
        } else { // station table
            tblRowData
                    .setTableCellData(
                            4,
                            new TableCellData(
                                    Math.round(new Float(report.getWindDir())),
                                    tm.getDirectionalThresholdValueCellType(
                                            DataUsageKey.DISPLAY,
                                            zone,
                                            MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_DIR_FROM
                                                    .getXmlKey(),
                                            MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_DIR_TO
                                                    .getXmlKey(), report
                                                    .getWindDir()), true));
        }

        tblRowData
                .setTableCellData(
                        5,
                        new TableCellData(
                                Math.round(new Float(report.getWindSpeed())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_WIND_SPEED
                                                .getXmlKey(), report
                                                .getWindSpeed()), true));

        tblRowData
                .setTableCellData(
                        6,
                        new TableCellData(
                                Math.round(new Float(report.getMaxWindSpeed())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_PEAK_WIND
                                                .getXmlKey(), report
                                                .getMaxWindSpeed()), true));

        tblRowData
                .setTableCellData(
                        7,
                        new TableCellData(
                                Math.round(new Float(report.getWindGust())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_GUST_SPEED
                                                .getXmlKey(), report
                                                .getWindGust()), true));

        int tmp = Math.round(new Float(report.getTemperature()));
        int dpt = Math.round(new Float(report.getDewpoint()));
        tblRowData.setTableCellData(
                8,
                new TableCellData(tmp, tm.getThresholdValueCellType(
                        DataUsageKey.DISPLAY, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_TEMP
                                .getXmlKey(), report.getTemperature()), true));

        tblRowData.setTableCellData(
                9,
                new TableCellData(dpt, tm.getThresholdValueCellType(
                        DataUsageKey.DISPLAY, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_DEWPT
                                .getXmlKey(), report.getDewpoint()), true));

        tblRowData
                .setTableCellData(
                        10,
                        new TableCellData(
                                Math.round(new Float(report.getDewpointDepr())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_T_TD
                                                .getXmlKey(), report
                                                .getDewpointDepr()), true));
        int relHum = 0;
        if ((tmp - dpt) == 0) {
            relHum = new Integer(100);
        } else {
            relHum = Math.round(new Float(report.getRelativeHumidity()));
        }
        tblRowData
                .setTableCellData(
                        11,
                        new TableCellData(
                                relHum,
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_REL_HUMIDITY
                                                .getXmlKey(), report
                                                .getRelativeHumidity()), true));

        if (areaId.equals(zone)) {
            // zone table: ALG at zone level
            tblRowData.setTableCellData(12, new TableCellData("", algCellType,
                    true));
        } else {
            // station table: fog not monitored
            tblRowData.setTableCellData(12, new TableCellData("",
                    CellType.NotAvailable, true));
        }

        return tblRowData;
    }

    /**
     * Returns a SafeSeas Zone/Station TableRowData object.
     * 
     * @param areaId
     *            (station for station table dialog or zone for zone table
     *            dialog)
     * @param zone
     * @param report
     * @param tm
     *            Abstract Threshold Manager
     * @param fogCellType
     * @return
     */
    public static TableRowData getSafeseasTableRowData(String areaId,
            String zone, ObReport report, AbstractThresholdMgr tm,
            CellType fogCellType) {

        TableRowData tblRowData = new TableRowData(
                CommonConfig.AppName.SAFESEAS);

        boolean isZone = false;
        if (areaId.equals(zone)) {
            isZone = true;
        }

        DataUsageKey dataUsageKey;
        if (tm.getDataUsageKey() != null) {
            dataUsageKey = tm.getDataUsageKey();
        } else {
            dataUsageKey = DataUsageKey.DISPLAY;
        }

        String hoverText = "";
        if (isZone) {
            AreaIdXML zoneXML = tm.areaConfigMgr.getAreaXml(zone);
            if (zoneXML != null) {
                hoverText = getZoneHoverText(zoneXML);
            }
        } else {
            hoverText = getStationHoverText(areaId);
        }

        tblRowData.setTableCellData(0, new TableCellData(areaId, hoverText,
                CellType.AreaId, false));

        tblRowData
                .setTableCellData(
                        1,
                        new TableCellData("", getCellTypeForSCA(zone, report,
                                tm), true));

        tblRowData.setTableCellData(2, new TableCellData("",
                getCellTypeForGaleWarn(zone, report, tm), true));

        tblRowData.setTableCellData(3, new TableCellData("",
                getCellTypeForStormWarn(zone, report, tm), true));

        tblRowData.setTableCellData(4, new TableCellData("",
                getCellTypeForHFWW(zone, report, tm), true));

        if (isZone) { // zone table
            tblRowData
                    .setTableCellData(
                            5,
                            new TableCellData(
                                    "",
                                    tm.getDirectionalThresholdValueCellType(
                                            DataUsageKey.DISPLAY,
                                            zone,
                                            MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_DIR_FROM
                                                    .getXmlKey(),
                                            MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_DIR_TO
                                                    .getXmlKey(), report
                                                    .getWindDir()), true));
        } else { // station table
            tblRowData
                    .setTableCellData(
                            5,
                            new TableCellData(
                                    Math.round(new Float(report.getWindDir())),
                                    tm.getDirectionalThresholdValueCellType(
                                            DataUsageKey.DISPLAY,
                                            zone,
                                            MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_DIR_FROM
                                                    .getXmlKey(),
                                            MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_DIR_TO
                                                    .getXmlKey(), report
                                                    .getWindDir()), true));
        }
        String mccWind;
        // wind speed
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            mccWind = MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_WIND_SPEED
                    .getXmlKey();
        } else {
            mccWind = MonitorConfigConstants.SafeSeasMonitor.SS_MON_METEO_WIND_SPEED
                    .getXmlKey();
        }
        tblRowData.setTableCellData(
                6,
                new TableCellData(Math.round(new Float(report.getWindSpeed())),
                        tm.getThresholdValueCellType(dataUsageKey, zone,
                                mccWind, report.getWindSpeed()), true));

        // peak wind
        String mccPeakWind;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            mccPeakWind = MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_PEAK_WIND
                    .getXmlKey();
        } else {
            mccPeakWind = MonitorConfigConstants.SafeSeasMonitor.SS_MON_METEO_PEAK_WIND
                    .getXmlKey();
        }
        tblRowData.setTableCellData(
                7,
                new TableCellData(
                        Math.round(new Float(report.getMaxWindSpeed())), tm
                                .getThresholdValueCellType(dataUsageKey, zone,
                                        mccPeakWind, report.getMaxWindSpeed()),
                        true));
        // wind gust
        String mccWindGust;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            mccWindGust = MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_GUST_SPEED
                    .getXmlKey();
        } else {
            mccWindGust = MonitorConfigConstants.SafeSeasMonitor.SS_MON_METEO_GUST_SPEED
                    .getXmlKey();
        }

        tblRowData.setTableCellData(
                8,
                new TableCellData(Math.round(new Float(report.getWindGust())),
                        tm.getThresholdValueCellType(dataUsageKey, zone,
                                mccWindGust, report.getWindGust()), true));

        // visibility
        float visValue = report.getVisibility(); // vis value in miles (statute
                                                 // miles)
        if (visValue == ObConst.MISSING) {
            tblRowData.setTableCellData(9, new TableCellData("",
                    CellType.NotAvailable, true));
        } else {
            RangesUtil util = RangesUtil.getInstance();
            visValue = visValue / milesPerNauticalMile; // vis value in nautical
                                                        // miles
            String visString = util.getVisStringFromMiles(visValue);
            visValue = visValue * 16.0f; // vis in units of
                                         // "(nautical miles)/16"; this is used
                                         // to compare with Red/Yellow threshold
                                         // values

            String mccVis;
            if (dataUsageKey == DataUsageKey.DISPLAY) {
                mccVis = MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_VIS
                        .getXmlKey();
            } else {
                mccVis = MonitorConfigConstants.SafeSeasMonitor.SS_MON_METEO_VIS
                        .getXmlKey();
            }

            TableCellData visCellData = new TableCellData(visString,
                    tm.getThresholdValueCellType(dataUsageKey, zone, mccVis,
                            visValue), true);

            visCellData.setValue(visValue); // visValue, instead of visString,
                                            // will be used for sorting

            tblRowData.setTableCellData(9, visCellData);
        }
        // temperature
        tblRowData
                .setTableCellData(
                        10,
                        new TableCellData(
                                Math.round(new Float(report.getTemperature())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_TEMP
                                                .getXmlKey(), report
                                                .getTemperature()), true));
        // dewpoint
        tblRowData
                .setTableCellData(
                        11,
                        new TableCellData(
                                Math.round(new Float(report.getDewpoint())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_DEWPT
                                                .getXmlKey(), report
                                                .getDewpoint()), true));
        // SLP
        tblRowData
                .setTableCellData(
                        12,
                        new TableCellData(
                                Math.round(new Float(report.getSeaLevelPress())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_SLP
                                                .getXmlKey(), report
                                                .getSeaLevelPress()), true));
        // SST
        tblRowData
                .setTableCellData(
                        13,
                        new TableCellData(
                                Math.round(new Float(report.getSeaSurfaceTemp())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_SST
                                                .getXmlKey(), report
                                                .getSeaSurfaceTemp()), true));
        // wave height
        tblRowData
                .setTableCellData(
                        14,
                        new TableCellData(
                                Math.round(new Float(report
                                        .getHighResWaveHeight())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_WAVE_HT
                                                .getXmlKey(), report
                                                .getHighResWaveHeight()), true));
        // wave steep
        tblRowData
                .setTableCellData(
                        15,
                        new TableCellData(
                                Math.round(new Float(report.getWaveSteepness())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_WAVE_STEEP
                                                .getXmlKey(), report
                                                .getWaveSteepness()), true));
        // swell height
        String mccSwell;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            mccSwell = MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_HT
                    .getXmlKey();
        } else {
            mccSwell = MonitorConfigConstants.SafeSeasMonitor.SS_MON_SWELL_PRIM_HT
                    .getXmlKey();
        }
        tblRowData.setTableCellData(
                16,
                new TableCellData(
                        Math.round(new Float(report.getPSwellHeight())), tm
                                .getThresholdValueCellType(dataUsageKey, zone,
                                        mccSwell, report.getPSwellHeight()),
                        true));
        // swell period
        String mccSwellPD;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            mccSwellPD = MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD
                    .getXmlKey();
        } else {
            mccSwellPD = MonitorConfigConstants.SafeSeasMonitor.SS_MON_SWELL_PRIM_PD
                    .getXmlKey();
        }
        tblRowData.setTableCellData(
                17,
                new TableCellData(
                        Math.round(new Float(report.getPSwellPeriod())), tm
                                .getThresholdValueCellType(dataUsageKey, zone,
                                        mccSwellPD, report.getPSwellPeriod()),
                        true));
        // swell dir TODO: from only
        String mccSwellDirFrom;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            mccSwellDirFrom = MonitorConfigConstants.SafeSeasDisplay.SS_DISP_WIND_DIR_FROM
                    .getXmlKey();
        } else {
            mccSwellDirFrom = MonitorConfigConstants.SafeSeasMonitor.SS_MON_SWELL_PRIM_DIR_FROM
                    .getXmlKey();
        }
        tblRowData.setTableCellData(
                18,
                new TableCellData(Math.round(new Float(report.getPSwellDir())),
                        tm.getThresholdValueCellType(dataUsageKey, zone,
                                mccSwellDirFrom, report.getPSwellDir()), true));
        // swell2 height
        String mccSwell2HT;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            mccSwell2HT = MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_HT
                    .getXmlKey();
        } else {
            mccSwell2HT = MonitorConfigConstants.SafeSeasMonitor.SS_MON_SWELL_SEC_HT
                    .getXmlKey();
        }
        tblRowData.setTableCellData(
                19,
                new TableCellData(
                        Math.round(new Float(report.getSSwellPeriod())), tm
                                .getThresholdValueCellType(dataUsageKey, zone,
                                        mccSwell2HT, report.getSSwellPeriod()),
                        true));
        // swell2 period
        String mccSwell2PD;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            mccSwell2PD = MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_PD
                    .getXmlKey();
        } else {
            mccSwell2PD = MonitorConfigConstants.SafeSeasMonitor.SS_MON_SWELL_SEC_PD
                    .getXmlKey();
        }
        tblRowData.setTableCellData(
                20,
                new TableCellData(
                        Math.round(new Float(report.getSSwellPeriod())), tm
                                .getThresholdValueCellType(dataUsageKey, zone,
                                        mccSwell2PD, report.getSSwellPeriod()),
                        true));
        // swell2 dir TODO: only from
        String mccSwell2DirFrom;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            mccSwell2DirFrom = MonitorConfigConstants.SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_FROM
                    .getXmlKey();
        } else {
            mccSwell2DirFrom = MonitorConfigConstants.SafeSeasMonitor.SS_MON_SWELL_SEC_DIR_FROM
                    .getXmlKey();
        }
        tblRowData
                .setTableCellData(
                        21,
                        new TableCellData(Math.round(new Float(report
                                .getPSwellDir())),
                                tm.getThresholdValueCellType(dataUsageKey,
                                        zone, mccSwell2DirFrom,
                                        report.getSSwellDir()), true));
        // fog
        if (isZone) {
            // zone table: fog monitored at zone level
            tblRowData.setTableCellData(22, new TableCellData("", fogCellType,
                    true));
        } else {
            // station table: fog not monitored
            tblRowData.setTableCellData(22, new TableCellData("",
                    CellType.NotMonitored, true));
        }

        return tblRowData;
    }

    /**
     * Returns a Snow Zone/Station TableRowData object.
     * 
     * @param areaId
     *            (station for station table dialog or zone for zone table
     *            dialog)
     * @param zone
     * @param report
     * @param tm
     *            Abstract Threshold Manager
     * @return
     */
    public static TableRowData getSnowTableRowData(String areaId, String zone,
            ObReport report, AbstractThresholdMgr tm) {

        TableRowData tblRowData = new TableRowData(CommonConfig.AppName.SNOW);

        boolean isZone = false;
        if (areaId.equals(zone)) {
            isZone = true;
        }

        DataUsageKey dataUsageKey;
        if (tm.getDataUsageKey() != null) {
            dataUsageKey = tm.getDataUsageKey();
        } else {
            dataUsageKey = DataUsageKey.DISPLAY;
        }

        String hoverText = "";
        if (isZone) {
            AreaIdXML zoneXML = tm.areaConfigMgr.getAreaXml(zone);
            if (zoneXML != null) {
                hoverText = getZoneHoverText(zoneXML);
            }
        } else {
            hoverText = getStationHoverText(areaId);
        }

        tblRowData.setTableCellData(0, new TableCellData(areaId, hoverText,
                CellType.AreaId, false));

        tblRowData.setTableCellData(1, new TableCellData("",
                getCellTypeForBlizWarn(zone, report, tm), true));

        tblRowData.setTableCellData(2, new TableCellData("",
                getCellTypeForFrzPrecip(zone, report, tm), true));

        tblRowData.setTableCellData(3, new TableCellData("",
                getCellTypeForHsnowWarn(zone, report, tm), true));

        // PresWx
        if (isZone) {
            // zone table: PreWx not monitored at zone/county level
            tblRowData.setTableCellData(4, new TableCellData("",
                    CellType.NotMonitored, true));
        } else {
            // station table
            if (report.getPresentWx().equals("")
                    || report.getPresentWx() == null) {
                tblRowData.setTableCellData(4, new TableCellData("",
                        CellType.NotAvailable, true));
            } else {
                tblRowData.setTableCellData(4,
                        new TableCellData(report.getPresentWx(),
                                CellType.PresWx, false));
            }
        }

        // wind direction
        if (isZone) { // zone table
            tblRowData
                    .setTableCellData(
                            5,
                            new TableCellData(
                                    "",
                                    tm.getDirectionalThresholdValueCellType(
                                            DataUsageKey.DISPLAY,
                                            zone,
                                            MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_DIR_FROM
                                                    .getXmlKey(),
                                            MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_DIR_TO
                                                    .getXmlKey(), report
                                                    .getWindDir()), true));
        } else { // station table
            tblRowData
                    .setTableCellData(
                            5,
                            new TableCellData(
                                    Math.round(new Float(report.getWindDir())),
                                    tm.getDirectionalThresholdValueCellType(
                                            DataUsageKey.DISPLAY,
                                            zone,
                                            MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_DIR_FROM
                                                    .getXmlKey(),
                                            MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_DIR_TO
                                                    .getXmlKey(), report
                                                    .getWindDir()), true));
        }
        // wind speed
        String wsk;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            wsk = MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_WIND_SPEED
                    .getXmlKey();
        } else {
            wsk = MonitorConfigConstants.SnowMonitor.SNOW_MON_METEO_WIND_SPEED
                    .getXmlKey();
        }
        tblRowData.setTableCellData(
                6,
                new TableCellData(Math.round(new Float(report.getWindSpeed())),
                        tm.getThresholdValueCellType(dataUsageKey, zone, wsk,
                                report.getWindSpeed()), true));
        // wind peak
        String wpk;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            wpk = MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_PEAK_WIND
                    .getXmlKey();
        } else {
            wpk = MonitorConfigConstants.SnowMonitor.SNOW_MON_METEO_PEAK_WIND
                    .getXmlKey();
        }
        tblRowData.setTableCellData(
                7,
                new TableCellData(
                        Math.round(new Float(report.getMaxWindSpeed())), tm
                                .getThresholdValueCellType(dataUsageKey, zone,
                                        wpk, report.getMaxWindSpeed()), true));
        // wind gust
        String wgk;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            wgk = MonitorConfigConstants.SnowDisplay.SNOW_DISP_WIND_GUST_SPEED
                    .getXmlKey();
        } else {
            wgk = MonitorConfigConstants.SnowMonitor.SNOW_MON_METEO_GUST_SPEED
                    .getXmlKey();
        }
        tblRowData.setTableCellData(
                8,
                new TableCellData(Math.round(new Float(report.getWindGust())),
                        tm.getThresholdValueCellType(dataUsageKey, zone, wgk,
                                report.getWindGust()), true));
        // temperature
        String tmprk;
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            tmprk = MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_TEMP
                    .getXmlKey();
        } else {
            tmprk = MonitorConfigConstants.SnowMonitor.SNOW_MON_METEO_TEMP
                    .getXmlKey();
        }
        tblRowData.setTableCellData(
                9,
                new TableCellData(
                        Math.round(new Float(report.getTemperature())), tm
                                .getThresholdValueCellType(dataUsageKey, zone,
                                        tmprk, report.getTemperature()), true));

        tblRowData
                .setTableCellData(
                        10,
                        new TableCellData(
                                Math.round(new Float(report.getDewpoint())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_DEWPT
                                                .getXmlKey(), report
                                                .getDewpoint()), true));

        // visibility
        float visValue = report.getVisibility(); // vis value in miles
        if (visValue == ObConst.MISSING) {
            tblRowData.setTableCellData(11, new TableCellData("",
                    CellType.NotAvailable, true));
        } else {
            RangesUtil util = RangesUtil.getInstance();
            String visString = util.getVisStringFromMiles(visValue);
            visValue = visValue * 16.0f; // vis in units of "miles/16"; this is
                                         // used to compare with Red/Yellow
                                         // threshold values

            String visKey;
            if (dataUsageKey == DataUsageKey.DISPLAY) {
                visKey = MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_VIS
                        .getXmlKey();
            } else {
                visKey = MonitorConfigConstants.SnowMonitor.SNOW_MON_METEO_VIS
                        .getXmlKey();
            }
            TableCellData visCellData = new TableCellData(visString,
                    tm.getThresholdValueCellType(dataUsageKey, zone, visKey,
                            visValue), true);

            visCellData.setValue(visValue); // visValue, instead of visString,
                                            // will be used for sorting

            tblRowData.setTableCellData(11, visCellData);
        }

        tblRowData
                .setTableCellData(
                        12,
                        new TableCellData(
                                Math.round(new Float(report.getSeaLevelPress())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SLP
                                                .getXmlKey(), report
                                                .getSeaLevelPress()), true));
        // wind chill
        tblRowData
                .setTableCellData(
                        13,
                        new TableCellData(
                                Math.round(new Float(report.getWindChill())),
                                tm.getThresholdValueCellType(
                                        dataUsageKey,
                                        zone,
                                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_WIND_CHILL
                                                .getXmlKey(), report
                                                .getWindChill()), true));

        tblRowData
                .setTableCellData(
                        14,
                        new TableCellData(
                                Math.round(new Float(report.getFrostbiteTime())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_FROSTBITE
                                                .getXmlKey(), report
                                                .getFrostbiteTime()), true));

        tblRowData
                .setTableCellData(
                        15,
                        new TableCellData(
                                Math.round(new Float(report.getHourlyPrecip())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_HOURLY_PRECIP
                                                .getXmlKey(), report
                                                .getHourlyPrecip()), true));
        // snow depth
        tblRowData
                .setTableCellData(
                        16,
                        new TableCellData(
                                Math.round(new Float(report.getSnowDepth())),
                                tm.getThresholdValueCellType(
                                        dataUsageKey,
                                        zone,
                                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SNOW_DEPTH
                                                .getXmlKey(), report
                                                .getSnowDepth()), true));

        tblRowData
                .setTableCellData(
                        17,
                        new TableCellData(
                                Math.round(new Float(report.getSnincrHourly())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SNINCR_HOURLY
                                                .getXmlKey(), report
                                                .getSnincrHourly()), true));

        tblRowData
                .setTableCellData(
                        18,
                        new TableCellData(
                                Math.round(new Float(report.getSnincrTotal())),
                                tm.getThresholdValueCellType(
                                        DataUsageKey.DISPLAY,
                                        zone,
                                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_SNINCR_TOTAL
                                                .getXmlKey(), report
                                                .getSnincrTotal()), true));

        return tblRowData;
    }

    /**
     * Gets Zone Hover Text.
     * 
     * @param zone
     * @return
     */
    private static String getZoneHoverText(AreaIdXML zoneXML) {

        String zone = zoneXML.getAreaId();
        String hoverText = tableUtil.zoneHoverTextMap.get(zone);

        if (hoverText == null) {

            ISpatialQuery sq = null;
            String sql = null;
            hoverText = zone.substring(0, 2) + ", ";

            try {
                if (MonitorAreaUtils.isMarineZone(zone)) {
                    sql = "select name from mapdata.marinezones where id = '"
                            + zone + "'";
                } else if (zone.charAt(2) == 'Z') { // forecast zone
                    String state_zone = zone.substring(0, 2)
                            + zone.substring(3);
                    sql = "select name from mapdata.zone where state_zone = '"
                            + state_zone + "'";
                } else { // County
                    String state = zone.substring(0, 2);
                    String fipsLike = "%" + zone.substring(3);
                    sql = "select countyname from mapdata.county where state = '"
                            + state + "' and fips like '" + fipsLike + "'";
                }

                sq = SpatialQueryFactory.create();
                Object[] results = sq.dbRequest(sql, "maps");
                if (results.length > 0) {
                    if (results[0] instanceof Object[]) {
                        Object[] res = (Object[]) results[0];
                        hoverText += (String) res[0];
                    } else {
                        hoverText += results[0].toString();
                    }
                } else {
                    if (zoneXML.getCLat() != null) {
                        hoverText += "(" + zoneXML.getCLat() + ", "
                                + zoneXML.getCLon() + ")";
                    }
                }
            } catch (Exception e) {
                statusHandler.error("Unable to query Zone Hover Text: sql: "
                        + sql, e);
            }

            tableUtil.zoneHoverTextMap.put(zone, hoverText);
        }

        return hoverText;
    }

    /**
     * Gets Station Hover Text.
     * 
     * @param stnId
     * @return
     */
    private static String getStationHoverText(String stnId) {

        String hoverText = tableUtil.stationHoverTextMap.get(stnId);

        if (hoverText == null) {

            String sql = "select catalogtype, name from common_obs_spatial where ( catalogtype=1 or catalogtype=33 or catalogtype = 32 or catalogtype=1000) and stationid = '"
                    + stnId + "'";

            ISpatialQuery sq = null;
            Integer stnType = null;
            String stnName = null;
            try {
                sq = SpatialQueryFactory.create();
                Object[] results = sq.dbRequest(sql, "metadata");
                if (results.length > 0) {
                    if (results[0] instanceof Object[]) {
                        Object[] res = (Object[]) results[0];
                        stnType = (Integer) res[0];
                        stnName = (String) res[1];
                    } else {
                        stnType = (Integer) results[0];
                        stnName = (String) results[1];
                    }
                    if (stnType.intValue() == 1) {
                        hoverText = stnId + "#METAR -- " + stnName;
                    } else if (stnType.intValue() == 33
                            || stnType.intValue() == 32) {
                        hoverText = stnId + "#MARITIME -- " + stnName;
                    } else if (stnType.intValue() == 1000) {
                        hoverText = stnId + "#MESONET -- " + stnName;
                    }
                } else {
                    hoverText = stnId;
                }

            } catch (Exception e) {
                statusHandler.error("Unable to query Station Hover Text: sql: "
                        + sql, e);
            }

            tableUtil.stationHoverTextMap.put(stnId, hoverText);
        }

        return hoverText;

    }

    /**
     * Gets Cell Type for SCA.
     * 
     * @param zone
     * @param report
     * @param tm
     *            Abstract Threshold Manager
     * @return
     */
    public static CellType getCellTypeForSCA(String zone, ObReport report,
            AbstractThresholdMgr tm) {

        float windSpd = report.getWindSpeed();
        float windGust = report.getWindGust();
        float waveHgt = report.getHighResWaveHeight();
        if (windSpd == ObConst.MISSING && windGust == ObConst.MISSING
                && waveHgt == ObConst.MISSING) {
            return CellType.NotAvailable;
        }
        CellType type = CellType.NotAvailable;
        CellType windSpeedType = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_SCA_WIND_SPEED
                                .getXmlKey(), windSpd);
        CellType windGustType = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_SCA_GUST_SPEED
                                .getXmlKey(), windGust);
        CellType waveHeightType = tm.getThresholdValueCellType(
                DataUsageKey.DISPLAY, zone,
                MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_SCA_WAVE_HT
                        .getXmlKey(), waveHgt);
        if (windSpeedType.equals(CellType.R) || windGustType.equals(CellType.R)
                || waveHeightType.equals(CellType.R)) {
            type = CellType.R;
        } else if (windSpeedType.equals(CellType.Y)
                || windGustType.equals(CellType.Y)
                || waveHeightType.equals(CellType.Y)) {
            type = CellType.Y;
        } else if (windSpeedType.equals(CellType.G)
                || windGustType.equals(CellType.G)
                || waveHeightType.equals(CellType.G)) {
            type = CellType.G;
        }
        return type;
    }

    /**
     * Gets Cell Type for Gale Warning.
     * 
     * @param zone
     * @param report
     * @param tm
     *            Abstract Threshold Manager
     * @return
     */
    public static CellType getCellTypeForGaleWarn(String zone, ObReport report,
            AbstractThresholdMgr tm) {

        CellType type = CellType.NotAvailable;
        CellType windSpeedType = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_GALE_WIND_SPEED
                                .getXmlKey(), report.getWindSpeed());
        CellType windGustType = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_GALE_GUST_SPEED
                                .getXmlKey(), report.getWindGust());
        if (windSpeedType.equals(CellType.R) || windGustType.equals(CellType.R)) {
            type = CellType.R;
        } else if (windSpeedType.equals(CellType.Y)
                || windGustType.equals(CellType.Y)) {
            type = CellType.Y;
        } else if (windSpeedType.equals(CellType.G)
                || windGustType.equals(CellType.G)) {
            type = CellType.G;
        }
        return type;
    }

    /**
     * Gets Cell Type for Storm Warning.
     * 
     * @param zone
     * @param report
     * @param tm
     *            Abstract Threshold Manager
     * @return
     */
    public static CellType getCellTypeForStormWarn(String zone,
            ObReport report, AbstractThresholdMgr tm) {

        CellType type = CellType.NotAvailable;
        CellType windSpeedType = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_STORM_WIND_SPEED
                                .getXmlKey(), report.getWindSpeed());
        CellType windGustType = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_STORM_GUST_SPEED
                                .getXmlKey(), report.getWindGust());
        if (windSpeedType.equals(CellType.R) || windGustType.equals(CellType.R)) {
            type = CellType.R;
        } else if (windSpeedType.equals(CellType.Y)
                || windGustType.equals(CellType.Y)) {
            type = CellType.Y;
        } else if (windSpeedType.equals(CellType.G)
                || windGustType.equals(CellType.G)) {
            type = CellType.G;
        }
        return type;
    }

    /**
     * Gets Cell Type for HFWW.
     * 
     * @param zone
     * @param report
     * @param tm
     *            Abstract Threshold Manager
     * @return
     */
    public static CellType getCellTypeForHFWW(String zone, ObReport report,
            AbstractThresholdMgr tm) {

        CellType type = CellType.NotAvailable;
        CellType windSpeedType = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_HFWW_WIND_SPEED
                                .getXmlKey(), report.getWindSpeed());
        CellType windGustType = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SafeSeasDisplay.SS_DISP_PROD_HFWW_GUST_SPEED
                                .getXmlKey(), report.getWindGust());
        if (windSpeedType.equals(CellType.R) || windGustType.equals(CellType.R)) {
            type = CellType.R;
        } else if (windSpeedType.equals(CellType.Y)
                || windGustType.equals(CellType.Y)) {
            type = CellType.Y;
        } else if (windSpeedType.equals(CellType.G)
                || windGustType.equals(CellType.G)) {
            type = CellType.G;
        }
        return type;
    }

    /**
     * Gets Cell Type for Fog.
     * 
     * @param zone
     * @param report
     * @param tm
     *            Abstract Threshold Manager
     * @return
     */
    public static CellType getCellTypeForFog(String zone, ObReport report,
            AbstractThresholdMgr tm) {

        CellType retVal = CellType.NotAvailable;
        DataUsageKey dataUsageKey;
        if (tm.getDataUsageKey() != null) {
            dataUsageKey = tm.getDataUsageKey();
        } else {
            dataUsageKey = DataUsageKey.DISPLAY;
        }

        float visValue = report.getVisibility();// in miles
        if (visValue != ObConst.MISSING) {
            visValue = visValue / milesPerNauticalMile; // in nautical miles
            String mccSafeseas;
            if (dataUsageKey == DataUsageKey.DISPLAY) {
                mccSafeseas = MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_VIS
                        .getXmlKey();
            } else {
                mccSafeseas = MonitorConfigConstants.SafeSeasMonitor.SS_MON_METEO_VIS
                        .getXmlKey();
            }

            retVal = tm.getThresholdValueCellType(dataUsageKey, zone,
                    mccSafeseas, visValue);
        }
        return retVal;
    }

    /**
     * get CellType for Bliz Warn based on combinations of individual parameters
     * in a report (based on SNOW User's Guide, OB8.3, February, 2008)
     * 
     * @param zone
     * @param report
     * @param tm
     *            Abstract Threshold Manager
     * @return
     */
    public static CellType getCellTypeForBlizWarn(String zone, ObReport report,
            AbstractThresholdMgr tm) {

        CellType type = CellType.NotAvailable; // default; assuming no
                                               // observation available

        String presentWx = report.getPresentWx();

        CellType windSpeed = CellType.NotAvailable;
        if (report.getWindSpeed() != ObConst.MISSING) {
            windSpeed = tm
                    .getThresholdValueCellType(
                            DataUsageKey.DISPLAY,
                            zone,
                            MonitorConfigConstants.SnowDisplay.SNOW_DISP_PROD_BLIZZ_WIND_SPEED
                                    .getXmlKey(), report.getWindSpeed());
        }

        CellType peakWind = CellType.NotAvailable;
        if (report.getMaxWindSpeed() != ObConst.MISSING) {
            peakWind = tm
                    .getThresholdValueCellType(
                            DataUsageKey.DISPLAY,
                            zone,
                            MonitorConfigConstants.SnowDisplay.SNOW_DISP_PROD_BLIZZ_PEAK_WIND
                                    .getXmlKey(), report.getMaxWindSpeed());
        }

        CellType windGust = CellType.NotAvailable;
        if (report.getWindGust() != ObConst.MISSING) {
            windGust = tm
                    .getThresholdValueCellType(
                            DataUsageKey.DISPLAY,
                            zone,
                            MonitorConfigConstants.SnowDisplay.SNOW_DISP_PROD_BLIZZ_GUST_SPEED
                                    .getXmlKey(), report.getWindGust());
        }

        CellType visibility = CellType.NotAvailable;
        if (report.getVisibility() != ObConst.MISSING) {
            // vis in units of "miles/16" is used to compare with Red/Yellow
            // threshold values
            visibility = tm.getThresholdValueCellType(DataUsageKey.DISPLAY,
                    zone,
                    MonitorConfigConstants.SnowDisplay.SNOW_DISP_PROD_BLIZZ_VIS
                            .getXmlKey(), report.getVisibility() * 16.0f);
        }

        if (presentWx.contains("SN") || presentWx.contains("BLSN")) {
            // snow or blowing snow observed
            if (windSpeed.equals(CellType.R) || peakWind.equals(CellType.R)
                    || windGust.equals(CellType.R)) {
                if (visibility.equals(CellType.R)) {
                    type = CellType.R;
                } else {
                    type = CellType.Y;
                }
            } else if (windSpeed.equals(CellType.Y)
                    || peakWind.equals(CellType.Y)
                    || windGust.equals(CellType.Y)) {
                type = CellType.Y;
            } else if (windSpeed.equals(CellType.G)
                    || peakWind.equals(CellType.G)
                    || windGust.equals(CellType.G)) {
                if (visibility.equals(CellType.R)
                        || visibility.equals(CellType.Y)) {
                    type = CellType.Y;
                } else if (visibility.equals(CellType.G)) {
                    type = CellType.G;
                } else {
                    type = CellType.NotAvailable;
                }
            } else {
                if (visibility.equals(CellType.R)
                        || visibility.equals(CellType.Y)) {
                    type = CellType.Y;
                } else {
                    type = CellType.NotAvailable;
                }
            }
        } else {
            // snow or blowing snow not observed
            type = CellType.NotAvailable;
        }

        return type;
    }

    /**
     * get CellType for Frz Precip based on combinations of individual
     * parameters in a report (based on SNOW User's Guide, OB8.3, February,
     * 2008)
     * 
     * @param zone
     * @param report
     * @param tm
     *            Abstract Threshold Manager
     * @return
     */
    public static CellType getCellTypeForFrzPrecip(String zone,
            ObReport report, AbstractThresholdMgr tm) {

        CellType type = CellType.NotAvailable; // default, assuming no
                                               // observation available

        String presentWx = report.getPresentWx();

        CellType temp = tm.getThresholdValueCellType(DataUsageKey.DISPLAY,
                zone, MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_TEMP
                        .getXmlKey(), report.getTemperature());

        CellType hourlyPrecip = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_METEO_HOURLY_PRECIP
                                .getXmlKey(), report.getHourlyPrecip());

        if (presentWx.contains("FZRA") || presentWx.contains("FZDZ")) {
            // freezing rain or freezing drizzle is observed
            type = CellType.R;
        } else if (presentWx.contains("RA") || presentWx.contains("DZ")) {
            // rain or drizzle is observed; determine threat level based on
            // temperature
            if (temp.equals(CellType.R)) {
                type = CellType.R;
            } else if (temp.equals(CellType.Y)) {
                type = CellType.Y;
            } else if (temp.equals(CellType.G)) {
                type = CellType.G;
            }
        } else {
            // neither rain nor drizzle is observed; determine threat level
            // based on temperature and hourly precipitation
            if (temp.equals(CellType.R) && hourlyPrecip.equals(CellType.R)) {
                type = CellType.R; // both Temp and Hourly Precip are RED
            } else if (temp.equals(CellType.G)
                    || hourlyPrecip.equals(CellType.G)) {
                type = CellType.G; // at least one of Temp and Hourly Precip is
                                   // GREEN
            } else if (hourlyPrecip.equals(CellType.G)) {
                // N/A if precipitation not observed (this is not described in
                // SNOW User's Guide!!!)
                type = CellType.NotAvailable;
            } else if (temp.equals(CellType.NotAvailable)
                    && hourlyPrecip.equals(CellType.NotAvailable)) {
                type = CellType.NotAvailable;
            } else {
                type = CellType.Y; // all other combinations (Yellow-Yellow or
                                   // Yellow-Red) are YELLOW
            }
        }

        return type;
    }

    /**
     * get CellType for Hsnow Warn based on combinations of individual
     * parameters in a report (based on SNOW User's Guide, OB8.3, February,
     * 2008)
     * 
     * @param zone
     * @param report
     * @param tm
     *            Abstract Threshold Manager
     * @return
     */
    public static CellType getCellTypeForHsnowWarn(String zone,
            ObReport report, AbstractThresholdMgr tm) {

        CellType type = CellType.NotAvailable; // default, assuming no
                                               // observation available
        CellType snowDepth = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_PROD_HSW_SNOW_DEPTH
                                .getXmlKey(), report.getSnowDepth());

        CellType snincrHourly = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_HOURLY
                                .getXmlKey(), report.getSnincrHourly());

        CellType snincrTotal = tm
                .getThresholdValueCellType(
                        DataUsageKey.DISPLAY,
                        zone,
                        MonitorConfigConstants.SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_TOTAL
                                .getXmlKey(), report.getSnincrTotal());

        if (snowDepth.equals(CellType.R) || snincrHourly.equals(CellType.R)
                || snincrTotal.equals(CellType.R)) {
            type = CellType.R;
        } else if (snowDepth.equals(CellType.Y)
                || snincrHourly.equals(CellType.Y)
                || snincrTotal.equals(CellType.Y)) {
            type = CellType.Y;
        } else if (snowDepth.equals(CellType.G)
                || snincrHourly.equals(CellType.G)
                || snincrTotal.equals(CellType.G)) {
            type = CellType.G;
        } else {
            type = CellType.NotAvailable; // none of the three parameters is
                                          // observed
        }

        return type;
    }

    /**
     * Gets History Table Row Data.
     * 
     * @param appName
     * @param obsType
     * @param report
     * @return
     */
    public static TableRowData getHistTableRowData(
            CommonConfig.AppName appName, ObsHistType obsType, ObReport report) {
        // TODO: add MESONET type
        if (appName == AppName.FOG) {
            if (obsType == ObsHistType.MARITIME) {
                return getFogMaritimeHistTableRowData(report);
            } else if (obsType == ObsHistType.METAR) {
                return getFogMetarHistTableRowData(report);
            } else {
                return null;
            }
        } else if (appName == AppName.SAFESEAS) {
            if (obsType == ObsHistType.MARITIME) {
                return getSafeseasMaritimeHistTableRowData(report);
            } else if (obsType == ObsHistType.METAR) {
                return getSafeSeasMetarHistTableRowData(report);
            } else {
                return null;
            }
        } else if (appName == AppName.SNOW) {
            if (obsType == ObsHistType.MARITIME) {
                return null; // no Maritime history table for Snow
            } else if (obsType == ObsHistType.METAR) {
                return getSnowMetarHistTableRowData(report);
            } else {
                return null;
            }
        }
        return null;
    }

    /**
     * Gets SNOW Metar History Table Row Data.
     * 
     * @param report
     * @return
     */
    private static TableRowData getSnowMetarHistTableRowData(ObReport report) {
        TableRowData tblRowData = new TableRowData(10);
        tblRowData.setTableCellData(0,
                new TableCellData(report.getObservationTime(), "HH:mm MMM dd",
                        CellType.ObsHist));
        tblRowData.setTableCellData(1,
                new TableCellData(new Float(report.getLatitude()),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(2,
                new TableCellData(new Float(report.getLongitude()),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(3,
                new TableCellData(Math.round(new Float(report.getWindDir())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(4,
                new TableCellData(Math.round(new Float(report.getWindSpeed())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(5,
                new TableCellData(Math.round(new Float(report.getWindGust())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(6, new TableCellData(report.getPressure(),
                CellType.ObsHist, CommonTableConfig.obsHistCols.P));
        tblRowData.setTableCellData(
                7,
                new TableCellData(
                        Math.round(new Float(report.getTemperature())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(8,
                new TableCellData(Math.round(new Float(report.getDewpoint())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(9,
                new TableCellData(report.getPressureChange(), CellType.ObsHist,
                        CommonTableConfig.obsHistCols.PTend));
        return tblRowData;
    }

    /**
     * Gets SAFESEAS Metar History Table Row Data.
     * 
     * @param report
     * @return
     */
    private static TableRowData getSafeSeasMetarHistTableRowData(ObReport report) {
        // same as getSnowHistTableRowData
        return getSnowMetarHistTableRowData(report);
    }

    /**
     * Gets SAFESEAS Maritime History Table Row Data.
     * 
     * @param report
     * @return
     */
    private static TableRowData getSafeseasMaritimeHistTableRowData(
            ObReport report) {
        TableRowData tblRowData = new TableRowData(17);
        tblRowData.setTableCellData(0,
                new TableCellData(report.getObservationTime(), "HH:mm MMM dd",
                        CellType.ObsHist));
        tblRowData.setTableCellData(1,
                new TableCellData(new Float(report.getLatitude()),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(2,
                new TableCellData(new Float(report.getLongitude()),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(3,
                new TableCellData(Math.round(new Float(report.getWindSpeed())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(
                4,
                new TableCellData(
                        Math.round(new Float(report.getMaxWindSpeed())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(5,
                new TableCellData(Math.round(new Float(report.getWindGust())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(6,
                new TableCellData(
                        Math.round(new Float(report.getVisibility())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(7, new TableCellData(report.getPressure(),
                CellType.ObsHist, CommonTableConfig.obsHistCols.P));
        tblRowData.setTableCellData(
                8,
                new TableCellData(Math.round(new Float(report
                        .getPressureChange())), CellType.ObsHist, true));
        tblRowData.setTableCellData(
                9,
                new TableCellData(
                        Math.round(new Float(report.getTemperature())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(10,
                new TableCellData(Math.round(new Float(report.getDewpoint())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(
                11,
                new TableCellData(Math.round(new Float(report
                        .getSeaSurfaceTemp())), CellType.ObsHist, true));
        tblRowData.setTableCellData(
                12,
                new TableCellData(Math.round(new Float(report
                        .getHighResWaveHeight())), CellType.ObsHist, true));
        tblRowData.setTableCellData(
                13,
                new TableCellData(Math.round(new Float(report
                        .getWaveSteepness())), CellType.ObsHist, true));
        tblRowData.setTableCellData(
                14,
                new TableCellData(
                        Math.round(new Float(report.getPSwellHeight())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(
                15,
                new TableCellData(
                        Math.round(new Float(report.getPSwellPeriod())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(16,
                new TableCellData(Math.round(new Float(report.getPSwellDir())),
                        CellType.ObsHist, true));
        return tblRowData;
    }

    /**
     * Gets Fog Maritime History Table Row Data.
     * 
     * @param report
     * @return
     */
    private static TableRowData getFogMaritimeHistTableRowData(ObReport report) {
        TableRowData tblRowData = new TableRowData(18);
        tblRowData.setTableCellData(0,
                new TableCellData(report.getObservationTime(), "HH:mm MMM dd",
                        CellType.ObsHist));
        tblRowData.setTableCellData(1,
                new TableCellData(new Float(report.getLatitude()),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(2,
                new TableCellData(new Float(report.getLongitude()),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(3,
                new TableCellData(Math.round(new Float(report.getWindSpeed())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(
                4,
                new TableCellData(
                        Math.round(new Float(report.getMaxWindSpeed())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(5,
                new TableCellData(Math.round(new Float(report.getWindGust())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(6,
                new TableCellData(
                        Math.round(new Float(report.getVisibility())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(7, new TableCellData(report.getPressure(),
                CellType.ObsHist, CommonTableConfig.obsHistCols.P));
        tblRowData.setTableCellData(
                8,
                new TableCellData(Math.round(new Float(report
                        .getPressureChange())), CellType.ObsHist, true));
        tblRowData.setTableCellData(
                9,
                new TableCellData(
                        Math.round(new Float(report.getTemperature())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(10,
                new TableCellData(Math.round(new Float(report.getDewpoint())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(
                11,
                new TableCellData(Math.round(new Float(report
                        .getSeaSurfaceTemp())), CellType.ObsHist, true));
        tblRowData.setTableCellData(
                12,
                new TableCellData(Math.round(new Float(report
                        .getHighResWaveHeight())), CellType.ObsHist, true));
        tblRowData.setTableCellData(
                13,
                new TableCellData(Math.round(new Float(report
                        .getWaveSteepness())), CellType.ObsHist, true));
        tblRowData.setTableCellData(
                14,
                new TableCellData(
                        Math.round(new Float(report.getPSwellHeight())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(
                15,
                new TableCellData(
                        Math.round(new Float(report.getPSwellPeriod())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(16,
                new TableCellData(Math.round(new Float(report.getPSwellDir())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(
                17,
                new TableCellData(Math.round(new Float(report
                        .getRelativeHumidity())), CellType.ObsHist, true));
        return tblRowData;
    }

    /**
     * Gets Fog Metar History Table Row Data.
     * 
     * @param report
     * @return
     */
    private static TableRowData getFogMetarHistTableRowData(ObReport report) {
        TableRowData tblRowData = new TableRowData(14);
        tblRowData.setTableCellData(0,
                new TableCellData(report.getObservationTime(), "HH:mm MMM dd",
                        CellType.ObsHist));
        tblRowData.setTableCellData(1,
                new TableCellData(new Float(report.getLatitude()),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(2,
                new TableCellData(new Float(report.getLongitude()),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(
                3,
                new TableCellData(Math.round(new Float(report
                        .getRelativeHumidity())), CellType.ObsHist, true));
        tblRowData.setTableCellData(4,
                new TableCellData(
                        Math.round(new Float(report.getVisibility())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(5,
                new TableCellData(Math.round(new Float(report.getCeiling())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(6,
                new TableCellData(Math.round(new Float(report.getWindDir())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(7,
                new TableCellData(Math.round(new Float(report.getWindSpeed())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(8,
                new TableCellData(Math.round(new Float(report.getWindGust())),
                        CellType.ObsHist, true));
        tblRowData.setTableCellData(9, new TableCellData(report.getPressure(),
                CellType.ObsHist, CommonTableConfig.obsHistCols.P));
        int tmph = Math.round(new Float(report.getTemperature()));
        int dpth = Math.round(new Float(report.getDewpoint()));
        tblRowData.setTableCellData(10, new TableCellData(tmph,
                CellType.ObsHist, true));
        tblRowData.setTableCellData(11, new TableCellData(dpth,
                CellType.ObsHist, true));
        tblRowData.setTableCellData(12, new TableCellData(tmph - dpth,
                CellType.ObsHist, true));
        tblRowData.setTableCellData(13,
                new TableCellData(report.getPressureChange(), CellType.ObsHist,
                        CommonTableConfig.obsHistCols.PTend));
        return tblRowData;
    }
    // TODO: add MESONET data
}
