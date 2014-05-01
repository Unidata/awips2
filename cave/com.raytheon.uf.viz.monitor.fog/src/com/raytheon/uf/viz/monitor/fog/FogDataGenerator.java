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
package com.raytheon.uf.viz.monitor.fog;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.raytheon.uf.common.dataplugin.fog.FogRecord.FOG_THREAT;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonTableConfig;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.CellType;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.data.ObsData;
import com.raytheon.uf.viz.monitor.data.TableCellData;
import com.raytheon.uf.viz.monitor.data.TableData;
import com.raytheon.uf.viz.monitor.data.TableRowData;
import com.raytheon.uf.viz.monitor.fog.threshold.FogThresholdMgr;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants;

/**
 * Generate Data for Fog Dialogs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/07/09                  dhladky    Initial Creation.
 * Oct.29, 2012 1297         skorolev   Changed HashMap to Map
 * Oct.31, 2012 1297         skorolev   Clean code
 * 
 * </pre>
 * 
 * @author dhladky
 * 
 */

public class FogDataGenerator {

    private CommonTableConfig ctc;

    private FogThresholdMgr ftm;

    /**
     * Generates Fog data
     */
    public FogDataGenerator() {
        ctc = CommonTableConfig.getInstance();
        ftm = FogThresholdMgr.getInstance();
    }

    /**
     * Generates the zone fog data for table
     * 
     * @param obsData
     * @param algThreats
     * @param date
     * @return fogTableData
     */
    public TableData generateZoneData(ObsData obsData,
            Map<String, FogRecord.FOG_THREAT> algThreats, Date date) {
        // TODO: in the future use the data passed to get the nominal time
        // closest for display in table and CAVE rendering.
        TableData fogTableData = new TableData(CommonConfig.AppName.FOG);
        if (obsData != null) {
            for (String zone : obsData.getContainer().keySet()) {
                addZoneRow(zone, obsData.getArea(zone).getBestAreaReport(date),
                        algThreats.get(zone), fogTableData);
            }
        }

        return fogTableData;
    }

    /**
     * Creates a Zone table row
     * 
     * @param zone
     * @param report
     * @param threat
     * @param td
     */
    private void addZoneRow(String zone, ObReport report,
            FogRecord.FOG_THREAT threat, TableData td) {
        TableRowData trd = new TableRowData(
                ctc.getTableColumnKeys(CommonConfig.AppName.FOG).length);

        trd.setTableCellData(0, new TableCellData(zone, zone, CellType.AreaId,
                false));

        trd.setTableCellData(
                1,
                new TableCellData(
                        new Float(report.getVisibility()).intValue(),
                        ftm.getThresholdValueCellType(
                                DataUsageKey.DISPLAY,
                                zone,
                                MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_VIS
                                        .getXmlKey(), report.getVisibility()),
                        true));

        trd.setTableCellData(2, new TableCellData(report.getPresentWx(), zone,
                CellType.NotMonitored, false));

        trd.setTableCellData(
                3,
                new TableCellData(
                        new Float(report.getCeiling()).intValue(),
                        ftm.getThresholdValueCellType(
                                DataUsageKey.DISPLAY,
                                zone,
                                MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_CEILING
                                        .getXmlKey(), report.getCeiling()),
                        true));

        trd.setTableCellData(
                4,
                new TableCellData(
                        new Float(report.getWindDir()).intValue(),
                        ftm.getThresholdValueCellType(
                                DataUsageKey.DISPLAY,
                                zone,
                                MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_DIR_FROM
                                        .getXmlKey(), report.getWindDir()),
                        true));

        trd.setTableCellData(
                5,
                new TableCellData(
                        new Float(report.getWindSpeed()).intValue(),
                        ftm.getThresholdValueCellType(
                                DataUsageKey.DISPLAY,
                                zone,
                                MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_WIND_SPEED
                                        .getXmlKey(), report.getWindSpeed()),
                        true));

        trd.setTableCellData(
                6,
                new TableCellData(
                        new Float(report.getMaxWindSpeed()).intValue(),
                        ftm.getThresholdValueCellType(
                                DataUsageKey.DISPLAY,
                                zone,
                                MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_PEAK_WIND
                                        .getXmlKey(), report.getMaxWindSpeed()),
                        true));

        trd.setTableCellData(
                7,
                new TableCellData(
                        new Float(report.getWindGust()).intValue(),
                        ftm.getThresholdValueCellType(
                                DataUsageKey.DISPLAY,
                                zone,
                                MonitorConfigConstants.FogDisplay.FOG_DISP_WIND_GUST_SPEED
                                        .getXmlKey(), report.getWindGust()),
                        true));

        trd.setTableCellData(
                8,
                new TableCellData(
                        new Float(report.getTemperature()).intValue(),
                        ftm.getThresholdValueCellType(
                                DataUsageKey.DISPLAY,
                                zone,
                                MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_TEMP
                                        .getXmlKey(), report.getTemperature()),
                        true));

        trd.setTableCellData(
                9,
                new TableCellData(
                        new Float(report.getDewpoint()).intValue(),
                        ftm.getThresholdValueCellType(
                                DataUsageKey.DISPLAY,
                                zone,
                                MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_DEWPT
                                        .getXmlKey(), report.getDewpoint()),
                        true));

        trd.setTableCellData(
                10,
                new TableCellData(new Float(report.getDewpointDepr())
                        .intValue(), ftm.getThresholdValueCellType(
                        DataUsageKey.DISPLAY, zone,
                        MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_T_TD
                                .getXmlKey(), report.getDewpointDepr()), true));

        trd.setTableCellData(
                11,
                new TableCellData(
                        new Float(report.getRelativeHumidity()).intValue(),
                        ftm.getThresholdValueCellType(
                                DataUsageKey.DISPLAY,
                                zone,
                                MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_REL_HUMIDITY
                                        .getXmlKey(), report
                                        .getRelativeHumidity()), true));

        trd.setTableCellData(12, new TableCellData("", zone,
                getAlgorithmCellType(threat), true));

        td.addReplaceDataRow(trd);
    }

    /**
     * Get threat cells type
     * 
     * @param threat
     * @return type
     */
    public CellType getAlgorithmCellType(FogRecord.FOG_THREAT threat) {
        CellType type = CellType.NotDetermined;
        if (threat == FogRecord.FOG_THREAT.GREEN) {
            type = CellType.G;
        } else if (threat == FogRecord.FOG_THREAT.YELLOW) {
            type = CellType.Y;
        } else if (threat == FogRecord.FOG_THREAT.RED) {
            type = CellType.R;
        }
        return type;
    }

    /**
     * Get threat types
     * 
     * @param map
     * @return types
     */
    public Map<String, CellType> getAlgCellTypes(Map<String, FOG_THREAT> map) {
        Map<String, CellType> types = new HashMap<String, CellType>();
        for (String zone : map.keySet()) {
            CellType type = getAlgorithmCellType(map.get(zone));
            types.put(zone, type);
        }
        return types;
    }
}
