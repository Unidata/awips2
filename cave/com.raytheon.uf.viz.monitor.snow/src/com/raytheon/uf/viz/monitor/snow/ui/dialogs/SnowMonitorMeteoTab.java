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
package com.raytheon.uf.viz.monitor.snow.ui.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowMonitorMeteoData;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SnowMonitor;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

/**
 * Snow Monitor Meteo Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * ????                   ????      Initial creation
 * Jun 02, 2016  5673     randerso  Fixed header alignment in threshold dialogs
 * 
 * </pre>
 * 
 * @author ????
 * @version 1.0
 */
public class SnowMonitorMeteoTab extends TabItemComp implements
        IUpdateMonitorMeteo {
    private SnowMonitorMeteoEditDlg meteoEditDlg;

    private ArrayList<String> areaIDArray;

    private ArrayList<SnowMonitorMeteoData> snowDataArray;

    public SnowMonitorMeteoTab(TabFolder parent, DataUsageKey duKey) {
        super(parent, duKey);
    }

    @Override
    protected void createListHeader() {
        /*
         * Create filler label.
         */
        createHeader("", 0, 0, false);

        /*
         * Monitor Meteo
         */
        createHeader("Wind\nSpeed(kt)", 1, 2, true);
        createHeader("Peak\nWind(kt)", 3, 4, true);
        createHeader("Gust\nSpeed(kt)", 5, 6, true);
        createHeader("Temp(F)", 7, 8, true);
        createHeader("Wind\nChill(F)", 9, 10, true);
        createHeader("Vis(mi)", 11, 12, true);
        createHeader("Snow\nDepth(in)", 13, 14, true);
    }

    @Override
    protected void populateTable() {
        if (snowDataArray == null) {
            createDataArray();
        }

        boolean update = false;
        if (dataTable.getItemCount() > 0) {
            update = true;
        }

        RangesUtil rangeUtil = RangesUtil.getInstance();

        areaIDArray = new ArrayList<String>();

        String tmpVisStr;
        String currentAreaID;

        double visVal = 0.0;

        SnowMonitorMeteoData smmd = null;

        int numColumns = 15;
        new TableColumn(dataTable, SWT.LEFT);
        for (int c = 1; c < numColumns; c++) {
            new TableColumn(dataTable, SWT.RIGHT);
        }

        for (int i = 0; i < snowDataArray.size(); i++) {
            TableItem item;
            if (update == true) {
                item = dataTable.getItem(i);
            } else {
                item = new TableItem(dataTable, SWT.NONE);
            }

            smmd = snowDataArray.get(i);

            currentAreaID = smmd.getAreaID();
            areaIDArray.add(currentAreaID);

            item.setText(0, currentAreaID);

            /*
             * Wind Speed
             */
            appendIntData(item, 1, smmd.getWindSpeedR(), smmd.getWindSpeedY());

            /*
             * Peak Wind
             */
            appendIntData(item, 3, smmd.getPeakWindR(), smmd.getPeakWindY());

            /*
             * Gust Speed
             */
            appendIntData(item, 5, smmd.getGustSpeedR(), smmd.getGustSpeedY());

            /*
             * Temperature
             */
            appendIntData(item, 7, smmd.getTempR(), smmd.getTempY());

            /*
             * Wind Chill
             */
            appendIntData(item, 9, smmd.getWindChillR(), smmd.getWindChillY());

            /*
             * Visibility
             */
            visVal = smmd.getVisR();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(10, String.format(dataFmt, tmpVisStr));

            visVal = smmd.getVisY();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(11, String.format(dataFmt, tmpVisStr));

            /*
             * Snow Depth
             */
            appendIntData(item, 13, smmd.getSnowDepthR(), smmd.getSnowDepthY());

        }

        packListControls();
    }

    private void createDataArray() {
        snowDataArray = new ArrayList<SnowMonitorMeteoData>();

        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = stm.getThresholdsXmlData(duKey);

        ArrayList<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            SnowMonitorMeteoData smmd = new SnowMonitorMeteoData();

            smmd.setAreaID(areaID);

            /*
             * Wind Speed
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_WIND_SPEED.getXmlKey();
            smmd.setWindSpeedR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            smmd.setWindSpeedY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Peak Wind
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_PEAK_WIND.getXmlKey();
            smmd.setPeakWindR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            smmd.setPeakWindY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Gust Speed
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_GUST_SPEED.getXmlKey();
            smmd.setGustSpeedR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            smmd.setGustSpeedY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Temperature
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_TEMP.getXmlKey();
            smmd.setTempR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            smmd.setTempY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Wind Chill
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_WIND_CHILL.getXmlKey();
            smmd.setWindChillR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            smmd.setWindChillY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Visibility
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_VIS.getXmlKey();
            smmd.setVisR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            smmd.setVisY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Snow Depth
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_SNOW_DEPTH.getXmlKey();
            smmd.setSnowDepthR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            smmd.setSnowDepthY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Add data to array.
             */
            snowDataArray.add(smmd);
        }
    }

    private SnowMonitorMeteoData getDataAtFirstSelection() {
        int index = dataTable.getSelectionIndex();

        return snowDataArray.get(index);
    }

    private void updateDataArray(SnowMonitorMeteoData smmd) {
        int[] dataListIndexes = dataTable.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];

            snowDataArray.get(currentIndex).updateData(smmd);
        }
    }

    @Override
    public void commitDataToXML() {
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (SnowMonitorMeteoData smmd : snowDataArray) {
            areaID = smmd.getAreaID();

            /*
             * Wind Speed
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    smmd.getWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    smmd.getWindSpeedY());

            /*
             * Peak Wind
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    smmd.getPeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    smmd.getPeakWindY());

            /*
             * Gust Speed
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    smmd.getGustSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    smmd.getGustSpeedY());

            /*
             * Temperature
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_TEMP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    smmd.getTempR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    smmd.getTempY());

            /*
             * Wind Chill
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_WIND_CHILL.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    smmd.getWindChillR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    smmd.getWindChillY());

            /*
             * Visibility
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_VIS.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    smmd.getVisR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    smmd.getVisY());

            /*
             * Snow Depth
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_SNOW_DEPTH.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    smmd.getSnowDepthR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    smmd.getSnowDepthY());
        }
    }

    @Override
    public void reloadData() {
        dataTable.removeAll();
        snowDataArray.clear();
        snowDataArray = null;

        populateTable();
    }

    @Override
    protected void editDataAction() {
        SnowMonitorMeteoData smmd = getDataAtFirstSelection();

        if (meteoEditDlg == null) {
            meteoEditDlg = new SnowMonitorMeteoEditDlg(getParent().getShell(),
                    smmd, this);
            meteoEditDlg.open();
            meteoEditDlg = null;
        }
    }

    @Override
    public void updateThresholdData(SnowMonitorMeteoData smmd) {
        updateDataArray(smmd);
        populateTable();
    }
}
