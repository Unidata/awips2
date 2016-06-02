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
package com.raytheon.uf.viz.monitor.safeseas.ui.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSMonitorMeteoData;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SafeSeasMonitor;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

/**
 * SAFESEAS Monitor Meteo Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------
 * Sep 17, 2014  2757     skorolev  Removed unnecessary printouts.
 * Jun 02, 2016  5673     randerso  Fixed header alignment in threshold dialogs
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */
public class SSMonitorMeteoTab extends TabItemComp implements
        IUpdateMonitorMeteo {
    private SSMonitorMeteoEditDlg monitorMeteoEditDlg;

    private ArrayList<String> areaIDArray;

    private ArrayList<SSMonitorMeteoData> ssDataArray;

    public SSMonitorMeteoTab(TabFolder parent, DataUsageKey duKey) {
        super(parent, duKey);
    }

    @Override
    protected void createListHeader() {
        /*
         * Create filler label.
         */
        createHeader("", 0, 0, false);

        /*
         * Meteo
         */
        createHeader("Wind\nSpeed(kt)", 1, 2, true);
        createHeader("Peak\nWind(kt)", 3, 4, true);
        createHeader("Gust\nSpeed(kt)", 5, 6, true);
        createHeader("Wave\nHeight(ft)", 7, 8, true);
        createHeader("Vis(mi)", 9, 10, true);
    }

    @Override
    protected void populateTable() {
        if (ssDataArray == null) {
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

        SSMonitorMeteoData ssmmd = null;

        int numColumns = 11;
        new TableColumn(dataTable, SWT.LEFT);
        for (int c = 1; c < numColumns; c++) {
            new TableColumn(dataTable, SWT.RIGHT);
        }

        for (int i = 0; i < ssDataArray.size(); i++) {

            TableItem item;
            if (update == true) {
                item = dataTable.getItem(i);
            } else {
                item = new TableItem(dataTable, SWT.NONE);
            }

            ssmmd = ssDataArray.get(i);

            currentAreaID = ssmmd.getAreaID();
            areaIDArray.add(currentAreaID);

            item.setText(0, currentAreaID);

            /*
             * Wind Speed
             */
            appendIntData(item, 1, ssmmd.getWindSpeedR(), ssmmd.getWindSpeedY());

            /*
             * Peak Wind
             */
            appendIntData(item, 3, ssmmd.getPeakWindR(), ssmmd.getPeakWindY());

            /*
             * Gust Wind
             */
            appendIntData(item, 5, ssmmd.getGustSpeedR(), ssmmd.getGustSpeedY());

            /*
             * Wave Height
             */
            appendIntData(item, 7, ssmmd.getWaveHgtR(), ssmmd.getWaveHgtY());

            /*
             * Visibility
             */
            visVal = ssmmd.getVisR();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(9, String.format(dataFmt, tmpVisStr));

            visVal = ssmmd.getVisY();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(10, String.format(dataFmt, tmpVisStr));

        }

        packListControls();
    }

    private void createDataArray() {
        ssDataArray = new ArrayList<SSMonitorMeteoData>();

        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = sstm.getThresholdsXmlData(duKey);

        ArrayList<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            SSMonitorMeteoData ssmmd = new SSMonitorMeteoData();

            ssmmd.setAreaID(areaID);

            /*
             * Wind Speed
             */
            xmlKey = SafeSeasMonitor.SS_MON_METEO_WIND_SPEED.getXmlKey();
            ssmmd.setWindSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssmmd.setWindSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Peak Wind
             */
            xmlKey = SafeSeasMonitor.SS_MON_METEO_PEAK_WIND.getXmlKey();
            ssmmd.setPeakWindR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssmmd.setPeakWindY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Gust Speed
             */
            xmlKey = SafeSeasMonitor.SS_MON_METEO_GUST_SPEED.getXmlKey();
            ssmmd.setGustSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssmmd.setGustSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Wave Height
             */
            xmlKey = SafeSeasMonitor.SS_MON_METEO_WAVE_HT.getXmlKey();
            ssmmd.setWaveHgtR(sstm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            ssmmd.setWaveHgtY(sstm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Visibility
             */
            xmlKey = SafeSeasMonitor.SS_MON_METEO_VIS.getXmlKey();
            ssmmd.setVisR(sstm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            ssmmd.setVisY(sstm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Add data to array.
             */
            ssDataArray.add(ssmmd);
        }
    }

    private SSMonitorMeteoData getDataAtFirstSelection() {
        int index = dataTable.getSelectionIndex();

        return ssDataArray.get(index);
    }

    private void updateDataArray(SSMonitorMeteoData ssmmd) {
        int[] dataListIndexes = dataTable.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];

            ssDataArray.get(currentIndex).updateData(ssmmd);
        }
    }

    @Override
    public void commitDataToXML() {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (SSMonitorMeteoData ssmmd : ssDataArray) {
            areaID = ssmmd.getAreaID();

            /*
             * Wind Speed
             */
            xmlKey = SafeSeasMonitor.SS_MON_METEO_WIND_SPEED.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssmmd.getWindSpeedR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssmmd.getWindSpeedY());

            /*
             * Peak Wind
             */
            xmlKey = SafeSeasMonitor.SS_MON_METEO_PEAK_WIND.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssmmd.getPeakWindR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssmmd.getPeakWindY());

            /*
             * Gust Speed
             */
            xmlKey = SafeSeasMonitor.SS_MON_METEO_GUST_SPEED.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssmmd.getGustSpeedR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssmmd.getGustSpeedY());

            /*
             * Wave Height
             */
            xmlKey = SafeSeasMonitor.SS_MON_METEO_WAVE_HT.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssmmd.getWaveHgtR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssmmd.getWaveHgtY());

            /*
             * Visibility
             */
            xmlKey = SafeSeasMonitor.SS_MON_METEO_VIS.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssmmd.getVisR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssmmd.getVisY());
        }
    }

    @Override
    public void reloadData() {
        dataTable.removeAll();
        ssDataArray.clear();
        ssDataArray = null;

        populateTable();
    }

    @Override
    protected void editDataAction() {
        SSMonitorMeteoData ssmmd = getDataAtFirstSelection();

        if (monitorMeteoEditDlg == null) {
            monitorMeteoEditDlg = new SSMonitorMeteoEditDlg(getParent()
                    .getShell(), ssmmd, this);
            monitorMeteoEditDlg.open();
            monitorMeteoEditDlg = null;
        }
    }

    @Override
    public void updateThresholdData(SSMonitorMeteoData ssmmd) {
        updateDataArray(ssmmd);
        populateTable();
    }
}
