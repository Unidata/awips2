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
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSDisplayWindData;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SafeSeasDisplay;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

/**
 * SAFESEAS Display Wind Table.
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
public class SSDisplayWindTab extends TabItemComp implements IUpdateDisplayWind {
    private SSDisplayWindEditDlg windEditDlg;

    private ArrayList<String> areaIDArray;

    private ArrayList<SSDisplayWindData> ssDataArray;

    public SSDisplayWindTab(TabFolder parent, DataUsageKey duKey) {
        super(parent, duKey);
    }

    @Override
    protected void createListHeader() {
        /*
         * Create filler label.
         */
        createHeader("", 0, 0, false);

        /*
         * Wind
         */
        createHeader("Wind\nSpeed(kt)", 1, 2, true);
        createHeader("Peak\nWind(kt)", 3, 4, true);
        createHeader("Gust\nSpeed(kt)", 5, 6, true);
        createHeader("Wind\nDir(deg)\n(from)", 7, 8, true);
        createHeader("Wind\nDir(deg)\n(to)", 9, 10, true);
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

        areaIDArray = new ArrayList<String>();

        String currentAreaID;

        SSDisplayWindData ssdwd = null;

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

            ssdwd = ssDataArray.get(i);

            currentAreaID = ssdwd.getAreaID();
            areaIDArray.add(currentAreaID);

            item.setText(0, currentAreaID);

            /*
             * Wind Speed
             */
            appendIntData(item, 1, ssdwd.getWindSpeedR(), ssdwd.getWindSpeedY());

            /*
             * Peak Wind
             */
            appendIntData(item, 3, ssdwd.getPeakWindR(), ssdwd.getPeakWindY());

            /*
             * Gust Speed
             */
            appendIntData(item, 5, ssdwd.getGustSpeedR(), ssdwd.getGustSpeedY());

            /*
             * Wind Direction From
             */
            appendIntData(item, 7, ssdwd.getWindDirFromY(),
                    ssdwd.getWindDirFromR());

            /*
             * Wind Direction To
             */
            appendIntData(item, 9, ssdwd.getWindDirToR(), ssdwd.getWindDirToY());

        }

        packListControls();
    }

    private void createDataArray() {
        ssDataArray = new ArrayList<SSDisplayWindData>();

        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = sstm.getThresholdsXmlData(duKey);

        ArrayList<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            SSDisplayWindData ssdwd = new SSDisplayWindData();

            ssdwd.setAreaID(areaID);

            /*
             * Wind Speed
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_WIND_SPEED.getXmlKey();
            ssdwd.setWindSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdwd.setWindSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Peak Wind
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_PEAK_WIND.getXmlKey();
            ssdwd.setPeakWindR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdwd.setPeakWindY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Gust Speed
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_GUST_SPEED.getXmlKey();
            ssdwd.setGustSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdwd.setGustSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Wind Direction From
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_DIR_FROM.getXmlKey();
            ssdwd.setWindDirFromR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdwd.setWindDirFromY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Wind Direction To
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_DIR_TO.getXmlKey();
            ssdwd.setWindDirToR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdwd.setWindDirToY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Add data to array.
             */
            ssDataArray.add(ssdwd);
        }
    }

    private SSDisplayWindData getDataAtFirstSelection() {

        int index = dataTable.getSelectionIndex();

        return ssDataArray.get(index);

    }

    private void updateDataArray(SSDisplayWindData ssdwd) {
        int[] dataListIndexes = dataTable.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];

            ssDataArray.get(currentIndex).updateData(ssdwd);
        }
    }

    @Override
    public void commitDataToXML() {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (SSDisplayWindData ssdwd : ssDataArray) {
            areaID = ssdwd.getAreaID();

            /*
             * Wind Speed
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_WIND_SPEED.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdwd.getWindSpeedR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdwd.getWindSpeedY());

            /*
             * Peak Wind
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_PEAK_WIND.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdwd.getPeakWindR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdwd.getPeakWindY());

            /*
             * Gust Speed
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_GUST_SPEED.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdwd.getGustSpeedR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdwd.getGustSpeedY());

            /*
             * Wind Direction From
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_DIR_FROM.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdwd.getWindDirFromR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdwd.getWindDirFromY());

            /*
             * Wind Direction To
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_DIR_TO.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdwd.getWindDirToR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdwd.getWindDirToY());
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
        SSDisplayWindData ssdwd = getDataAtFirstSelection();

        if (windEditDlg == null) {
            windEditDlg = new SSDisplayWindEditDlg(getParent().getShell(),
                    ssdwd, this);
            windEditDlg.open();
            windEditDlg = null;
        }
    }

    @Override
    public void updateThresholdData(SSDisplayWindData ssdwd) {
        updateDataArray(ssdwd);
        populateTable();
    }
}
