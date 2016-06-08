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

import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.SnowDisplay;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.xml.AreaXML;
import com.raytheon.uf.common.monitor.xml.ThresholdsXML;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowDisplayWindData;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;

/**
 * SNOW Display Wind Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * ????                   ????      Initial creation
 * Dec 26, 2015  5115     skorolev  Corrected imports.
 * Jun 02, 2016  5673     randerso  Fixed header alignment in threshold dialogs
 * 
 * </pre>
 * 
 * @author ????
 * @version 1.0
 */
public class SnowDisplayWindTab extends TabItemComp implements
        IUpdateDisplayWind {
    private SnowDisplayWindEditDlg windEditDlg;

    private ArrayList<String> areaIDArray;

    private ArrayList<SnowDisplayWindData> snowDataArray;

    public SnowDisplayWindTab(TabFolder parent, DataUsageKey duKey) {
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
        if (snowDataArray == null) {
            createDataArray();
        }

        boolean update = false;
        if (dataTable.getItemCount() > 0) {
            update = true;
        }

        areaIDArray = new ArrayList<String>();

        String currentAreaID;

        SnowDisplayWindData sdwd = null;

        int numColumns = 11;
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

            sdwd = snowDataArray.get(i);

            currentAreaID = sdwd.getAreaID();
            areaIDArray.add(currentAreaID);

            item.setText(0, currentAreaID);

            /*
             * Wind Speed
             */
            appendIntData(item, 1, sdwd.getWindWindSpeedR(),
                    sdwd.getWindWindSpeedY());

            /*
             * Peak Wind
             */
            appendIntData(item, 3, sdwd.getWindPeakR(), sdwd.getWindPeakY());

            /*
             * Gust Speed
             */
            appendIntData(item, 5, sdwd.getWindGustR(), sdwd.getWindGustY());

            /*
             * Wind Direction From
             */
            appendIntData(item, 7, sdwd.getWindDirFromY(),
                    sdwd.getWindDirFromR());

            /*
             * Wind Direction To
             */
            appendIntData(item, 9, sdwd.getWindDirToR(), sdwd.getWindDirToY());

        }

        packListControls();
    }

    private void createDataArray() {
        snowDataArray = new ArrayList<SnowDisplayWindData>();

        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = stm.getThresholdsXmlData(duKey);

        ArrayList<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            SnowDisplayWindData sdwd = new SnowDisplayWindData();

            sdwd.setAreaID(areaID);

            /*
             * Wind Speed
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_WIND_SPEED.getXmlKey();
            sdwd.setWindWindSpeedR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdwd.setWindWindSpeedY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Peak Wind
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_PEAK_WIND.getXmlKey();
            sdwd.setWindPeakR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdwd.setWindPeakY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Gust Speed
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_GUST_SPEED.getXmlKey();
            sdwd.setWindGustR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdwd.setWindGustY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Wind Direction From
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_DIR_FROM.getXmlKey();
            sdwd.setWindDirFromR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdwd.setWindDirFromY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Wind Direction From
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_DIR_TO.getXmlKey();
            sdwd.setWindDirToR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdwd.setWindDirToY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Add data to array.
             */
            snowDataArray.add(sdwd);
        }
    }

    private SnowDisplayWindData getDataAtFirstSelection() {
        int index = dataTable.getSelectionIndex();

        return snowDataArray.get(index);
    }

    private void updateDataArray(SnowDisplayWindData sdwd) {
        int[] dataListIndexes = dataTable.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];

            snowDataArray.get(currentIndex).updateData(sdwd);
        }
    }

    @Override
    public void commitDataToXML() {
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (SnowDisplayWindData sdwd : snowDataArray) {
            areaID = sdwd.getAreaID();

            /*
             * Wind Speed
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdwd.getWindWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdwd.getWindWindSpeedY());

            /*
             * Peak Wind
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdwd.getWindPeakR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdwd.getWindPeakY());

            /*
             * Gust Speed
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdwd.getWindGustR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdwd.getWindGustY());

            /*
             * Wind Direction From
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_DIR_FROM.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdwd.getWindDirFromR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdwd.getWindDirFromY());

            /*
             * Wind Direction To
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_DIR_TO.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdwd.getWindDirToR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdwd.getWindDirToY());
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
        SnowDisplayWindData sdwd = getDataAtFirstSelection();

        if (windEditDlg == null) {
            windEditDlg = new SnowDisplayWindEditDlg(getParent().getShell(),
                    sdwd, this);
            windEditDlg.open();
            windEditDlg = null;
        }
    }

    @Override
    public void updateThresholdData(SnowDisplayWindData sdwd) {
        updateDataArray(sdwd);
        populateTable();
    }
}
