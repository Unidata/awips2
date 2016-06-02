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
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSDisplayProductData;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SafeSeasDisplay;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

/**
 * SAFESEAS Display Product Table.
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
public class SSDisplayProductTab extends TabItemComp implements
        IUpdateDisplayProduct {
    SSDisplayProductEditDlg productEditDlg;

    private ArrayList<String> areaIDArray;

    private ArrayList<SSDisplayProductData> ssDataArray;

    public SSDisplayProductTab(TabFolder parent, DataUsageKey duKey) {
        super(parent, duKey);
    }

    @Override
    protected void createListHeader() {
        /*
         * Create filler label.
         */
        createGroupHeader("", 0, 0, false);
        createHeader("", 0, 0, false);

        /*
         * Small AirCraft Warning
         */
        createGroupHeader("Small Craft Advisory", 1, 8, true);
        createHeader("Wind\nSpeed(kt)", 1, 2, true);
        createHeader("Gust\nSpeed(kt)", 3, 4, true);
        createHeader("Peak\nWind(kt)", 5, 6, true);
        createHeader("Wave\nHeight(ft)", 7, 8, true);

        /*
         * Gale Warning
         */
        createGroupHeader("Gale Warning", 9, 14, true);
        createHeader("Wind\nSpeed(kt)", 9, 10, true);
        createHeader("Gust\nSpeed(kt)", 11, 12, true);
        createHeader("Peak\nWind(kt)", 13, 14, true);

        /*
         * Storm Warning
         */
        createGroupHeader("Storm Warning", 15, 20, true);
        createHeader("Wind\nSpeed(kt)", 15, 16, true);
        createHeader("Gust\nSpeed(kt)", 17, 18, true);
        createHeader("Peak\nWind(kt)", 19, 20, true);

        /*
         * HFWW (Hurricane Force Wind Warning)
         */
        createGroupHeader("HFWW", 21, 26, true);
        createHeader("Wind\nSpeed(kt)", 21, 22, true);
        createHeader("Gust\nSpeed(kt)", 23, 24, true);
        createHeader("Peak\nWind(kt)", 25, 26, true);
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

        SSDisplayProductData ssdpd = null;

        int numColumns = 27;
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

            ssdpd = ssDataArray.get(i);

            currentAreaID = ssdpd.getAreaID();
            areaIDArray.add(currentAreaID);

            item.setText(0, currentAreaID);

            /*
             * Small Craft Advisory
             */
            appendIntData(item, 1, ssdpd.getScaWindSpeedR(),
                    ssdpd.getScaWindSpeedY());
            appendIntData(item, 3, ssdpd.getScaGustSpeedR(),
                    ssdpd.getScaGustSpeedY());
            appendIntData(item, 5, ssdpd.getScaPeakWindR(),
                    ssdpd.getScaPeakWindY());
            appendIntData(item, 7, ssdpd.getScaWaveHgtR(),
                    ssdpd.getScaWaveHgtY());

            /*
             * Gale Warning
             */
            appendIntData(item, 9, ssdpd.getGaleWindSpeedR(),
                    ssdpd.getGaleWindSpeedY());
            appendIntData(item, 11, ssdpd.getGaleGustSpeedR(),
                    ssdpd.getGaleGustSpeedY());
            appendIntData(item, 13, ssdpd.getGalePeakWindR(),
                    ssdpd.getGalePeakWindY());

            /*
             * Storm Warning
             */
            appendIntData(item, 15, ssdpd.getStormWrnWindSpeedR(),
                    ssdpd.getStormWrnWindSpeedY());
            appendIntData(item, 17, ssdpd.getStormWrnGustSpeedR(),
                    ssdpd.getStormWrnGustSpeedY());
            appendIntData(item, 19, ssdpd.getStormWrnPeakWindR(),
                    ssdpd.getStormWrnPeakWindY());

            /*
             * HFWW
             */
            appendIntData(item, 21, ssdpd.getHfwwWindSpeedR(),
                    ssdpd.getHfwwWindSpeedY());
            appendIntData(item, 23, ssdpd.getHfwwGustSpeedR(),
                    ssdpd.getHfwwGustSpeedY());
            appendIntData(item, 25, ssdpd.getHfwwPeakWindR(),
                    ssdpd.getHfwwPeakWindY());

        }

        packListControls();
    }

    private void createDataArray() {
        ssDataArray = new ArrayList<SSDisplayProductData>();

        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = sstm.getThresholdsXmlData(duKey);

        ArrayList<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            SSDisplayProductData ssdpd = new SSDisplayProductData();

            ssdpd.setAreaID(areaID);

            /*
             * Small Craft Advisory
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_WIND_SPEED.getXmlKey();
            ssdpd.setScaWindSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdpd.setScaWindSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_GUST_SPEED.getXmlKey();
            ssdpd.setScaGustSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdpd.setScaGustSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_PEAK_WIND.getXmlKey();
            ssdpd.setScaPeakWindR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdpd.setScaPeakWindY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_WAVE_HT.getXmlKey();
            ssdpd.setScaWaveHgtR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdpd.setScaWaveHgtY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Gale Warning
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_WIND_SPEED.getXmlKey();
            ssdpd.setGaleWindSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdpd.setGaleWindSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_GUST_SPEED.getXmlKey();
            ssdpd.setGaleGustSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdpd.setGaleGustSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_PEAK_WIND.getXmlKey();
            ssdpd.setGalePeakWindR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdpd.setGalePeakWindY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Storm Warning
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_WIND_SPEED.getXmlKey();
            ssdpd.setStormWrnWindSpeedR(sstm.getThresholdValue(duKey,
                    threshKeyR, areaID, xmlKey));
            ssdpd.setStormWrnWindSpeedY(sstm.getThresholdValue(duKey,
                    threshKeyY, areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_GUST_SPEED.getXmlKey();
            ssdpd.setStormWrnGustSpeedR(sstm.getThresholdValue(duKey,
                    threshKeyR, areaID, xmlKey));
            ssdpd.setStormWrnGustSpeedY(sstm.getThresholdValue(duKey,
                    threshKeyY, areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_PEAK_WIND.getXmlKey();
            ssdpd.setStormWrnPeakWindR(sstm.getThresholdValue(duKey,
                    threshKeyR, areaID, xmlKey));
            ssdpd.setStormWrnPeakWindY(sstm.getThresholdValue(duKey,
                    threshKeyY, areaID, xmlKey));

            /*
             * HFWW
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_WIND_SPEED.getXmlKey();
            ssdpd.setHfwwWindSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdpd.setHfwwWindSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_GUST_SPEED.getXmlKey();
            ssdpd.setHfwwGustSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdpd.setHfwwGustSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_PEAK_WIND.getXmlKey();
            ssdpd.setHfwwPeakWindR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdpd.setHfwwPeakWindY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Add data to the array.
             */
            ssDataArray.add(ssdpd);
        }
    }

    private SSDisplayProductData getDataAtFirstSelection() {
        int index = dataTable.getSelectionIndex();

        return ssDataArray.get(index);
    }

    private void updateDataArray(SSDisplayProductData ssdpd) {
        int[] dataListIndexes = dataTable.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];

            ssDataArray.get(currentIndex).updateData(ssdpd);
        }
    }

    @Override
    public void commitDataToXML() {
        SSThresholdMgr stm = SSThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (SSDisplayProductData ssdpd : ssDataArray) {
            areaID = ssdpd.getAreaID();

            /*
             * Small Craft Advisory
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getScaWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getScaWindSpeedY());

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getScaGustSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getScaGustSpeedY());

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getScaPeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getScaPeakWindY());

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_WAVE_HT.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getScaWaveHgtR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getScaWaveHgtY());

            /*
             * Gale Warning
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getGaleWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getGaleWindSpeedY());

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getGaleGustSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getGaleGustSpeedY());

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getGalePeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getGalePeakWindY());

            /*
             * Storm Warning
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getStormWrnWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getStormWrnWindSpeedY());

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getStormWrnGustSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getStormWrnGustSpeedY());

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getStormWrnPeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getStormWrnPeakWindY());

            /*
             * HFWW
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getHfwwWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getHfwwWindSpeedY());

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getHfwwGustSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getHfwwGustSpeedY());

            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdpd.getHfwwPeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdpd.getHfwwPeakWindY());
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
        SSDisplayProductData sspdp = getDataAtFirstSelection();

        if (productEditDlg == null) {
            productEditDlg = new SSDisplayProductEditDlg(
                    getParent().getShell(), sspdp, this);
            productEditDlg.open();
            productEditDlg = null;
        }
    }

    @Override
    public void updateThresholdData(SSDisplayProductData sspdp) {
        updateDataArray(sspdp);
        populateTable();
    }

}
