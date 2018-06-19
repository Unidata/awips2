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
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowDisplayProductData;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;

/**
 * SNOW Display Product Table.
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
public class SnowDisplayProductTab extends TabItemComp implements
        IUpdateDisplayProduct {
    private SnowDisplayProductEditDlg productEditDlg;

    private ArrayList<String> areaIDArray;

    private ArrayList<SnowDisplayProductData> snowDataArray;

    public SnowDisplayProductTab(TabFolder parent, DataUsageKey duKey) {
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
         * Blizzard Warning
         */
        createGroupHeader("Blizzard Warning", 1, 8, true);
        createHeader("Vis(mi)", 1, 2, true);
        createHeader("Wind\nSpeed(kt)", 3, 4, true);
        createHeader("Gust\nSpeed(kt)", 5, 6, true);
        createHeader("Peak\nWind(kt)", 7, 8, true);

        /*
         * Freezing Precip
         */
        createGroupHeader("Freezing Precip", 9, 12, true);
        createHeader("Temp(F)", 9, 10, true);
        createHeader("Hourly\nPrcp(in)", 11, 12, true);

        /*
         * Heavy Snow Warning
         */
        createGroupHeader("Heavy Snow Warning", 13, 18, true);
        createHeader("SNINCR\nHr(in)", 13, 14, true);
        createHeader("SNINCR\nTot(in)", 15, 16, true);
        createHeader("Snow\nDepth(in)", 17, 18, true);
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

        SnowDisplayProductData sdpd = null;

        int numColumns = 19;
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

            sdpd = snowDataArray.get(i);

            currentAreaID = sdpd.getAreaID();
            areaIDArray.add(currentAreaID);

            item.setText(0, currentAreaID);

            /*
             * Blizzard Warning
             */
            visVal = sdpd.getBlizWrnVisR();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(1, String.format(dataFmt, tmpVisStr));

            visVal = sdpd.getBlizWrnVisY();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(2, String.format(dataFmt, tmpVisStr));

            appendIntData(item, 3, sdpd.getBlizWrnWindSpdR(),
                    sdpd.getBlizWrnWindSpdY());
            appendIntData(item, 5, sdpd.getBlizWrnGustSpdR(),
                    sdpd.getBlizWrnGustSpdY());
            appendIntData(item, 7, sdpd.getBlizWrnPeakWindR(),
                    sdpd.getBlizWrnPeakWindY());

            /*
             * Freezing Precip
             */
            appendIntData(item, 9, sdpd.getFrzPrecipTempR(),
                    sdpd.getFrzPrecipTempY());
            appendDecimalData(item, 11, sdpd.getFrzPrecipHrlyPrcpR(),
                    sdpd.getFrzPrecipHrlyPrcpY());

            /*
             * Heavy Snow Warning
             */
            appendIntData(item, 13, sdpd.getHvySnowSnincrHrR(),
                    sdpd.getHvySnowSnincrHrY());
            appendIntData(item, 15, sdpd.getHvySnowSnincrTotR(),
                    sdpd.getHvySnowSnincrTotY());
            appendIntData(item, 17, sdpd.getHvySnowDepthR(),
                    sdpd.getHvySnowDepthY());

        }

        packListControls();
    }

    private void createDataArray() {
        snowDataArray = new ArrayList<SnowDisplayProductData>();

        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = stm.getThresholdsXmlData(duKey);

        ArrayList<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            SnowDisplayProductData sdpd = new SnowDisplayProductData();

            sdpd.setAreaID(areaID);

            /*
             * Blizzard Warning
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_VIS.getXmlKey();
            sdpd.setBlizWrnVisR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdpd.setBlizWrnVisY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_WIND_SPEED.getXmlKey();
            sdpd.setBlizWrnWindSpdR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdpd.setBlizWrnWindSpdY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_GUST_SPEED.getXmlKey();
            sdpd.setBlizWrnGustSpdR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdpd.setBlizWrnGustSpdY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_PEAK_WIND.getXmlKey();
            sdpd.setBlizWrnPeakWindR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdpd.setBlizWrnPeakWindY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Freezing Precip
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_FRZ_TEMP.getXmlKey();
            sdpd.setFrzPrecipTempR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdpd.setFrzPrecipTempY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SnowDisplay.SNOW_DISP_PROD_FRZ_HOURLY_PRECIP.getXmlKey();
            sdpd.setFrzPrecipHrlyPrcpR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdpd.setFrzPrecipHrlyPrcpY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Heavy Snow Warning
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_HOURLY.getXmlKey();
            sdpd.setHvySnowSnincrHrR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdpd.setHvySnowSnincrHrY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_TOTAL.getXmlKey();
            sdpd.setHvySnowSnincrTotR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdpd.setHvySnowSnincrTotY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNOW_DEPTH.getXmlKey();
            sdpd.setHvySnowDepthR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdpd.setHvySnowDepthY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Add data to array.
             */
            snowDataArray.add(sdpd);
        }
    }

    private SnowDisplayProductData getDataAtFirstSelection() {
        int index = dataTable.getSelectionIndex();

        return snowDataArray.get(index);
    }

    private void updateDataArray(SnowDisplayProductData sdpd) {
        int[] dataListIndexes = dataTable.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];

            snowDataArray.get(currentIndex).updateData(sdpd);
        }
    }

    @Override
    public void commitDataToXML() {
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (SnowDisplayProductData sdpd : snowDataArray) {
            areaID = sdpd.getAreaID();

            /*
             * Blizzard Warning
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_VIS.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdpd.getBlizWrnVisR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdpd.getBlizWrnVisY());

            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdpd.getBlizWrnWindSpdR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdpd.getBlizWrnWindSpdY());

            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdpd.getBlizWrnGustSpdR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdpd.getBlizWrnGustSpdY());

            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdpd.getBlizWrnPeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdpd.getBlizWrnPeakWindY());

            /*
             * Freezing Precip
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_FRZ_TEMP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdpd.getFrzPrecipTempR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdpd.getFrzPrecipTempY());

            xmlKey = SnowDisplay.SNOW_DISP_PROD_FRZ_HOURLY_PRECIP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdpd.getFrzPrecipHrlyPrcpR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdpd.getFrzPrecipHrlyPrcpY());

            /*
             * Heavy Snow Warning
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_HOURLY.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdpd.getHvySnowSnincrHrR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdpd.getHvySnowSnincrHrY());

            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_TOTAL.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdpd.getHvySnowSnincrTotR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdpd.getHvySnowSnincrTotY());

            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNOW_DEPTH.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdpd.getHvySnowDepthR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdpd.getHvySnowDepthY());
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
        SnowDisplayProductData sdpd = getDataAtFirstSelection();

        if (productEditDlg == null) {
            productEditDlg = new SnowDisplayProductEditDlg(getParent()
                    .getShell(), sdpd, this);
            productEditDlg.open();
            productEditDlg = null;
        }
    }

    @Override
    public void updateThresholdData(SnowDisplayProductData sdpd) {
        updateDataArray(sdpd);
        populateTable();
    }
}
