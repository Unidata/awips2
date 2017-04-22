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
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.SnowDisplay;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.xml.AreaXML;
import com.raytheon.uf.common.monitor.xml.ThresholdsXML;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowDisplayMeteoData;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;

/**
 * Snow Display Meteo Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------
 * May 21, 2014  3086     skorolev  Cleaned code.
 * Dec 26, 2015 5114       skorolev    Corrected imports.
 * Jun 02, 2016  5673     randerso  Fixed header alignment in threshold dialogs
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class SnowDisplayMeteoTab extends TabItemComp implements
        IUpdateDisplayMeteo {

    /** Dialog used for editing the display meteo data. */
    private SnowDisplayMeteoEditDlg meteoEditDlg;

    /** List of zones */
    private List<String> areaIDArray;

    /** Snow Display Meteo Data. */
    private List<SnowDisplayMeteoData> snowDataArray;

    /**
     * Constructor
     * 
     * @param parent
     * @param duKey
     *            threshold usage data key
     */
    public SnowDisplayMeteoTab(TabFolder parent, DataUsageKey duKey) {
        super(parent, duKey);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#createListHeader(org
     * .eclipse.swt.widgets.Composite)
     */
    @Override
    protected void createListHeader() {
        /* Create filler label. */
        createHeader("", 0, 0, false);

        /* Meteo */
        createHeader("Temp(F)", 1, 2, true);
        createHeader("Dewpt(F)", 3, 4, true);
        createHeader("Vis(mi)", 5, 6, true);
        createHeader("SLP(mb)", 7, 8, true);
        createHeader("Hourly\nPrcp(in)", 9, 10, true);
        createHeader("Wind\nChill(F)", 11, 12, true);
        createHeader("Frost\nBite\nTime(min)", 13, 14, true);
        createHeader("Snow\nDepth(in)", 15, 16, true);
        createHeader("SNINCR\nHr(in)", 17, 18, true);
        createHeader("SNINCR\nTot(in)", 19, 20, true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#populateList()
     */
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

        SnowDisplayMeteoData sdmd = null;

        int numColumns = 21;
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

            sdmd = snowDataArray.get(i);

            currentAreaID = sdmd.getAreaID();
            areaIDArray.add(currentAreaID);

            item.setText(0, currentAreaID);

            /* Temperature */
            appendIntData(item, 1, sdmd.getTempR(), sdmd.getTempY());

            /* Dew point */
            appendIntData(item, 3, sdmd.getDewpointR(), sdmd.getDewpointY());

            /* Visibility */
            visVal = sdmd.getVisR();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(5, String.format(dataFmt, tmpVisStr));

            visVal = sdmd.getVisY();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(6, String.format(dataFmt, tmpVisStr));

            /* SLP */
            appendIntData(item, 7, sdmd.getSlpR(), sdmd.getSlpY());

            /* Hourly Precipitation */
            appendDecimalData(item, 9, sdmd.getHrPrecipR(), sdmd.getHrPrecipY());

            /* Wind Chill */
            appendIntData(item, 11, sdmd.getWindChillR(), sdmd.getWindChillY());

            /* Frost Bite */
            appendIntData(item, 13, sdmd.getFrostBiteR(), sdmd.getFrostBiteY());

            /* Snow Depth */
            appendIntData(item, 15, sdmd.getSnowDepthR(), sdmd.getSnowDepthY());

            /* SNINCR Hourly */
            appendIntData(item, 17, sdmd.getSnincrHrlyR(),
                    sdmd.getSnincrHrlyY());

            /* SNINCR Total */
            appendIntData(item, 19, sdmd.getSnincrTotR(), sdmd.getSnincrTotY());

        }
        packListControls();
    }

    /**
     * Creates Data Array.
     */
    private void createDataArray() {
        snowDataArray = new ArrayList<SnowDisplayMeteoData>();

        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = stm.getThresholdsXmlData(duKey);

        List<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            SnowDisplayMeteoData sdmd = new SnowDisplayMeteoData();

            sdmd.setAreaID(areaID);

            /* Tempature */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_TEMP.getXmlKey();
            sdmd.setTempR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setTempY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Dew point */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_DEWPT.getXmlKey();
            sdmd.setDewpointR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setDewpointY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Visibility */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_VIS.getXmlKey();
            sdmd.setVisR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setVisY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* SLP */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SLP.getXmlKey();
            sdmd.setSlpR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setSlpY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Hourly Precipitation */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_HOURLY_PRECIP.getXmlKey();
            sdmd.setHrPrecipR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setHrPrecipY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Wind Chill */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_WIND_CHILL.getXmlKey();
            sdmd.setWindChillR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setWindChillY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Frost Bite */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_FROSTBITE.getXmlKey();
            sdmd.setFrostBiteR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setFrostBiteY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Snow Depth */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNOW_DEPTH.getXmlKey();
            sdmd.setSnowDepthR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setSnowDepthY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* SNINCR Hourly */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNINCR_HOURLY.getXmlKey();
            sdmd.setSnincrHrlyR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdmd.setSnincrHrlyY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /* SNINCR Total */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNINCR_TOTAL.getXmlKey();
            sdmd.setSnincrTotR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setSnincrTotY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Add data to array. */
            snowDataArray.add(sdmd);
        }
    }

    /**
     * Gets Data At First Selection.
     * 
     * @return selected data
     */
    private SnowDisplayMeteoData getDataAtFirstSelection() {
        int index = dataTable.getSelectionIndex();
        return snowDataArray.get(index);
    }

    /**
     * Updates Data Array.
     * 
     * @param sdmd
     *            Display Meteo Data
     */
    private void updateDataArray(SnowDisplayMeteoData sdmd) {
        int[] dataListIndexes = dataTable.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];

            snowDataArray.get(currentIndex).updateData(sdmd);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#commitDataToXML()
     */
    @Override
    public void commitDataToXML() {
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (SnowDisplayMeteoData sdmd : snowDataArray) {
            areaID = sdmd.getAreaID();

            /* Temperature */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_TEMP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getTempR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getTempY());

            /* Dew point */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_DEWPT.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getDewpointR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getDewpointY());

            /* Visibility */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_VIS.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getVisR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getVisY());

            /* SLP */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SLP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getSlpR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getSlpY());

            /* Hourly Precipitation */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_HOURLY_PRECIP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getHrPrecipR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getHrPrecipY());

            /* Wind Chill */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_WIND_CHILL.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getWindChillR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getWindChillY());

            /* Frost Bite */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_FROSTBITE.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getFrostBiteR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getFrostBiteY());

            /* Snow Depth */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNOW_DEPTH.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getSnowDepthR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getSnowDepthY());

            /* SNINCR Hourly */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNINCR_HOURLY.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getSnincrHrlyR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getSnincrHrlyY());

            /* SNINCR Total */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNINCR_TOTAL.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getSnincrTotR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getSnincrTotY());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#reloadData()
     */
    @Override
    public void reloadData() {
        dataTable.removeAll();
        snowDataArray.clear();
        snowDataArray = null;
        populateTable();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#editDataAction()
     */
    @Override
    protected void editDataAction() {
        SnowDisplayMeteoData sdmd = getDataAtFirstSelection();

        if (meteoEditDlg == null) {
            meteoEditDlg = new SnowDisplayMeteoEditDlg(getParent().getShell(),
                    sdmd, this);
            meteoEditDlg.open();
            meteoEditDlg = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.snow.ui.dialogs.IUpdateDisplayMeteo#
     * updateThresholdData
     * (com.raytheon.uf.viz.monitor.snow.threshold.SnowDisplayMeteoData)
     */
    @Override
    public void updateThresholdData(SnowDisplayMeteoData sdmd) {
        updateDataArray(sdmd);
        populateTable();
    }
}
