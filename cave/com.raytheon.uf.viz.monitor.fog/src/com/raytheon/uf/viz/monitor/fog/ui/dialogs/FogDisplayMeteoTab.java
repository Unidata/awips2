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
package com.raytheon.uf.viz.monitor.fog.ui.dialogs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.fog.threshold.FogDisplayMeteoData;
import com.raytheon.uf.viz.monitor.fog.threshold.FogThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.FogDisplay;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

/**
 * Fog Display Meteo Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------
 * May 21, 2014  3086     skorolev  Cleaned code
 * Jun 02, 2016  5673     randerso  Fixed header alignment in threshold dialogs
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class FogDisplayMeteoTab extends TabItemComp implements
        IUpdateDisplayMeteo {
    /** Dialog used for editing the display meteo data. */
    private FogDisplayMeteoEditDlg fogMeteoEditDlg;

    /** List of zones */
    private List<String> areaIDArray;

    /** Fog Display Meteo Data. */
    private List<FogDisplayMeteoData> fogDataArray;

    /**
     * Constructor
     * 
     * @param parent
     * @param duKey
     *            threshold usage data key
     */
    public FogDisplayMeteoTab(TabFolder parent, DataUsageKey duKey) {
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
        createHeader("Vis(mi)", 1, 2, true);
        createHeader("Ceiling\n(100ft)", 3, 4, true);
        createHeader("Temp(F)", 5, 6, true);
        createHeader("Dewpt(F)", 7, 8, true);
        createHeader("T-Td(F)", 9, 10, true);
        createHeader("Rel Hum\n(%)", 11, 12, true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#populateList()
     */
    @Override
    protected void populateTable() {
        if (fogDataArray == null) {
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

        FogDisplayMeteoData fdmd = null;

        int numColumns = 13;
        new TableColumn(dataTable, SWT.LEFT);
        for (int c = 1; c < numColumns; c++) {
            new TableColumn(dataTable, SWT.RIGHT);
        }

        for (int i = 0; i < fogDataArray.size(); i++) {

            TableItem item;
            if (update == true) {
                item = dataTable.getItem(i);
            } else {
                item = new TableItem(dataTable, SWT.NONE);
            }

            fdmd = fogDataArray.get(i);

            currentAreaID = fdmd.getAreaID();
            areaIDArray.add(currentAreaID);

            item.setText(0, currentAreaID);

            /* Visibility */
            visVal = fdmd.getMeteoVisR();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(1, String.format(dataFmt, tmpVisStr));

            visVal = fdmd.getMeteoVisY();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(2, String.format(dataFmt, tmpVisStr));

            /* Ceiling */
            appendIntData(item, 3, fdmd.getMeteoCeilingR(),
                    fdmd.getMeteoCeilingY());

            /* Temperature */
            appendIntData(item, 5, fdmd.getMeteoTempR(), fdmd.getMeteoTempY());

            /* Dewpoint */
            appendIntData(item, 7, fdmd.getMeteoDewpointR(),
                    fdmd.getMeteoDewpointY());

            /* T-Td */
            appendIntData(item, 9, fdmd.getMeteoTtdR(), fdmd.getMeteoTtdY());

            /* Relative Humidity */
            appendIntData(item, 11, fdmd.getMeteoRelHumR(),
                    fdmd.getMeteoRelHumY());

        }
        packListControls();
    }

    /**
     * Creates Data Array.
     */
    private void createDataArray() {
        fogDataArray = new ArrayList<FogDisplayMeteoData>();

        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = ftm.getThresholdsXmlData(duKey);

        List<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            FogDisplayMeteoData fdmd = new FogDisplayMeteoData();

            fdmd.setAreaID(areaID);

            /* Visibility */
            xmlKey = FogDisplay.FOG_DISP_METEO_VIS.getXmlKey();
            fdmd.setMeteoVisR(ftm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            fdmd.setMeteoVisY(ftm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Ceiling */
            xmlKey = FogDisplay.FOG_DISP_METEO_CEILING.getXmlKey();
            fdmd.setMeteoCeilingR(ftm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            fdmd.setMeteoCeilingY(ftm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /* Temperature */
            xmlKey = FogDisplay.FOG_DISP_METEO_TEMP.getXmlKey();
            fdmd.setMeteoTempR(ftm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            fdmd.setMeteoTempY(ftm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Dewpoint */
            xmlKey = FogDisplay.FOG_DISP_METEO_DEWPT.getXmlKey();
            fdmd.setMeteoDewpointR(ftm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            fdmd.setMeteoDewpointY(ftm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /* T-Td */
            xmlKey = FogDisplay.FOG_DISP_METEO_T_TD.getXmlKey();
            fdmd.setMeteoTtdR(ftm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            fdmd.setMeteoTtdY(ftm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Relative Humidity */
            xmlKey = FogDisplay.FOG_DISP_METEO_REL_HUMIDITY.getXmlKey();
            fdmd.setMeteoRelHumR(ftm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            fdmd.setMeteoRelHumY(ftm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /* Add data to array. */
            fogDataArray.add(fdmd);
        }
    }

    /**
     * Gets Data At First Selection.
     * 
     * @return selected data
     */
    private FogDisplayMeteoData getDataAtFirstSelection() {
        int index = dataTable.getSelectionIndex();
        return fogDataArray.get(index);
    }

    /**
     * Updates Data Array.
     * 
     * @param fdmd
     *            Display Meteo Data
     */
    private void updateDataArray(FogDisplayMeteoData fdmd) {
        int[] dataListIndexes = dataTable.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];
            fogDataArray.get(currentIndex).updateData(fdmd);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#commitDataToXML()
     */
    @Override
    public void commitDataToXML() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (FogDisplayMeteoData fdmd : fogDataArray) {
            areaID = fdmd.getAreaID();

            /* Visibility */
            xmlKey = FogDisplay.FOG_DISP_METEO_VIS.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoVisR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoVisY());

            /* Ceiling */
            xmlKey = FogDisplay.FOG_DISP_METEO_CEILING.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoCeilingR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoCeilingY());

            /* Temperature */
            xmlKey = FogDisplay.FOG_DISP_METEO_TEMP.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoTempR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoTempY());

            /* Dewpoint */
            xmlKey = FogDisplay.FOG_DISP_METEO_DEWPT.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoDewpointR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoDewpointY());

            /* T-Td */
            xmlKey = FogDisplay.FOG_DISP_METEO_T_TD.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoTtdR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoTtdY());

            /* Relative Humidity */
            xmlKey = FogDisplay.FOG_DISP_METEO_REL_HUMIDITY.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoRelHumR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoRelHumY());
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
        fogDataArray.clear();
        fogDataArray = null;
        populateTable();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#editDataAction()
     */
    @Override
    protected void editDataAction() {
        FogDisplayMeteoData fdmd = getDataAtFirstSelection();

        if (fogMeteoEditDlg == null) {
            fogMeteoEditDlg = new FogDisplayMeteoEditDlg(
                    getParent().getShell(), fdmd, this);
            fogMeteoEditDlg.open();
            fogMeteoEditDlg = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.fog.ui.dialogs.IUpdateDisplayMeteo#
     * updateThresholdData
     * (com.raytheon.uf.viz.monitor.fog.threshold.FogDisplayMeteoData)
     */
    @Override
    public void updateThresholdData(FogDisplayMeteoData fdmd) {
        updateDataArray(fdmd);
        populateTable();
    }
}
