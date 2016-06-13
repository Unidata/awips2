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
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.SafeSeasDisplay;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.xml.AreaXML;
import com.raytheon.uf.common.monitor.xml.ThresholdsXML;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSDisplayMeteoData;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;

/**
 * SAFESEAS Display Meteo Table
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------
 * May 21, 2014  3086     skorolev  Cleaned code
 * Dec 26, 2015 5114       skorolev    Corrected imports.
 * Jun 02, 2016  5673     randerso  Fixed header alignment in threshold dialogs
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class SSDisplayMeteoTab extends TabItemComp implements
        IUpdateDisplayMeteo {

    /** Dialog used for editing the display meteo data. */
    private SSDisplayMeteoEditDlg editMeteoDlg;

    /** List of zones */
    private List<String> areaIDArray;

    /** SAFESEAS Data Array */
    private List<SSDisplayMeteoData> ssDataArray;

    /**
     * Constructor
     * 
     * @param parent
     * @param duKey
     *            threshold usage data key
     */
    public SSDisplayMeteoTab(TabFolder parent, DataUsageKey duKey) {
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
        /*
         * Create filler label.
         */
        createHeader("", 0, 0, false);

        /*
         * Wind
         */
        createHeader("Vis(nm)", 1, 2, true);
        createHeader("Temp(F)", 3, 4, true);
        createHeader("Dewpt(F)", 5, 6, true);
        createHeader("SLP(mb)", 7, 8, true);
        createHeader("SST(F)", 9, 10, true);
        createHeader("Wave\nHeight(ft)", 11, 12, true);
        createHeader("Wave\nSteep\n(x1000)", 13, 14, true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#populateList()
     */
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

        SSDisplayMeteoData ssdmd = null;

        int numColumns = 15;
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

            ssdmd = ssDataArray.get(i);

            currentAreaID = ssdmd.getAreaID();
            areaIDArray.add(currentAreaID);

            item.setText(0, currentAreaID);

            /* Visibility */
            visVal = ssdmd.getVisR();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(1, String.format(dataFmt, tmpVisStr));

            visVal = ssdmd.getVisY();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(2, String.format(dataFmt, tmpVisStr));

            /* Temperature */
            appendIntData(item, 3, ssdmd.getTempR(), ssdmd.getTempY());

            /* Dew point */
            appendIntData(item, 5, ssdmd.getDewpointR(), ssdmd.getDewpointY());

            /* SLP */
            appendIntData(item, 7, ssdmd.getSlpR(), ssdmd.getSlpY());

            /* SST */
            appendIntData(item, 9, ssdmd.getSstR(), ssdmd.getSstY());

            /* Wave Height */
            appendIntData(item, 11, ssdmd.getWaveHgtR(), ssdmd.getWaveHgtY());

            /* Wave Steep */
            appendIntData(item, 13, ssdmd.getWaveSteepR(),
                    ssdmd.getWaveSteepY());

        }
        packListControls();
    }

    /**
     * Create Data Array.
     */
    private void createDataArray() {
        ssDataArray = new ArrayList<SSDisplayMeteoData>();

        SSThresholdMgr sstm = new SSThresholdMgr();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = sstm.getThresholdsXmlData(duKey);

        List<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            SSDisplayMeteoData ssdmd = new SSDisplayMeteoData();

            ssdmd.setAreaID(areaID);

            /* Visibility */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_VIS.getXmlKey();
            ssdmd.setVisR(sstm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            ssdmd.setVisY(sstm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Temperature */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_TEMP.getXmlKey();
            ssdmd.setTempR(sstm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            ssdmd.setTempY(sstm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Dew point */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_DEWPT.getXmlKey();
            ssdmd.setDewpointR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdmd.setDewpointY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /* SLP */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_SLP.getXmlKey();
            ssdmd.setSlpR(sstm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            ssdmd.setSlpY(sstm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* SST */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_SST.getXmlKey();
            ssdmd.setSstR(sstm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            ssdmd.setSstY(sstm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Wave Height */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_WAVE_HT.getXmlKey();
            ssdmd.setWaveHgtR(sstm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            ssdmd.setWaveHgtY(sstm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Wave Steep */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_WAVE_STEEP.getXmlKey();
            ssdmd.setWaveSteepR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdmd.setWaveSteepY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /* Add data to array. */
            ssDataArray.add(ssdmd);
        }
    }

    /**
     * Gets Data At First Selection.
     * 
     * @return selected data
     */
    private SSDisplayMeteoData getDataAtFirstSelection() {
        int index = dataTable.getSelectionIndex();
        return ssDataArray.get(index);
    }

    /**
     * Updates Data Array.
     * 
     * @param ssdmd
     *            Display Meteo Data
     */
    private void updateDataArray(SSDisplayMeteoData ssdmd) {
        int[] dataListIndexes = dataTable.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];
            ssDataArray.get(currentIndex).updateData(ssdmd);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#commitDataToXML()
     */
    @Override
    public void commitDataToXML() {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();
        String xmlKey;
        String areaID;

        for (SSDisplayMeteoData ssdmd : ssDataArray) {
            areaID = ssdmd.getAreaID();

            /* Visibility */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_VIS.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdmd.getVisR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdmd.getVisY());

            /* Temperature */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_TEMP.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdmd.getTempR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdmd.getTempY());

            /* Dew point */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_DEWPT.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdmd.getDewpointR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdmd.getDewpointY());

            /* SLP */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_SLP.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdmd.getSlpR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdmd.getSlpY());

            /* SST */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_SST.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdmd.getSstR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdmd.getSstY());

            /* Wave Height */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_WAVE_HT.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdmd.getWaveHgtR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdmd.getWaveHgtY());

            /* Wave Steep */
            xmlKey = SafeSeasDisplay.SS_DISP_METEO_WAVE_STEEP.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdmd.getWaveSteepR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdmd.getWaveSteepY());
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
        ssDataArray.clear();
        ssDataArray = null;
        populateTable();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#editDataAction()
     */
    @Override
    protected void editDataAction() {
        SSDisplayMeteoData ssdmd = getDataAtFirstSelection();

        if (editMeteoDlg == null) {
            editMeteoDlg = new SSDisplayMeteoEditDlg(getParent().getShell(),
                    ssdmd, this);
            editMeteoDlg.open();
            editMeteoDlg = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.safeseas.ui.dialogs.IUpdateDisplayMeteo#
     * updateThresholdData
     * (com.raytheon.uf.viz.monitor.safeseas.threshold.SSDisplayMeteoData)
     */
    @Override
    public void updateThresholdData(SSDisplayMeteoData ssdmd) {
        updateDataArray(ssdmd);
        populateTable();
    }
}
