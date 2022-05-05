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

import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.FogMonitor;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.fog.threshold.FogMonitorMeteoData;
import com.raytheon.uf.viz.monitor.fog.threshold.FogThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;

/**
 * Fog Monitor Meteo Table
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 21, 2014  3086     skorolev  Cleaned code.
 * Dec 26, 2015  5115     skorolev  Corrected imports
 * Jun 02, 2016  5673     randerso  Fixed header alignment in threshold dialogs
 * Jun 26, 2018  7004     tgurney   Center-align table data
 * May 21, 2019  7689     randerso  Refactor handling of FSSObs thresholds
 *
 * </pre>
 *
 */
public class FogMonitorMeteoTab extends TabItemComp
        implements IUpdateMonitorMeteo {

    /** Dialog used for editing the Monitor meteo data. */
    private FogMonitorMeteoEditDlg fogMeteoEditDlg;

    /** Data Array */
    private List<FogMonitorMeteoData> fogDataArray;

    /**
     * Constructor.
     *
     * @param parent
     * @param duKey
     */
    public FogMonitorMeteoTab(TabFolder parent, DataUsageKey duKey) {
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
        createHeader("Vis(mi)", 1, 2, true);
    }

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

        List<String> areaIDArray = new ArrayList<>();

        String tmpVisStr;
        String currentAreaID;

        double visVal = 0.0;

        FogMonitorMeteoData fmmd = null;

        int numColumns = 3;
        for (int c = 0; c < numColumns; c++) {
            new TableColumn(dataTable, SWT.CENTER);
        }

        for (int i = 0; i < fogDataArray.size(); i++) {

            TableItem item;
            if (update) {
                item = dataTable.getItem(i);
            } else {
                item = new TableItem(dataTable, SWT.NONE);
            }

            fmmd = fogDataArray.get(i);

            currentAreaID = fmmd.getAreaID();
            areaIDArray.add(currentAreaID);

            item.setText(0, currentAreaID);

            /*
             * Visibility
             */
            visVal = fmmd.getMeteoVisR();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(1, formatTableData(tmpVisStr));

            visVal = fmmd.getMeteoVisY();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            item.setText(2, formatTableData(tmpVisStr));

        }

        packListControls();
    }

    /**
     * Create Data Array.
     */
    private void createDataArray() {
        fogDataArray = new ArrayList<>();

        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        String xmlKey;

        List<String> areaList = ftm.getCfgMgr().getAreaList();
        for (String areaID : areaList) {
            FogMonitorMeteoData fmmd = new FogMonitorMeteoData();
            fmmd.setAreaID(areaID);

            /*
             * Visibility
             */
            xmlKey = FogMonitor.FOG_MONITOR_METEO_VIS.getXmlKey();
            fmmd.setMeteoVisR(
                    ftm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            fmmd.setMeteoVisY(
                    ftm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            fogDataArray.add(fmmd);
        }
    }

    /**
     * Gets Data At First Selection
     *
     * @return
     */
    private FogMonitorMeteoData getDataAtFirstSelection() {

        int index = dataTable.getSelectionIndex();

        return fogDataArray.get(index);

    }

    /**
     * Update Fog Data Array.
     *
     * @param fmmd
     *            Meteo data
     */
    private void updateFogDataArray(FogMonitorMeteoData fmmd) {
        int[] dataListIndexes = dataTable.getSelectionIndices();
        int currentIndex = 0;

        for (int dataListIndexe : dataListIndexes) {
            currentIndex = dataListIndexe;
            fogDataArray.get(currentIndex).updateData(fmmd);
        }
    }

    @Override
    public void commitDataToXML() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (FogMonitorMeteoData fdmd : fogDataArray) {
            areaID = fdmd.getAreaID();

            /*
             * Visibility
             */
            xmlKey = FogMonitor.FOG_MONITOR_METEO_VIS.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoVisR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoVisY());
        }
    }

    @Override
    public void reloadData() {
        dataTable.removeAll();
        fogDataArray.clear();
        fogDataArray = null;
        populateTable();
    }

    @Override
    protected void editDataAction() {
        FogMonitorMeteoData fdmd = getDataAtFirstSelection();

        if (fogMeteoEditDlg == null) {
            fogMeteoEditDlg = new FogMonitorMeteoEditDlg(getParent().getShell(),
                    fdmd, this);
            fogMeteoEditDlg.open();
            fogMeteoEditDlg = null;
        }
    }

    @Override
    public void updateThresholdData(FogMonitorMeteoData fmmd) {
        updateFogDataArray(fmmd);
        populateTable();
    }
}
