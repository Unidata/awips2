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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.fog.threshold.FogMonitorMeteoData;
import com.raytheon.uf.viz.monitor.fog.threshold.FogThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.FogMonitor;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

/**
 * Fog Monitor Meteo Table
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- -------------
 * May 21, 2014 3086       skorolev    Cleaned code.
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class FogMonitorMeteoTab extends TabItemComp implements
        IUpdateMonitorMeteo {

    /** Dialog used for editing the Monitor meteo data. */
    private FogMonitorMeteoEditDlg fogMeteoEditDlg;

    /** List of zones */
    private List<String> areaIDArray;

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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#createListHeader(org
     * .eclipse.swt.widgets.Composite)
     */
    @Override
    protected void createListHeader(Composite parentComp) {
        Composite lblComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(5, false);
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        lblComp.setLayout(gl);

        /*
         * Create filler label.
         */
        GridData gd = new GridData(71, SWT.DEFAULT);
        Label fillerLbl = new Label(lblComp, SWT.CENTER);
        fillerLbl.setLayoutData(gd);

        /*
         * Meteo
         */
        Composite meteoComp = createGroupComposite(lblComp, 10, null);
        createLabelComp(meteoComp, "Vis(mi)", "", true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#populateList()
     */
    @Override
    protected void populateList() {
        if (fogDataArray == null) {
            createDataArray();
        }

        boolean update = false;
        if (dataList.getItemCount() > 0) {
            update = true;
        }

        RangesUtil rangeUtil = RangesUtil.getInstance();

        areaIDArray = new ArrayList<String>();

        String tmpVisStr;
        String currentAreaID;

        double visVal = 0.0;

        StringBuilder sb = null;
        FogMonitorMeteoData fmmd = null;

        for (int i = 0; i < fogDataArray.size(); i++) {
            sb = new StringBuilder();

            fmmd = fogDataArray.get(i);

            currentAreaID = fmmd.getAreaID();
            areaIDArray.add(currentAreaID);

            sb.append(String.format(areaIdFmt, currentAreaID));

            /*
             * Visibility
             */
            visVal = fmmd.getMeteoVisR();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            sb.append(String.format(dataFmt, tmpVisStr));

            visVal = fmmd.getMeteoVisY();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            sb.append(String.format(dataFmt, tmpVisStr));

            sb.append(" ");

            if (update == true) {
                dataList.setItem(i, sb.toString());
            } else {
                dataList.add(sb.toString());
            }
        }

        packListControls();
    }

    /**
     * Create Data Array.
     */
    private void createDataArray() {
        fogDataArray = new ArrayList<FogMonitorMeteoData>();

        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        ThresholdsXML threshXML = ftm.getThresholdsXmlData(duKey);

        List<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            String areaID = area.getAreaId();
            FogMonitorMeteoData fmmd = new FogMonitorMeteoData();
            fmmd.setAreaID(areaID);

            /*
             * Visibility
             */
            String xmlKey = FogMonitor.FOG_MONITOR_METEO_VIS.getXmlKey();
            fmmd.setMeteoVisR(ftm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            fmmd.setMeteoVisY(ftm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            fogDataArray.add(fmmd);
        }
    }

    /**
     * Gets Data At First Selection
     * 
     * @return
     */
    private FogMonitorMeteoData getDataAtFirstSelection() {

        int index = dataList.getSelectionIndex();

        return fogDataArray.get(index);

    }

    /**
     * Update Fog Data Array.
     * 
     * @param fmmd
     *            Meteo data
     */
    private void updateFogDataArray(FogMonitorMeteoData fmmd) {
        int[] dataListIndexes = dataList.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];
            fogDataArray.get(currentIndex).updateData(fmmd);
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#reloadData()
     */
    @Override
    public void reloadData() {
        dataList.removeAll();
        fogDataArray.clear();
        fogDataArray = null;
        populateList();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#editDataAction()
     */
    @Override
    protected void editDataAction() {
        FogMonitorMeteoData fdmd = getDataAtFirstSelection();

        if (fogMeteoEditDlg == null) {
            fogMeteoEditDlg = new FogMonitorMeteoEditDlg(
                    getParent().getShell(), fdmd, this);
            fogMeteoEditDlg.open();
            fogMeteoEditDlg = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.fog.ui.dialogs.IUpdateMonitorMeteo#
     * updateThresholdData
     * (com.raytheon.uf.viz.monitor.fog.threshold.FogMonitorMeteoData)
     */
    @Override
    public void updateThresholdData(FogMonitorMeteoData fmmd) {
        updateFogDataArray(fmmd);
        populateList();
    }
}
