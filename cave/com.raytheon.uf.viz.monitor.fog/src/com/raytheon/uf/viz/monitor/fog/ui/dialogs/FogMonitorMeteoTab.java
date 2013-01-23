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
import java.util.Collections;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.fog.threshold.FogMonitorMeteoData;
import com.raytheon.uf.viz.monitor.fog.threshold.FogThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.FogMonitor;

public class FogMonitorMeteoTab extends TabItemComp implements
        IUpdateMonitorMeteo {
    private MonitorConfigurationManager areaConfigMgr = null;

    private FogMonitorMeteoEditDlg fogMeteoEditDlg;

    private ArrayList<String> areaIDArray;

    private ArrayList<FogMonitorMeteoData> fogDataArray;

    public FogMonitorMeteoTab(TabFolder parent, DataUsageKey duKey) {
        super(parent, duKey);
    }

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

    private void createDataArray() {
        fogDataArray = new ArrayList<FogMonitorMeteoData>();

        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        areaConfigMgr = getAreaConfigMgr();

        List<String> areas = areaConfigMgr.getAreaList();
        Collections.sort(areas);

        for (String area : areas) {

            FogMonitorMeteoData fmmd = new FogMonitorMeteoData();

            fmmd.setAreaID(area);

            /*
             * Visibility
             */
            String xmlKey = FogMonitor.FOG_MONITOR_METEO_VIS.getXmlKey();
            fmmd.setMeteoVisR(ftm.getThresholdValue(duKey, threshKeyR, area,
                    xmlKey));
            fmmd.setMeteoVisY(ftm.getThresholdValue(duKey, threshKeyY, area,
                    xmlKey));

            fogDataArray.add(fmmd);
        }
    }

    private FogMonitorMeteoData getDataAtFirstSelection() {

        int index = dataList.getSelectionIndex();

        return fogDataArray.get(index);

    }

    private void updateFogDataArray(FogMonitorMeteoData fmmd) {
        int[] dataListIndexes = dataList.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];

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
        dataList.removeAll();
        fogDataArray.clear();
        fogDataArray = null;

        populateList();
    }

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

    @Override
    public void updateThresholdData(FogMonitorMeteoData fmmd) {
        updateFogDataArray(fmmd);
        populateList();
    }

    private MonitorConfigurationManager getAreaConfigMgr() {
        if (areaConfigMgr == null) {
            LocalizationManager mgr = LocalizationManager.getInstance();
            String siteScope = mgr.getCurrentSite();

            areaConfigMgr = FogMonitorConfigurationManager.getInstance();
            areaConfigMgr.readConfigXml(siteScope);
        }
        return areaConfigMgr;
    }

}
