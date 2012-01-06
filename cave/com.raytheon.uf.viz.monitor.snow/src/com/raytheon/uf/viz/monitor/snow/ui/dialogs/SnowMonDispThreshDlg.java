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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.snow.SnowMonitor;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.MonitorDisplayThreshDlg;

public class SnowMonDispThreshDlg extends MonitorDisplayThreshDlg {
    private SnowDisplayProductTab displayProductTab;

    private SnowDisplayWindTab displayWindTab;

    private SnowDisplayMeteoTab displayMeteoTab;

    private SnowMonitorMeteoTab monitorMeteoTab;

    public SnowMonDispThreshDlg(Shell parent, CommonConfig.AppName appName,
            DataUsageKey displayType) {
        super(parent, appName, displayType);
    }

    @Override
    protected void createTabItems() {
        if (displayType == DataUsageKey.DISPLAY) {
            createDisplayTabs();
        } else {
            createMonitorTabs();
        }
    }

    private void createDisplayTabs() {
        TabItem productTab = new TabItem(tabFolder, SWT.NONE);
        productTab.setText("Product");
        displayProductTab = new SnowDisplayProductTab(tabFolder, displayType);
        productTab.setControl(displayProductTab);

        TabItem windTab = new TabItem(tabFolder, SWT.NONE);
        windTab.setText("Wind");
        displayWindTab = new SnowDisplayWindTab(tabFolder, displayType);
        windTab.setControl(displayWindTab);

        TabItem meteoTab = new TabItem(tabFolder, SWT.NONE);
        meteoTab.setText("Meteo");
        displayMeteoTab = new SnowDisplayMeteoTab(tabFolder, displayType);
        meteoTab.setControl(displayMeteoTab);
    }

    private void createMonitorTabs() {
        TabItem meteoTab = new TabItem(tabFolder, SWT.NONE);
        meteoTab.setText("Meteo");
        monitorMeteoTab = new SnowMonitorMeteoTab(tabFolder, displayType);
        meteoTab.setControl(monitorMeteoTab);
    }

    @Override
    protected void commitChangeAction() {
        if (displayType == DataUsageKey.DISPLAY) {
            displayProductTab.commitDataToXML();
            displayWindTab.commitDataToXML();
            displayMeteoTab.commitDataToXML();

            SnowMonitor.getInstance().thresholdUpdate(null);

       } else if (displayType == DataUsageKey.MONITOR) {
            monitorMeteoTab.commitDataToXML();
        }
    }

    @Override
    protected AbstractThresholdMgr getThresholdMgr() {
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();
        return stm;
    }

    @Override
    protected void reloadThresholds() {
        displayProductTab.reloadData();
        displayWindTab.reloadData();
        displayMeteoTab.reloadData();
    }
}
