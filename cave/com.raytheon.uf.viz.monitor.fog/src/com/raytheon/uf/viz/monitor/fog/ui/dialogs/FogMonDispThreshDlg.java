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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.fog.FogMonitor;
import com.raytheon.uf.viz.monitor.fog.threshold.FogThresholdMgr;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.MonitorDisplayThreshDlg;

public class FogMonDispThreshDlg extends MonitorDisplayThreshDlg
{
    FogDisplayMeteoTab displayMeteoTab;
    FogDisplayWindTab displayWindTab;
    FogMonitorMeteoTab monitorMeteoTab;
    
    public FogMonDispThreshDlg(Shell parent, CommonConfig.AppName appName,
            DataUsageKey displayType)
    {    
        super(parent, appName, displayType);
    }
    
    @Override
    protected void createTabItems()
    {
        if (displayType == DataUsageKey.DISPLAY)
        {
            createDisplayTabs();
        }
        else if (displayType == DataUsageKey.MONITOR)
        {
            createMonitorTabs();
        }
    }
    
    private void createDisplayTabs()
    {

        TabItem meteoTab = new TabItem(tabFolder, SWT.NONE);
        meteoTab.setText("Meteo");
        displayMeteoTab = new FogDisplayMeteoTab(tabFolder, displayType);
        meteoTab.setControl(displayMeteoTab);
        
        TabItem windTab = new TabItem(tabFolder, SWT.NONE);
        windTab.setText("Wind");
        displayWindTab = new FogDisplayWindTab(tabFolder, displayType);
        windTab.setControl(displayWindTab);        
    }
    
    private void createMonitorTabs()
    {
        TabItem meteoTab = new TabItem(tabFolder, SWT.NONE);
        meteoTab.setText("Meteo");
        monitorMeteoTab = new FogMonitorMeteoTab(tabFolder, displayType);
        meteoTab.setControl(monitorMeteoTab);
    }

    @Override
    protected void commitChangeAction()
    {
        if (displayType == DataUsageKey.DISPLAY)
        {
            displayMeteoTab.commitDataToXML();
            displayWindTab.commitDataToXML();
            
            FogMonitor.getInstance().thresholdUpdate(null);
        }
        else if (displayType == DataUsageKey.MONITOR)
        {
            monitorMeteoTab.commitDataToXML();
        }
        
    }

    @Override
    protected AbstractThresholdMgr getThresholdMgr()
    {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();
        return ftm;
    }

    @Override
    protected void reloadThresholds()
    {
        displayMeteoTab.reloadData();
        displayWindTab.reloadData();        
    }
}
