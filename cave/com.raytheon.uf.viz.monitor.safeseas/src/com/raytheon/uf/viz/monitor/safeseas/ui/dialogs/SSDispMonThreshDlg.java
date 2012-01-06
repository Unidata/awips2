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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.safeseas.SafeSeasMonitor;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.MonitorDisplayThreshDlg;

public class SSDispMonThreshDlg extends MonitorDisplayThreshDlg
{
    private SSDisplayProductTab displayProductTab;
    private SSDisplayWindTab displayWindTab;
    private SSDisplayMeteoTab displayMeteoTab;
    private SSDisplaySwellTab displaySwellTab;
    private SSMonitorMeteoTab monitorMeteoTab;
    private SSMonitorSwellTab monitorSwellTab;
    
    
    public SSDispMonThreshDlg(Shell parent, CommonConfig.AppName appName,
            DataUsageKey duKey)
    {    
        super(parent, appName, duKey);
    }
    
    @Override
    protected void createTabItems()
    {
        if (displayType == DataUsageKey.DISPLAY)
        {
            createDisplayTabs();
        }
        else
        {
            createMonitorTabs();
        }
    }
    
    private void createDisplayTabs()
    {
        TabItem productTab = new TabItem(tabFolder, SWT.NONE);
        productTab.setText("Product");
        displayProductTab = new SSDisplayProductTab(tabFolder, displayType);
        productTab.setControl(displayProductTab);
        
        TabItem windTab = new TabItem(tabFolder, SWT.NONE);
        windTab.setText("Wind");
        displayWindTab = new SSDisplayWindTab(tabFolder, displayType);
        windTab.setControl(displayWindTab);
        
        TabItem meteoTab = new TabItem(tabFolder, SWT.NONE);
        meteoTab.setText("Meteo");
        displayMeteoTab = new SSDisplayMeteoTab(tabFolder, displayType);
        meteoTab.setControl(displayMeteoTab);
        
        TabItem swellTab = new TabItem(tabFolder, SWT.NONE);
        swellTab.setText("Swell");
        displaySwellTab = new SSDisplaySwellTab(tabFolder, displayType);
        swellTab.setControl(displaySwellTab);
    }
    
    private void createMonitorTabs()
    {
        TabItem meteoTab = new TabItem(tabFolder, SWT.NONE);
        meteoTab.setText("Meteo");
        monitorMeteoTab = new SSMonitorMeteoTab(tabFolder, displayType);
        meteoTab.setControl(monitorMeteoTab);
        
        TabItem swellTab = new TabItem(tabFolder, SWT.NONE);
        swellTab.setText("Swell");
        monitorSwellTab = new SSMonitorSwellTab(tabFolder, displayType);
        swellTab.setControl(monitorSwellTab);
    }

    @Override
    protected void commitChangeAction()
    {
        if (displayType == DataUsageKey.DISPLAY)
        {
            displayProductTab.commitDataToXML();
            displayWindTab.commitDataToXML();
            displayMeteoTab.commitDataToXML();
            displaySwellTab.commitDataToXML();
            
            SafeSeasMonitor.getInstance().thresholdUpdate(null);
        }
        else if (displayType == DataUsageKey.MONITOR)
        {
            monitorMeteoTab.commitDataToXML();
            monitorSwellTab.commitDataToXML();
        }        
    }

    @Override
    protected AbstractThresholdMgr getThresholdMgr()
    {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();
        return sstm;
    }

    @Override
    protected void reloadThresholds()
    {
        displayProductTab.reloadData();
        displayWindTab.reloadData();
        displayMeteoTab.reloadData();
        displaySwellTab.reloadData();        
    }
}

