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

/**
 * Fog Monitor/Display Threshold Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 7, 2012  #1351      skorolev    Cleaned up code.
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */
public class FogMonDispThreshDlg extends MonitorDisplayThreshDlg {
    /** Display Meteo Table **/
    FogDisplayMeteoTab displayMeteoTab;

    /** Display Wind Table **/
    FogDisplayWindTab displayWindTab;

    /** Monitor Meteo Table **/
    FogMonitorMeteoTab monitorMeteoTab;

    /**
     * Constructor
     * 
     * @param parent
     * @param appName
     * @param displayType
     */
    public FogMonDispThreshDlg(Shell parent, CommonConfig.AppName appName,
            DataUsageKey displayType) {
        super(parent, appName, displayType);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.MonitorDisplayThreshDlg#createTabItems
     * ()
     */
    @Override
    protected void createTabItems() {
        if (displayType == DataUsageKey.DISPLAY) {
            createDisplayTabs();
        } else if (displayType == DataUsageKey.MONITOR) {
            createMonitorTabs();
        }
    }

    /**
     * Creates Display Tables.
     */
    private void createDisplayTabs() {

        TabItem meteoTab = new TabItem(tabFolder, SWT.NONE);
        meteoTab.setText("Meteo");
        displayMeteoTab = new FogDisplayMeteoTab(tabFolder, displayType);
        meteoTab.setControl(displayMeteoTab);

        TabItem windTab = new TabItem(tabFolder, SWT.NONE);
        windTab.setText("Wind");
        displayWindTab = new FogDisplayWindTab(tabFolder, displayType);
        windTab.setControl(displayWindTab);
    }

    /**
     * Creates Monitor Tables.
     */
    private void createMonitorTabs() {
        TabItem meteoTab = new TabItem(tabFolder, SWT.NONE);
        meteoTab.setText("Meteo");
        monitorMeteoTab = new FogMonitorMeteoTab(tabFolder, displayType);
        meteoTab.setControl(monitorMeteoTab);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.MonitorDisplayThreshDlg#
     * commitChangeAction()
     */
    @Override
    protected void commitChangeAction() {
        if (displayType == DataUsageKey.DISPLAY) {
            displayMeteoTab.commitDataToXML();
            displayWindTab.commitDataToXML();

            FogMonitor.getInstance().thresholdUpdate(null);
        } else if (displayType == DataUsageKey.MONITOR) {
            monitorMeteoTab.commitDataToXML();
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.MonitorDisplayThreshDlg#
     * getThresholdMgr()
     */
    @Override
    protected AbstractThresholdMgr getThresholdMgr() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();
        return ftm;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.MonitorDisplayThreshDlg#
     * reloadThresholds()
     */
    @Override
    protected void reloadThresholds() {
        displayMeteoTab.reloadData();
        displayWindTab.reloadData();
    }
}
