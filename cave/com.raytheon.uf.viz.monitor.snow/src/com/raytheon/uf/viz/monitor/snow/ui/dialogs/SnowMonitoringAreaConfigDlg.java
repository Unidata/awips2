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

import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.snow.SnowMonitor;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg;

/**
 * SNOW Monitor area configuration dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan  5, 2010            mpduff      Initial creation
 * Nov 27, 2012 1351       skorolev    Changes for non-blocking dialog.
 * Jan 29, 2014 2757       skorolev    Changed OK button handler.
 * Apr 23, 2014 3054       skorolev    Fixed issue with removing a new station from list.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SnowMonitoringAreaConfigDlg extends MonitoringAreaConfigDlg {

    public SnowMonitoringAreaConfigDlg(Shell parent, String title) {
        super(parent, title, AppName.SNOW);
        readConfigData();
    }

    private SnowMonitorConfigurationManager configManager = SnowMonitorConfigurationManager
            .getInstance();

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg#
     * handleOkBtnSelection()
     */
    @Override
    protected void handleOkBtnSelection() {
        // Check for changes in the data
        if (dataIsChanged()) {
            int choice = showMessage(shell, SWT.OK | SWT.CANCEL,
                    "SNOW Monitor Confirm Changes",
                    "Want to update the SNOW setup files?");
            if (choice == SWT.OK) {
                // Save the config xml file
                configManager.setTimeWindow(timeWindow.getSelection());
                resetStatus();
                configManager.saveConfigData();
                /**
                 * DR#11279: re-initialize threshold manager and the monitor
                 * using new monitor area configuration
                 */
                SnowThresholdMgr.reInitialize();
                SnowMonitor.reInitialize();

                if ((!configManager.getAddedZones().isEmpty())
                        || (!configManager.getAddedStations().isEmpty())) {
                    if (editDialog() == SWT.YES) {
                        SnowMonDispThreshDlg snowMonitorDlg = new SnowMonDispThreshDlg(
                                shell, CommonConfig.AppName.SNOW,
                                DataUsageKey.MONITOR);
                        snowMonitorDlg.open();
                    }
                }
                configManager.getAddedZones().clear();
                configManager.getAddedStations().clear();
            }
        } else {
            String message3 = "No changes made.\nDo you want to exit?";
            int yesno = showMessage(shell,
                    SWT.ICON_QUESTION | SWT.YES | SWT.NO, "Exit", message3);
            if (yesno == SWT.NO) {
                return;
            }
            setReturnValue(true);
            close();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg#
     * setAlgorithmText()
     */
    @Override
    protected void setAlgorithmText() {
        // Not used for SNOW
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg#readConfigData
     * ()
     */
    @Override
    protected void readConfigData() {
        configManager.readConfigXml(currentSite);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg#setValues
     * ()
     */
    @Override
    protected void setValues() {
        timeWindow.setSelection(configManager.getTimeWindow());
        setTimeScaleLabel();
    }

}
