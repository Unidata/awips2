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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg#
     * handleOkBtnSelection()
     */
    @Override
    protected void handleOkBtnSelection() {
        SnowMonitorConfigurationManager configManager = SnowMonitorConfigurationManager
                .getInstance();
        // Check for changes in the data
        if (!configManager.getAddedZones().isEmpty()
                || !configManager.getAddedZones().isEmpty()) {
            int choice = showMessage(shell, SWT.OK | SWT.CANCEL,
                    "SNOW Monitor Confirm Changes",
                    "Want to update the SNOW setup files?");
            if (choice == SWT.OK) {
                // Save the config xml file
                configManager.setTimeWindow(timeScale.getSelection());
                configManager.saveConfigData();
                /**
                 * DR#11279: re-initialize threshold manager and the monitor
                 * using new monitor area configuration
                 */
                SnowThresholdMgr.reInitialize();
                SnowMonitor.reInitialize();
                showMessage(shell, SWT.ICON_INFORMATION | SWT.OK,
                        "SNOW Config Change",
                        "You're updating the SNOW monitoring settings."
                                + "\n\nIf SNOW is running anywhere within "
                                + "the office, please clear it.\n");

                String message2 = "New zones have been added, and their monitoring thresholds "
                        + "have been set to default values; would you like to modify "
                        + "their threshold values now?";
                int yesno = showMessage(shell, SWT.ICON_QUESTION | SWT.YES
                        | SWT.NO, "Edit Thresholds Now?", message2);
                if (yesno == SWT.YES) {
                    SnowMonDispThreshDlg snowMonitorDlg = new SnowMonDispThreshDlg(
                            shell, CommonConfig.AppName.SNOW,
                            DataUsageKey.MONITOR);
                    snowMonitorDlg.open();
                }
            }
        } else {
            String message3 = "No changes made.\nDo you want to exit?";
            int yesno = showMessage(shell,
                    SWT.ICON_QUESTION | SWT.YES | SWT.NO, "Exit", message3);
            if (yesno == SWT.NO) {
                return;
            }
        }
        setReturnValue(true);
        close();
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
        SnowMonitorConfigurationManager configManager = SnowMonitorConfigurationManager
                .getInstance();
        configManager.readConfigXml(currentSite);
    }

}
