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

import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.safeseas.SafeSeasMonitor;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2010            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SSMonitoringAreaConfigDlg extends MonitoringAreaConfigDlg {

    public SSMonitoringAreaConfigDlg(Shell parent, String title) {
        super(parent, title, AppName.SAFESEAS);
        readConfigData();
    }

    @Override
    protected void handleOkBtnSelection() {
        SSMonitorConfigurationManager configManager = SSMonitorConfigurationManager
                .getInstance();
        // Check for changes in the data
        int choice = showMessage(shell, SWT.OK | SWT.CANCEL,
                "SAFESEAS Monitor Confirm Changes",
                "Want to update the SAFESEAS setup files?");

        if (choice == SWT.OK) {
            // Save the config xml file
            configManager.setShipDistance(distanceScale.getSelection());
            configManager.setTimeWindow(timeScale.getSelection());
            configManager.setUseAlgorithms(fogChk.getSelection());
            configManager.saveConfigData();

            /**
             * DR#11279: re-initialize threshold manager and the monitor using
             * new monitor area configuration
             */
            SSThresholdMgr.reInitialize();
            SafeSeasMonitor.reInitialize();

            showMessage(shell, SWT.OK, "SAFESEAS Config Change",
                    "You're updating the SAFESEAS monitoring settings."
                            + "\n\nIf SAFESEAS is running anywhere within "
                            + "the office, please clear it.\n");
            if (configManager.getAddedZones().size() > 0
                    || addedZones.size() > 0) {
                String message2 = "New zones have been added, and their monitoring thresholds "
                        + "have been set to default values; would you like to modify "
                        + "their threshold values now?";
                int yesno = showMessage(shell, SWT.ICON_QUESTION | SWT.YES
                        | SWT.NO, "Edit Thresholds Now?", message2);
                if (yesno == SWT.YES) {

                    SSDispMonThreshDlg ssMonitorDlg = new SSDispMonThreshDlg(
                            shell, CommonConfig.AppName.SAFESEAS,
                            DataUsageKey.MONITOR);
                    ssMonitorDlg.open();
                }
            }
            shell.dispose();
        }
    }

    @Override
    protected void setAlgorithmText() {
        fogChk.setText("The Fog Monitor overall threat level is "
                + "considered when determining the anchor color.");
    }

    @Override
    protected void readConfigData() {
        SSMonitorConfigurationManager configManager = SSMonitorConfigurationManager
                .getInstance();
        configManager.readConfigXml(currentSite);
    }
}
