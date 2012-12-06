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

import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.fog.FogMonitor;
import com.raytheon.uf.viz.monitor.fog.threshold.FogThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg;

/**
 * Fog Monitor area configuration dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan  5, 2010            mpduff       Initial creation
 * Nov 27, 2012 1351       skorolev     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FogMonitoringAreaConfigDlg extends MonitoringAreaConfigDlg {
    public FogMonitoringAreaConfigDlg(Shell parent, String title) {
        super(parent, title, AppName.FOG);
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
        FogMonitorConfigurationManager configManager = FogMonitorConfigurationManager
                .getInstance();
        // Check for changes in the data
        if (!configManager.getAddedZones().isEmpty()
                || !configManager.getAddedZones().isEmpty()) {
            int choice = showMessage(shell, SWT.OK | SWT.CANCEL,
                    "Fog Monitor Confirm Changes",
                    "Want to Update Fog Monitor's Setup files?");
            if (choice == SWT.OK) {
                // Save the config xml file
                configManager.setShipDistance(distanceScale.getSelection());
                configManager.setTimeWindow(timeScale.getSelection());
                configManager.setUseAlgorithms(fogChk.getSelection());
                configManager.saveConfigData();
                /**
                 * DR#11279: re-initialize threshold manager and the monitor
                 * using new monitor area configuration
                 */
                FogThresholdMgr.reInitialize();
                FogMonitor.reInitialize();

                String message = "New zones have been added, the display "
                        + "thresholds for the new zones are set to "
                        + "default values, you may edit them with the display "
                        + "thresholds editor which can be launched from Fog Monitor "
                        + "zone table.\n\nIf Fog Monitor is running anywhere within "
                        + "the office, clear it.\n";

                showMessage(shell, SWT.ICON_INFORMATION | SWT.OK,
                        "Fog Monitor Confirm Changes", message);

                if ((!configManager.getAddedZones().isEmpty())
                        || (!configManager.getAddedZones().isEmpty())) {
                    String message2 = "New zones have been added, and their monitoring thresholds "
                            + "have been set to default values; would you like to modify "
                            + "their threshold values now?";
                    int yesno = showMessage(shell, SWT.ICON_QUESTION | SWT.YES
                            | SWT.NO, "Edit Thresholds Now?", message2);
                    if (yesno == SWT.YES) {
                        FogMonDispThreshDlg fogMonitorDlg = new FogMonDispThreshDlg(
                                shell, CommonConfig.AppName.FOG,
                                DataUsageKey.MONITOR);
                        fogMonitorDlg.open();
                    }
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
        fogChk.setText("Fog Monitor algorithms' threat level is considered when determining\n"
                + "the guardian icon color.");
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
        FogMonitorConfigurationManager configManager = FogMonitorConfigurationManager
                .getInstance();
        configManager.readConfigXml(currentSite);
    }
}
