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
 * SAFESEAS area configuration dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan  5, 2010            mpduff      Initial creation
 * Nov 27, 2012 1351       skorolev    Changes for non-blocking dialog.
 * Jan 29, 2014 2757       skorolev    Changed OK button handler.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SSMonitoringAreaConfigDlg extends MonitoringAreaConfigDlg {

    /**
     * Constructor
     * 
     * @param parent
     * @param title
     */
    public SSMonitoringAreaConfigDlg(Shell parent, String title) {
        super(parent, title, AppName.SAFESEAS);
        readConfigData();
    }

    private SSMonitorConfigurationManager configManager = SSMonitorConfigurationManager
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
        if (!configManager.getAddedZones().isEmpty()
                || !configManager.getAddedStations().isEmpty()
                || this.timeWindowChanged || this.shipDistanceChanged
                || this.fogChkChanged || this.maZonesRemoved) {
            int choice = showMessage(shell, SWT.OK | SWT.CANCEL,
                    "SAFESEAS Monitor Confirm Changes",
                    "Want to update the SAFESEAS setup files?");
            if (choice == SWT.OK) {
                // Save the config xml file
                configManager.setTimeWindow(timeWindow.getSelection());
                this.timeWindowChanged = false;
                configManager.setShipDistance(shipDistance.getSelection());
                this.shipDistanceChanged = false;
                configManager.setUseAlgorithms(fogChk.getSelection());
                this.fogChkChanged = false;
                this.maZonesRemoved = false;
                configManager.saveConfigData();
                /**
                 * DR#11279: re-initialize threshold manager and the monitor
                 * using new monitor area configuration
                 */
                SSThresholdMgr.reInitialize();
                SafeSeasMonitor.reInitialize();
                if ((!configManager.getAddedZones().isEmpty())
                        || (!configManager.getAddedStations().isEmpty())) {

                    String message = "You're updating the SAFESEAS monitoring settings."
                            + "\n\nIf SAFESEAS is running anywhere within "
                            + "the office, please clear it.\n";

                    showMessage(shell, SWT.OK, "SAFESEAS Config Change",
                            message);

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
                    configManager.getAddedZones().clear();
                    configManager.getAddedStations().clear();
                }
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
        fogChk.setText("The Fog Monitor overall threat level is "
                + "considered when determining the anchor color.");
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
        shipDistance.setSelection(configManager.getShipDistance());
        setShipDistScaleLabel();
        fogChk.setSelection(configManager.isUseAlgorithms());
    }
}
