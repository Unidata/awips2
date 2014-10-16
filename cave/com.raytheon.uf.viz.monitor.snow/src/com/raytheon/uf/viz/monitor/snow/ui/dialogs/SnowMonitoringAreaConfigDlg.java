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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.snow.SnowMonitor;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

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
 * Apr 28, 2014 3086       skorolev    Updated snowConfigManager.
 * Sep 04, 2014 3220       skorolev    Added fireConfigUpdateEvent method. Updated handler.
 * Sep 19, 2014 2757       skorolev    Updated handlers for dialog buttons.
 * Oct 16, 2014 3220       skorolev    Corrected getInstance() method.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SnowMonitoringAreaConfigDlg extends MonitoringAreaConfigDlg {

    private SnowMonDispThreshDlg snowMonitorDlg;

    /**
     * Constructor
     * 
     * @param parent
     * @param title
     */
    public SnowMonitoringAreaConfigDlg(Shell parent, String title) {
        super(parent, title, AppName.SNOW);
        SnowMonitor.getInstance();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg#
     * handleApplyBtnSelection()
     */
    @Override
    protected void handleOkBtnSelection() {
        if (dataIsChanged()) {
            int choice = showMessage(shell, SWT.OK | SWT.CANCEL,
                    "SNOW Monitor Confirm Changes",
                    "Want to update the SNOW setup files?");
            if (choice == SWT.OK) {
                // Save the config xml file
                getValues();
                resetStatus();
                configMgr.saveConfigXml();
                configMgr.saveAdjacentAreaConfigXml();

                SnowThresholdMgr.reInitialize();
                fireConfigUpdateEvent();
                if ((!configMgr.getAddedZones().isEmpty())
                        || (!configMgr.getAddedStations().isEmpty())) {
                    if (editDialog() == SWT.YES) {
                        if ((snowMonitorDlg == null)
                                || snowMonitorDlg.isDisposed()) {
                            snowMonitorDlg = new SnowMonDispThreshDlg(shell,
                                    CommonConfig.AppName.SNOW,
                                    DataUsageKey.MONITOR);
                        }
                        snowMonitorDlg.setCloseCallback(new ICloseCallback() {

                            @Override
                            public void dialogClosed(Object returnValue) {
                                // Clean added zones and stations. Close dialog.
                                configMgr.getAddedZones().clear();
                                configMgr.getAddedStations().clear();
                                setReturnValue(true);
                                close();
                            }
                        });
                        snowMonitorDlg.open();
                    }
                    // Clean added zones and stations.
                    configMgr.getAddedZones().clear();
                    configMgr.getAddedStations().clear();
                }
            }
        }
        if ((snowMonitorDlg == null) || snowMonitorDlg.isDisposed()) {
            setReturnValue(true);
            close();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg#getInstance
     * ()
     */
    @Override
    protected FSSObsMonitorConfigurationManager getInstance() {
        return FSSObsMonitorConfigurationManager.getSnowObsManager();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg#disposed()
     */
    @Override
    protected void disposed() {
        configMgr = null;
    }

    /**
     * Fire Table reload event
     */
    private void fireConfigUpdateEvent() {
        final IMonitorConfigurationEvent me = new IMonitorConfigurationEvent(
                configMgr);
        shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                SnowMonitor.getInstance().configUpdate(me);
            }
        });
    }
}
