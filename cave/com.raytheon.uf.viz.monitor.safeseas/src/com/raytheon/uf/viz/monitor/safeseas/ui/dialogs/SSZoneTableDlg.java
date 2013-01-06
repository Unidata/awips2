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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.fog.FogRecord.FOG_THREAT;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.data.ObConst.DisplayVarName;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.listeners.IMonitorListener;
import com.raytheon.uf.viz.monitor.safeseas.SafeSeasMonitor;
import com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants;

/**
 * SAFESEAS Zone Table Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 30, 2009 3424       zhao         use ObMultiHrsReports for obs data archive
 * Oct 30, 2012 1297       skorolev     Changed HashMap to Map
 * Nov 10, 2012 1297       skorolev     Added initiateProdArray
 * 
 * </pre>
 * 
 * @author zhao
 * @version 1.0
 */
public class SSZoneTableDlg extends ZoneTableDlg {

    /** SAFESEAS threshold dialog. **/
    private SSDispMonThreshDlg ssThreshDlg;

    /** Swell column names in the zone and station table. **/
    private String[] ssSwellCols = { "SSZT_SwellPeriod", "SSZT_Swell2Period" };

    /**
     * Constructor (Dec 30, 2009, zhao)
     * 
     * @param parent
     */
    public SSZoneTableDlg(Shell parent, ObMultiHrsReports obData) {
        super(parent, obData, CommonConfig.AppName.SAFESEAS);
    }

    /**
     * Constructor
     * 
     * @param parent
     */
    public SSZoneTableDlg(Shell parent) {
        super(parent, CommonConfig.AppName.SAFESEAS);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg#initiateProdArray()
     */
    @Override
    public void initiateProdArray() {
        varName = config.getSafeseasZoneStnTableColVarNames()[colIndex];
        // Fill product arrays
        prodArray = new ArrayList<String>();
        String[] varprefs = { "VAR_", "SCA_", "GALE_", "STORM_", "HURRICANE_" };
        for (DisplayVarName var : DisplayVarName.values()) {
            String dispVarName = var.name();
            if (colIndex == 1 && dispVarName.startsWith(varprefs[1])) {
                prodArray.add(dispVarName);
            } else if (colIndex == 2 && dispVarName.startsWith(varprefs[2])) {
                prodArray.add(dispVarName);
            } else if (colIndex == 3 && dispVarName.startsWith(varprefs[3])) {
                prodArray.add(dispVarName);
            } else if (colIndex == 4 && dispVarName.startsWith(varprefs[4])) {
                prodArray.add(dispVarName);
            } else if (dispVarName.startsWith(varprefs[0])
                    && dispVarName.equals(varprefs[0] + varName.name())) {
                prodArray.add(dispVarName);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg#configThreshAction()
     */
    @Override
    protected void configThreshAction() {
        if (ssThreshDlg == null) {
            ssThreshDlg = new SSDispMonThreshDlg(getParent().getShell(),
                    CommonConfig.AppName.SAFESEAS, DataUsageKey.DISPLAY);
            ssThreshDlg.open();
            ssThreshDlg = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.listeners.IMonitorListener#notify(com.raytheon
     * .uf.viz.monitor.events.IMonitorEvent)
     */
    @Override
    public void notify(IMonitorEvent me) {
        if (zoneTable.isDisposed()) {
            return;
        }

        if (me.getSource() instanceof SafeSeasMonitor) {
            SafeSeasMonitor monitor = (SafeSeasMonitor) me.getSource();
            Date date = monitor.getDialogTime();
            if (date != null) {
                Date nominalTime = date;
                ObMultiHrsReports obData = monitor.getObData();
                if (!isLinkedToFrame()) {
                    nominalTime = obData.getLatestNominalTime();
                }
                Map<String, FOG_THREAT> fogAlgThreats = monitor
                        .getAlgorithmData(nominalTime);
                obData.setFogAlgCellType(monitor.getAlgCellTypes(fogAlgThreats));
                this.updateTableDlg(monitor.getObData().getObHourReports(
                        nominalTime));
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener#
     * addMonitorControlListener(com.raytheon.uf.viz.monitor.IMonitor)
     */
    @Override
    public void addMonitorControlListener(IMonitor monitor) {
        getMonitorControlListeners().add(monitor);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener#
     * fireConfigUpdate
     * (com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent)
     */
    @Override
    public void fireConfigUpdate(IMonitorConfigurationEvent imce) {
        // Not used
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener#fireKillMonitor
     * ()
     */
    @Override
    public void fireKillMonitor() {
        // Not used
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener#
     * fireThresholdUpdate
     * (com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent)
     */
    @Override
    public void fireThresholdUpdate(IMonitorThresholdEvent imte) {
        // Not used
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg#
     * getMonitorControlListeners()
     */
    @Override
    public List<IMonitor> getMonitorControlListeners() {
        return controlListeners;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener#
     * removeMonitorContorlListener(com.raytheon.uf.viz.monitor.IMonitor)
     */
    @Override
    public void removeMonitorContorlListener(IMonitor monitor) {
        getMonitorControlListeners().remove(monitor);

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg#getConfigMgr()
     */
    @Override
    protected MonitorConfigurationManager getConfigMgr() {
        return SSMonitorConfigurationManager.getInstance();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg#handleLinkToFrame()
     */
    @Override
    protected void handleLinkToFrame() {
        linkedToFrame = linkToFrameChk.getSelection();
        SafeSeasMonitor.getInstance().fireMonitorEvent(
                this.getClass().getName());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg#shellDisposeAction()
     */
    @Override
    protected void shellDisposeAction() {
        // Not used
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener#
     * fireDialogShutdown
     * (com.raytheon.uf.viz.monitor.listeners.IMonitorListener)
     */
    @Override
    public void fireDialogShutdown(IMonitorListener iml) {
        // Not used
    }

    @Override
    protected void setZoneSortColumnAndDirection() {
        if (zoneTblData != null) {
            zoneSortColumn = zoneTblData.getSortColumn();
            zoneSortDirection = zoneTblData.getSortDirection();
            if (zoneSortColumn == zoneTable.getColumnIndex(appName,
                    ssSwellCols[0])
                    || zoneSortColumn == zoneTable.getColumnIndex(appName,
                            ssSwellCols[1])) {
                if (MonitorConfigConstants.isRankSwellPeriodHigh()) {
                    zoneSortDirection = SWT.DOWN;
                } else {
                    zoneSortDirection = SWT.UP;
                }
            }
        }
    }

    @Override
    protected void setStnSortColumnAndDirection() {
        if (stnTblData != null) {
            stnSortColumn = stnTblData.getSortColumn();
            stnSortDirection = stnTblData.getSortDirection();
            if (stnSortColumn == stationTable.getColumnIndex(appName,
                    ssSwellCols[0])
                    || stnSortColumn == stationTable.getColumnIndex(appName,
                            ssSwellCols[1])) {
                if (MonitorConfigConstants.isRankSwellPeriodHigh()) {
                    stnSortDirection = SWT.DOWN;
                } else {
                    stnSortDirection = SWT.UP;
                }
            }
        }
    }
}
