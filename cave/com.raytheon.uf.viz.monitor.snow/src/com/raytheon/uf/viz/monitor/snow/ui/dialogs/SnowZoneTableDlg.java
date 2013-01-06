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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.data.ObConst.DisplayVarName;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.listeners.IMonitorListener;
import com.raytheon.uf.viz.monitor.snow.SnowMonitor;
import com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg;

/**
 * The SNOW Zone Table dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2009 3424       zhao/wkwock/slav Automatically updates snow display. Display station data.
 * Dec 2,  2009 3424       zhao/wkwock/slav Fix display SNOW 2nd time problem.
 * Dec 18, 2009 3424       zhao        use ObMultiHrsReports for obs data archive
 * July 20,2010 4891       skorolev    added code to fireDialogShutdown
 * Nov. 8, 2012 1297       skorolev    Added initiateProdArray method
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */

public class SnowZoneTableDlg extends ZoneTableDlg {

    private SnowMonDispThreshDlg snowThreshDlg;

    /**
     * @param parent
     * @param obData
     */
    public SnowZoneTableDlg(Shell parent, ObMultiHrsReports obData) {
        super(parent, obData, CommonConfig.AppName.SNOW);
    }

    /**
     * Constructor
     * 
     * @param parent
     */
    public SnowZoneTableDlg(Shell parent) {
        super(parent, CommonConfig.AppName.SNOW);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg#initiateProdArray()
     */
    @Override
    public void initiateProdArray() {
        varName = config.getSnowZoneStnTableColVarNames()[colIndex];
        // Fill product array
        prodArray = new ArrayList<String>();
        String[] varprefs = { "VAR_", "BLIZ_", "FRZ_", "HSW_" };
        for (DisplayVarName var : DisplayVarName.values()) {
            String dispVarName = var.name();
            if (colIndex == 1 && dispVarName.startsWith(varprefs[1])) {
                prodArray.add(dispVarName);
            } else if (colIndex == 2 && dispVarName.startsWith(varprefs[2])) {
                prodArray.add(dispVarName);
            } else if (colIndex == 3 && dispVarName.startsWith(varprefs[3])) {
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
        if (snowThreshDlg == null) {
            snowThreshDlg = new SnowMonDispThreshDlg(getParent().getShell(),
                    CommonConfig.AppName.SNOW, DataUsageKey.DISPLAY);
            snowThreshDlg.open();
            snowThreshDlg = null;
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

        if (me.getSource() instanceof SnowMonitor) {
            SnowMonitor monitor = (SnowMonitor) me.getSource();
            Date date = monitor.getDialogTime();
            if (date != null) {
                if (!isLinkedToFrame()) {
                    date = monitor.getObData().getLatestNominalTime();
                }
                this.updateTableDlg(monitor.getObData().getObHourReports(date));
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
     * @see com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener#
     * fireDialogShutdown
     * (com.raytheon.uf.viz.monitor.listeners.IMonitorListener)
     */
    @Override
    public void fireDialogShutdown(IMonitorListener iml) {
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
        return SnowMonitorConfigurationManager.getInstance();
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
        SnowMonitor.getInstance().fireMonitorEvent(this.getClass().getName());
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
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg#
     * setZoneSortColumnAndDirection()
     */
    @Override
    protected void setZoneSortColumnAndDirection() {
        if (zoneTblData != null) {
            zoneSortColumn = zoneTblData.getSortColumn();
            zoneSortDirection = zoneTblData.getSortDirection();
        }
        return;
    }

    @Override
    protected void setStnSortColumnAndDirection() {
        if (stnTblData != null) {
            stnSortColumn = stnTblData.getSortColumn();
            stnSortDirection = stnTblData.getSortDirection();
        }
    }

}
