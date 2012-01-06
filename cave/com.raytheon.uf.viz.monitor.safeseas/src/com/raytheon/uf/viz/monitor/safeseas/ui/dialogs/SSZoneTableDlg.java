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
import java.util.HashMap;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.fog.FogRecord.FOG_THREAT;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.listeners.IMonitorListener;
import com.raytheon.uf.viz.monitor.safeseas.SafeSeasMonitor;
import com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg;

/**
 * ( where is the "SOFTWARE HISTORY" section of this file? )
 * 
 * Dec 30, 2009 3424 zhao use ObMultiHrsReports for obs data archive
 * 
 */
public class SSZoneTableDlg extends ZoneTableDlg {
    private SSDispMonThreshDlg ssThreshDlg;

    /**
     * Constructor (Dec 30, 2009, zhao)
     * 
     * @param parent
     */
    public SSZoneTableDlg(Shell parent, ObMultiHrsReports obData) {
        super(parent, obData, CommonConfig.AppName.SAFESEAS);
    }

    public SSZoneTableDlg(Shell parent) {
        super(parent, CommonConfig.AppName.SAFESEAS);
    }

    @Override
    protected void configThreshAction() {
        if (ssThreshDlg == null) {
            ssThreshDlg = new SSDispMonThreshDlg(getParent().getShell(),
                    CommonConfig.AppName.SAFESEAS, DataUsageKey.DISPLAY);
            ssThreshDlg.open();
            ssThreshDlg = null;
        }
    }

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
				HashMap<String, FOG_THREAT> fogAlgThreats = monitor
                        .getAlgorithmData(nominalTime);
				obData.setFogAlgCellType(monitor.getAlgCellTypes(fogAlgThreats));
				this.updateTableDlg(monitor.getObData().getObHourReports(nominalTime));
			}
		}
    }

    @Override
    public void addMonitorControlListener(IMonitor monitor) {
        getMonitorControlListeners().add(monitor);
    }

    @Override
    public void fireConfigUpdate(IMonitorConfigurationEvent imce) {
        // TODO Auto-generated method stub

    }



    @Override
    public void fireKillMonitor() {
        // TODO Auto-generated method stub

    }

    @Override
    public void fireThresholdUpdate(IMonitorThresholdEvent imte) {
        // TODO Auto-generated method stub

    }

    @Override
    public ArrayList<IMonitor> getMonitorControlListeners() {
        return controlListeners;
    }

    @Override
    public void removeMonitorContorlListener(IMonitor monitor) {
        getMonitorControlListeners().remove(monitor);

    }

	@Override
	protected MonitorConfigurationManager getConfigMgr() {
		return SSMonitorConfigurationManager.getInstance();
	}

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
        // TODO Auto-generated method stub

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
        // TODO Auto-generated method stub

    }
}
