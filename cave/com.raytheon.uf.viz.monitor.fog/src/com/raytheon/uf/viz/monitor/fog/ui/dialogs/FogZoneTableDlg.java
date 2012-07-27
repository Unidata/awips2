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

import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.CellType;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.fog.FogDataGenerator;
import com.raytheon.uf.viz.monitor.fog.FogMonitor;
import com.raytheon.uf.viz.monitor.listeners.IMonitorListener;
import com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg;

/**
 * (Where is the history log for this file???)
 * 
 * Jan 25, 2010 #4281 zhao Modified the notify method
 * Jun 16, 2012 14386 zhao Modified the notify method
 * 
 * @author
 * 
 */

public class FogZoneTableDlg extends ZoneTableDlg {
    private FogMonDispThreshDlg fogThreshDlg;

    public FogZoneTableDlg(Shell parent, ObMultiHrsReports obData) {
        super(parent, obData, CommonConfig.AppName.FOG);
    }

    public FogZoneTableDlg(Shell parent) {
        super(parent, CommonConfig.AppName.FOG);
    }

    @Override
    protected void configThreshAction() {
        if (fogThreshDlg == null) {
            fogThreshDlg = new FogMonDispThreshDlg(getParent().getShell(),
                    CommonConfig.AppName.FOG, DataUsageKey.DISPLAY);
            fogThreshDlg.open();
            fogThreshDlg = null;
        }
    }

    @Override
    public void notify(IMonitorEvent me) {

        if (zoneTable.isDisposed()) {
            return;
        }

        // TODO: Silver Springs Guys. We now have competing ways of adding the
        // data to the table.
        // You need to merge the two. The way we had setup originally was to use
        // the DataGenerator.
        // You guys have this ObMultiHrsReports. Find a way that will
        // incorporate both for the obs and
        // The algorithm output.

		if (me.getSource() instanceof FogMonitor) {

			FogMonitor fog = (FogMonitor) me.getSource();
			Date date = fog.getDialogDate();
            if (date != null) {
                Date nominalTime = date;
				ObMultiHrsReports obData = fog.getObData();
//				if (!isLinkedToFrame()) {
//					nominalTime = obData.getLatestNominalTime();
//				}
				FogDataGenerator fdg = new FogDataGenerator();
				HashMap<String, CellType> fogAlgCellType = fdg
                        .getAlgCellTypes(fog.getAlgorithmData(nominalTime));
				obData.setFogAlgCellType(fogAlgCellType);
				this.updateTableDlg(obData.getObHourReports(nominalTime));
            }
		}
    }

    /**
     * Jan 25, 2010, #4281, zhao, Modified to pass an ObMultiHrsReports object
     * to table dialog
     * 
     * @Override public void notify(IMonitorEvent me) { if
     *           (zoneTable.isDisposed()) return;
     * 
     *           if (me.getSource() instanceof FogMonitor) { FogMonitor monitor
     *           = (FogMonitor)me.getSource();
     *           this.updateTableDlg(monitor.getObData()); }
     * 
     *           //if (me.getSource() instanceof FogMonitor) { //
     *           IMPORTANT!!!!!! For now we just grab the most recent time from
     *           the OBSTable // When we have the CAVE rendering working we will
     *           grab it from the CaveResource! // Date date = new Date(); //
     *           FogMonitor fog = (FogMonitor)me.getSource(); //
     *           FogDataGenerator fdg = new FogDataGenerator(); // TableData
     *           tZoneTableData = fdg.generateZoneData(fog.getTableData(),
     *           fog.getAlgorithmData(), date); //
     *           updateZoneTable(tZoneTableData, fog.getStationTableData(),
     *           date); //} }
     */

    @Override
    public void addMonitorControlListener(IMonitor monitor) {
        getMonitorControlListeners().add(monitor);
    }

    @Override
    public void fireConfigUpdate(IMonitorConfigurationEvent imce) {
        // TODO Auto-generated method stub

    }

    @Override
    public void fireDialogShutdown(IMonitorListener iml) {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                Iterator<IMonitor> iter = getMonitorControlListeners()
                        .iterator();
                while (iter.hasNext()) {
                    ((FogMonitor) iter.next()).closeDialog();
                }
            }
        });
    }

    @Override
    public void fireKillMonitor() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                Iterator<IMonitor> iter = getMonitorControlListeners()
                        .iterator();
                while (iter.hasNext()) {
                    ((FogMonitor) iter.next()).nullifyMonitor();
                }
            }
        });
    }

    @Override
    public void fireThresholdUpdate(IMonitorThresholdEvent imte) {
        // TODO Auto-generated method stub

    }

    @Override
    public void removeMonitorContorlListener(IMonitor monitor) {
        getMonitorControlListeners().remove(monitor);
    }

	@Override
	protected MonitorConfigurationManager getConfigMgr() {
		return FogMonitorConfigurationManager.getInstance();
	}

    @Override
    protected void handleLinkToFrame() {
        linkedToFrame = linkToFrameChk.getSelection();
        FogMonitor.getInstance().fireMonitorEvent(this.getClass().getName());
    }

    /**
     * 
     */
    protected void unregisterDialogFromMonitor() {
        this.fireDialogShutdown(this);
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

}
