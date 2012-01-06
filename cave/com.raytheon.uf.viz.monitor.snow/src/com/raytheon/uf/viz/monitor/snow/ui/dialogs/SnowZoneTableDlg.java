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

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
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
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */

public class SnowZoneTableDlg extends ZoneTableDlg {
    // private ZoneTableComp zoneTableComp;

    private SnowMonDispThreshDlg snowThreshDlg;

    /**
     * Constructor (Dec 16, 2009, zhao)
     * 
     * @param parent
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

    @Override
    protected void configThreshAction() {
        if (snowThreshDlg == null) {
            snowThreshDlg = new SnowMonDispThreshDlg(getParent().getShell(),
                    CommonConfig.AppName.SNOW, DataUsageKey.DISPLAY);
            snowThreshDlg.open();
            snowThreshDlg = null;
        }
    }

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
        // Display.getDefault().asyncExec(new Runnable() {
        // public void run() {
        // Iterator<IMonitor> iter = getMonitorControlListeners()
        // .iterator();
        // while (iter.hasNext()) {
        // ((SnowMonitor) iter.next()).closeDialog();
        // }
        // }
        // });

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
		return SnowMonitorConfigurationManager.getInstance();
	}
	
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
        // shell.addDisposeListener(new DisposeListener() {
        // @Override
        // public void widgetDisposed(DisposeEvent e) {
        // System.out.println("Fog monitor dialog DISPOSED");
        // unregisterDialogFromMonitor();
        // }
        // });
    }

    /**
     * 
     */
    protected void unregisterDialogFromMonitor() {
        fireDialogShutdown(this);
    }

}
