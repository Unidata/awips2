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

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.CellType;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.data.ObConst.DisplayVarName;
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
 * Fog Zone Table Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 25, 2010 #4281      zhao Modified the notify method 
 * Jun 16, 2012 14386      zhao Modified the notify method
 * Oct 30, 2012            skorolev    Changed HashMap to Map
 * Nov 11, 2012 1297       skorolev    Added initiateProdArray
 * Dec 03, 2012 15216/15639 zhao fixed a bug related to Link-to-Frame 
 * Dec  7, 2012 #1351      skorolev    Changes for non-blocking dialogs.
 * 
 * </pre>
 * 
 * @author ?
 * @version 1.0
 */
public class FogZoneTableDlg extends ZoneTableDlg {
    private FogMonDispThreshDlg fogThreshDlg;

    public FogZoneTableDlg(Shell parent, ObMultiHrsReports obData) {
        super(parent, obData, CommonConfig.AppName.FOG);
    }

    public FogZoneTableDlg(Shell parent) {
        super(parent, CommonConfig.AppName.FOG);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg#initiateProdArray()
     */
    @Override
    public void initiateProdArray() {
        varName = config.getFogZoneStnTableColVarNames()[colIndex];
        prodArray = new ArrayList<String>();
        String varpref = "VAR_";
        for (DisplayVarName var : DisplayVarName.values()) {
            String dispVarName = var.name();
            if (dispVarName.equals(varpref + varName.name())) {
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
        if (fogThreshDlg == null) {
            fogThreshDlg = new FogMonDispThreshDlg(getParent().getShell(),
                    CommonConfig.AppName.FOG, DataUsageKey.DISPLAY);
        }
        fogThreshDlg.open();
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
				if (!isLinkedToFrame()) {
					nominalTime = obData.getLatestNominalTime();
				}                
                FogDataGenerator fdg = new FogDataGenerator();
                Map<String, CellType> fogAlgCellType = fdg.getAlgCellTypes(fog
                        .getAlgorithmData(nominalTime));
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener#fireKillMonitor
     * ()
     */
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
        return FogMonitorConfigurationManager.getInstance();
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
        FogMonitor.getInstance().fireMonitorEvent(this.getClass().getName());
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

    @Override
    protected void setZoneSortColumnAndDirection() {
        if (zoneTblData != null) {
            zoneSortColumn = zoneTblData.getSortColumn();
            zoneSortDirection = zoneTblData.getSortDirection();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg#
     * setStnSortColumnAndDirection()
     */
    @Override
    protected void setStnSortColumnAndDirection() {
        if (stnTblData != null) {
            stnSortColumn = stnTblData.getSortColumn();
            stnSortDirection = stnTblData.getSortDirection();
        }
    }

}
