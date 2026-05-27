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
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.data.ObConst.DisplayVarName;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.safeseas.SafeSeasMonitor;
import com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg;

/**
 * SAFESEAS Zone Table Dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 30, 2009  3424     zhao      use ObMultiHrsReports for obs data archive
 * Oct 30, 2012  1297     skorolev  Changed HashMap to Map
 * Nov 10, 2012  1297     skorolev  Added initiateProdArray
 * Dec 07, 2012  1351     skorolev  Changes for non-blocking dialogs.
 * Apr 28, 2014  3086     skorolev  Updated getConfigMgr method.
 * Sep 04, 2014  3220     skorolev  Removed "site". Added check on dispose.
 * Sep 18, 2015  3873     skorolev  Adjusted to AppName and MonName.
 * Dec 17, 2015  3873     dhladky   Abstracted handling of dialogTime and Zone
 *                                  dialog events.
 * Jul 10, 2018  6766     randerso  Fixed dialog closing for recent changes to
 *                                  CaveSWTDialog. Implemented do nothing
 *                                  interface methods in base class.
 *
 * </pre>
 *
 * @author zhao
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
     * @param obData
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

    @Override
    public void initiateProdArray() {
        varName = config.getSafeseasZoneStnTableColVarNames()[colIndex];
        // Fill product arrays
        prodArray = new ArrayList<>();
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

    @Override
    protected void configThreshAction() {
        if (ssThreshDlg == null || ssThreshDlg.isDisposed()) {
            ssThreshDlg = new SSDispMonThreshDlg(getParent().getShell(),
                    CommonConfig.AppName.SAFESEAS, DataUsageKey.DISPLAY);
        }
        ssThreshDlg.open();
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
                Map<String, FOG_THREAT> fogAlgThreats = monitor
                        .getAlgorithmData(nominalTime);
                obData.setFogAlgCellType(
                        monitor.getAlgCellTypes(fogAlgThreats));
                this.updateTableDlg(obData.getObHourReports(nominalTime));
            }
        }
    }

    @Override
    public void addMonitorControlListener(IMonitor monitor) {
        getMonitorControlListeners().add(monitor);
    }

    @Override
    public List<IMonitor> getMonitorControlListeners() {
        return controlListeners;
    }

    @Override
    public void removeMonitorContorlListener(IMonitor monitor) {
        getMonitorControlListeners().remove(monitor);

    }

    @Override
    protected void handleLinkToFrame() {
        linkedToFrame = linkToFrameChk.getSelection();
        SafeSeasMonitor.getInstance()
                .fireMonitorEvent(this.getClass().getName());
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

    @Override
    protected FSSObsMonitorConfigurationManager getMonitorAreaConfigInstance() {
        if (configMgr == null || configMgr.isPopulated()) {
            configMgr = FSSObsMonitorConfigurationManager
                    .getInstance(AppName.SAFESEAS);
        }
        return configMgr;
    }
}
