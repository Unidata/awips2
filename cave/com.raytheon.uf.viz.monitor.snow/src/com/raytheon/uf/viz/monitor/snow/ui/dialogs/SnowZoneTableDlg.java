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

import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.data.ObConst.DisplayVarName;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.snow.SnowMonitor;
import com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg;

/**
 * The SNOW Zone Table dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Nov 30, 2009  3424     zhao/wkwock  Automatically updates snow display.
 *                                     Display station data.
 * Dec 02, 2009  3424     zhao/wkwock  Fix display SNOW 2nd time problem.
 * Dec 18, 2009  3424     zhao         use ObMultiHrsReports for obs data
 *                                     archive
 * July 20,2010  4891     skorolev     added code to fireDialogShutdown
 * Nov. 8, 2012  1297     skorolev     Added initiateProdArray method
 * Dec 07, 2012  1351     skorolev     Changes for non-blocking dialogs
 * Apr 28, 2014  3086     skorolev     Updated getConfigMgr method.
 * Sep 04, 2014  3220     skorolev     Removed "site". Added check on dispose.
 * Sep 18, 2015  3873     skorolev     Adjusted to AppName and MonName.
 * Dec 17, 2015  3873     dhladky      Abstracted handling of dialogTime and
 *                                     Zone dialog events.
 * Jan 04, 2016  5115     skorolev     Corrected imports and replaced AppName
 *                                     with MonName.
 * Jul 10, 2018  6766     randerso     Fixed dialog closing for recent changes
 *                                     to CaveSWTDialog. Implemented do nothing
 *                                     interface methods in base class.
 *
 * </pre>
 *
 * @author
 */

public class SnowZoneTableDlg extends ZoneTableDlg {

    /** SNOW threshold dialog. **/
    private SnowMonDispThreshDlg snowThreshDlg;

    /**
     * Constructor
     *
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

    @Override
    public void initiateProdArray() {
        varName = config.getSnowZoneStnTableColVarNames()[colIndex];
        // Fill product array
        prodArray = new ArrayList<>();
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

    @Override
    protected void configThreshAction() {
        if (snowThreshDlg == null || snowThreshDlg.isDisposed()) {
            snowThreshDlg = new SnowMonDispThreshDlg(getParent().getShell(),
                    CommonConfig.AppName.SNOW, DataUsageKey.DISPLAY);
        }
        snowThreshDlg.open();
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
                this.updateTableDlg(monitor.getObData().getObHourReports(date));
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
        SnowMonitor.getInstance().fireMonitorEvent(this.getClass().getName());
    }

    @Override
    protected FSSObsMonitorConfigurationManager getMonitorAreaConfigInstance() {
        if (configMgr == null || configMgr.isPopulated()) {
            configMgr = FSSObsMonitorConfigurationManager
                    .getInstance(AppName.SNOW);
        }
        return configMgr;
    }
}
