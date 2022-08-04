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

import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.data.ObConst.DisplayVarName;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.CellType;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
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
 * Date          Ticket#      Engineer  Description
 * ------------- ------------ --------- ----------------------------------------
 * Jan 25, 2010  4281         zhao      Modified the notify method
 * Jun 16, 2012  14386        zhao      Modified the notify method
 * Oct 30, 2012               skorolev  Changed HashMap to Map
 * Nov 11, 2012  1297         skorolev  Added initiateProdArray
 * Dec 03, 2012  15216/15639  zhao      fixed a bug related to Link-to-Frame
 * Dec 07, 2012  1351         skorolev  Changes for non-blocking dialogs.
 * Apr 28, 2014  3086         skorolev  Updated getConfigMgr method.
 * Sep 04, 2014  3220         skorolev  Removed "site". Added check on dispose.
 * Sep 18, 2015  3873         skorolev  Adjusted to AppName and MonName.
 * Dec 17, 2015  3873         dhladky   Abstracted handling of dialogTime and
 *                                      Zone dialog events.
 * Jan 04, 2015  5115         skorolev  Corrected imports and replaced AppName
 *                                      with MonName.
 * Jul 10, 2018  6766         randerso  Fixed dialog closing for recent changes
 *                                      to CaveSWTDialog. Implemented do nothing
 *                                      interface methods in base class.
 *
 * </pre>
 *
 * @author ?
 */
public class FogZoneTableDlg extends ZoneTableDlg {

    /** Fog threshold dialog. **/
    private FogMonDispThreshDlg fogThreshDlg;

    /**
     * Constructor
     *
     * @param parent
     * @param obData
     */
    public FogZoneTableDlg(Shell parent, ObMultiHrsReports obData) {
        super(parent, obData, CommonConfig.AppName.FOG);
    }

    /**
     * @param parent
     */
    public FogZoneTableDlg(Shell parent) {
        super(parent, CommonConfig.AppName.FOG);
    }

    @Override
    public void initiateProdArray() {
        varName = config.getFogZoneStnTableColVarNames()[colIndex];
        prodArray = new ArrayList<>();
        String varpref = "VAR_";
        for (DisplayVarName var : DisplayVarName.values()) {
            String dispVarName = var.name();
            if (dispVarName.equals(varpref + varName.name())) {
                prodArray.add(dispVarName);
            }
        }
    }

    @Override
    protected void configThreshAction() {
        if (fogThreshDlg == null || fogThreshDlg.isDisposed()) {
            fogThreshDlg = new FogMonDispThreshDlg(getParent().getShell(),
                    CommonConfig.AppName.FOG, DataUsageKey.DISPLAY);
        }
        fogThreshDlg.open();
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
            Date date = fog.getDialogTime();
            if (date != null) {
                Date nominalTime = date;
                ObMultiHrsReports obData = fog.getObData();
                FogDataGenerator fdg = new FogDataGenerator();
                Map<String, CellType> fogAlgCellType = fdg
                        .getAlgCellTypes(fog.getAlgorithmData(nominalTime));
                obData.setFogAlgCellType(fogAlgCellType);
                this.updateTableDlg(obData.getObHourReports(nominalTime));
            }
        }
    }

    @Override
    public void addMonitorControlListener(IMonitor monitor) {
        getMonitorControlListeners().add(monitor);
    }

    @Override
    public void fireDialogShutdown(IMonitorListener iml) {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
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
            @Override
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
    public void removeMonitorContorlListener(IMonitor monitor) {
        getMonitorControlListeners().remove(monitor);
    }

    @Override
    protected void handleLinkToFrame() {
        linkedToFrame = linkToFrameChk.getSelection();
        FogMonitor.getInstance().fireMonitorEvent(this.getClass().getName());
    }

    @Override
    protected FSSObsMonitorConfigurationManager getMonitorAreaConfigInstance() {
        if (configMgr == null || configMgr.isPopulated()) {
            configMgr = FSSObsMonitorConfigurationManager
                    .getInstance(AppName.FOG);
        }
        return configMgr;
    }
}
