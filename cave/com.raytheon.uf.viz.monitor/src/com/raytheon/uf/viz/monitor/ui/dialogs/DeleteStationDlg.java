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
package com.raytheon.uf.viz.monitor.ui.dialogs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.xml.AreaIdXML;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog for deleting stations.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2009            lvenable     Initial creation
 * Nov 20, 2012 1297      skorolev     Changes for non-blocking dialog.
 * Apr 23, 2014 3054      skorolev     Fixed issue with deleting a new station.
 * Apr 28, 2014 3086      skorolev     Removed local getAreaConfigMgr method.
 * Aug 17, 2015 3841      skorolev     Corrected deleteSelected method.
 * Nov 12, 2015 3841      dhladky      Augmented Slav's work.
 * Dec 02, 2015 3873      dhladky      Pulled 3841 to 16.1.1.
 * Jun 18, 2018 7023      tgurney      deleteSelected() set the right return value
 *
 * </pre>
 *
 * @author lvenable
 */
public class DeleteStationDlg extends CaveSWTDialog {

    /** Station list control. */
    private org.eclipse.swt.widgets.List stationList;

    /** Control font. */
    private Font controlFont;

    /** Monitoring Area Configuration Dialog */
    private final MonitoringAreaConfigDlg macDlg;

    private final List<String> newAddedStns = new ArrayList<>();

    private List<String> deletedStns = new ArrayList<>();

    /**
     * Constructor.
     *
     * @param parent
     *            Parent shell.
     * @param appName
     *            Application name.
     * @param area
     * @param macDlg
     */
    public DeleteStationDlg(Shell parent, AppName appName,
            MonitoringAreaConfigDlg macDlg) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText(appName.toString() + ": Delete a Newly Entered Station");
        this.macDlg = macDlg;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(deletedStns);
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        createListControl();
        createBottomButtons();
        FSSObsMonitorConfigurationManager cfgMgr = macDlg.getInstance();
        List<AreaIdXML> areaList = cfgMgr.getConfigXml().getAreaIds();
        newAddedStns.addAll(cfgMgr.getNewlyAddedStations(areaList));
        areaList = cfgMgr.getAdjAreaConfigXml().getAreaIds();
        newAddedStns.addAll(cfgMgr.getNewlyAddedStations(areaList));
        populate();
    }

    /**
     * Creates the list control.
     */
    private void createListControl() {
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));
        Label stationLbl = new Label(listComp, SWT.NONE);
        stationLbl.setText("Available Stations to delete:");
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 350;
        gd.heightHint = 250;
        stationList = new org.eclipse.swt.widgets.List(listComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        stationList.setFont(controlFont);
        stationList.setLayoutData(gd);
    }

    /**
     * Creates the Delete Station and Close buttons.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        Button deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete Station");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String delStn = deleteSelected();
                if (!delStn.isEmpty()) {
                    deletedStns.add(delStn);
                    setReturnValue(deletedStns);
                }
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Populate list of added stations.
     */
    private void populate() {
        stationList.setItems(
                newAddedStns.toArray(new String[newAddedStns.size()]));
    }

    /**
     * Delete stations from the list.
     */
    private String deleteSelected() {
        String retval = "";
        if (stationList.getItemCount() != 0) {
            if (stationList.getSelectionIndex() != -1) {
                int idx = stationList.getSelectionIndex();
                String selection = stationList.getItem(idx);
                stationList.remove(selection);
                FSSObsMonitorConfigurationManager cfgMgr = macDlg.getInstance();
                List<AreaIdXML> areaXmlList = cfgMgr.getConfigXml()
                        .getAreaIds();
                cfgMgr.removeStation(selection.split("#")[0], areaXmlList);
                areaXmlList = cfgMgr.getAdjAreaConfigXml().getAreaIds();
                cfgMgr.removeStation(selection.split("#")[0], areaXmlList);
                newAddedStns.remove(selection);
                populate();
                macDlg.maStationsChanged = true;
                retval = selection;
            } else {
                MessageBox messageBox = new MessageBox(shell,
                        SWT.ICON_INFORMATION | SWT.NONE);
                messageBox.setText("Selection error.");
                messageBox.setMessage("Please select station to delete.");
                messageBox.open();
                stationList.select(0);
            }
        }
        return retval;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }
}
