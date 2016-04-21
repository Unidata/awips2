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
package com.raytheon.viz.ghg.monitor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ghg.monitor.data.GhgAlertData;
import com.raytheon.viz.ghg.monitor.data.GhgAlertsConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the GHG Monitor Alert Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation
 * 15 NOV 2012  1298       rferrel     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class GhgAlertDlg extends CaveSWTDialog {
    /**
     * Array of Enable Alert containers.
     */
    private ArrayList<GhgEnableAlertComp> alertCompArray;

    /**
     * Array of alert list controls.
     */
    private ArrayList<GhgFilterListGroup> listGroupArray;

    /**
     * Alert My WFO check box.
     */
    private Button alertMyWfoChk;

    /**
     * Alert Text Products check box.
     */
    private Button alertTextProductsChk;

    private GhgAlertsConfigData alerts = null;

    /**
     * Filter list control names.
     */
    private GhgConfigData.AlertsFilterEnum[] filters = {
            GhgConfigData.AlertsFilterEnum.Action,
            GhgConfigData.AlertsFilterEnum.PhenSig,
            GhgConfigData.AlertsFilterEnum.Pil };

    public static Map<GhgConfigData.AlertsFilterEnum, String[]> filterToEnumMap = new HashMap<GhgConfigData.AlertsFilterEnum, String[]>() {

        private static final long serialVersionUID = 6183513849706287870L;
        {
            put(GhgConfigData.AlertsFilterEnum.Action,
                    GhgConfigData.vtecActionNames);
            put(GhgConfigData.AlertsFilterEnum.PhenSig, GhgConfigData
                    .getInstance().getPhenSigCodes());
            put(GhgConfigData.AlertsFilterEnum.Pil, GhgConfigData.vtecPILNames);
        }
    };

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public GhgAlertDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("GHG Monitor Alert Dialog");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        // Initialize all of the controls and layouts
        initializeComponents();

        setInitialDataValues();
    }

    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents() {
        createTopLabel();

        createCenterControlComp();

        addHorizontalSeparator(shell);

        createBottomButtons();
    }

    /**
     * Create the top description label.
     */
    private void createTopLabel() {
        Label topLabel = new Label(shell, SWT.NONE);
        topLabel.setText("Define alert times for the 'alert1', 'alert2', "
                + "and 'expired' alert types.  Define alert filters for events to alert.");
    }

    /**
     * Create the center control container.
     */
    private void createCenterControlComp() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite centerComp = new Composite(shell, SWT.NONE);
        centerComp.setLayout(new GridLayout(2, false));
        centerComp.setLayoutData(gd);

        createLeftAlertControls(centerComp);
        createFilterControls(centerComp);
    }

    /**
     * Create the control on the left side of the display.
     * 
     * @param centerComp
     *            Center composite.
     */
    private void createLeftAlertControls(Composite centerComp) {
        Composite alertComp = new Composite(centerComp, SWT.NONE);
        alertComp.setLayout(new GridLayout(1, false));

        alertCompArray = new ArrayList<GhgEnableAlertComp>();

        alertCompArray.add(new GhgEnableAlertComp(alertComp,
                GhgConfigData.AlertsEnum.AlertLvl1, 30, true));
        alertCompArray.add(new GhgEnableAlertComp(alertComp,
                GhgConfigData.AlertsEnum.AlertLvl2, 10, true));
        alertCompArray.add(new GhgEnableAlertComp(alertComp,
                GhgConfigData.AlertsEnum.ExpiredAlert, 0, false));

        Composite checkComp = new Composite(alertComp, SWT.NONE);
        checkComp.setLayout(new GridLayout(2, false));

        alertMyWfoChk = new Button(checkComp, SWT.CHECK);
        alertMyWfoChk.setText("Alert for My WFO Only  ");
        alertMyWfoChk.setSelection(true);

        alertTextProductsChk = new Button(checkComp, SWT.CHECK);
        alertTextProductsChk.setText("Alert for Test Products");
        alertTextProductsChk.setSelection(true);
    }

    /**
     * Create the filter controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createFilterControls(Composite parentComp) {
        listGroupArray = new ArrayList<GhgFilterListGroup>();

        Composite filterComp = new Composite(parentComp, SWT.NONE);
        filterComp.setLayout(new GridLayout(3, false));

        for (GhgConfigData.AlertsFilterEnum filter : filters) {
            String[] values = filterToEnumMap.get(filter);

            listGroupArray.add(new GhgFilterListGroup(filterComp, filter,
                    values) {
                /**
                 * The standard GhgFilterListGroup updates FILTERS, as soon as a
                 * change is made. We want to update ALERTS, and only after the
                 * 'Apply Filter' button is pushed. the updateDisplay() method
                 * in the base class does all the things we don't want, so just
                 * stub it out.
                 */
                @Override
                protected void updateDisplay() {
                    // do nothing
                }
            });
        }
    }

    /**
     * Add a horizontal line separator to the display.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void addHorizontalSeparator(Composite parentComp) {
        // add a separator line
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        buttonArea.setLayout(new GridLayout(1, false));

        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(2, true));

        gd = new GridData(120, SWT.DEFAULT);
        Button applyFilterBtn = new Button(buttons, SWT.PUSH);
        applyFilterBtn.setText("Apply Filter");
        applyFilterBtn.setLayoutData(gd);
        applyFilterBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateAlertingValues();
                setReturnValue(alerts);
                close();
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(null);
                close();
            }
        });
    }

    /**
     * updates the display to reflect the values in the configuration.
     */
    private void setInitialDataValues() {
        alerts = GhgConfigData.getInstance().getAlerts();
        if (alerts != null) {
            /*
             * restore the previously saved alert data.
             */
            Map<GhgConfigData.AlertsEnum, GhgAlertData> map = new HashMap<GhgConfigData.AlertsEnum, GhgAlertData>();
            for (GhgAlertData alert : alerts.getAlerts()) {
                map.put(alert.getType(), alert);
            }
            for (GhgEnableAlertComp comp : alertCompArray) {
                GhgConfigData.AlertsEnum key = comp.getType();
                comp.setAlertData(map.get(key));
            }
            /*
             * update the selection lists.
             */
            Map<GhgConfigData.AlertsFilterEnum, String[]> selections = new HashMap<GhgConfigData.AlertsFilterEnum, String[]>();
            for (GhgConfigData.AlertsFilterEnum filter : filters) {
                selections.put(filter, alerts.getSelections(filter));
            }
            for (GhgFilterListGroup comp : listGroupArray) {
                GhgConfigData.AlertsFilterEnum key = comp.getType();
                comp.setSelValues(selections.get(key));
            }
            alertMyWfoChk.setSelection(alerts.isLocal());
            alertTextProductsChk.setSelection(alerts.isTest());
        }
    }

    public void setAlerts(GhgAlertsConfigData alerts) {
        this.alerts = alerts.clone();
    }

    /**
     * 
     */
    private void updateAlertingValues() {
        if (alerts != null) {

            /*
             * loop through the list groups get the selected indices
             */
            for (GhgFilterListGroup comp : listGroupArray) {
                String[] selections = comp.getSelections();
                alerts.setSelections(comp.getType(), selections);
            }
            /*
             * update the "My WFO" and "TEST Product" values
             */
            alerts.setLocal(alertMyWfoChk.getSelection());
            alerts.setTest(alertTextProductsChk.getSelection());
        }
    }
}