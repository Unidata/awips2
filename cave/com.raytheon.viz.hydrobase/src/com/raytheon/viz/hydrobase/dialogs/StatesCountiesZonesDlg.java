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
package com.raytheon.viz.hydrobase.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.CountiesData;
import com.raytheon.viz.hydrocommon.data.EligZoneData;
import com.raytheon.viz.hydrocommon.data.StateData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the States/Counties/Zones dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 8, 2008				lvenable	Initial creation
 * Jan 8, 2008  1802        askripsk    Connect to DB.
 * Jun 11, 2013 2088        rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class StatesCountiesZonesDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StatesCountiesZonesDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * States list control.
     */
    private List statesList;

    /**
     * States state text control.
     */
    private Text statesStateTF;

    /**
     * States name text control.
     */
    private Text statesNameTF;

    /**
     * States add button
     */
    private Button statesAddBtn;

    /**
     * States update button.
     */
    private Button statesUpdateBtn;

    /**
     * States delete button.
     */
    private Button statesDeleteBtn;

    /**
     * Counties list control.
     */
    private List countiesList;

    /**
     * Counties state text control.
     */
    private Text countiesStateTF;

    /**
     * Counties county text control.
     */
    private Text countiesCountyTF;

    /**
     * Counties FIPS text control.
     */
    private Text countiesFipsTF;

    /**
     * Counties WFO text control.
     */
    private Text countiesWfoTF;

    /**
     * Counties primary text control.
     */
    private Text countiesPrimaryTF;

    /**
     * Counties secondary text control.
     */
    private Text countiesSecondaryTF;

    /**
     * Counties add button.
     */
    private Button countiesAddBtn;

    /**
     * Counties update button.
     */
    private Button countiesUpdateBtn;

    /**
     * Counties delete button.
     */
    private Button countiesDeleteBtn;

    /**
     * Zones list control.
     */
    private List zonesList;

    /**
     * Zones state text control.
     */
    private Text zonesStateTF;

    /**
     * Zones number text control.
     */
    private Text zonesNumTF;

    /**
     * Zones description text control.
     */
    private Text zonesDescTF;

    /**
     * Zones add button.
     */
    private Button zonesAddBtn;

    /**
     * Zones update button.
     */
    private Button zonesUpdateBtn;

    /**
     * Zones delete button.
     */
    private Button zonesDeleteBtn;

    /**
     * Zone Cache
     */
    private java.util.List<EligZoneData> zoneData;

    /**
     * County Cache
     */
    private java.util.List<CountiesData> countyData;

    /**
     * State Cache
     */
    private java.util.List<StateData> stateData;

    /**
     * Display states for the dialog
     */
    private enum DialogStates {
        NO_ZONES, ZONES_AVAILABLE, NO_STATES, STATES_AVAILABLE, NO_COUNTIES, COUNTIES_AVAILABLE
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public StatesCountiesZonesDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("States/Counties/Zones");
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createStateCountyZoneGroups();

        createCloseButton();

        getDialogData();
    }

    /**
     * Create a composite to hold the States/Counties/Zones groups.
     */
    private void createStateCountyZoneGroups() {
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(3, false));

        createStatesGroup(mainComp);
        createCountiesGroup(mainComp);
        createZonesGroup(mainComp);
    }

    /**
     * Create the states group and controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createStatesGroup(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group statesGroup = new Group(parentComp, SWT.NONE);
        statesGroup.setLayout(new GridLayout(2, false));
        statesGroup.setLayoutData(gd);
        statesGroup.setText(" States ");

        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.horizontalIndent = 4;
        Label stateListLbl = new Label(statesGroup, SWT.NONE);
        stateListLbl.setText(getStateListLblText());
        stateListLbl.setFont(controlFont);
        stateListLbl.setLayoutData(gd);

        gd = new GridData(225, 350);
        gd.horizontalSpan = 2;
        statesList = new List(statesGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        statesList.setLayoutData(gd);
        statesList.setFont(controlFont);
        statesList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateStateInformation();
            }
        });

        Label stateLbl = new Label(statesGroup, SWT.NONE);
        stateLbl.setText("State");

        Label nameLbl = new Label(statesGroup, SWT.NONE);
        nameLbl.setText("Name");

        gd = new GridData(30, SWT.DEFAULT);
        statesStateTF = new Text(statesGroup, SWT.BORDER);
        statesStateTF.setLayoutData(gd);
        statesStateTF.setTextLimit(2);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        statesNameTF = new Text(statesGroup, SWT.BORDER);
        statesNameTF.setLayoutData(gd);
        statesNameTF.setTextLimit(20);

        // -------------------------------------
        // Add control buttons
        // -------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Composite buttonComp = new Composite(statesGroup, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        statesAddBtn = new Button(buttonComp, SWT.PUSH);
        statesAddBtn.setText("Add");
        statesAddBtn.setLayoutData(gd);
        statesAddBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveStateRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        statesUpdateBtn = new Button(buttonComp, SWT.PUSH);
        statesUpdateBtn.setText("Update");
        statesUpdateBtn.setLayoutData(gd);
        statesUpdateBtn.setEnabled(false);
        statesUpdateBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveStateRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        statesDeleteBtn = new Button(buttonComp, SWT.PUSH);
        statesDeleteBtn.setText("Delete");
        statesDeleteBtn.setLayoutData(gd);
        statesDeleteBtn.setEnabled(false);
        statesDeleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteStateRecord();
            }
        });
    }

    /**
     * Create the counties group and controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createCountiesGroup(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group countiesGroup = new Group(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(5, false);
        gl.horizontalSpacing = 10;
        countiesGroup.setLayout(gl);
        countiesGroup.setLayoutData(gd);
        countiesGroup.setText(" Counties ");

        gd = new GridData();
        gd.horizontalSpan = 5;
        gd.horizontalIndent = 4;
        Label countiesListLbl = new Label(countiesGroup, SWT.NONE);
        countiesListLbl.setText(getCountiesListLblText());
        countiesListLbl.setFont(controlFont);
        countiesListLbl.setLayoutData(gd);

        gd = new GridData(500, 325);
        gd.horizontalSpan = 5;
        countiesList = new List(countiesGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        countiesList.setLayoutData(gd);
        countiesList.setFont(controlFont);
        countiesList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateCountyInformation();
            }
        });

        Label stateLbl = new Label(countiesGroup, SWT.NONE);
        stateLbl.setText("State");

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label countyLbl = new Label(countiesGroup, SWT.NONE);
        countyLbl.setText("County");
        countyLbl.setLayoutData(gd);

        Label fipsLbl = new Label(countiesGroup, SWT.NONE);
        fipsLbl.setText("FIPS");

        Label wfoLbl = new Label(countiesGroup, SWT.NONE);
        wfoLbl.setText("WFO");

        gd = new GridData(30, SWT.DEFAULT);
        countiesStateTF = new Text(countiesGroup, SWT.BORDER);
        countiesStateTF.setLayoutData(gd);
        countiesStateTF.setTextLimit(2);

        gd = new GridData(230, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        countiesCountyTF = new Text(countiesGroup, SWT.BORDER);
        countiesCountyTF.setLayoutData(gd);
        countiesCountyTF.setTextLimit(20);

        gd = new GridData(45, SWT.DEFAULT);
        countiesFipsTF = new Text(countiesGroup, SWT.BORDER);
        countiesFipsTF.setLayoutData(gd);
        countiesFipsTF.setTextLimit(4);

        gd = new GridData(45, SWT.DEFAULT);
        countiesWfoTF = new Text(countiesGroup, SWT.BORDER);
        countiesWfoTF.setLayoutData(gd);
        countiesWfoTF.setTextLimit(3);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label primaryLbl = new Label(countiesGroup, SWT.NONE);
        primaryLbl.setText("Sevice Backup Primary:");
        primaryLbl.setLayoutData(gd);

        gd = new GridData(40, SWT.DEFAULT);
        countiesPrimaryTF = new Text(countiesGroup, SWT.BORDER);
        countiesPrimaryTF.setLayoutData(gd);
        countiesPrimaryTF.setTextLimit(3);

        Label secondaryLbl = new Label(countiesGroup, SWT.NONE);
        secondaryLbl.setText("Secondary:");

        gd = new GridData(40, SWT.DEFAULT);
        countiesSecondaryTF = new Text(countiesGroup, SWT.BORDER);
        countiesSecondaryTF.setLayoutData(gd);
        countiesSecondaryTF.setTextLimit(3);

        // -------------------------------------
        // Add control buttons
        // -------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 5;
        Composite buttonComp = new Composite(countiesGroup, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        countiesAddBtn = new Button(buttonComp, SWT.PUSH);
        countiesAddBtn.setText("Add");
        countiesAddBtn.setLayoutData(gd);
        countiesAddBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveCountyRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        countiesUpdateBtn = new Button(buttonComp, SWT.PUSH);
        countiesUpdateBtn.setText("Update");
        countiesUpdateBtn.setLayoutData(gd);
        countiesUpdateBtn.setEnabled(false);
        countiesUpdateBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveCountyRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        countiesDeleteBtn = new Button(buttonComp, SWT.PUSH);
        countiesDeleteBtn.setText("Delete");
        countiesDeleteBtn.setLayoutData(gd);
        countiesDeleteBtn.setEnabled(false);
        countiesDeleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteCountyRecord();
            }
        });
    }

    /**
     * Create the zones group and controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createZonesGroup(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group zonesGroup = new Group(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 10;
        zonesGroup.setLayout(gl);
        zonesGroup.setLayoutData(gd);
        zonesGroup.setText(" Zones ");

        gd = new GridData();
        gd.horizontalSpan = 3;
        gd.horizontalIndent = 4;
        Label zonesListLbl = new Label(zonesGroup, SWT.NONE);
        zonesListLbl.setText(getZonesListLblText());
        zonesListLbl.setFont(controlFont);
        zonesListLbl.setLayoutData(gd);

        gd = new GridData(225, 350);
        gd.horizontalSpan = 3;
        zonesList = new List(zonesGroup, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        zonesList.setLayoutData(gd);
        zonesList.setFont(controlFont);
        zonesList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateZoneInformation();
            }
        });

        Label stateLbl = new Label(zonesGroup, SWT.NONE);
        stateLbl.setText("State");

        Label numLbl = new Label(zonesGroup, SWT.NONE);
        numLbl.setText("Num");

        Label descLbl = new Label(zonesGroup, SWT.NONE);
        descLbl.setText("Description");

        gd = new GridData(30, SWT.DEFAULT);
        zonesStateTF = new Text(zonesGroup, SWT.BORDER);
        zonesStateTF.setLayoutData(gd);
        zonesStateTF.setTextLimit(2);

        gd = new GridData(40, SWT.DEFAULT);
        zonesNumTF = new Text(zonesGroup, SWT.BORDER);
        zonesNumTF.setLayoutData(gd);
        zonesNumTF.setTextLimit(3);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        zonesDescTF = new Text(zonesGroup, SWT.BORDER);
        zonesDescTF.setLayoutData(gd);
        zonesDescTF.setTextLimit(20);

        // -------------------------------------
        // Add control buttons
        // -------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        Composite buttonComp = new Composite(zonesGroup, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        zonesAddBtn = new Button(buttonComp, SWT.PUSH);
        zonesAddBtn.setText("Add");
        zonesAddBtn.setLayoutData(gd);
        zonesAddBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveZoneRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        zonesUpdateBtn = new Button(buttonComp, SWT.PUSH);
        zonesUpdateBtn.setText("Update");
        zonesUpdateBtn.setLayoutData(gd);
        zonesUpdateBtn.setEnabled(false);
        zonesUpdateBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveZoneRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        zonesDeleteBtn = new Button(buttonComp, SWT.PUSH);
        zonesDeleteBtn.setText("Delete");
        zonesDeleteBtn.setLayoutData(gd);
        zonesDeleteBtn.setEnabled(false);
        zonesDeleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteZoneRecord();
            }
        });
    }

    /**
     * Create the close button at the bottom of the dialog.
     */
    private void createCloseButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Get the States list control label text.
     * 
     * @return Label text.
     */
    private String getStateListLblText() {
        String format = "%S  %S";
        String labelStr = String.format(format, "State", "Name");

        return labelStr;
    }

    /**
     * Get the Counties list control label text.
     * 
     * @return Label text.
     */
    private String getCountiesListLblText() {
        String format = "%S %S                %S   %S   %S  %S";
        String labelStr = String.format(format, "State", "County", "FIPS",
                "WFO", "Primary", "Secondary");

        return labelStr;
    }

    /**
     * Get the Zones list control label text.
     * 
     * @return Label text.
     */
    private String getZonesListLblText() {
        String format = "%S  %S  %S";
        String labelStr = String.format(format, "St.", "Num", "Description");

        return labelStr;
    }

    // ------------------------------------------
    // ------------------------------------------
    // DATA Retrieval
    // ------------------------------------------
    // ------------------------------------------
    private void getDialogData() {
        getStateData();
        getCountyData();
        getZoneData();
    }

    private void getStateData() {
        try {
            stateData = HydroDBDataManager.getInstance().getData(
                    StateData.class);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to load state data ", e);
        }

        updateStateDisplay();
    }

    private void getCountyData() {
        try {
            countyData = HydroDBDataManager.getInstance().getData(
                    CountiesData.class);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to load counties data ", e);
        }

        updateCountyDisplay();
    }

    private void getZoneData() {
        try {
            zoneData = HydroDBDataManager.getInstance().getData(
                    EligZoneData.class);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to load zone data ",
                    e);
        }

        updateZoneDisplay();
    }

    private void updateStateDisplay() {
        // States
        String format = "%-6S %S";
        statesList.removeAll();

        if (stateData.size() > 0) {
            for (StateData currState : stateData) {
                statesList.add(String.format(format, currState.getState(),
                        currState.getName()));
            }

            updateDialogState(DialogStates.STATES_AVAILABLE);
        } else {
            updateDialogState(DialogStates.NO_STATES);
        }
    }

    private void updateCountyDisplay() {
        // Counties
        String format = "%-5S %-22S %3S   %3S     %3S      %S";
        countiesList.removeAll();

        if (countyData.size() > 0) {
            for (CountiesData currCounty : countyData) {
                countiesList.add(String.format(format, currCounty.getState(),
                        currCounty.getCounty(), currCounty.getCountyNumber(),
                        currCounty.getWfo(), currCounty.getPrimaryBack(),
                        currCounty.getSecondaryBack()));
            }

            updateDialogState(DialogStates.COUNTIES_AVAILABLE);
        } else {
            updateDialogState(DialogStates.NO_COUNTIES);
        }
    }

    private void updateZoneDisplay() {
        // Zones
        String format = "%-3S  %-4S %S";
        zonesList.removeAll();

        if (zoneData.size() > 0) {
            for (EligZoneData currZone : zoneData) {
                zonesList.add(String.format(format, currZone.getState(),
                        currZone.getZoneNumber(), currZone.getDescription()));
            }

            updateDialogState(DialogStates.ZONES_AVAILABLE);
        } else {
            updateDialogState(DialogStates.NO_ZONES);
        }
    }

    // ------------------------------------------
    // ------------------------------------------
    // Update Information
    // ------------------------------------------
    // ------------------------------------------
    private void updateStateInformation() {
        StateData currData = getSelectedState();

        if (currData != null) {
            statesStateTF.setText(currData.getState());
            statesNameTF.setText(currData.getName());
        }
    }

    private void updateCountyInformation() {
        CountiesData currData = getSelectedCounty();

        if (currData != null) {
            countiesStateTF.setText(currData.getState());
            countiesCountyTF.setText(currData.getCounty());
            countiesFipsTF.setText(currData.getCountyNumber());
            countiesWfoTF.setText(currData.getWfo());
            countiesPrimaryTF.setText(currData.getPrimaryBack());
            countiesSecondaryTF.setText(currData.getSecondaryBack());
        }
    }

    private void updateZoneInformation() {
        EligZoneData currData = getSelectedZone();

        if (currData != null) {
            zonesStateTF.setText(currData.getState());
            zonesNumTF.setText(currData.getZoneNumber());
            zonesDescTF.setText(currData.getDescription());
        }
    }

    // ------------------------------------------
    // ------------------------------------------
    // SAVE
    // ------------------------------------------
    // ------------------------------------------
    private void saveStateRecord() {
        if (!statesStateTF.getText().equals("")) {
            StateData newData = new StateData();

            newData.setName(statesNameTF.getText());
            newData.setState(statesStateTF.getText());

            try {
                HydroDBDataManager.getInstance().putData(newData);

                getStateData();
            } catch (VizException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("An error occurred while trying to save the State");
                mb.open();
            }
        } else {
            missingValueMessage("State");
        }
    }

    private void saveCountyRecord() {
        if (countiesCountyTF.getText().equals("")) {
            missingValueMessage("County");
        } else if (countiesStateTF.getText().equals("")) {
            missingValueMessage("State");
        } else if (countiesWfoTF.getText().equals("")) {
            missingValueMessage("WFO");
        } else if (countiesPrimaryTF.getText().equals("")) {
            missingValueMessage("Primary Backup");
        } else if (countiesSecondaryTF.getText().equals("")) {
            missingValueMessage("Secondary Backup");
        } else if (checkCountyFKConstraintsMet()) {
            CountiesData newData = new CountiesData();

            newData.setState(countiesStateTF.getText());
            newData.setCounty(countiesCountyTF.getText());
            newData.setCountyNumber(countiesFipsTF.getText());
            newData.setWfo(countiesWfoTF.getText());
            newData.setPrimaryBack(countiesPrimaryTF.getText());
            newData.setSecondaryBack(countiesSecondaryTF.getText());

            try {
                HydroDBDataManager.getInstance().putData(newData);

                getCountyData();
            } catch (VizException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("An error occurred while trying to save the County");
                mb.open();
            }
        }

    }

    private void saveZoneRecord() {
        if (zonesStateTF.getText().equals("")) {
            missingValueMessage("State");
        } else if (zonesNumTF.getText().equals("")) {
            missingValueMessage("Zone Number");
        } else if (checkStateFKConstraintsMet()) {
            EligZoneData newData = new EligZoneData();

            newData.setState(zonesStateTF.getText());
            newData.setZoneNumber(zonesNumTF.getText());
            newData.setDescription(zonesDescTF.getText());

            try {
                HydroDBDataManager.getInstance().putData(newData);

                getZoneData();
            } catch (VizException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("An error occurred while trying to save the Zone");
                mb.open();
            }
        }

    }

    private void missingValueMessage(String field) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setText("Unable to Save");
        mb.setMessage(field + " cannot be empty.");
        mb.open();
    }

    // ------------------------------------------
    // ------------------------------------------
    // DELETE
    // ------------------------------------------
    // ------------------------------------------
    private void deleteStateRecord() {
        StateData currData = getSelectedState();

        if (currData != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete the selected state?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    HydroDBDataManager.getInstance().deleteRecord(currData);

                    // Refresh the cache
                    getStateData();
                } catch (VizException e) {
                    mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Unable to Delete");
                    mb.setMessage("An error occurred while trying to delete the State");
                    mb.open();
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a state first.");
            mb.open();
        }
    }

    private void deleteCountyRecord() {
        CountiesData currData = getSelectedCounty();

        if (currData != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete the selected county?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    HydroDBDataManager.getInstance().deleteRecord(currData);

                    // Refresh the cache
                    getCountyData();
                } catch (VizException e) {
                    mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Unable to Delete");
                    mb.setMessage("An error occurred while trying to delete the County");
                    mb.open();
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a county first.");
            mb.open();
        }
    }

    private void deleteZoneRecord() {
        EligZoneData currData = getSelectedZone();

        if (currData != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete the selected zone?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    HydroDBDataManager.getInstance().deleteRecord(currData);

                    // Refresh the cache
                    getZoneData();
                } catch (VizException e) {
                    mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Unable to Delete");
                    mb.setMessage("An error occurred while trying to delete the Zone");
                    mb.open();
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a zone first.");
            mb.open();
        }
    }

    // ------------------------------------------
    // ------------------------------------------
    // Get Currently Selected
    // ------------------------------------------
    // ------------------------------------------
    private StateData getSelectedState() {
        StateData rval = null;

        if (statesList.getSelectionCount() > 0) {
            rval = stateData.get(statesList.getSelectionIndex());
        }

        return rval;
    }

    private CountiesData getSelectedCounty() {
        CountiesData rval = null;

        if (countiesList.getSelectionCount() > 0) {
            rval = countyData.get(countiesList.getSelectionIndex());
        }

        return rval;
    }

    private EligZoneData getSelectedZone() {
        EligZoneData rval = null;

        if (zonesList.getSelectionCount() > 0) {
            rval = zoneData.get(zonesList.getSelectionIndex());
        }

        return rval;
    }

    // ------------------------------------------
    // ------------------------------------------
    // FK Checks
    // ------------------------------------------
    // ------------------------------------------
    private boolean checkCountyFKConstraintsMet() {
        boolean rval = false;

        String wfoQuery = "SELECT wfo FROM wfo WHERE wfo='%s'"; // primary_back,
        // secondary_back,
        // wfo
        String stateQuery = "SELECT state FROM state WHERE state='%s'"; // state

        try {
            QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                    String.format(wfoQuery, countiesPrimaryTF.getText()));

            // Check if Primary Backup Exists
            if (data.getResultCount() > 0) {
                data = HydroDBDataManager.getInstance().runMappedQuery(
                        String.format(wfoQuery, countiesSecondaryTF.getText()));

                // Check if Secondary Backup Exists
                if (data.getResultCount() > 0) {
                    data = HydroDBDataManager.getInstance().runMappedQuery(
                            String.format(wfoQuery, countiesWfoTF.getText()));

                    // Check if WFO Exists
                    if (data.getResultCount() > 0) {
                        data = HydroDBDataManager.getInstance().runMappedQuery(
                                String.format(stateQuery,
                                        countiesStateTF.getText()));

                        // Check if State Exists
                        if (data.getResultCount() > 0) {
                            rval = true;
                        } else {
                            MessageBox mb = new MessageBox(shell,
                                    SWT.ICON_ERROR | SWT.OK);
                            mb.setText("Unable to Save");
                            mb.setMessage("Please choose a State that exists.");
                            mb.open();
                        }
                    } else {
                        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                                | SWT.OK);
                        mb.setText("Unable to Save");
                        mb.setMessage("Please choose a WFO that exists.");
                        mb.open();
                    }
                } else {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Unable to Save");
                    mb.setMessage("Please choose a Secondary Backup that exists.");
                    mb.open();
                }
            } else {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("Please choose a Primary Backup that exists.");
                mb.open();
            }
        } catch (VizException e) {
            // don't care, just return false
        }

        return rval;
    }

    private boolean checkStateFKConstraintsMet() {
        boolean rval = false;

        String stateQuery = "SELECT state FROM state WHERE state='%s'"; // state

        try {
            QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                    String.format(stateQuery, zonesStateTF.getText()));

            // Check if State Exists
            if (data.getResultCount() > 0) {

                rval = true;
            } else {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("Please choose a State that exists.");
                mb.open();
            }

        } catch (VizException e) {
            // don't care, just return false
        }

        return rval;
    }

    private void updateDialogState(DialogStates currState) {
        switch (currState) {
        case NO_COUNTIES:
            countiesDeleteBtn.setEnabled(false);
            countiesUpdateBtn.setEnabled(false);
            break;
        case COUNTIES_AVAILABLE:
            countiesDeleteBtn.setEnabled(true);
            countiesUpdateBtn.setEnabled(true);
            break;
        case NO_STATES:
            statesDeleteBtn.setEnabled(false);
            statesUpdateBtn.setEnabled(false);
            break;
        case STATES_AVAILABLE:
            statesDeleteBtn.setEnabled(true);
            statesUpdateBtn.setEnabled(true);
            break;
        case NO_ZONES:
            zonesDeleteBtn.setEnabled(false);
            zonesUpdateBtn.setEnabled(false);
            break;
        case ZONES_AVAILABLE:
            zonesDeleteBtn.setEnabled(true);
            zonesUpdateBtn.setEnabled(true);
            break;
        default:
            break;
        }
    }
}
