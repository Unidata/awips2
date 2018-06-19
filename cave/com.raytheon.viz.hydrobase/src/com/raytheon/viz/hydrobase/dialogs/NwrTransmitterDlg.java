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
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrobase.listeners.ICountyStateListener;
import com.raytheon.viz.hydrocommon.data.CountiesData;
import com.raytheon.viz.hydrocommon.data.CountyTransmitData;
import com.raytheon.viz.hydrocommon.data.NWRTransmitterData;
import com.raytheon.viz.hydrocommon.datamanager.AddModifyLocationDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displayed the NWR transmitter dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 5, 2008				lvenable	Initial creation
 * Jan 9, 2008  1802        askripsk    Connect to DB.
 * Apr 17,2013  1790        rferrel     Changes for non-blocking CountyStateDlg.
 *                                      Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class NwrTransmitterDlg extends CaveSWTDialog implements
        ICountyStateListener {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(NwrTransmitterDlg.class);

    /**
     * Allow one Count/State dialog.
     */
    private CountyStateDlg countyStateDlg;

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Main data list control.
     */
    private List mainDataList;

    /**
     * Active check box.
     */
    private Button activeChk;

    /**
     * WFO combo box.
     */
    private Combo wfoCbo;

    /**
     * Call sign text control.
     */
    private Text callSignTF;

    /**
     * Coverage area text control.
     */
    private Text coverageAreaTF;

    /**
     * Latitude text control.
     */
    private Text latTF;

    /**
     * Longitude text control.
     */
    private Text lonTF;

    /**
     * Frequency text control.
     */
    private Text frequencyTF;

    /**
     * Power text control.
     */
    private Text powerTF;

    /**
     * Product code text control.
     */
    private Text productCodeTF;

    /**
     * City text control.
     */
    private Text cityTF;

    /**
     * County/State text control.
     */
    private Text countyStateTF;

    /**
     * Pseudo county number text control.
     */
    private Text pseudoCntyNumTF;

    /**
     * Clear button.
     */
    private Button clearBtn;

    /**
     * Apply button.
     */
    private Button applyBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Counties covered by transmitter label.
     */
    private Label countyCovLbl;

    /**
     * County coverage list control.
     */
    private List cntyCovList;

    /**
     * County add button.
     */
    private Button cntyAddBtn;

    /**
     * County delete button.
     */
    private Button cntyDeleteBtn;

    /**
     * County available list control.
     */
    private List cntyAvailList;

    /**
     * Cache of available counties
     */
    private java.util.List<CountiesData> availableCounties;

    /**
     * Cache of Transmitters
     */
    private java.util.List<NWRTransmitterData> txData;

    /**
     * Cache of Transmitter's Counties
     */
    private java.util.List<CountyTransmitData> selectedCounties;

    /**
     * Cache of selected county and state
     */
    private CountiesData txCountyState = null;

    /**
     * Possible states for the dialog
     */
    private enum DialogState {
        DATA_AVAILABLE, NO_DATA, COUNTIES_AVAILABLE, NO_COUNTIES, NEW_RECORD, COUNTIES_SELECTED, NO_COUNTIES_SELECTED
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public NwrTransmitterDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("NWR Transmitter");
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
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        controlFont.dispose();
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
        setReturnValue(false);
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createNwrTransmitterGroup();
        createCountyCoverageGroup();

        createCloseButton();

        loadStaticData();

        getDialogData();
    }

    /**
     * Create NWR transmitter group and controls.
     */
    private void createNwrTransmitterGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group nwrTransGroup = new Group(shell, SWT.NONE);
        nwrTransGroup.setLayout(new GridLayout(1, false));
        nwrTransGroup.setLayoutData(gd);
        nwrTransGroup.setText(" NWR Transmitter Information ");

        // -------------------------------------------------
        // Top data list and labels
        // -------------------------------------------------
        gd = new GridData();
        gd.horizontalIndent = 4;
        Label topListLabel = new Label(nwrTransGroup, SWT.NONE);
        topListLabel.setText(getDataListTopLabelText());
        topListLabel.setFont(controlFont);
        topListLabel.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 4;
        Label bottomListLabel = new Label(nwrTransGroup, SWT.NONE);
        bottomListLabel.setText(getDataListBottomLabelText());
        bottomListLabel.setFont(controlFont);
        bottomListLabel.setLayoutData(gd);

        gd = new GridData(1050, 250);
        mainDataList = new List(nwrTransGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        mainDataList.setLayoutData(gd);
        mainDataList.setFont(controlFont);
        mainDataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateInformation();
            }
        });

        // -------------------------------------------------
        // Middle text and other controls
        // -------------------------------------------------
        Composite middleComp = new Composite(nwrTransGroup, SWT.NONE);
        middleComp.setLayout(new GridLayout(9, false));

        //
        // Call Sign
        //
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label callSignLbl = new Label(middleComp, SWT.RIGHT);
        callSignLbl.setText("Call Sign:");
        callSignLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        callSignTF = new Text(middleComp, SWT.BORDER);
        callSignTF.setLayoutData(gd);
        callSignTF.setTextLimit(6);

        //
        // Active
        //
        gd = new GridData();
        gd.horizontalIndent = 10;
        activeChk = new Button(middleComp, SWT.CHECK);
        activeChk.setText("Active");
        activeChk.setLayoutData(gd);

        //
        // Programming WFO
        //
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label wfoLbl = new Label(middleComp, SWT.RIGHT);
        wfoLbl.setText("Programming WFO:");
        wfoLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        wfoCbo = new Combo(middleComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        wfoCbo.setFont(controlFont);
        wfoCbo.setLayoutData(gd);

        //
        // Coverage Area
        //
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 50;
        Label coverageAreaLbl = new Label(middleComp, SWT.RIGHT);
        coverageAreaLbl.setText("Coverage Area:");
        coverageAreaLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        coverageAreaTF = new Text(middleComp, SWT.BORDER);
        coverageAreaTF.setLayoutData(gd);
        coverageAreaTF.setTextLimit(25);

        //
        // Lat /Lon
        //
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label latLonLbl = new Label(middleComp, SWT.RIGHT);
        latLonLbl.setText("Lat/Lon:");
        latLonLbl.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        latTF = new Text(middleComp, SWT.BORDER);
        latTF.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        lonTF = new Text(middleComp, SWT.BORDER);
        lonTF.setLayoutData(gd);

        //
        // Frequency
        //
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label freqLbl = new Label(middleComp, SWT.RIGHT);
        freqLbl.setText("Frequency(Mhz):");
        freqLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        frequencyTF = new Text(middleComp, SWT.BORDER);
        frequencyTF.setLayoutData(gd);

        //
        // Power
        //
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label powerLbl = new Label(middleComp, SWT.RIGHT);
        powerLbl.setText("Power(Watts):");
        powerLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        powerTF = new Text(middleComp, SWT.BORDER);
        powerTF.setLayoutData(gd);

        //
        // Product Code
        //
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label prodCodeLbl = new Label(middleComp, SWT.RIGHT);
        prodCodeLbl.setText("Product Code:");
        prodCodeLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        productCodeTF = new Text(middleComp, SWT.BORDER);
        productCodeTF.setLayoutData(gd);
        productCodeTF.setTextLimit(3);

        //
        // City
        //
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label cityLbl = new Label(middleComp, SWT.RIGHT);
        cityLbl.setText("City:");
        cityLbl.setLayoutData(gd);

        gd = new GridData(180, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        cityTF = new Text(middleComp, SWT.BORDER);
        cityTF.setLayoutData(gd);
        cityTF.setTextLimit(20);

        //
        // County / State
        //
        gd = new GridData(120, SWT.DEFAULT);
        Button countyStateBtn = new Button(middleComp, SWT.PUSH);
        countyStateBtn.setText("County/State");
        countyStateBtn.setLayoutData(gd);
        countyStateBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                openCountyStateDlg();
            }
        });

        gd = new GridData(220, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        countyStateTF = new Text(middleComp, SWT.BORDER);
        countyStateTF.setLayoutData(gd);
        countyStateTF.setEditable(false);

        //
        // Pseudo County Number
        //
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label pseudoLbl = new Label(middleComp, SWT.RIGHT);
        pseudoLbl.setText("Pseudo County Number:");
        pseudoLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        pseudoCntyNumTF = new Text(middleComp, SWT.BORDER);
        pseudoCntyNumTF.setLayoutData(gd);
        pseudoCntyNumTF.setTextLimit(4);

        // -------------------------------------------------
        // Action buttons
        // -------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 9;
        Composite buttonComp = new Composite(nwrTransGroup, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 120;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        clearBtn = new Button(buttonComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setLayoutData(gd);
        clearBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                newRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });
    }

    /**
     * Create county coverage group and control.
     */
    private void createCountyCoverageGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group countyCoverGroup = new Group(shell, SWT.NONE);
        countyCoverGroup.setLayout(new GridLayout(3, false));
        countyCoverGroup.setLayoutData(gd);
        countyCoverGroup.setText(" County Coverage Information ");

        // -------------------------------------------------------
        // Create counties covered list control
        // -------------------------------------------------------
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite cntyCoveredComp = new Composite(countyCoverGroup, SWT.NONE);
        cntyCoveredComp.setLayout(new GridLayout(1, false));
        cntyCoveredComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        countyCovLbl = new Label(cntyCoveredComp, SWT.CENTER);
        countyCovLbl.setText("Counties covered by Transmitter: ");
        countyCovLbl.setLayoutData(gd);

        gd = new GridData(275, 125);
        gd.horizontalAlignment = SWT.CENTER;
        cntyCovList = new List(cntyCoveredComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        cntyCovList.setLayoutData(gd);
        cntyCovList.setFont(controlFont);

        // -------------------------------------------------------
        // Create action control
        // -------------------------------------------------------
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Composite actionBtnComp = new Composite(countyCoverGroup, SWT.NONE);
        actionBtnComp.setLayout(new GridLayout(1, false));
        actionBtnComp.setLayoutData(gd);

        int buttonWidth = 100;

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        cntyAddBtn = new Button(actionBtnComp, SWT.PUSH);
        cntyAddBtn.setText("<-- Add");
        cntyAddBtn.setLayoutData(gd);
        cntyAddBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveCountyRecord();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        cntyDeleteBtn = new Button(actionBtnComp, SWT.PUSH);
        cntyDeleteBtn.setText("Delete");
        cntyDeleteBtn.setLayoutData(gd);
        cntyDeleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteCountyRecord();
            }
        });

        // -------------------------------------------------------
        // Create available counties control
        // -------------------------------------------------------
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite cntyavailComp = new Composite(countyCoverGroup, SWT.NONE);
        cntyavailComp.setLayout(new GridLayout(1, false));
        cntyavailComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label countyAvailLbl = new Label(cntyavailComp, SWT.CENTER);
        countyAvailLbl.setText("Available Counties");
        countyAvailLbl.setLayoutData(gd);

        gd = new GridData(275, 125);
        gd.horizontalAlignment = SWT.CENTER;
        cntyAvailList = new List(cntyavailComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        cntyAvailList.setLayoutData(gd);
        cntyAvailList.setFont(controlFont);
    }

    /**
     * Create the close button and the bottom of the dialog.
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
     * Get the data list control top label text.
     * 
     * @return Top label text.
     */
    private String getDataListTopLabelText() {
        String format = "%-119S %S %S";
        String labelStr = String.format(format, "Call", "Prod.", "Cnty.");

        return labelStr;
    }

    /**
     * Get the data list control bottom label text.
     * 
     * @return Bottom label text.
     */
    private String getDataListBottomLabelText() {
        String format = "%S   %S %S     %S %S %S                 %S       %S  %S  %S  %S";
        String labelStr = String
                .format(format, "Sign", "WFO", "Transmitter City",
                        "Transmitter County,", "State", "Coverage Area",
                        "Lat  /  Lon", "Freq.", "Power", "Code", "Num");

        return labelStr;
    }

    /**
     * Loads WFO and available counties into the form
     */
    private void loadStaticData() {

        try {
            // WFO combo
            wfoCbo.removeAll();

            AddModifyLocationDataManager man = AddModifyLocationDataManager
                    .getInstance();

            for (String wfo : man.getWFOs()) {
                wfoCbo.add(wfo);
            }
            wfoCbo.select(0);

            // Counties
            cntyAvailList.removeAll();
            String cntFormat = "%-20s %-2s";

            // Get the counties from the DB
            availableCounties = HydroDBDataManager.getInstance().getData(
                    CountiesData.class);

            // update the display with the data
            if (availableCounties.size() > 0) {
                for (CountiesData currCounty : availableCounties) {
                    cntyAvailList.add(String.format(cntFormat,
                            currCounty.getCounty(), currCounty.getState()));
                }

                updateDialogState(DialogState.COUNTIES_AVAILABLE);
            } else {
                updateDialogState(DialogState.NO_COUNTIES);
            }

        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Uable to load static data. ", e);
        }

    }

    /**
     * Retrieves the list of transmitters from the database
     */
    private void getDialogData() {
        try {
            txData = HydroDBDataManager.getInstance().getData(
                    NWRTransmitterData.class);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Uable to load data. ", e);
        }

        updateDisplay();
    }

    /**
     * Populates the main list with the list of transmitters from the database
     */
    private void updateDisplay() {
        // Clear the display
        mainDataList.removeAll();
        clearInformation();

        String format = "%-6.6S %3.3S %-20.20S %-20.20S %2.2S   %-25.25S %9.9S %9.9S  %7.7S  %4.4S  %3.3S   %3.3S  %S";

        if (txData.size() > 0) {
            for (NWRTransmitterData currTx : txData) {
                mainDataList.add(String.format(format, currTx.getCallSign(),
                        currTx.getWfo(), currTx.getCity(), currTx.getCounty(),
                        currTx.getState(), currTx.getCoverageArea(),
                        HydroDataUtils.getLatLonDisplayString(currTx
                                .getLatitude()), HydroDataUtils
                                .getLatLonDisplayString(currTx.getLongitude()),
                        HydroDataUtils.getDisplayString("%s", "%7.3f",
                                currTx.getTransmitFrequency()), HydroDataUtils
                                .getDisplayString(currTx.getTransmitPower()),
                        currTx.getTransmitProductCode(), currTx
                                .getTransmitCountyNumber(), currTx
                                .getUseTransmitter()));
            }

            updateDialogState(DialogState.DATA_AVAILABLE);
        } else {
            updateDialogState(DialogState.NO_DATA);
        }
    }

    /**
     * Clears the form on the dialog
     */
    private void clearInformation() {
        mainDataList.deselectAll();

        callSignTF.setText("");
        activeChk.setSelection(false);
        wfoCbo.select(0);
        coverageAreaTF.setText("");
        latTF.setText("");
        lonTF.setText("");
        frequencyTF.setText("");
        powerTF.setText("");
        productCodeTF.setText("");
        cityTF.setText("");
        countyStateTF.setText("");
        pseudoCntyNumTF.setText("");

        cntyCovList.removeAll();
        txCountyState = null;
    }

    /**
     * Sets the dialog up for entry of a new transmitter
     */
    private void newRecord() {
        clearInformation();
        updateDialogState(DialogState.NEW_RECORD);
    }

    /**
     * Saves the transmitter to the database.
     * 
     * @return TRUE if the save was successful, else FALSE
     */
    private boolean saveRecord() {
        boolean successful = false;

        if (callSignTF.getText().equals("")) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Unable to Save");
            mb.setMessage("Call Sign is required.");
            mb.open();
        } else if (countyStateTF.getText().equals("")) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Unable to Save");
            mb.setMessage("State is required.");
            mb.open();
        } else {
            NWRTransmitterData newData = new NWRTransmitterData();

            Double dTmp = null;
            Integer iTmp = null;

            // Call Sign
            newData.setCallSign(callSignTF.getText());

            // WFO
            newData.setWfo(wfoCbo.getItem(wfoCbo.getSelectionIndex()));

            // City
            newData.setCity(cityTF.getText());

            // County/State
            newData.setCounty(txCountyState.getCounty());
            newData.setState(txCountyState.getState());

            // Coverage Area
            newData.setCoverageArea(coverageAreaTF.getText());

            // Latitude
            dTmp = HydroDataUtils.getLatitudeFromTF(shell, latTF);
            if (dTmp == null) {
                return successful;
            }
            newData.setLatitude(dTmp);

            // Latitude
            dTmp = HydroDataUtils.getLongitudeFromTF(shell, lonTF);
            if (dTmp == null) {
                return successful;
            }
            newData.setLongitude(dTmp);

            // Tx Freq
            dTmp = HydroDataUtils.getDoubleFromTF(shell, frequencyTF,
                    "Frequency");
            if (dTmp == null) {
                return successful;
            }
            newData.setTransmitFrequency(dTmp);

            // Tx Power
            iTmp = HydroDataUtils.getIntegerFromTF(shell, powerTF, "Power");
            if (iTmp == null) {
                return successful;
            }
            newData.setTransmitPower(iTmp);

            // Tx Product Code
            newData.setTransmitProductCode(productCodeTF.getText());

            // Tx County Num
            newData.setTransmitCountyNumber(pseudoCntyNumTF.getText());

            // Use Transmitter
            newData.setUseTransmitter(activeChk.getSelection() ? "T" : "F");

            try {
                HydroDBDataManager.getInstance().putData(newData);

                // Refresh Tx cache
                getDialogData();

                successful = true;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Uable to save city data. ", e);
            }
        }

        return successful;
    }

    /**
     * Saves the county to the database as assigned to the currently selected
     * transmitter
     */
    private void saveCountyRecord() {
        NWRTransmitterData currData = getSelectedRecord();
        CountiesData currCounty = getSelectedCounty();

        if (currData != null && currCounty != null) {
            CountyTransmitData newData = new CountyTransmitData();

            newData.setCallSign(currData.getCallSign());
            newData.setCounty(currCounty.getCounty());
            newData.setState(currCounty.getState());

            try {
                HydroDBDataManager.getInstance().putData(newData);

                // Update county cache
                getSelectedCountiesData();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Uable to save county data. ", e);
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a transmitter and a county");
            mb.open();
        }
    }

    /**
     * Prompts the user and after confirmation, deletes the transmitter
     */
    private void deleteRecord() {
        NWRTransmitterData currData = getSelectedRecord();

        if (currData != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage(String.format(
                    "Do you wish to delete Transmitter %s?",
                    currData.getCallSign()));

            int result = mb.open();

            if (result == SWT.OK) {
                // Need to check if there are any counties covered by the
                // Transmitter
                if (selectedCounties.size() > 0) {
                    mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Unable to Delete");
                    mb.setMessage(String
                            .format("Can not delete NWR Transmitter %s.\n"
                                    + "There is at least one county associated with it.",
                                    currData.getCallSign()));
                    mb.open();
                } else {
                    try {
                        HydroDBDataManager.getInstance().deleteRecord(currData);

                        // Refresh the cache
                        getDialogData();
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Uable to delete city data. ", e);
                    }
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a transmitter first.");
            mb.open();
        }
    }

    /**
     * Prompts the user and after confirmation, deletes the county from the
     * transmitter
     */
    private void deleteCountyRecord() {
        CountyTransmitData currData = getSelectedCountyToDelete();

        if (currData != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage(String.format(
                    "Do you wish to delete this County from Transmitter %s",
                    currData.getCallSign()));

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    HydroDBDataManager.getInstance().deleteRecord(currData);

                    // Refresh the cache
                    getSelectedCountiesData();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Uable to delete county data. ", e);
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a county first.");
            mb.open();
        }
    }

    /**
     * Populates the form with the selected transmitter's information
     */
    private void updateInformation() {
        NWRTransmitterData currData = getSelectedRecord();

        if (currData != null) {
            callSignTF.setText(currData.getCallSign());
            activeChk.setSelection(currData.getUseTransmitter().equals("T"));

            // WFO
            String wfo = currData.getWfo();
            for (int i = 0; i < wfoCbo.getItemCount(); i++) {
                if (wfoCbo.getItem(i).equals(wfo)) {
                    wfoCbo.select(i);
                    break;
                }
            }

            coverageAreaTF.setText(currData.getCoverageArea());
            latTF.setText(HydroDataUtils.getLatLonDisplayString(currData
                    .getLatitude()));
            lonTF.setText(HydroDataUtils.getLatLonDisplayString(currData
                    .getLongitude()));
            frequencyTF.setText(HydroDataUtils.getDisplayString(currData
                    .getTransmitFrequency()));
            powerTF.setText(HydroDataUtils.getDisplayString(currData
                    .getTransmitPower()));
            productCodeTF.setText(currData.getTransmitProductCode());
            cityTF.setText(currData.getCity());

            // County, State
            countyStateTF.setText(currData.getCounty() + ","
                    + currData.getState());
            txCountyState = new CountiesData();
            txCountyState.setCounty(currData.getCounty());
            txCountyState.setState(currData.getState());

            pseudoCntyNumTF.setText(currData.getTransmitCountyNumber());

            getSelectedCountiesData();
        }
    }

    /**
     * Retrieves the counties assigned to the selected transmitter from the
     * database and populates the selected counties list
     */
    private void getSelectedCountiesData() {
        NWRTransmitterData currData = getSelectedRecord();
        cntyCovList.removeAll();

        if (currData != null) {
            // Load Counties covered
            CountyTransmitData seedData = new CountyTransmitData();
            seedData.setCallSign(currData.getCallSign());

            try {
                selectedCounties = HydroDBDataManager.getInstance().getData(
                        seedData);

                if (selectedCounties.size() > 0) {
                    for (CountyTransmitData currCounty : selectedCounties) {
                        cntyCovList.add(String.format("%-20s %-2s",
                                currCounty.getCounty(), currCounty.getState()));
                    }

                    updateDialogState(DialogState.COUNTIES_SELECTED);
                } else {
                    updateDialogState(DialogState.NO_COUNTIES_SELECTED);
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Uable to load county data. ", e);
            }
        }
    }

    /**
     * @return The selected Transmitter or NULL if one is not selected.
     */
    private NWRTransmitterData getSelectedRecord() {
        NWRTransmitterData currTx = null;

        if (mainDataList.getSelectionCount() > 0) {
            currTx = txData.get(mainDataList.getSelectionIndex());
        }

        return currTx;
    }

    /**
     * @return The selected county assigned to the transmitter or NULL if one is
     *         not selected.
     */
    private CountyTransmitData getSelectedCountyToDelete() {
        CountyTransmitData currCounty = null;

        if (cntyCovList.getSelectionCount() > 0) {
            currCounty = selectedCounties.get(cntyCovList.getSelectionIndex());
        }

        return currCounty;
    }

    /**
     * Returns the selected Available County
     * 
     * @return The selected available county or NULL if one is not selected
     */
    private CountiesData getSelectedCounty() {
        CountiesData currCounty = null;

        if (cntyAvailList.getSelectionCount() > 0) {
            currCounty = availableCounties.get(cntyAvailList
                    .getSelectionIndex());
        }

        return currCounty;
    }

    /**
     * Updates the dialog to reflect the current state.
     * 
     * @param currState
     *            The current state of the dialog
     */
    private void updateDialogState(DialogState currState) {
        switch (currState) {
        case NO_DATA:
            clearBtn.setEnabled(false);
            deleteBtn.setEnabled(false);
            applyBtn.setEnabled(false);
            break;
        case DATA_AVAILABLE:
            clearBtn.setEnabled(true);
            deleteBtn.setEnabled(true);
            applyBtn.setEnabled(true);
            break;
        case NO_COUNTIES:
            cntyAddBtn.setEnabled(false);
            break;
        case COUNTIES_AVAILABLE:
            cntyAddBtn.setEnabled(true);
            break;
        case NO_COUNTIES_SELECTED:
            cntyDeleteBtn.setEnabled(false);
            break;
        case COUNTIES_SELECTED:
            cntyDeleteBtn.setEnabled(true);
            break;
        case NEW_RECORD:
            applyBtn.setEnabled(true);
            break;
        default:
            break;
        }

    }

    /**
     * Opens the County/State selection dialog
     */
    private void openCountyStateDlg() {
        if (countyStateDlg == null || countyStateDlg.isDisposed()) {
            countyStateDlg = new CountyStateDlg(shell);
            countyStateDlg.addListener(this);
            countyStateDlg.open();
        } else {
            countyStateDlg.bringToTop();
        }

        // Set the selection county/state dlg
        if (!countyStateTF.getText().equals("")) {
            // Set selection in dialog to current location's settings
            CountiesData currCountyState = new CountiesData();

            String[] countyState = countyStateTF.getText().split(",");
            currCountyState.setCounty(countyState[0].trim());
            currCountyState.setState(countyState[1].trim());

            // Tell county/state dialog what to select
            countyStateDlg.setSelection(currCountyState);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.hydrobase.listeners.ICountyStateListener#notifyUpdate
     * (com.raytheon.viz.hydrocommon.data.CountiesData)
     */
    @Override
    public void notifyUpdate(CountiesData selectedCountyState) {
        if (selectedCountyState.getCounty().startsWith("XXXXX")) {
            txCountyState = null;
        } else {
            txCountyState = selectedCountyState;
        }
        countyStateTF.setText(selectedCountyState.getCounty() + ", "
                + selectedCountyState.getState());
    }
}
