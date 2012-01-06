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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.DamTypeData;
import com.raytheon.viz.hydrocommon.data.ReservoirData;
import com.raytheon.viz.hydrocommon.data.ReservoirOwnerData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.hydrocommon.util.StnClassSyncUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Reservoir dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 10, 2008				lvenable	Initial creation.
 * 12/21/2008   1782        grichard    Connected to IHFS DB.
 * 05/05/2009         mpduff      Added form validation.
 * 11/03/2011	11440       lbousaidi	removed form validation and only kept one 
 * 										validation check for date to match AWIPSI.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ReservoirDlg extends CaveSWTDialog implements IHydroDialog {

    /**
     * Name text control.
     */
    private Text nameTF;

    /**
     * Impound date text control.
     */
    private Text impoundTF;

    /**
     * Flag indicating if validation is taking place.
     */
    private boolean validationInProgress = false;

    /**
     * Gates text control.
     */
    private Text gatesTF;

    /**
     * Type combo box.
     */
    private Combo typeCbo;

    /**
     * Owner combo box.
     */
    private Combo ownerCbo;

    /**
     * State text control.
     */
    private Text stateTF;

    /**
     * Id text control.
     */
    private Text idTF;

    /**
     * Flood control check box.
     */
    private Button floodControlChk;

    /**
     * Hydro electric check box.
     */
    private Button hydroElecChk;

    /**
     * Low flow augmentation check box.
     */
    private Button lowFlowAugChk;

    /**
     * Navigation check box.
     */
    private Button navigationChk;

    /**
     * Recreation check box.
     */
    private Button recreationChk;

    /**
     * Water supply check box.
     */
    private Button waterSupplyChk;

    /**
     * Maximum surcharge text control.
     */
    private Text maxSurchargeTF;

    /**
     * Top text control.
     */
    private Text topTF;

    /**
     * Sill text control.
     */
    private Text sillTF;

    /**
     * Reservoir text control.
     */
    private Text reservoirTF;

    /**
     * Flood text control.
     */
    private Text floodTF;

    /**
     * Spillway text control.
     */
    private Text spillwayTF;

    /**
     * Conservation text control.
     */
    private Text conservationTF;

    /**
     * Dead text control.
     */
    private Text deadTF;

    /**
     * Okay button.
     */
    private Button okBtn;

    /**
     * Cancel button.
     */
    private Button cancelBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * States of dialog.
     */
    private enum DialogStates {
        DATA_AVAILABLE, NO_DATA_AVAILABLE
    }

    /**
     * Dialog state.
     */
    private DialogStates dialogState;

    /**
     * Dam Type Data.
     */
    private ArrayList<DamTypeData> damTypesData;

    /**
     * Reservoir Owner Data.
     */
    private ArrayList<ReservoirOwnerData> resOwnerData;

    /**
     * Reservoir Data.
     */
    private ArrayList<ReservoirData> resData;

    /**
     * Location Identifier.
     */
    private String lid;

    /**
     * Date format.
     */
    private SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public ReservoirDlg(Shell parent, String titleInfo, String lid) {
        super(parent);
        setText("Reservoir" + titleInfo);

        this.lid = lid;
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
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
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        // Initialize all of the controls and layouts
        createInformationGroup();

        createElevationPoolGroups();

        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();

        // Populate combos
        populateCombos();

        // Get the dialog data
        getDialogData();
    }

    /**
     * Create the information group and controls.
     */
    private void createInformationGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group infoGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 45;
        infoGroup.setLayout(gl);
        infoGroup.setLayoutData(gd);
        infoGroup.setText(" Information ");

        // ----------------------------------------
        // Create the controls on the left side
        // of the Information group.
        // ----------------------------------------
        Composite leftComp = new Composite(infoGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        leftComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        leftComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label nameLbl = new Label(leftComp, SWT.RIGHT);
        nameLbl.setText("Name: ");
        nameLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        nameTF = new Text(leftComp, SWT.BORDER);
        nameTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label impoundLbl = new Label(leftComp, SWT.RIGHT);
        impoundLbl.setText("Impound Date: ");
        impoundLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        impoundTF = new Text(leftComp, SWT.BORDER);
        impoundTF.setLayoutData(gd);
        /**
         * Add listeners for impound date text control.
         */
        impoundTF.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent ke) {
                if ((ke.keyCode == SWT.KEYPAD_CR) || (ke.keyCode == SWT.CR)) {
                    validationInProgress = true;
                    validateEntryData(impoundTF);
                    validationInProgress = false;
                }
            }

            public void keyReleased(KeyEvent ke) {
                // Do nothing...
            }
        });

        impoundTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent event) {
                // Check if the text field is being validated
                // by the 'Enter' key.
                if (validationInProgress == true) {
                    return;
                }
                validateEntryData(impoundTF);
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label gatesLbl = new Label(leftComp, SWT.RIGHT);
        gatesLbl.setText("Gates: ");
        gatesLbl.setLayoutData(gd);

        gd = new GridData(40, SWT.DEFAULT);
        gatesTF = new Text(leftComp, SWT.BORDER);
        gatesTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label typeLbl = new Label(leftComp, SWT.RIGHT);
        typeLbl.setText("Type: ");
        typeLbl.setLayoutData(gd);

        typeCbo = new Combo(leftComp, SWT.DROP_DOWN | SWT.READ_ONLY);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label ownerLbl = new Label(leftComp, SWT.RIGHT);
        ownerLbl.setText("Owner: ");
        ownerLbl.setLayoutData(gd);

        ownerCbo = new Combo(leftComp, SWT.DROP_DOWN | SWT.READ_ONLY);

        // ----------------------------------------
        // Create the controls on the right side
        // of the Information group.
        // ----------------------------------------
        Composite rightComp = new Composite(infoGroup, SWT.NONE);
        gl = new GridLayout(4, false);
        gl.verticalSpacing = 8;
        rightComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        rightComp.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 4;
        Label inventoryLbl = new Label(rightComp, SWT.NONE);
        inventoryLbl.setText("National Inventory of Dams");
        inventoryLbl.setLayoutData(gd);

        Label stateLbl = new Label(rightComp, SWT.NONE);
        stateLbl.setText("State: ");

        gd = new GridData(35, SWT.DEFAULT);
        stateTF = new Text(rightComp, SWT.BORDER);
        stateTF.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        Label idLbl = new Label(rightComp, SWT.RIGHT);
        idLbl.setText("Id: ");
        idLbl.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        idTF = new Text(rightComp, SWT.BORDER);
        idTF.setLayoutData(gd);

        Label usesLbl = new Label(rightComp, SWT.NONE);
        usesLbl.setText("Uses: ");

        gd = new GridData();
        gd.horizontalSpan = 3;
        floodControlChk = new Button(rightComp, SWT.CHECK);
        floodControlChk.setText("Flood Control");
        floodControlChk.setLayoutData(gd);

        // filler label
        new Label(rightComp, SWT.NONE);

        gd = new GridData();
        gd.horizontalSpan = 3;
        hydroElecChk = new Button(rightComp, SWT.CHECK);
        hydroElecChk.setText("Hydroelectric");
        hydroElecChk.setLayoutData(gd);

        // filler label
        new Label(rightComp, SWT.NONE);

        gd = new GridData();
        gd.horizontalSpan = 3;
        lowFlowAugChk = new Button(rightComp, SWT.CHECK);
        lowFlowAugChk.setText("Low Flow Augmentation");
        lowFlowAugChk.setLayoutData(gd);

        // filler label
        new Label(rightComp, SWT.NONE);

        gd = new GridData();
        gd.horizontalSpan = 3;
        navigationChk = new Button(rightComp, SWT.CHECK);
        navigationChk.setText("Navigation");
        navigationChk.setLayoutData(gd);

        // filler label
        new Label(rightComp, SWT.NONE);

        gd = new GridData();
        gd.horizontalSpan = 3;
        recreationChk = new Button(rightComp, SWT.CHECK);
        recreationChk.setText("Recreation");
        recreationChk.setLayoutData(gd);

        // filler label
        new Label(rightComp, SWT.NONE);

        gd = new GridData();
        gd.horizontalSpan = 3;
        waterSupplyChk = new Button(rightComp, SWT.CHECK);
        waterSupplyChk.setText("Water Supply");
        waterSupplyChk.setLayoutData(gd);
    }

    /**
     * Create the elevations and pools group and controls.
     */
    private void createElevationPoolGroups() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite elevPoolComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        elevPoolComp.setLayout(gl);
        elevPoolComp.setLayoutData(gd);

        int textControlWidth = 100;

        // --------------------------------------------
        // Create the Elevation group and controls
        // --------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group elevGroup = new Group(elevPoolComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        elevGroup.setLayout(gl);
        elevGroup.setLayoutData(gd);
        elevGroup.setText(" Elevations ");

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label maxSurLbl = new Label(elevGroup, SWT.RIGHT);
        maxSurLbl.setText("Max Surcharge: ");
        maxSurLbl.setLayoutData(gd);

        gd = new GridData(textControlWidth, SWT.DEFAULT);
        maxSurchargeTF = new Text(elevGroup, SWT.BORDER);
        maxSurchargeTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label topLbl = new Label(elevGroup, SWT.RIGHT);
        topLbl.setText("Top: ");
        topLbl.setLayoutData(gd);

        gd = new GridData(textControlWidth, SWT.DEFAULT);
        topTF = new Text(elevGroup, SWT.BORDER);
        topTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label sillLbl = new Label(elevGroup, SWT.RIGHT);
        sillLbl.setText("Sill: ");
        sillLbl.setLayoutData(gd);

        gd = new GridData(textControlWidth, SWT.DEFAULT);
        sillTF = new Text(elevGroup, SWT.BORDER);
        sillTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label reservoirLbl = new Label(elevGroup, SWT.RIGHT);
        reservoirLbl.setText("Reservoir: ");
        reservoirLbl.setLayoutData(gd);

        gd = new GridData(textControlWidth, SWT.DEFAULT);
        reservoirTF = new Text(elevGroup, SWT.BORDER);
        reservoirTF.setLayoutData(gd);

        // --------------------------------------------
        // Create the Pool group and controls
        // --------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group poolGroup = new Group(elevPoolComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        poolGroup.setLayout(gl);
        poolGroup.setLayoutData(gd);
        poolGroup.setText(" Pools ");

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label floodLbl = new Label(poolGroup, SWT.RIGHT);
        floodLbl.setText("Flood: ");
        floodLbl.setLayoutData(gd);

        gd = new GridData(textControlWidth, SWT.DEFAULT);
        floodTF = new Text(poolGroup, SWT.BORDER);
        floodTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label spillwayLbl = new Label(poolGroup, SWT.RIGHT);
        spillwayLbl.setText("Spillway: ");
        spillwayLbl.setLayoutData(gd);

        gd = new GridData(textControlWidth, SWT.DEFAULT);
        spillwayTF = new Text(poolGroup, SWT.BORDER);
        spillwayTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label conservationLbl = new Label(poolGroup, SWT.RIGHT);
        conservationLbl.setText("Conservation: ");
        conservationLbl.setLayoutData(gd);

        gd = new GridData(textControlWidth, SWT.DEFAULT);
        conservationTF = new Text(poolGroup, SWT.BORDER);
        conservationTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label deadLbl = new Label(poolGroup, SWT.RIGHT);
        deadLbl.setText("Dead: ");
        deadLbl.setLayoutData(gd);

        gd = new GridData(textControlWidth, SWT.DEFAULT);
        deadTF = new Text(poolGroup, SWT.BORDER);
        deadTF.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (validateDateForm()) {
                    if (saveRecord()) {
                        shell.dispose();
                    }
                }
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });
    }

    /**
     * Populate the dam type and reservoir owner combo controls with data from
     * the database.
     */
    private void populateCombos() {

        // Get the Dam Type Data
        DamTypeData seedData1 = new DamTypeData();
        seedData1.setType("Unk");

        if (damTypesData != null) {
            damTypesData.clear();
        }
        try {
            damTypesData = HydroDBDataManager.getInstance().getData(seedData1);
        } catch (VizException e) {
            // e.printStackTrace();
        }

        // Get the Reservoir Owner Data
        ReservoirOwnerData seedData2 = new ReservoirOwnerData();
        seedData2.setOwner("Unk");

        if (resOwnerData != null) {
            resOwnerData.clear();
        }
        try {
            resOwnerData = HydroDBDataManager.getInstance().getData(seedData2);
        } catch (VizException e) {
            // e.printStackTrace();
        }

        // Type combo
        for (DamTypeData data : damTypesData) {
            typeCbo.add(data.getType());
        }
        typeCbo.select(typeCbo.getItemCount() - 1);
        // Owner combo
        for (ReservoirOwnerData data : resOwnerData) {
            ownerCbo.add(data.getOwner());
        }
        ownerCbo.select(ownerCbo.getItemCount() - 1);

    }

    /**
     * Get the reservoir data from the database that matched the location ID.
     */
    @Override
    public void getDialogData() {

        // Get the Reservoir Data
        ReservoirData seedData = new ReservoirData();
        seedData.setLid(lid);

        if (resData != null) {
            resData.clear();
        }
        try {
            resData = HydroDBDataManager.getInstance().getData(seedData);
        } catch (VizException e) {
            // e.printStackTrace();
        }

        updateDialogDisplay();
    }

    /**
     * Update the reservoir data.
     */
    @Override
    public void updateDialogDisplay() {

        for (ReservoirData currData : resData) {

            nameTF.setText((currData.getName() != null) ? currData.getName()
                    : "");
            if (currData.getType() != null) {
                typeCbo.select(typeCbo.indexOf(currData.getType()));
            } else {
                typeCbo.select(typeCbo.indexOf("Unk"));
            }
            if (currData.getOwner() != null) {
                ownerCbo.select(ownerCbo.indexOf(currData.getOwner()));
            } else {
                ownerCbo.select(ownerCbo.indexOf("Unk"));
            }
            deadTF.setText(HydroDataUtils.getDisplayString(currData
                    .getDeadpool()));
            conservationTF.setText(HydroDataUtils.getDisplayString(currData
                    .getConserpool()));
            floodTF.setText(HydroDataUtils.getDisplayString(currData
                    .getFloodpool()));
            spillwayTF.setText(HydroDataUtils.getDisplayString(currData
                    .getSpillway()));
            sillTF.setText(HydroDataUtils.getDisplayString(currData.getSill()));
            reservoirTF.setText(HydroDataUtils.getDisplayString(currData
                    .getElev()));
            topTF.setText(HydroDataUtils.getDisplayString(currData.getTop()));
            maxSurchargeTF.setText(HydroDataUtils.getDisplayString(currData
                    .getSurchg()));
            gatesTF.setText(HydroDataUtils
                    .getDisplayString(currData.getGates()));
            impoundTF.setText((currData.getImpounded() != null) ? dateFormat
                    .format(currData.getImpounded()) : "");
            floodControlChk
                    .setSelection((currData.getUses() != null) ? currData
                            .getUses().contains("F") : false);
            hydroElecChk.setSelection((currData.getUses() != null) ? currData
                    .getUses().contains("H") : false);
            lowFlowAugChk.setSelection((currData.getUses() != null) ? currData
                    .getUses().contains("L") : false);
            navigationChk.setSelection((currData.getUses() != null) ? currData
                    .getUses().contains("N") : false);
            recreationChk.setSelection((currData.getUses() != null) ? currData
                    .getUses().contains("R") : false);
            waterSupplyChk.setSelection((currData.getUses() != null) ? currData
                    .getUses().contains("W") : false);
            stateTF.setText((currData.getDamids() != null) ? currData
                    .getDamids() : "");
            idTF.setText((currData.getDamidn() != null) ? currData.getDamidn()
                    : "");

            break; // we just want the first element!

        }

        if (resData.size() > 0) {
            dialogState = DialogStates.DATA_AVAILABLE;
        } else {
            dialogState = DialogStates.NO_DATA_AVAILABLE;
        }
        updateDialogState();
    }

    /**
     * Update the dialog's state.
     */
    @Override
    public void updateDialogState() {
        switch (dialogState) {
        case DATA_AVAILABLE:
            okBtn.setEnabled(true);
            cancelBtn.setEnabled(true);
            deleteBtn.setEnabled(true);
            break;
        case NO_DATA_AVAILABLE:
            okBtn.setEnabled(true);
            cancelBtn.setEnabled(true);
            deleteBtn.setEnabled(false);
            break;
        default:
            break;
        }
    }

    @Override
    public void updateInformation() {
    }

    @Override
    public ReservoirData getSelectedDatum() {
        return resData.get(0);
    }

    /**
     * Validate the user input.
     * 
     * @param input
     *            Input string.
     * @return True if the input is good, false otherwise.
     */
    private boolean checkInput(String input) {
        return input.matches("((((0[1-9])|(1[0-2]))" + "/"
                + "((0[1-9])|([1-2][0-9])|(3[0-1]))" + "/"
                + "([1-2]\\d{3}))|())");
    }

    /**
     * Display an information box with a message.
     * 
     * @param information
     *            Message string.
     */
    public void userInformation(String information) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setText("Invalid Date");
        mb.setMessage(information);
        mb.open();
    }

    /**
     * Validate the user inputs in the value text control.
     */
    @Override
    public boolean validateEntryData(Text tf) {
        if (checkInput(tf.getText()) == false) {
            userInformation("Please enter a valid date in the form: MM/DD/YYYY");
            tf.setFocus();
            tf.selectAll();
        }
        return true;
    }

    /**
     * Verify that the date is filled out correctly before submission.
     * 
     * @return true if form is correctly filled out
     */
    private boolean validateDateForm() {
        boolean isValid = true;      

       if (impoundTF.getText().equals("")) {       
        	 isValid = false;
        }

        if (!isValid) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Date");
            mb
                    .setMessage("Please enter a valid date in the form:MM/DD/YYYY");
            
            mb.open();
        }

        return isValid;
    }

    /**
     * Save the record to the database.
     */
    @Override
    public boolean saveRecord() {

        // Data to save.
        ReservoirData dataToSave = new ReservoirData();

        dataToSave.setLid(lid);
        dataToSave.setName(nameTF.getText());
        dataToSave.setType(typeCbo.getText());
        dataToSave.setOwner(ownerCbo.getText());
        if (!deadTF.getText().equals("")) {
        	dataToSave.setDeadpool(Double.parseDouble(deadTF.getText()));
        }
        if (!conservationTF.getText().equals("")){
        	dataToSave.setConserpool(Double.parseDouble(conservationTF.getText()));
        }
        if (!floodTF.getText().equals("")){
        	dataToSave.setFloodpool(Double.parseDouble(floodTF.getText()));
        }
        if (!spillwayTF.getText().equals("")) {
        	dataToSave.setSpillway(Double.parseDouble(spillwayTF.getText()));
        }
        if (!sillTF.getText().equals("")) {
        	dataToSave.setSill(Double.parseDouble(sillTF.getText()));
        }
        if (!reservoirTF.getText().equals("")) {
        	dataToSave.setElev(Double.parseDouble(reservoirTF.getText()));
        }
        if (!topTF.getText().equals("")) {
        	dataToSave.setTop(Double.parseDouble(topTF.getText()));
        }
        if (!maxSurchargeTF.getText().equals("")) {
        	dataToSave.setSurchg(Double.parseDouble(maxSurchargeTF.getText()));
        }
        if (!gatesTF.getText().equals("")) {
        	dataToSave.setGates(Integer.parseInt(gatesTF.getText()));
        }
        if (!impoundTF.getText().equals("")) {
            try {
                dataToSave.setImpounded(dateFormat.parse(impoundTF.getText()));

            } catch (ParseException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Date");
                mb
                        .setMessage("Please enter a valid date in the form: MM/DD/YYYY");
                mb.open();
                return false;
            }
        }
        StringBuilder usesMask = new StringBuilder();
        usesMask.append(floodControlChk.getSelection() ? "F" : "");
        usesMask.append(hydroElecChk.getSelection() ? "H" : "");
        usesMask.append(lowFlowAugChk.getSelection() ? "L" : "");
        usesMask.append(navigationChk.getSelection() ? "N" : "");
        usesMask.append(recreationChk.getSelection() ? "R" : "");
        usesMask.append(waterSupplyChk.getSelection() ? "W" : "");
        dataToSave.setUses(usesMask.toString().equals("") ? null : usesMask
                .toString());

        dataToSave.setDamids(stateTF.getText().equals("") ? null : stateTF
                .getText());
        dataToSave.setDamidn(idTF.getText().equals("") ? null : idTF.getText());

        // Save to DB
        try {
            HydroDBDataManager.getInstance().putData(dataToSave);

            // Synchronize StnClass table
            StnClassSyncUtil.setStnClass(lid);
        } catch (VizException e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Unable to Save");
            mb.setMessage("An error occurred while trying to save.");

            String cause = e.getCause().getMessage();

            int causeStart = cause.indexOf("ERROR:");

            // If the exception contain the SQL exception "ERROR:"
            if (causeStart > 0) {
                int causeEnd = cause.indexOf("\n", causeStart);

                cause = cause.substring(causeStart, causeEnd);

                if (cause.contains("datum_rvr_fk")) {
                    mb.setMessage("Please enter data for " + lid
                            + " in the River Gauge dialog first");
                }
            }

            mb.open();

            e.printStackTrace();
            return false;
        }

        // Refresh the data
        getDialogData();
        return true;
    }

    /**
     * Delete the record from the database.
     */
    @Override
    public void deleteRecord() {
        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Delete Confirmation");
        mb.setMessage("Do you wish to delete this entry?");

        int result = mb.open();

        if (result == SWT.OK) {
            try {
                try {
                    ReservoirData selectedDatum = getSelectedDatum();
                    HydroDBDataManager.getInstance()
                            .deleteRecord(selectedDatum);

                    // Synchronize StnClass table
                    StnClassSyncUtil.setStnClass(selectedDatum.getLid());
                } catch (ArrayIndexOutOfBoundsException e) {
                    MessageBox mbDel = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mbDel.setText("Unable to Delete");
                    mbDel
                            .setMessage("No item is selected in the reservoir list");
                    mbDel.open();
                }

                clearForm();

            } catch (VizException e) {
                MessageBox mbDel = new MessageBox(shell, SWT.ICON_ERROR
                        | SWT.OK);
                mbDel.setText("Unable to Delete");
                mbDel.setMessage("An error occurred while trying to delete.");
                mbDel.open();

                e.printStackTrace();
            }
        }

        // Refresh the data
        getDialogData();
    }

    /**
     * Check for an empty or null string.
     * 
     * @param value
     *            The String to check
     * @return True if value is valid string
     */
    private boolean checkString(String value) {
        boolean isValid = true;

        if ((value == null) || value.trim().equals("")) {
            isValid = false;
        }

        return isValid;
    }

    /**
     * Clear the text fields in the form.
     */
    @Override
    public void clearForm() {
        nameTF.setText("");

        typeCbo.select(typeCbo.indexOf("Unk"));

        ownerCbo.select(ownerCbo.indexOf("Unk"));

        deadTF.setText("");
        conservationTF.setText("");
        floodTF.setText("");
        spillwayTF.setText("");
        sillTF.setText("");
        reservoirTF.setText("");
        topTF.setText("");
        maxSurchargeTF.setText("");
        gatesTF.setText("");
        impoundTF.setText("");
        floodControlChk.setSelection(false);
        hydroElecChk.setSelection(false);
        lowFlowAugChk.setSelection(false);
        navigationChk.setSelection(false);
        recreationChk.setSelection(false);
        waterSupplyChk.setSelection(false);
        stateTF.setText("");
        idTF.setText("");
    }
}
