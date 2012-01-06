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

import java.util.ArrayList;

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
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.ReferencesData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the References dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 8, 2008				lvenable	Initial creation.
 * 12/19/2008   1782        grichard    Connected to IHFS DB.
 * Nov 03 2011	11273		lbousaidi   make changes to update an existing entry
 * 										without creating new entry 	
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ReferencesDlg extends CaveSWTDialog implements IHydroDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Data list control.
     */
    private List referenceList;

    /**
     * Data text control.
     */
    private Text referenceTF;

    /**
     * OK button (apply changes and exit).
     */
    private Button okBtn;

    /**
     * Apply changes button.
     */
    private Button applyBtn;

    /**
     * Delete record button.
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
     * Array of reference data.
     */
    private ArrayList<ReferencesData> referenceData;

    /**
     * Location ID.
     */
    private String lid;
    
    private boolean newInsert=false;
    
    

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public ReferencesDlg(Shell parent, String titleInfo, String lid) {
        super(parent);
        setText("References" + titleInfo);

        this.lid = lid;
        // TODO Remove/comment-out this test code statement:
        // this.lid = "ABSM8";
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

        // Initialize all of the controls and layouts
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createListControls();

        createInformationGroup();

        createBottomButtons();

        // Get the dialog data
        getDialogData();
    }

    /**
     * Create the main controls on the dialog.
     */
    private void createListControls() {
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(1, false));

        Label referenceLbl = new Label(controlComp, SWT.NONE);
        referenceLbl.setText("Reference");

        // --------------------------------------
        // Create the data list control
        // --------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 700;
        gd.heightHint = 150;
        referenceList = new List(controlComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        referenceList.setLayoutData(gd);
        referenceList.setFont(controlFont);
        referenceList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateInformation();

                if (referenceList.getSelectionIndex() >= 0) {
                    deleteBtn.setEnabled(true);
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        referenceTF = new Text(controlComp, SWT.BORDER);
        referenceTF.setFont(controlFont);
        referenceTF.setLayoutData(gd);
    }

    /**
     * Create the Information group and controls.
     */
    private void createInformationGroup() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(5, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 90;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setEnabled(false);
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (saveRecord()) {
                    shell.dispose();
                }
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setEnabled(false);
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
                getDialogData();
                referenceList.setSelection(0);
                updateInformation();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            	newInsert=true;
                clearForm();
                okBtn.setEnabled(true);
                applyBtn.setEnabled(true);
                deleteBtn.setEnabled(false);
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setEnabled(false);
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });
    }

    /**
     * Get the reference data from the database that matched the location ID.
     */
    @Override
    public void getDialogData() {
        ReferencesData seedData = new ReferencesData();
        seedData.setLid(lid);

        if (referenceData != null) {
            referenceData.clear();
        }
        try {
            referenceData = HydroDBDataManager.getInstance().getData(seedData);
        } catch (VizException e) {
            // e.printStackTrace();
        }

        updateDialogDisplay();
    }

    /**
     * Update the reference list control.
     */
    @Override
    public void updateDialogDisplay() {

        referenceList.removeAll();

        String currReference;

        /*
         * Add the references to the reference list control.
         */
        for (ReferencesData currData : referenceData) {
            currReference = (currData.getReference() != null) ? currData
                    .getReference() : "";
            referenceList.add(currReference);

        }

        if (referenceData.size() > 0) {
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
            applyBtn.setEnabled(true);
            deleteBtn.setEnabled(true);
            break;
        case NO_DATA_AVAILABLE:
            okBtn.setEnabled(false);
            applyBtn.setEnabled(false);
            deleteBtn.setEnabled(false);
            break;
        default:
            break;
        }
    }

    /**
     * Update the reference information control.
     */
    @Override
    public void updateInformation() {
        ReferencesData currData = getSelectedDatum();

        referenceTF.setText((currData.getReference() != null) ? currData
                .getReference() : "");
    }

    /**
     * Obtain the currently selected reference data.
     * 
     * @return the reference data
     */
    @Override
    public ReferencesData getSelectedDatum() {
        return referenceData.get(referenceList.getSelectionIndex());
    }

    /**
     * Validate the user entry. Checks for an empty entry in the text control.
     * 
     * @return True if there is data in the references text control.
     */
    @Override
    public boolean validateEntryData(Text tf) {

        if ((tf.getText()).trim().length() == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Error");
            mb.setMessage("You must enter a reference in the text field.");
            mb.open();

            return false;
        }

        return true;
    }

    /**
     * Save the record to the database.
     */
    @Override
    public boolean saveRecord() {

        // Validate entry data.
        if (validateEntryData(referenceTF) == false) {
            return false;
        }

        // Data to save.
        ReferencesData dataToSave = new ReferencesData();

        dataToSave.setLid(lid);

        dataToSave.setReference(referenceTF.getText());

        // Save to DB
        try {
        	   if ((referenceList.getSelectionIndex() < 0) && 
        			   (newInsert)) {
        		   HydroDBDataManager.getInstance().putData(dataToSave);
           		   newInsert=false;
        	   } else {
        	
        		   //Data Listed
        		   ReferencesData dataDisplayed = new ReferencesData();
        		   dataDisplayed.setLid(lid);
        		   dataDisplayed.setReference(referenceList.
        				   getItem(referenceList.getSelectionIndex()));
        		   HydroDBDataManager.getInstance().
        		   		   putNewData(dataToSave, dataDisplayed, newInsert);
        		   referenceList.setSelection(referenceList.getSelectionIndex());
        		   newInsert=false;
        	   }
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
                    HydroDBDataManager.getInstance().deleteRecord(
                            getSelectedDatum());
                } catch (ArrayIndexOutOfBoundsException e) {
                    MessageBox mbDel = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mbDel.setText("Unable to Delete");
                    mbDel
                            .setMessage("No item is selected in the reference list");
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
        
        if (referenceList.getItemCount() !=0 ) {
        	referenceList.setSelection(0);
        	updateInformation();
        } 
        

    }

    /**
     * Clear the text fields in the form.
     */
    @Override
    public void clearForm() {
        referenceTF.setText("");
    }

}
