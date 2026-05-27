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
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.PublicationsData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Publications dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 5, 2008				lvenable	Initial creation.
 * 12/18/2008   1782        grichard    Connected to IHFS DB.
 * Apr 19, 2013 1790        rferrel     Made dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class PublicationsDlg extends CaveSWTDialog implements IHydroDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PublicationsDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Begin text control.
     */
    private Text beginTF;

    /**
     * End text control.
     */
    private Text endTF;

    /**
     * Location text control.
     */
    private Text locationTF;

    /**
     * Flag indicating if validation is taking place.
     */
    private boolean validationInProgress = false;

    /**
     * Okay button.
     */
    private Button okBtn;

    /**
     * Apply button.
     */
    private Button applyBtn;

    /**
     * Cancel button.
     */
    private Button cancelBtn;

    /**
     * New button.
     */
    private Button newBtn;

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
     * Publications Data.
     */
    private java.util.List<PublicationsData> pubData;

    /**
     * Location Identifier.
     */
    private String lid;

    /**
     * Date format.
     */
    private SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");

    /**
     * Non-blocking Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public PublicationsDlg(Shell parent, String titleInfo, String lid) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Publications" + titleInfo);

        this.lid = lid;
        // TODO Remove/comment-out this test code statement:
        // this.lid = "ABSM8";

        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
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
        setReturnValue(lid);
        // Initialize all of the controls and layouts
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createListControls();

        createInformationGroup();

        createBottomButtons();

        // Get the dialog data
        getDialogData();
    }

    /**
     * Create the data label and list control.
     */
    private void createListControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));
        listComp.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 4;
        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText(getDataListLabelText());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        // --------------------------------------
        // Create the data list control
        // --------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 450;
        gd.heightHint = 125;
        dataList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        dataList.setLayoutData(gd);
        dataList.setFont(controlFont);
        dataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateInformation();
            }
        });
    }

    /**
     * Create the Information group and controls.
     */
    private void createInformationGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group infoGroup = new Group(shell, SWT.NONE);
        infoGroup.setLayout(new GridLayout(4, false));
        infoGroup.setLayoutData(gd);
        infoGroup.setText(" Information ");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label beginLbl = new Label(infoGroup, SWT.RIGHT);
        beginLbl.setText("Begin: ");
        beginLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        beginTF = new Text(infoGroup, SWT.BORDER);
        beginTF.setLayoutData(gd);
        /**
         * Add listeners to the beginning date text control.
         */
        beginTF.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent ke) {
                if (ke.keyCode == SWT.KEYPAD_CR || ke.keyCode == SWT.CR) {
                    validationInProgress = true;
                    validateEntryData(beginTF);
                    validationInProgress = false;
                }
            }

            public void keyReleased(KeyEvent ke) {
                // Do nothing...
            }
        });

        beginTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent event) {
                // Check if the text field is being validated
                // by the 'Enter' key.
                if (validationInProgress == true) {
                    return;
                }
                validateEntryData(beginTF);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label endLbl = new Label(infoGroup, SWT.RIGHT);
        endLbl.setText("End: ");
        endLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        endTF = new Text(infoGroup, SWT.BORDER);
        endTF.setLayoutData(gd);
        /**
         * Add listeners to the ending date text control.
         */
        endTF.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent ke) {
                if (ke.keyCode == SWT.KEYPAD_CR || ke.keyCode == SWT.CR) {
                    validationInProgress = true;
                    validateEntryData(endTF);
                    validationInProgress = false;
                }
            }

            public void keyReleased(KeyEvent ke) {
                // Do nothing...
            }
        });

        endTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent event) {
                // Check if the text field is being validated
                // by the 'Enter' key.
                if (validationInProgress == true) {
                    return;
                }
                validateEntryData(endTF);
            }
        });

        Label locationLbl = new Label(infoGroup, SWT.NONE);
        locationLbl.setText("Location: ");

        gd = new GridData(250, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        locationTF = new Text(infoGroup, SWT.BORDER);
        locationTF.setLayoutData(gd);
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
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (saveRecord()) {
                    close();
                }
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
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
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });
    }

    /**
     * Get the data list control label text.
     * 
     * @return Label text.
     */
    private String getDataListLabelText() {
        String format = "%S                                            %S";

        String labelStr = String.format(format, "Begin", "End");

        return labelStr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.hydrobase.dialogs.IHydroDialog#getDialogData()
     */
    @Override
    public void getDialogData() {
        PublicationsData seedData = new PublicationsData();
        seedData.setLid(lid);

        if (pubData != null) {
            pubData.clear();
        }
        try {
            pubData = HydroDBDataManager.getInstance().getData(seedData);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to get publication data. ", e);
        }

        updateDialogDisplay();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.hydrobase.dialogs.IHydroDialog#updateDialogDisplay()
     */
    @Override
    public void updateDialogDisplay() {

        dataList.removeAll();

        String currBeginDate;
        String currEndDate;

        for (PublicationsData currData : pubData) {
            currBeginDate = (currData.getBegin() != null) ? dateFormat
                    .format(currData.getBegin()) : "";

            currEndDate = (currData.getEnd() != null) ? dateFormat
                    .format(currData.getEnd()) : "";

            dataList.add(String.format("%-11s %34s %-11s", currBeginDate, " ",
                    currEndDate));
        }

        if (pubData.size() > 0) {
            dialogState = DialogStates.DATA_AVAILABLE;
        } else {
            dialogState = DialogStates.NO_DATA_AVAILABLE;
        }
        updateDialogState();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.hydrobase.dialogs.IHydroDialog#updateDialogState()
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.hydrobase.dialogs.IHydroDialog#updateInformation()
     */
    @Override
    public void updateInformation() {
        PublicationsData currData = getSelectedDatum();

        beginTF.setText((currData.getBegin() != null) ? dateFormat
                .format(currData.getBegin()) : "");
        endTF.setText((currData.getEnd() != null) ? dateFormat.format(currData
                .getEnd()) : "");
        locationTF
                .setText((currData.getPub() != null) ? currData.getPub() : "");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.hydrobase.dialogs.IHydroDialog#getSelectedDatum()
     */
    @Override
    public PublicationsData getSelectedDatum() {
        return pubData.get(dataList.getSelectionIndex());
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.hydrobase.dialogs.IHydroDialog#validateEntryData(org
     * .eclipse.swt.widgets.Text)
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.hydrobase.dialogs.IHydroDialog#saveRecord()
     */
    @Override
    public boolean saveRecord() {

        // Data to save.
        PublicationsData dataToSave = new PublicationsData();

        dataToSave.setLid(lid);

        dataToSave.setPub(locationTF.getText());

        if (!beginTF.getText().equals("")) {
            try {
                dataToSave.setBegin(dateFormat.parse(beginTF.getText()));

            } catch (ParseException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Date");
                mb.setMessage("Please enter a valid date in the form: MM/DD/YYYY");
                mb.open();
                return false;
            }
        }

        if (!endTF.getText().equals("")) {
            try {
                dataToSave.setEnd(dateFormat.parse(endTF.getText()));
            } catch (ParseException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Date");
                mb.setMessage("Please enter a valid date in the form: MM/DD/YYYY");
                mb.open();
                return false;
            }
        }

        // Save to DB
        try {
            HydroDBDataManager.getInstance().putData(dataToSave);
        } catch (VizException e) {

            String cause = e.getCause().getMessage();

            int causeStart = cause.indexOf("ERROR:");

            // If the exception contain the SQL exception "ERROR:"
            if (causeStart > 0) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("An error occurred while trying to save.");
                int causeEnd = cause.indexOf("\n", causeStart);

                cause = cause.substring(causeStart, causeEnd);

                if (cause.contains("datum_rvr_fk")) {
                    mb.setMessage("Please enter data for " + lid
                            + " in the River Gauge dialog first");
                }
                mb.open();
            } else {

                statusHandler.handle(Priority.PROBLEM,
                        "Unable to save publication data. ", e);
            }
            return false;
        }

        // Refresh the data
        getDialogData();
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.hydrobase.dialogs.IHydroDialog#deleteRecord()
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
                    mbDel.setMessage("No item is selected in the publications list");
                    mbDel.open();
                }

                clearForm();

            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to delete publication data. ", e);
            }
        }

        // Refresh the data
        getDialogData();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.hydrobase.dialogs.IHydroDialog#clearForm()
     */
    @Override
    public void clearForm() {
        dataList.removeAll();
        beginTF.setText("");
        endTF.setText("");
        locationTF.setText("");
    }
}
