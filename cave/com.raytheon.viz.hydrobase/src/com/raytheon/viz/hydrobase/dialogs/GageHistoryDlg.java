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
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
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
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.GageDBData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Gage History dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation
 * Jan 8, 2008  1802        askripsk    Connect to DB.
 * Apr 19, 2013 170-        rferrel     Make dialog non-blocking.
 * Nov 30, 2015  14228      wkwock      Update remark limit to 510.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class GageHistoryDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GageHistoryDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Type list control.
     */
    private List typeList;

    /**
     * Owner list control.
     */
    private List ownerList;

    /**
     * Maintenance list control.
     */
    private List maintList;

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

    private String currentLocText = null;

    /**
     * OK button.
     */
    private Button okBtn;

    /**
     * Apply button.
     */
    private Button applyBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Cache of Gages
     */
    private java.util.List<GageDBData> gageData;

    /**
     * Date format
     */
    private SimpleDateFormat dateFormat;

    /**
     * States of the dialog
     */
    private enum DialogState {
        NEW_RECORD, NO_DATA, DATA_AVAILABLE
    }

    /**
     * Current location
     */
    private String lid;

    /**
     * Non-blocking Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    
    private final int MAX_REMARK_CHAR = 510;

    public GageHistoryDlg(Shell parent, String titleInfo, String lid) {
        super(parent);
        setText("Gage History" + titleInfo);

        this.lid = lid;

        dateFormat = new SimpleDateFormat("MM/dd/yyyy");
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
        initializeComponents();

        getDialogData();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        // Create Data List control.
        createDataListControl();

        createInformationGroup();

        createLocationGroup();

        createBottomButtons();

        loadStaticData();
    }

    /**
     * Create the data list control.
     */
    private void createDataListControl() {
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));

        GridData gd = new GridData();
        gd.horizontalIndent = 4;
        Label typeLbl = new Label(listComp, SWT.NONE);
        typeLbl.setText(getListLabelText());
        typeLbl.setFont(controlFont);
        typeLbl.setLayoutData(gd);

        gd = new GridData(700, 100);
        dataList = new List(shell, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        dataList.setLayoutData(gd);
        dataList.setFont(controlFont);
        dataList.addSelectionListener(new SelectionAdapter() {
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
        infoGroup.setLayout(new GridLayout(2, false));
        infoGroup.setLayoutData(gd);
        infoGroup.setText(" Information ");

        // ------------------------------------
        // Create the left list composite.
        // ------------------------------------
        Composite leftComp = new Composite(infoGroup, SWT.NONE);
        leftComp.setLayout(new GridLayout(2, false));

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label typeLbl = new Label(leftComp, SWT.RIGHT);
        typeLbl.setText("Type: ");
        typeLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 175;
        gd.heightHint = 120;
        typeList = new List(leftComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        typeList.setLayoutData(gd);
        typeList.setFont(controlFont);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label ownerLbl = new Label(leftComp, SWT.RIGHT);
        ownerLbl.setText("Owner: ");
        ownerLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 175;
        gd.heightHint = 120;
        ownerList = new List(leftComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        ownerList.setLayoutData(gd);
        ownerList.setFont(controlFont);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label maintLbl = new Label(leftComp, SWT.RIGHT);
        maintLbl.setText("Maint: ");
        maintLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 175;
        gd.heightHint = 120;
        maintList = new List(leftComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        maintList.setLayoutData(gd);
        maintList.setFont(controlFont);

        // ------------------------------------
        // Create the right composite.
        // ------------------------------------
        Composite rightComp = new Composite(infoGroup, SWT.NONE);
        rightComp.setLayout(new GridLayout(2, false));
        rightComp.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, true));

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        Label beginLbl = new Label(rightComp, SWT.RIGHT);
        beginLbl.setText("Begin: ");
        beginLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        beginTF = new Text(rightComp, SWT.BORDER);
        beginTF.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        Label endLbl = new Label(rightComp, SWT.RIGHT);
        endLbl.setText("End: ");
        endLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        endTF = new Text(rightComp, SWT.BORDER);
        endTF.setLayoutData(gd);
    }

    /**
     * Create the location group and controls.
     */
    private void createLocationGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group locationGroup = new Group(shell, SWT.NONE);
        locationGroup.setLayout(new GridLayout(1, false));
        locationGroup.setLayoutData(gd);
        locationGroup.setText(" Location ");

        gd = new GridData(500, 120);
        locationTF = new Text(locationGroup, SWT.BORDER | SWT.MULTI | SWT.WRAP);
        locationTF.setLayoutData(gd);
        locationTF.setTextLimit(MAX_REMARK_CHAR);
        currentLocText = locationTF.getText();
        ModifyListener listener = new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                if (locationTF.getText().length() > MAX_REMARK_CHAR) {
                    locationTF.setText(currentLocText);
                    shell.getDisplay().beep();
                } else
                    currentLocText = locationTF.getText();
            }
        };

        locationTF.addModifyListener(listener);

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
        okBtn.setEnabled(false);
        okBtn.addSelectionListener(new SelectionAdapter() {
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
        applyBtn.setEnabled(false);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                newRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.setEnabled(false);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });
    }

    /**
     * Get the label text for the data list control.
     * 
     * @return Label text.
     */
    private String getListLabelText() {
        String format = "%S                            %S                       %S             %S";

        String labelStr = String
                .format(format, "Type", "Owner", "Begin", "End");

        return labelStr;
    }

    /**
     * Obtain and display gage static data.
     */
    private void loadStaticData() {
        typeList.removeAll();
        ownerList.removeAll();
        maintList.removeAll();

        try {
            // Type
            for (String currType : getGageData("gagetype", "type")) {
                typeList.add(currType);
            }

            // Owner
            for (String currOwner : getGageData("gageowner", "owner")) {
                ownerList.add(currOwner);
            }

            // Maintaining Agency
            for (String currAgency : getGageData("gagemaint", "maint")) {
                maintList.add(currAgency);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to load static data. ", e);
        }
    }

    /**
     * Get gage data and update the display.
     * 
     * @param table
     * @param column
     * @return
     * @throws VizException
     */
    private java.util.List<String> getGageData(String table, String column)
            throws VizException {
        java.util.List<String> rval = new ArrayList<String>();

        String query = "SELECT %s FROM %s ORDER BY %s";
        query = String.format(query, column, table, column);

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                query);

        int i = data.getColumnNames().get(column);
        for (QueryResultRow currRow : data.getRows()) {
            rval.add((String) currRow.getColumn(i));
        }

        return rval;
    }

    /**
     * Get gage data for the station location.
     */
    private void getDialogData() {
        GageDBData seedData = new GageDBData();
        seedData.setLid(lid);

        try {
            gageData = HydroDBDataManager.getInstance().getData(seedData);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to load data. ", e);
        }

        updateDisplay();
    }

    /**
     * Update display with gage data.
     */
    private void updateDisplay() {
        String format = "%-15s                  %-15s           %-10s       %-10s";
        dataList.removeAll();
        clearInformation();

        if (gageData.size() > 0) {
            String endDate;
            for (GageDBData currData : gageData) {
                endDate = (currData.getEndDate() != null) ? dateFormat
                        .format(currData.getEndDate()) : "";
                dataList.add(String.format(format, currData.getType(),
                        currData.getOwner(),
                        dateFormat.format(currData.getBeginDate()), endDate));
            }

            updateDialogState(DialogState.DATA_AVAILABLE);
        } else {
            updateDialogState(DialogState.NO_DATA);
        }

    }

    /**
     * Display currently selected record's information.
     */
    private void updateInformation() {
        GageDBData currData = getSelectedRecord();

        if (currData != null) {
            // Type
            String type = currData.getType();
            for (int i = 0; i < typeList.getItemCount(); i++) {
                if (typeList.getItem(i).equals(type)) {
                    typeList.select(i);
                    break;
                }
            }

            // Owner
            String owner = currData.getOwner();
            for (int i = 0; i < ownerList.getItemCount(); i++) {
                if (ownerList.getItem(i).equals(owner)) {
                    ownerList.select(i);
                    break;
                }
            }

            // Maint
            String maint = currData.getMaintainingAgency();
            for (int i = 0; i < maintList.getItemCount(); i++) {
                if (maintList.getItem(i).equals(maint)) {
                    maintList.select(i);
                    break;
                }
            }

            beginTF.setText(dateFormat.format(currData.getBeginDate()));
            if (currData.getEndDate() != null) {
                endTF.setText(dateFormat.format(currData.getEndDate()));
            } else {
                endTF.setText("");
            }

            locationTF.setText(currData.getRemark());
        }
    }

    /**
     * Verify and save gage data.
     * 
     * @return true when record saved
     */
    private boolean saveRecord() {
        boolean successful = false;

        if (beginTF.getText().equals("")) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Unable to Save");
            mb.setMessage("The Begin Date cannot be empty.");
            mb.open();
        } else {
            GageDBData newData = new GageDBData();

            // LID
            newData.setLid(lid);

            // Begin Data
            try {
                newData.setBeginDate(dateFormat.parse(beginTF.getText()));
            } catch (ParseException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("Please enter the Begin Date\nin the form: YYYY-MM-DD");
                mb.open();

                return successful;
            }

            // End Date
            if (endTF.getText().equals("")) {
                newData.setEndDate((Date) null);
            } else {
                try {
                    newData.setEndDate(dateFormat.parse(endTF.getText()));
                } catch (ParseException e) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Unable to Save");
                    mb.setMessage("Please enter the End Date\nin the form: YYYY-MM-DD");
                    mb.open();

                    return successful;
                }
            }

            // TYPE
            newData.setType(typeList.getItem(typeList.getSelectionIndex()));

            // Owner
            newData.setOwner(ownerList.getItem(ownerList.getSelectionIndex()));

            // Maintaining Agency
            newData.setMaintainingAgency(maintList.getItem(maintList
                    .getSelectionIndex()));

            // Location/Remark
            newData.setRemark(locationTF.getText());

            try {
                HydroDBDataManager.getInstance().putData(newData);

                successful = true;

                // Refresh Cache
                getDialogData();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error while trying to save the City. ", e);
            }
        }

        return successful;
    }

    /**
     * Delete selected record from data base and update display.
     */
    private void deleteRecord() {
        GageDBData currData = getSelectedRecord();

        if (currData != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete this entry?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    HydroDBDataManager.getInstance().deleteRecord(currData);

                    // Refresh the cache
                    getDialogData();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to delete entry. ", e);
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select an entry first.");
            mb.open();
        }
    }

    /**
     * Clear information fields and update to new record.
     */
    private void newRecord() {
        clearInformation();
        updateDialogState(DialogState.NEW_RECORD);
    }

    /**
     * Clear all the information list and fields.
     */
    private void clearInformation() {
        typeList.select(0);
        ownerList.select(0);
        maintList.select(0);

        beginTF.setText("");
        endTF.setText("");
        locationTF.setText("");
    }

    /**
     * Get the gage data for the current selection in the data list.
     * 
     * @return currData
     */
    private GageDBData getSelectedRecord() {
        GageDBData currData = null;

        if (dataList.getSelectionCount() > 0) {
            currData = gageData.get(dataList.getSelectionIndex());
        }

        return currData;
    }

    /**
     * Update button enable state base on the state.
     * 
     * @param currState
     */
    private void updateDialogState(DialogState currState) {
        switch (currState) {
        case NEW_RECORD:
            okBtn.setEnabled(true);
            applyBtn.setEnabled(true);
            break;
        case NO_DATA:
            okBtn.setEnabled(false);
            applyBtn.setEnabled(false);
            deleteBtn.setEnabled(false);
            break;
        case DATA_AVAILABLE:
            okBtn.setEnabled(true);
            applyBtn.setEnabled(true);
            deleteBtn.setEnabled(true);
            break;
        default:
            break;
        }

    }
}
