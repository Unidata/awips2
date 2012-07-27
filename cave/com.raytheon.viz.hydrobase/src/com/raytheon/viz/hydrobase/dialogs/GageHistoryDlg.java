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
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class GageHistoryDlg extends CaveSWTDialog {

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
    
    private String currentLocText=null;
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
    private ArrayList<GageDBData> gageData;

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
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public GageHistoryDlg(Shell parent, String titleInfo, String lid) {
        super(parent);
        setText("Gage History" + titleInfo);

        this.lid = lid;

        dateFormat = new SimpleDateFormat("MM/dd/yyyy");
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
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
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
        locationTF.setTextLimit(255);
        currentLocText=locationTF.getText();
        ModifyListener listener = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (locationTF.getText().length()>255){
        			locationTF.setText(currentLocText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentLocText=locationTF.getText();
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
                    shell.dispose();
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
                shell.dispose();
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
            e.printStackTrace();
        }
    }

    private ArrayList<String> getGageData(String table, String column)
            throws VizException {
        ArrayList<String> rval = new ArrayList<String>();

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

    private void getDialogData() {
        GageDBData seedData = new GageDBData();
        seedData.setLid(lid);

        try {
            gageData = HydroDBDataManager.getInstance().getData(seedData);
        } catch (VizException e) {
            e.printStackTrace();
        }

        updateDisplay();
    }

    private void updateDisplay() {
        String format = "%-15s                  %-15s           %-10s       %-10s";
        dataList.removeAll();
        clearInformation();

        if (gageData.size() > 0) {
            String endDate;
            for (GageDBData currData : gageData) {
                endDate = (currData.getEndDate() != null) ? dateFormat
                        .format(currData.getEndDate()) : "";
                dataList.add(String.format(format, currData.getType(), currData
                        .getOwner(),
                        dateFormat.format(currData.getBeginDate()), endDate));
            }

            updateDialogState(DialogState.DATA_AVAILABLE);
        } else {
            updateDialogState(DialogState.NO_DATA);
        }

    }

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
                mb
                        .setMessage("Please enter the Begin Date\nin the form: YYYY-MM-DD");
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
                    mb
                            .setMessage("Please enter the End Date\nin the form: YYYY-MM-DD");
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
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb
                        .setMessage("An error occurred while trying to save the City");
                mb.open();

                e.printStackTrace();
            }
        }

        return successful;
    }

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
                    mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Unable to Delete");
                    mb
                            .setMessage("An error occurred while trying to delete the entry");
                    mb.open();

                    e.printStackTrace();
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select an entry first.");
            mb.open();
        }
    }

    private void newRecord() {
        clearInformation();
        updateDialogState(DialogState.NEW_RECORD);
    }

    private void clearInformation() {
        typeList.select(0);
        ownerList.select(0);
        maintList.select(0);

        beginTF.setText("");
        endTF.setText("");
        locationTF.setText("");
    }

    private GageDBData getSelectedRecord() {
        GageDBData currData = null;

        if (dataList.getSelectionCount() > 0) {
            currData = gageData.get(dataList.getSelectionIndex());
        }

        return currData;
    }

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
