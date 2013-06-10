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
import com.raytheon.viz.hydrocommon.data.BenchmarkData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Benchmark dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Sep 2008             lvenable    Initial creation.
 * 17 Dec 2008  1787       askripsk    Connect to database.
 * 17 Apr 2013  1790       rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class BenchmarkDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BenchmarkDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Number text control.
     */
    private Text numberTF;

    /**
     * Elevation text control.
     */
    private Text elevationTF;

    /**
     * Description text control.
     */
    private Text descriptionTF;

    /**
     * Delete button
     */
    private Button deleteBtn;

    /**
     * OK Button
     */
    private Button okBtn;

    /**
     * Apply Button
     */
    private Button applyBtn;

    /**
     * Information group
     */
    private Group infoGroup;

    /**
     * The station's data that is being managed.
     */
    private String lid;

    /**
     * Benchmark data for the current location.
     */
    private java.util.List<BenchmarkData> benchData;

    /**
     * Dialog states.
     */
    private enum DialogStates {
        NO_DATA, DATA_AVAILABLE, NEW_ENTRY
    }

    /**
     * Non-blocking Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public BenchmarkDlg(Shell parent, String titleInfo, String lid) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Benchmark" + titleInfo);
        this.lid = lid;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
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

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createListControls();

        createInformationGroup();

        createBottomButtons();
        // Get the dialog data
        getDialogData();
    }

    /**
     * Create the data list control and label.
     */
    private void createListControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));
        listComp.setLayoutData(gd);

        // Create the data list label
        gd = new GridData();
        gd.horizontalIndent = 4;
        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText(getListLabelText());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        // Create the data list control
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 700;
        gd.heightHint = 125;
        dataList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        dataList.setLayoutData(gd);
        dataList.setFont(controlFont);
        dataList.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                updateInformationDisplay();
            }
        });
    }

    /**
     * Create the Information group and controls.
     */
    private void createInformationGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        infoGroup = new Group(shell, SWT.NONE);
        infoGroup.setLayout(new GridLayout(2, false));
        infoGroup.setLayoutData(gd);
        infoGroup.setText(" Information ");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label numberLbl = new Label(infoGroup, SWT.NONE);
        numberLbl.setText("Number:");
        numberLbl.setLayoutData(gd);

        Label elevationLbl = new Label(infoGroup, SWT.NONE);
        elevationLbl.setText("Elevation:");

        gd = new GridData(80, SWT.DEFAULT);
        numberTF = new Text(infoGroup, SWT.BORDER);
        numberTF.setLayoutData(gd);
        numberTF.setTextLimit(6);

        gd = new GridData(130, SWT.DEFAULT);
        elevationTF = new Text(infoGroup, SWT.BORDER);
        elevationTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label descriptionLbl = new Label(infoGroup, SWT.NONE);
        descriptionLbl.setText("Description:");
        descriptionLbl.setLayoutData(gd);

        gd = new GridData(450, 80);
        gd.horizontalSpan = 2;
        descriptionTF = new Text(infoGroup, SWT.BORDER | SWT.MULTI | SWT.WRAP);
        descriptionTF.setLayoutData(gd);
        descriptionTF.setTextLimit(255);
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
            public void widgetSelected(SelectionEvent event) {
                boolean close = false;

                if (numberTF.getText().equals("")
                        && elevationTF.getText().equals("")) {
                    close = true;
                } else {
                    close = saveRecord();
                }

                if (close) {
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
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });
    }

    /**
     * Get the label text for the data list.
     * 
     * @return The label text.
     */
    private String getListLabelText() {
        String format = "   %S                                                           %S";

        String labelStr = String.format(format, "Number", "Elevation");

        return labelStr;
    }

    /**
     * Get the data for the lid and update the display.
     */
    private void getDialogData() {
        if (benchData == null) {
            benchData = new ArrayList<BenchmarkData>();
        } else {
            benchData.clear();
        }

        clearInformation();

        BenchmarkData seedData = new BenchmarkData();
        seedData.setLid(lid);

        try {
            benchData = HydroDBDataManager.getInstance().getData(seedData);
        } catch (VizException e) {
            e.printStackTrace();
        }

        updateDialogDisplay();
    }

    /**
     * Displays the data for the location.
     */
    private void updateDialogDisplay() {
        dataList.removeAll();

        if (benchData.size() > 0) {
            updateDialogState(DialogStates.DATA_AVAILABLE);
            for (BenchmarkData currData : benchData) {
                dataList.add(getDisplayString(currData));
            }

            dataList.select(0);
            updateInformationDisplay();
        } else {
            updateDialogState(DialogStates.NO_DATA);
        }
    }

    /**
     * Converts the Benchmark data to a display string.
     * 
     * @param currData
     *            The data to be displayed.
     * @return The string with the benchmark data.
     */
    private String getDisplayString(BenchmarkData currData) {
        String displayFormat = "%-6s %47s %14s";
        return String.format(
                displayFormat,
                currData.getBenchmarkNumber(),
                " ",
                HydroDataUtils.getDisplayString("%14s", "%8.3f",
                        currData.getElevation()));
    }

    /**
     * Updates the Information group for the currently selected record.
     */
    private void updateInformationDisplay() {
        BenchmarkData currData = getCurrentlySelectedData();

        numberTF.setText(currData.getBenchmarkNumber());
        elevationTF.setText(HydroDataUtils.getDisplayString(currData
                .getElevation()));
        descriptionTF.setText(currData.getRemark());
    }

    /**
     * Clears the Information group.
     */
    private void clearInformation() {
        numberTF.setText("");
        elevationTF.setText("");
        descriptionTF.setText("");
    }

    /**
     * Gets the object corresponding to the currently selected item.
     * 
     * @return The benchmark data for the currently selected item.
     */
    private BenchmarkData getCurrentlySelectedData() {
        BenchmarkData rval = null;

        if (dataList.getSelectionCount() > 0) {
            rval = benchData.get(dataList.getSelectionIndex());
        }

        return rval;
    }

    /**
     * Updates the dialog state.
     * 
     * @param currState
     *            The current state of the dialog.
     */
    private void updateDialogState(DialogStates currState) {
        switch (currState) {
        case NO_DATA:
            okBtn.setEnabled(false);
            applyBtn.setEnabled(false);
            deleteBtn.setEnabled(false);

            infoGroup.setEnabled(false);
            break;
        case DATA_AVAILABLE:
            okBtn.setEnabled(true);
            applyBtn.setEnabled(true);
            deleteBtn.setEnabled(true);

            infoGroup.setEnabled(true);
            break;
        case NEW_ENTRY:
            clearInformation();
            okBtn.setEnabled(true);
            applyBtn.setEnabled(true);

            infoGroup.setEnabled(true);
            break;
        default:
            break;
        }
    }

    /**
     * Sets up the dialog for a new record to be entered.
     */
    private void newRecord() {
        updateDialogState(DialogStates.NEW_ENTRY);
    }

    /**
     * Saves the currently displayed record to the database.
     * 
     * @return True if the data entered is valid and the save was successful
     */
    private boolean saveRecord() {
        boolean successful = false;

        Double elev = HydroDataUtils.getDoubleFromTF(shell, elevationTF,
                "Elevation");
        if (elev != null && checkFKConstraintsMet()) {
            BenchmarkData currData = new BenchmarkData();

            currData.setLid(lid);
            currData.setBenchmarkNumber(numberTF.getText());
            currData.setElevation(elev);
            currData.setRemark(descriptionTF.getText());

            try {
                HydroDBDataManager.getInstance().putData(currData);

                successful = true;

                getDialogData();
            } catch (VizException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("An error occurred while trying to save.");
                mb.open();

                e.printStackTrace();
            }
        }
        return successful;
    }

    /**
     * Deletes the currently selected item.
     */
    private void deleteRecord() {
        BenchmarkData currData = getCurrentlySelectedData();

        if (currData != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete this entry?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    HydroDBDataManager.getInstance().deleteRecord(currData);

                    getDialogData();
                } catch (VizException e) {
                    MessageBox mbErr = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mbErr.setText("Unable to Delete");
                    mbErr.setMessage("An error occurred while trying to delete the record.");
                    mbErr.open();

                    e.printStackTrace();
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select an item from the list first");
            mb.open();
        }
    }

    /**
     * Checks the foreign key constraints for the record.
     * 
     * @return True if the foreign key constraints are met.
     */
    private boolean checkFKConstraintsMet() {
        boolean rval = false;

        // Lid must exist in riverStat table
        String query = "Select lid FROM riverstat WHERE lid='" + lid + "'";

        try {
            QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                    query);

            if (data.getResultCount() > 0) {
                rval = true;
            } else {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("Data for the location must be add via the River Gauge dialog first.");
                mb.open();
            }
        } catch (VizException e) {
            // don't care, just return false
            statusHandler.handle(Priority.PROBLEM, "Checking constraints", e);
        }

        return rval;
    }
}
