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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
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

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.HydroGenStationData;
import com.raytheon.viz.hydrocommon.datamanager.DataTrashCanDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Hydrogen Configuration dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation
 * Dec 29, 2008 1802        askripsk    Connect to database.
 * Apr 19, 2013 1790        rferrel     Make dialog non-blocking.
 * Mar 31, 2014 #2970       lvenable    Put dispose checks in the runAsync calls.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class HydroGenConfigDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HydroGenConfigDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Location list control.
     */
    private List locationList;

    /**
     * Location text control.
     */
    private Text locationTF;

    /**
     * Forecast type source combo control.
     */
    private Combo fcstTypeSourceCbo;

    /**
     * Type source combo box.
     */
    private Combo typeSourceCbo;

    /**
     * Physical element list control.
     */
    private List peList;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Cache of display data.
     */
    private java.util.List<HydroGenStationData> stationData;

    /**
     * System wait cursor no need to dispose.
     */
    Cursor waitCursor;

    /**
     * Non-blocking Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public HydroGenConfigDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("HydroGen Configuration");
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
        waitCursor = shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createSummaryGroup();

        createSelectedItemGroup();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();

        loadStaticData();

        getDialogData();
    }

    /**
     * Create the Summary group and controls.
     */
    private void createSummaryGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group summaryGroup = new Group(shell, SWT.NONE);
        summaryGroup.setLayout(new GridLayout(1, false));
        summaryGroup.setLayoutData(gd);
        summaryGroup.setText(" Summary by Location of HgStation ");

        gd = new GridData();
        gd.horizontalIndent = 4;
        Label listLbl = new Label(summaryGroup, SWT.NONE);
        listLbl.setText(getLocationListLabelText());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        gd = new GridData(650, 300);
        locationList = new List(summaryGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        locationList.setLayoutData(gd);
        locationList.setFont(controlFont);
        locationList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateInformationDisplay();
            }
        });
    }

    /**
     * Create the Selected Item group and controls.
     */
    private void createSelectedItemGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group selectedItemGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        selectedItemGroup.setLayout(gl);
        selectedItemGroup.setLayoutData(gd);
        selectedItemGroup.setText(" Selected Item ");

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label locationLbl = new Label(selectedItemGroup, SWT.RIGHT);
        locationLbl.setText("Location:");
        locationLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        locationTF = new Text(selectedItemGroup, SWT.BORDER);
        locationTF.setLayoutData(gd);
        locationTF.setTextLimit(8);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 50;
        Label fcstTypSrcLbl = new Label(selectedItemGroup, SWT.RIGHT);
        fcstTypSrcLbl.setText("Forecast TypeSource:");
        fcstTypSrcLbl.setLayoutData(gd);

        fcstTypeSourceCbo = new Combo(selectedItemGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        fcstTypeSourceCbo.select(0);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label typSrcLbl = new Label(selectedItemGroup, SWT.RIGHT);
        typSrcLbl.setText("TypeSource:");
        typSrcLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        typeSourceCbo = new Combo(selectedItemGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        typeSourceCbo.select(0);
        typeSourceCbo.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.TOP, false, true);
        Label peLbl = new Label(selectedItemGroup, SWT.RIGHT);
        peLbl.setText("Physical Elements:");
        peLbl.setLayoutData(gd);

        gd = new GridData(350, 100);
        gd.horizontalSpan = 3;
        peList = new List(selectedItemGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        peList.setLayoutData(gd);
        peList.setFont(controlFont);
    }

    /**
     * Create the bottom buttons on the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 120;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
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

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
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
     * Get the label text for the location list control.
     * 
     * @return The label text.
     */
    private String getLocationListLabelText() {
        String format = "%S  %S   %S    %S    %S";

        String labelStr = String.format(format, "Location", "HSA", "PE", "TS",
                "Forecast TS");

        return labelStr;
    }

    /**
     * Loads static data (i.e. TS, FcstTS, PE) from the database into the
     * dialog.
     */
    private void loadStaticData() {
        try {
            // Load Type Source
            typeSourceCbo.removeAll();
            for (String currTs : getShefTs()) {
                typeSourceCbo.add(currTs);
            }
            typeSourceCbo.select(0);

            // Load Fcst Type Source
            fcstTypeSourceCbo.removeAll();
            for (String currTs : getShefFcstTs()) {
                fcstTypeSourceCbo.add(currTs);
            }
            fcstTypeSourceCbo.select(0);

            // Load Physical Element Lists
            peList.removeAll();
            for (String currPE : DataTrashCanDataManager.getInstance()
                    .getPEList()) {
                if (currPE.startsWith("H") || currPE.startsWith("Q")) {
                    peList.add(currPE);
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to load static data. ", e);
        }
    }

    /**
     * Retrieves the hydrogen data from the database.
     */
    private void getDialogData() {
        shell.setCursor(waitCursor);

        Job job = new Job("HydroGen") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    stationData = HydroDBDataManager.getInstance().getData(
                            HydroGenStationData.class);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to load data. ", e);
                }

                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        if (isDisposed()) {
                            return;
                        }
                        updateDialogDisplay();
                        shell.setCursor(null);
                    }
                });
                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }

    /**
     * Updates the display of station data.
     */
    private void updateDialogDisplay() {
        String format = "%-9S %3S   %2S    %2S        %S";

        // Clear any old data
        locationList.removeAll();

        String displayStr;
        for (HydroGenStationData currData : stationData) {
            displayStr = String.format(format, currData.getLid(),
                    currData.getHsa(), currData.getPe(), currData.getTs(),
                    currData.getForecastTs());

            locationList.add(displayStr);
        }
    }

    /**
     * Retrieves the SHEF TS from the database.
     * 
     * @return The display string for the TS.
     * @throws VizException
     */
    public java.util.List<String> getShefTs() throws VizException {
        java.util.List<String> rval = new ArrayList<String>();

        String tsQuery = "SELECT name, ts FROM shefts WHERE ts LIKE 'P%' or ts LIKE 'R%' ORDER BY ts";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                tsQuery);

        if (data != null) {
            for (QueryResultRow currTs : data.getRows()) {
                String name = (String) currTs.getColumn(data.getColumnNames()
                        .get("name"));
                String ts = (String) currTs.getColumn(data.getColumnNames()
                        .get("ts"));
                rval.add(String.format("%s (%s)", ts, name));
            }
        }

        return rval;
    }

    /**
     * Retrieves the SHEF Fcst TS from the database.
     * 
     * @return The display string for the TS.
     * @throws VizException
     */
    public java.util.List<String> getShefFcstTs() throws VizException {
        java.util.List<String> rval = new ArrayList<String>();

        String tsQuery = "SELECT name, ts FROM shefts WHERE ts LIKE 'C%' or ts LIKE 'F%' ORDER BY ts";

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                tsQuery);

        if (data != null) {
            for (QueryResultRow currTs : data.getRows()) {
                String name = (String) currTs.getColumn(data.getColumnNames()
                        .get("name"));
                String ts = (String) currTs.getColumn(data.getColumnNames()
                        .get("ts"));
                rval.add(String.format("%s (%s)", ts, name));
            }
        }

        return rval;
    }

    /**
     * Populates the form with the selected item's data.
     */
    private void updateInformationDisplay() {
        HydroGenStationData selectedData = getSelectedData();

        if (selectedData != null) {
            // Set Lid
            locationTF.setText(selectedData.getLid());

            // Set Fcst TS
            for (int i = 0; i < fcstTypeSourceCbo.getItemCount(); i++) {
                if (fcstTypeSourceCbo.getItem(i).startsWith(
                        selectedData.getForecastTs())) {
                    fcstTypeSourceCbo.select(i);
                    break;
                }
            }

            // Set TS
            for (int i = 0; i < typeSourceCbo.getItemCount(); i++) {
                if (typeSourceCbo.getItem(i).startsWith(selectedData.getTs())) {
                    typeSourceCbo.select(i);
                    break;
                }
            }

            // Set PE
            for (int i = 0; i < peList.getItemCount(); i++) {
                if (peList.getItem(i).split(" ")[0]
                        .equals(selectedData.getPe())) {
                    peList.select(i);
                    break;
                }
            }
        }
    }

    /**
     * Returns the currently selected station data.
     * 
     * @return Returns the station data if one is selected, else null.
     */
    private HydroGenStationData getSelectedData() {
        HydroGenStationData currData = null;

        if (locationList.getSelectionCount() > 0) {
            currData = stationData.get(locationList.getSelectionIndex());
        }

        return currData;
    }

    /**
     * Prompts the user for confirmation and deletes the selected record
     */
    private void deleteRecord() {
        HydroGenStationData selectedData = getSelectedData();

        if (selectedData != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete this entry?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    HydroDBDataManager.getInstance().deleteRecord(selectedData);

                    // Refresh data
                    getDialogData();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to delete the record. ", e);
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a record first.");
        }
    }

    /**
     * Saves the currently displayed record to the database.
     */
    private void saveRecord() {
        if (!locationTF.equals("")) {
            if (checkFKConstraintsMet(locationTF.getText())) {
                HydroGenStationData newData = new HydroGenStationData();

                newData.setLid(locationTF.getText());
                newData.setPe(getSelectedPE());
                newData.setTs(getSelectedTS());
                newData.setForecastTs(getSelectedFcstTS());

                try {
                    HydroDBDataManager.getInstance().putData(newData);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to save the data. ", e);
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Value");
            mb.setMessage("Please enter a location.");
            mb.open();
        }

        // Refresh data
        getDialogData();
    }

    /**
     * Returns the currently selected PE.
     * 
     * @return
     */
    private String getSelectedPE() {
        String rval = "";

        String selectedPE = peList.getSelection()[0];

        rval = selectedPE.split(" ")[0];

        return rval;
    }

    /**
     * Returns the currently selected TS.
     * 
     * @return
     */
    private String getSelectedTS() {
        String rval = "";

        int i = typeSourceCbo.getSelectionIndex();

        if (i >= 0) {
            String selectedTS = typeSourceCbo.getItem(i);

            rval = selectedTS.split(" ")[0];
        }

        return rval;
    }

    /**
     * Returns the currently selected fcst TS.
     * 
     * @return
     */
    private String getSelectedFcstTS() {
        String rval = "";

        int i = fcstTypeSourceCbo.getSelectionIndex();

        if (i >= 0) {
            String selectedTS = fcstTypeSourceCbo.getItem(i);

            rval = selectedTS.split(" ")[0];
        }

        return rval;
    }

    /**
     * Checks the foreign key constraints for the record.
     * 
     * @return True if the foreign key constraints are met.
     */
    private boolean checkFKConstraintsMet(String lid) {
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
                mb.setMessage("The location must be added via the River Gauge dialog first.");
                mb.open();
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to verify constraints. ", e);
        }

        return rval;
    }
}
