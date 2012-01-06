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

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.DatumData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Datum dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation
 * Dec 5, 2008  1744        askripsky   Connected to DB
 * 12/19/2008   1782        grichard    Implemented IHydroDialog
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class DatumDlg extends CaveSWTDialog implements IHydroDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Date text control.
     */
    private Text dateTF;

    private Button saveBtn;

    private Button deleteBtn;

    private Button closeBtn;

    /**
     * Elevation text control.
     */
    private Text elevationTF;

    private enum DialogStates {
        DATA_AVAILABLE, NO_DATA_AVAILABLE
    }

    private DialogStates dialogState;

    private ArrayList<DatumData> locationDatum;

    private String lid;

    private SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public DatumDlg(Shell parent, String titleInfo, String lid) {
        super(parent);
        setText("Datum" + titleInfo);

        this.lid = lid;
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    @Override
    protected Layout constructShellLayout() {
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

        createListControls();

        createInformationGroup();

        createBottomButtons();
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
        listLbl.setText(getListLabelText());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        // --------------------------------------
        // Create the data list control
        // --------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 500;
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
     * Create the information group and controls.
     */
    private void createInformationGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group infoGroup = new Group(shell, SWT.NONE);
        infoGroup.setLayout(new GridLayout(2, false));
        infoGroup.setLayoutData(gd);
        infoGroup.setText(" Information ");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label numberLbl = new Label(infoGroup, SWT.NONE);
        numberLbl.setText("Date:");
        numberLbl.setLayoutData(gd);

        Label elevationLbl = new Label(infoGroup, SWT.NONE);
        elevationLbl.setText("Elevation:");

        gd = new GridData(80, SWT.DEFAULT);
        dateTF = new Text(infoGroup, SWT.BORDER);
        dateTF.setLayoutData(gd);

        gd = new GridData(130, SWT.DEFAULT);
        elevationTF = new Text(infoGroup, SWT.BORDER);
        elevationTF.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 90;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
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
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Get the label text for the data list control.
     * 
     * @return Label text.
     */
    private String getListLabelText() {
        String format = " %S                                       %S";

        String labelStr = String.format(format, "Date", "Elevation");

        return labelStr;
    }

    /**
     * Gets the Datum data from the DB
     */
    @Override
    public void getDialogData() {
        DatumData seedData = new DatumData();
        seedData.setLid(lid);

        try {
            locationDatum = HydroDBDataManager.getInstance().getData(seedData);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        updateDialogDisplay();
    }

    @Override
    public void updateDialogDisplay() {

        dataList.removeAll();

        String currElevation;
        String currDate;

        for (DatumData currData : locationDatum) {
            if (currData.getElevation() != HydroConstants.MISSING_VALUE) {
                currElevation = String.format("%8.3f", currData.getElevation());
            } else {
                currElevation = "";
            }

            currDate = (currData.getDate() != null) ? dateFormat
                    .format(currData.getDate()) : "";

            dataList.add(String.format("%-11s %25s %14s", currDate, " ",
                    currElevation));
        }

        if (locationDatum.size() > 0) {
            dialogState = DialogStates.DATA_AVAILABLE;
        } else {
            dialogState = DialogStates.NO_DATA_AVAILABLE;
        }
        updateDialogState();
    }

    @Override
    public void updateDialogState() {
        switch (dialogState) {
        case DATA_AVAILABLE:
            deleteBtn.setEnabled(true);
            break;
        case NO_DATA_AVAILABLE:
            deleteBtn.setEnabled(false);
            break;
        default:
            break;
        }
    }

    @Override
    public void updateInformation() {
        DatumData currData = getSelectedDatum();

        dateTF.setText((currData.getDate() != null) ? dateFormat
                .format(currData.getDate()) : "");
        elevationTF
                .setText((currData.getElevation() != HydroConstants.MISSING_VALUE) ? Double
                        .toString(currData.getElevation())
                        : "");
    }

    @Override
    public DatumData getSelectedDatum() {
        return locationDatum.get(dataList.getSelectionIndex());
    }

    @Override
    public boolean validateEntryData(Text tf) {
        return true;
    }

    @Override
    public boolean saveRecord() {
        DatumData dataToSave = new DatumData();

        dataToSave.setLid(lid);

        try {
            dataToSave.setDate(dateFormat.parse(dateTF.getText()));
        } catch (ParseException e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Date");
            mb.setMessage("Please enter a valid date in the form: MM/DD/YYYY");
            mb.open();

            return false;
        }

        try {
            dataToSave.setElevation(Double.parseDouble(elevationTF.getText()));
        } catch (Exception e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Elevation");
            mb.setMessage("Please enter a valid numeric value for Elevation");
            mb.open();

            return false;
        }

        // Save to DB
        try {
            HydroDBDataManager.getInstance().putData(dataToSave);
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

    @Override
    public void deleteRecord() {
        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Delete Confirmation");
        mb.setMessage("Do you wish to delete this entry?");

        int result = mb.open();

        if (result == SWT.OK) {
            try {
                HydroDBDataManager.getInstance().deleteRecord(
                        getSelectedDatum());

                dateTF.setText("");
                elevationTF.setText("");
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

    @Override
    public void clearForm() {
    }
}
