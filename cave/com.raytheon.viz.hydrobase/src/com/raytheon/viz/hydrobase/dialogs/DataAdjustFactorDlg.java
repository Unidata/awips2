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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.DataAdjustFactorData;
import com.raytheon.viz.hydrocommon.datamanager.DataAdjustFactorDataManager;
import com.raytheon.viz.hydrocommon.datamanager.DataIngestFilterDataManager;
import com.raytheon.viz.hydrocommon.datamanager.DataTrashCanDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Data Adjustment Factors dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation.
 * Dec 15, 2008 1787        askripsk    Add DB connectivity.
 * Mar 05, 2010 1928        mpduff      Added additional form validation.
 * Oct 26, 2010 5937        Judy Wang   Converted lower case to upper
 *                                      case in Location box.
 * Apr 18, 2013 1790        rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class DataAdjustFactorDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataAdjustFactorDlg.class);

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
     * Duration combo box.
     */
    private Combo durationCbo;

    /**
     * Type source combo box.
     */
    private Combo typeSrcCbo;

    /**
     * Extremum combo box.
     */
    private Combo extremumCbo;

    /**
     * Physical Element list control.
     */
    private List peList;

    /**
     * Divisor text control.
     */
    private Text divisorTF;

    /**
     * Base text control.
     */
    private Text baseTF;

    /**
     * Multiplier text control.
     */
    private Text multiplierTF;

    /**
     * Adder text control.
     */
    private Text adderTF;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public DataAdjustFactorDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Data Adjustment Factors");
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
        setReturnValue(false);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createSummaryGroup();
        createSelectedItemGroup();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();

        loadStaticData();

        getDialogData(true);
    }

    /**
     * Create the summary group and controls.
     */
    private void createSummaryGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group summaryGroup = new Group(shell, SWT.NONE);
        summaryGroup.setLayout(new GridLayout(1, false));
        summaryGroup.setLayoutData(gd);
        summaryGroup
                .setText(" Summary by Location of Data Adjustment Factors ");

        gd = new GridData();
        gd.horizontalIndent = 5;
        Label listLbl = new Label(summaryGroup, SWT.NONE);
        listLbl.setText(getLocationListLabel());
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
                updateSelectedInformation();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label adjustValLbl = new Label(summaryGroup, SWT.CENTER);
        adjustValLbl
                .setText("Adjusted Value = (((Raw Value / Divisor) + Base) * Multiplier) + Adder");
        adjustValLbl.setFont(controlFont);
        adjustValLbl.setLayoutData(gd);
    }

    /**
     * Create the selected item group and controls.
     */
    private void createSelectedItemGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group selectedItemGroup = new Group(shell, SWT.NONE);
        selectedItemGroup.setLayout(new GridLayout(4, false));
        selectedItemGroup.setLayoutData(gd);
        selectedItemGroup
                .setText(" Summary by Location of Data Adjustment Factors ");

        // ------------------------------------------------------
        // Create the location text, combo, and list controls
        // ------------------------------------------------------

        // Location
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label locationLbl = new Label(selectedItemGroup, SWT.RIGHT);
        locationLbl.setText("Location:");
        locationLbl.setLayoutData(gd);

        gd = new GridData(55, SWT.DEFAULT);
        locationTF = new Text(selectedItemGroup, SWT.BORDER);
        locationTF.setLayoutData(gd);
        locationTF.setTextLimit(8);
        locationTF.addListener(SWT.Verify, new Listener() {
            public void handleEvent(Event e) {
                String newStr = e.text;
                char[] newChars = new char[newStr.length()];
                newStr.getChars(0, newChars.length, newChars, 0);
                for (int i = 0; i < newChars.length; i++) {
                    if (!('0' <= newChars[i] && newChars[i] <= '9')
                            && !('a' <= newChars[i] && newChars[i] <= 'z')
                            && !('A' <= newChars[i] && newChars[i] <= 'Z')) {
                        e.doit = false;
                    }
                }
                e.text = e.text.toUpperCase();
            }
        });
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.verticalSpan = 4;
        Label filler = new Label(selectedItemGroup, SWT.NONE);
        filler.setLayoutData(gd);

        // Physical Element label
        Label peLbl = new Label(selectedItemGroup, SWT.RIGHT);
        peLbl.setText("Physical Element:");

        // Duration
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label durationLbl = new Label(selectedItemGroup, SWT.RIGHT);
        durationLbl.setText("Duration:");
        durationLbl.setLayoutData(gd);

        durationCbo = new Combo(selectedItemGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        // TODO: need to populate list with real data...
        durationCbo.add("Instantaneous (0)");
        durationCbo.add("1 Minute (1)");
        durationCbo.select(0);

        gd = new GridData(250, 125);
        gd.verticalSpan = 3;
        peList = new List(selectedItemGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        peList.setLayoutData(gd);
        peList.setFont(controlFont);

        // Type Source
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label typeSrcLbl = new Label(selectedItemGroup, SWT.RIGHT);
        typeSrcLbl.setText("Type Source:");
        typeSrcLbl.setLayoutData(gd);

        typeSrcCbo = new Combo(selectedItemGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        // TODO: need to populate list with real data...
        typeSrcCbo.add("Contingency Forecast (CF)");
        typeSrcCbo.add("Nonspecific Cntngncy (CZ)");
        typeSrcCbo.select(0);

        // Extremum
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label extremumLbl = new Label(selectedItemGroup, SWT.RIGHT);
        extremumLbl.setText("Extremum:");
        extremumLbl.setLayoutData(gd);

        extremumCbo = new Combo(selectedItemGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        // TODO: need to populate list with real data...
        extremumCbo.add("Maximum of 1 Hour (D)");
        extremumCbo.add("Maximum of 3 Hours (E)");
        extremumCbo.select(0);

        // ------------------------------------------------
        // Create the divisor, base, multiplier, and adder
        // controls.
        // ------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 4;
        Composite bottomComp = new Composite(selectedItemGroup, SWT.NONE);
        bottomComp.setLayout(new GridLayout(4, true));
        bottomComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label divisorLbl = new Label(bottomComp, SWT.CENTER);
        divisorLbl.setText("Divisor:");
        divisorLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label baseLbl = new Label(bottomComp, SWT.CENTER);
        baseLbl.setText("Base:");
        baseLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label multiplierLbl = new Label(bottomComp, SWT.CENTER);
        multiplierLbl.setText("Multiplier:");
        multiplierLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label adderLbl = new Label(bottomComp, SWT.CENTER);
        adderLbl.setText("Adder:");
        adderLbl.setLayoutData(gd);

        int textControlWidth = 80;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = textControlWidth;
        divisorTF = new Text(bottomComp, SWT.BORDER);
        divisorTF.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = textControlWidth;
        baseTF = new Text(bottomComp, SWT.BORDER);
        baseTF.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = textControlWidth;
        multiplierTF = new Text(bottomComp, SWT.BORDER);
        multiplierTF.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = textControlWidth;
        adderTF = new Text(bottomComp, SWT.BORDER);
        adderTF.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 120;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button updateInsertBtn = new Button(buttonComp, SWT.PUSH);
        updateInsertBtn.setText("Update/Insert");
        updateInsertBtn.setLayoutData(gd);
        updateInsertBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button deleteBtn = new Button(buttonComp, SWT.PUSH);
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
     * Get the label text for the location list control.
     * 
     * @return Label text.
     */
    private String getLocationListLabel() {
        String format = "%S  %S  %S   %S  %S      %S         %S      %S       %S";

        String labelStr = String.format(format, "Location", "PE", "Dur", "TS",
                "Ext", "Divisor", "Base", "Multiplier", "Adder");

        return labelStr;
    }

    /**
     * Retrieves the adjust factor data from the db
     */
    private void getDialogData(boolean force) {
        DataAdjustFactorDataManager man = DataAdjustFactorDataManager
                .getInstance();

        locationList.removeAll();

        try {
            for (DataAdjustFactorData currData : man.getAdjustFactorData(force)) {
                locationList.add(man.getAdjustFactorString(currData));
            }

            if (locationList.getItemCount() > 0) {
                locationList.select(0);
                updateSelectedInformation();
            }

        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Problem getting dialog data ", e);
        }

    }

    /**
     * Loads Duration, TS, Extremum and PE data from DB into Dialog.
     */
    private void loadStaticData() {
        DataIngestFilterDataManager man = DataIngestFilterDataManager
                .getInstance();

        try {
            // Load Duration
            durationCbo.removeAll();
            for (String currDur : man.getShefDur()) {
                durationCbo.add(currDur);
            }
            durationCbo.select(0);

            // Load Type Source
            typeSrcCbo.removeAll();
            for (String currTs : man.getShefTs()) {
                typeSrcCbo.add(currTs);
            }
            typeSrcCbo.select(0);

            // Load Extremum
            extremumCbo.removeAll();
            for (String currExt : man.getShefExtremum()) {
                extremumCbo.add(currExt);
            }
            extremumCbo.select(0);

            // Load Physical Element Lists
            peList.removeAll();
            for (String currPE : DataTrashCanDataManager.getInstance()
                    .getPEList()) {
                peList.add(currPE);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Problem loading static data ", e);
        }
    }

    /**
     * Validates data and saves record to DB.
     */
    private void saveRecord() {
        DataAdjustFactorData currData = new DataAdjustFactorData();

        // LID
        if (locationTF.getText().length() < 1) {
            showMessage("Invalid Location", "Please enter a valid location.");
            return;
        }
        currData.setLid(locationTF.getText());

        // Duration
        currData.setDuration(getSelectedIntValue(durationCbo));

        // TS
        currData.setTypeSource(getSelectedStringValue(typeSrcCbo));

        // Extremum
        currData.setExtremum(getSelectedStringValue(extremumCbo));

        // PE
        if (peList.getSelectionCount() != 1) {
            showMessage("Select PE",
                    "A Physical Element must be selected from the list.");
            return;
        }
        String selectedPE = peList.getSelection()[0];
        currData.setPe(selectedPE.split(" ")[0]);

        Double temp = null;
        // Divisor
        temp = HydroDataUtils.getDoubleFromTF(shell, divisorTF, "Divisor");
        if (temp == null) {
            return;
        }
        currData.setDivisor(temp);

        // Base
        temp = HydroDataUtils.getDoubleFromTF(shell, baseTF, "Base");
        if (temp == null) {
            return;
        }
        currData.setBase(temp);

        // Multiplier
        temp = HydroDataUtils
                .getDoubleFromTF(shell, multiplierTF, "Multiplier");
        if (temp == null) {
            return;
        }
        currData.setMultiplier(temp);

        // Adder
        temp = HydroDataUtils.getDoubleFromTF(shell, adderTF, "Adder");
        if (temp == null) {
            return;
        }
        currData.setAdder(temp);

        try {
            HydroDBDataManager.getInstance().putData(currData);

            getDialogData(true);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Probem saving record ");
        }
    }

    /**
     * Updates the Summary data for the currently selected data.
     */
    private void updateSelectedInformation() {
        DataAdjustFactorData currData = DataAdjustFactorDataManager
                .getInstance()
                .getSelectedData(locationList.getSelectionIndex());

        // LID
        locationTF.setText(currData.getLid());

        // Dur
        durationCbo.select(0);
        for (int i = 0; i < durationCbo.getItemCount(); i++) {
            if (durationCbo.getItem(i).contains(
                    "(" + currData.getDuration() + ")")) {
                durationCbo.select(i);
                break;
            }
        }

        // TypeSrc
        typeSrcCbo.select(0);
        for (int i = 0; i < typeSrcCbo.getItemCount(); i++) {
            if (typeSrcCbo.getItem(i).contains(
                    "(" + currData.getTypeSource() + ")")) {
                typeSrcCbo.select(i);
                break;
            }
        }

        // Extremum
        extremumCbo.select(0);
        for (int i = 0; i < extremumCbo.getItemCount(); i++) {
            if (extremumCbo.getItem(i).contains(
                    "(" + currData.getExtremum() + ")")) {
                extremumCbo.select(i);
                break;
            }
        }

        // Physical Element
        peList.select(0);
        for (int i = 0; i < peList.getItemCount(); i++) {
            if (peList.getItem(i).split(" ")[0].equals(currData.getPe())) {
                peList.select(i);
                break;
            }
        }

        // Divisor
        divisorTF
                .setText(HydroDataUtils.getDisplayString(currData.getDivisor()));

        // Base
        baseTF.setText(HydroDataUtils.getDisplayString(currData.getBase()));

        // Multiplier
        multiplierTF.setText(HydroDataUtils.getDisplayString(currData
                .getMultiplier()));

        // Adder
        adderTF.setText(HydroDataUtils.getDisplayString(currData.getAdder()));
    }

    /**
     * Parses out the DB value out of the display value.
     * 
     * @return The DB value for the display string.
     */
    private int getSelectedIntValue(Combo currCombo) {
        int rval = HydroConstants.MISSING_VALUE;

        if (currCombo.getSelectionIndex() >= 0) {
            // Build regex for Mileage part of detail
            String durRegex = "\\((\\d*)\\)";
            Pattern durPattern = Pattern.compile(durRegex);

            Matcher durMatcher = durPattern.matcher(currCombo.getItem(currCombo
                    .getSelectionIndex()));

            // Find the Duration
            if (durMatcher.find()) {
                rval = Integer.parseInt(durMatcher.group(1));
            }
        }

        return rval;
    }

    /**
     * Parses out the DB value out of the display value.
     * 
     * @return The DB value for the display string.
     */
    private String getSelectedStringValue(Combo currCombo) {
        String rval = "";

        if (currCombo.getSelectionIndex() >= 0) {
            // Build regex for Mileage part of detail
            String durRegex = "\\((\\w+)\\)";
            Pattern durPattern = Pattern.compile(durRegex);

            Matcher durMatcher = durPattern.matcher(currCombo.getItem(currCombo
                    .getSelectionIndex()));

            // Find the Duration
            if (durMatcher.find()) {
                rval = durMatcher.group(1);
            }
        }

        return rval;
    }

    /**
     * Prompts the user for confirmation and then deletes the currently selected
     * item.
     */
    private void deleteRecord() {
        if (locationList.getSelectionCount() > 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete this entry?");
            int results = mb.open();

            if (results == SWT.OK) {
                DataAdjustFactorData currData = DataAdjustFactorDataManager
                        .getInstance().getSelectedData(
                                locationList.getSelectionIndex());

                try {
                    HydroDBDataManager.getInstance().deleteRecord(currData);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Problem deleting record ", e);
                }

                getDialogData(true);
            }
        } else {
            showMessage("Invalid Selection",
                    "Please select an ingest filter first.");
        }
    }

    /**
     * Display error message.
     * 
     * @param title
     * @param message
     */
    private void showMessage(String title, String message) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setText(title);
        mb.setMessage(message);
        mb.open();
    }

}
