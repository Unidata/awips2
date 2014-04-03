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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.DataLimitData;
import com.raytheon.viz.hydrocommon.data.LocationData;
import com.raytheon.viz.hydrocommon.data.LocationDataLimitData;
import com.raytheon.viz.hydrocommon.datamanager.DataTrashCanDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.datamanager.QcAlertAlarmLimitsDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * this class displays the QC Alert and Alarm dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 5, 2008				lvenable	Initial creation.
 * Dec 8, 2008  1744        askripsk    Connect to DB.
 * May 5, 2009  2181        mpduff      Keep selection upon submit.
 * Jun 16,2010  5526        lbousaidi   Start/End date not correct
 * Oct 27,2011  11305       lbousaidi   change some logic to have physical
 * 										elements matches the selection of default limits
 * Apr 19, 2013 1790        rferrel     Make dialog non-blocking.
 * Nov 26, 2013 15800       wkwock      Fix unhandled event loop 
 * Jan 07, 2013 16643       snaples     Fixed changeFormat to use string formatting instead of converting to Date.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class QcAlertAlarmLimitsDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(QcAlertAlarmLimitsDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Limit combo box.
     */
    private Combo limitCbo;

    /**
     * Location check box.
     */
    private Button locationChk;

    /**
     * Location limit text control.
     */
    private Text locationLimitTF;

    /**
     * Physical element check box.
     */
    private Button physElemChk;

    /**
     * Physical element list control.
     */
    private List physElemList;

    /**
     * Limits list control.
     */
    private List limitsList;

    /**
     * Location selected item text control.
     */
    private Text locationSelItemTF;

    /**
     * Duration combo box.
     */
    private Combo durationCbo;

    /**
     * Start data text control.
     */
    private Text startDateTF;

    /**
     * End data text control.
     */
    private Text endDateTF;

    /**
     * Physical element selected item list.
     */
    private List physElemSelItemList;

    /**
     * Gross range minimum text control.
     */
    private Text grossRangeMinTF;

    /**
     * Gross range maximum text control.
     */
    private Text grossRangeMaxTF;

    /**
     * Reasonable range minimum text control.
     */
    private Text reasonableRangeMinTF;

    /**
     * Reasonable range maximum text control.
     */
    private Text reasonableRangeMaxTF;

    /**
     * Rate of change text control.
     */
    private Text rateOfChangeTF;

    /**
     * Alert upper text control.
     */
    private Text alertUpperTF;

    /**
     * Alert lower text control.
     */
    private Text alertLowerTF;

    /**
     * Alert rate of change text control.
     */
    private Text alertRocTF;

    /**
     * Alert difference text control.
     */
    private Text alertDiffTF;

    /**
     * Alarm upper text control.
     */
    private Text alarmUpperTF;

    /**
     * Alarm lower text control.
     */
    private Text alarmLowerTF;

    /**
     * Alarm rate of change text control.
     */
    private Text alarmRocTF;

    /**
     * Alarm difference text control.
     */
    private Text alarmDiffTF;

    /**
     * Apply button.
     */
    private Button applyBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * The currently selected Limit group
     */
    private Group limitSelectedGroup;

    /**
     * Date validator.
     */
    private SimpleDateFormat dateFormat1, dateFormat2;

    /**
     * Selection index.
     */
    private int selection = -9999;

    /**
     * States for the dialog.
     */
    private enum DialogStates {
        NEW_LOCATION_ENTRY, NEW_DEFAULT_ENTRY, NORMAL_MODE, NO_ENTRIES, DEFAULT_LIMITS, LOCATION_LIMITS
    };

    /**
     * System wait cursor no need to dispose.
     */
    private Cursor waitCursor;

    /**
     * Non-blocking Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public QcAlertAlarmLimitsDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Quality Control and Alert/Alarm Limits");

        dateFormat1 = new SimpleDateFormat("MM/dd");
        dateFormat1.setTimeZone(TimeZone.getTimeZone("GMT"));

        dateFormat2 = new SimpleDateFormat("MM-dd");
        dateFormat2.setTimeZone(TimeZone.getTimeZone("GMT"));

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

        waitCursor = shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);

        // Initialize all of the controls and layouts
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createLimitsGroup();
        createLimitsSelectedGroup();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();

        // Load the data for the dialog
        initializeData();
    }

    /**
     * Create Limits group and controls.
     */
    private void createLimitsGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group limitGroup = new Group(shell, SWT.NONE);
        limitGroup.setLayout(new GridLayout(1, false));
        limitGroup.setLayoutData(gd);
        limitGroup.setText(" Limits ");

        // ------------------------------------------------
        // Create top controls
        // ------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite topComp = new Composite(limitGroup, SWT.NONE);
        topComp.setLayout(new GridLayout(7, false));
        topComp.setLayoutData(gd);

        Label listLabel = new Label(topComp, SWT.NONE);
        listLabel.setText("List:");

        limitCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        limitCbo.add("Default Limits");
        limitCbo.add("Location Limits");
        limitCbo.select(0);
        limitCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (limitCbo.getSelectionIndex() == 0) {
                    locationChk.setSelection(false);
                    locationChk.setEnabled(false);
                    locationLimitTF.setText("");
                    locationLimitTF.setEnabled(false);

                    updateDialogState(DialogStates.DEFAULT_LIMITS);
                } else {
                    locationChk.setEnabled(true);

                    updateDialogState(DialogStates.LOCATION_LIMITS);
                }

                loadData();
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label filterByLabel = new Label(topComp, SWT.RIGHT);
        filterByLabel.setText("Filter By:");
        filterByLabel.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 5;
        locationChk = new Button(topComp, SWT.CHECK);
        locationChk.setText("Location");
        locationChk.setEnabled(false);
        locationChk.setLayoutData(gd);
        locationChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (locationChk.getSelection() == true) {
                    locationLimitTF.setEnabled(true);
                } else {
                    locationLimitTF.setEnabled(false);
                }

                loadData();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        locationLimitTF = new Text(topComp, SWT.BORDER);
        locationLimitTF.setLayoutData(gd);
        locationLimitTF.setEnabled(false);
        locationLimitTF.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
            }

            @Override
            public void keyReleased(KeyEvent e) {
                loadData();
            }
        });

        gd = new GridData();
        gd.horizontalIndent = 5;
        physElemChk = new Button(topComp, SWT.CHECK);
        physElemChk.setText("PhysElem");
        physElemChk.setLayoutData(gd);
        physElemChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                loadData();
            }

        });

        gd = new GridData(300, 125);
        gd.verticalSpan = 2;
        physElemList = new List(topComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        physElemList.setLayoutData(gd);
        physElemList.setFont(controlFont);
        physElemList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Only filter display on change if currently selected as a
                // filter option
                if (physElemChk.getSelection()) {
                    loadData();
                }
            }

        });

        gd = new GridData();
        gd.horizontalSpan = 6;
        Label notesLbl = new Label(topComp, SWT.NONE);
        notesLbl.setText(getNotesText());
        notesLbl.setFont(controlFont);
        notesLbl.setLayoutData(gd);

        // --------------------------------------------------
        // Create bottom List and labels
        // --------------------------------------------------
        gd = new GridData();
        gd.horizontalIndent = 4;
        Label topListLbl = new Label(limitGroup, SWT.NONE);
        topListLbl.setText(getListLabelTopText());
        topListLbl.setFont(controlFont);
        topListLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 4;
        Label bottomListLbl = new Label(limitGroup, SWT.NONE);
        bottomListLbl.setText(getListLabelBottomText());
        bottomListLbl.setFont(controlFont);
        bottomListLbl.setLayoutData(gd);

        gd = new GridData(1100, 125);
        limitsList = new List(limitGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        limitsList.setLayoutData(gd);
        limitsList.setFont(controlFont);
        limitsList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getSelectedLimit();
            }
        });
    }

    /**
     * Create Limits Selected group and controls.
     */
    private void createLimitsSelectedGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        limitSelectedGroup = new Group(shell, SWT.NONE);
        limitSelectedGroup.setLayout(new GridLayout(3, false));
        limitSelectedGroup.setLayoutData(gd);
        limitSelectedGroup.setText("Limits For Selected Item ");

        // -----------------------------------------------------
        // Create the location, duration, and date controls
        // -----------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.TOP, true, true);
        Composite leftComp = new Composite(limitSelectedGroup, SWT.NONE);
        leftComp.setLayout(new GridLayout(2, false));
        leftComp.setLayoutData(gd);

        // Location
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label locationLbl = new Label(leftComp, SWT.RIGHT);
        locationLbl.setText("Location:");
        locationLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        locationSelItemTF = new Text(leftComp, SWT.BORDER);
        locationSelItemTF.setLayoutData(gd);
        locationSelItemTF.setTextLimit(8);

        // Duration
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label durationLbl = new Label(leftComp, SWT.RIGHT);
        durationLbl.setText("Duration:");
        durationLbl.setLayoutData(gd);

        durationCbo = new Combo(leftComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        durationCbo.setLayoutData(gd);

        // Start Date
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label startDateLbl = new Label(leftComp, SWT.RIGHT);
        startDateLbl.setText("Start MM/DD:");
        startDateLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        startDateTF = new Text(leftComp, SWT.BORDER);
        startDateTF.setLayoutData(gd);
        startDateTF.setTextLimit(5);

        // End Date
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label endDateLbl = new Label(leftComp, SWT.RIGHT);
        endDateLbl.setText("End MM/DD:");
        endDateLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        endDateTF = new Text(leftComp, SWT.BORDER);
        endDateTF.setLayoutData(gd);
        endDateTF.setTextLimit(5);

        // -----------------------------------------------------
        // Create the physical element controls
        // -----------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalAlignment = SWT.CENTER;
        Composite centerComp = new Composite(limitSelectedGroup, SWT.NONE);
        centerComp.setLayout(new GridLayout(1, false));
        centerComp.setLayoutData(gd);

        Label peLbl = new Label(centerComp, SWT.NONE);
        peLbl.setText("Physical Element:");

        gd = new GridData(200, 220);
        physElemSelItemList = new List(centerComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        physElemSelItemList.setLayoutData(gd);
        physElemSelItemList.setFont(controlFont);

        // -----------------------------------------------------
        // Create the QC and Alarm/Alert controls
        // -----------------------------------------------------
        Composite rightComp = new Composite(limitSelectedGroup, SWT.NONE);
        rightComp.setLayout(new GridLayout(1, false));

        // QC Group
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group qcGroup = new Group(rightComp, SWT.NONE);
        qcGroup.setLayout(new GridLayout(3, false));
        qcGroup.setLayoutData(gd);
        qcGroup.setText(" Quality Control Limits ");

        // Filler Label
        new Label(qcGroup, SWT.NONE);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label minLbl = new Label(qcGroup, SWT.CENTER);
        minLbl.setText("Min");
        minLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label maxLbl = new Label(qcGroup, SWT.CENTER);
        maxLbl.setText("Max");
        maxLbl.setLayoutData(gd);

        // Gross Range
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label grossRangeLbl = new Label(qcGroup, SWT.RIGHT);
        grossRangeLbl.setText("Gross Range:");
        grossRangeLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        grossRangeMinTF = new Text(qcGroup, SWT.BORDER);
        grossRangeMinTF.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        grossRangeMaxTF = new Text(qcGroup, SWT.BORDER);
        grossRangeMaxTF.setLayoutData(gd);

        // Reasonable Range
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label reasonableLbl = new Label(qcGroup, SWT.RIGHT);
        reasonableLbl.setText("Reasonable Range:");
        reasonableLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        reasonableRangeMinTF = new Text(qcGroup, SWT.BORDER);
        reasonableRangeMinTF.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        reasonableRangeMaxTF = new Text(qcGroup, SWT.BORDER);
        reasonableRangeMaxTF.setLayoutData(gd);

        // Rate of Change
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label rateOfChangeLbl = new Label(qcGroup, SWT.RIGHT);
        rateOfChangeLbl.setText("Rate of Change:");
        rateOfChangeLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        rateOfChangeTF = new Text(qcGroup, SWT.BORDER);
        rateOfChangeTF.setLayoutData(gd);

        Label unitHourLbl = new Label(qcGroup, SWT.RIGHT);
        unitHourLbl.setText("Units/Hour");

        // Alert & Alarm Group
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group aaGroup = new Group(rightComp, SWT.NONE);
        aaGroup.setLayout(new GridLayout(5, false));
        aaGroup.setLayoutData(gd);
        aaGroup.setText(" Alert/Alarm Limits ");

        // Filler Label
        new Label(aaGroup, SWT.NONE);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label upperLbl = new Label(aaGroup, SWT.CENTER);
        upperLbl.setText("Upper");
        upperLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label lowerLbl = new Label(aaGroup, SWT.CENTER);
        lowerLbl.setText("Lower");
        lowerLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label rocLbl = new Label(aaGroup, SWT.CENTER);
        rocLbl.setText("ROC");
        rocLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label diffLbl = new Label(aaGroup, SWT.CENTER);
        diffLbl.setText("Diff");
        diffLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label alertLbl = new Label(aaGroup, SWT.RIGHT);
        alertLbl.setText("Alert:");
        alertLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        alertUpperTF = new Text(aaGroup, SWT.BORDER);
        alertUpperTF.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        alertLowerTF = new Text(aaGroup, SWT.BORDER);
        alertLowerTF.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        alertRocTF = new Text(aaGroup, SWT.BORDER);
        alertRocTF.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        alertDiffTF = new Text(aaGroup, SWT.BORDER);
        alertDiffTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label alarmLbl = new Label(aaGroup, SWT.RIGHT);
        alarmLbl.setText("Alarm:");
        alarmLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        alarmUpperTF = new Text(aaGroup, SWT.BORDER);
        alarmUpperTF.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        alarmLowerTF = new Text(aaGroup, SWT.BORDER);
        alarmLowerTF.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        alarmRocTF = new Text(aaGroup, SWT.BORDER);
        alarmRocTF.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        alarmDiffTF = new Text(aaGroup, SWT.BORDER);
        alarmDiffTF.setLayoutData(gd);
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
        Button okBtn = new Button(buttonComp, SWT.PUSH);
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
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
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
        Button newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                newEntry();
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
     * Get the text for the notes label.
     * 
     * @return The label text.
     */
    private String getNotesText() {
        StringBuilder str = new StringBuilder();

        str.append("Notes:\n");
        str.append("1) Individual check is not performed if the limit value is not defined.\n");
        str.append("2) If the limits defined for location, default limits not considered\n");
        str.append("   even if location limits are undefined.");

        return str.toString();
    }

    /**
     * Get the text for the label for the top list control.
     * 
     * @return The label text.
     */
    private String getListLabelTopText() {
        String fmt = "                                               %S           %S    %S"
                + "       %S              %S";

        String text = String.format(fmt, "Gross", "Reasonable", "Rate of",
                "Alert Limit", "Alarm Limit");

        return text;
    }

    /**
     * Get the text for the label for the bottom list control.
     * 
     * @return The label text.
     */
    private String getListLabelBottomText() {
        String fmt = "%S   %S    %S   %S    %S        %S        %S     %S      %S   "
                + "%S  %S   %S   %S  %S  %S  %S   %S   %S";
        String text = String.format(fmt, "Location", "PE", "Dur", "Start",
                "End", "Min", "Max", "Min", "Max", "Change", "Upper", "Lower",
                "ROC", "Diff", "Upper", "Lower", "ROC", "Diff");

        return text;
    }

    /**
     * Get data to populate GUI.
     */
    private void initializeData() {

        setBusy(true);

        Job job = new Job("QcAlertAlarmLimits") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                // Populate PhysElem
                try {
                    final java.util.List<String> peList = DataTrashCanDataManager
                            .getInstance().getPEList();
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            for (String currPE : peList) {
                                physElemList.add(currPE);
                                physElemSelItemList.add(currPE);

                            }
                        }
                    });

                    final java.util.List<String> durList = QcAlertAlarmLimitsDataManager
                            .getInstance().getShefDur();

                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            durationCbo.setItems(durList.toArray(new String[0]));
                            updateDialogState(DialogStates.DEFAULT_LIMITS);
                            loadData();
                        }
                    });
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to load data. ", e);
                } finally {

                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            setBusy(false);
                        }
                    });
                }

                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }

    private void setBusy(boolean busy) {
        Cursor cursor = null;
        if (busy) {
            cursor = waitCursor;
        }
        shell.setCursor(cursor);
    }

    private void loadData() {
        loadData(false);
    }

    private void loadData(boolean forceLoad) {
        limitsList.removeAll();

        String dayStartChange, dayEndChange;
        try {
            if (currentlyDisplayingDefaultLimits()) {
                QcAlertAlarmLimitsDataManager man = QcAlertAlarmLimitsDataManager
                        .getInstance();
                for (DataLimitData currData : man
                        .getDefaultLimits(physElemChk.getSelection(),
                                getSelectedPEs(), forceLoad)) {

                    // Start/End dates format will be changed to MM/DD only for
                    // display

                    dayStartChange = changeFormat(currData.getMonthDayStart(),
                            dateFormat2, dateFormat1);
                    dayEndChange = changeFormat(currData.getMonthDayEnd(),
                            dateFormat2, dateFormat1);
                    currData.setMonthDayStart(dayStartChange);
                    currData.setMonthDayEnd(dayEndChange);

                    limitsList.add(man.getDefaultLimitString(currData));

                    dayStartChange = changeFormat(currData.getMonthDayStart(),
                            dateFormat1, dateFormat2);
                    dayEndChange = changeFormat(currData.getMonthDayEnd(),
                            dateFormat1, dateFormat2);
                    currData.setMonthDayStart(dayStartChange);
                    currData.setMonthDayEnd(dayEndChange);

                    updateDefaultInformationDisplay(currData);

                }
                if (!physElemChk.getSelection()) {
                    limitsList.setSelection(0);
                    getSelectedLimit();
                }
            } else {
                QcAlertAlarmLimitsDataManager man = QcAlertAlarmLimitsDataManager
                        .getInstance();
                for (LocationDataLimitData currData : man
                        .getLocationLimits(locationChk.getSelection(),
                                locationLimitTF.getText(),
                                physElemChk.getSelection(), getSelectedPEs(),
                                forceLoad)) {
                    dayStartChange = changeFormat(currData.getMonthDayStart(),
                            dateFormat2, dateFormat1);
                    dayEndChange = changeFormat(currData.getMonthDayEnd(),
                            dateFormat2, dateFormat1);
                    currData.setMonthDayStart(dayStartChange);
                    currData.setMonthDayEnd(dayEndChange);

                    limitsList.add(man.getLocationLimitString(currData));

                    dayStartChange = changeFormat(currData.getMonthDayStart(),
                            dateFormat1, dateFormat2);
                    dayEndChange = changeFormat(currData.getMonthDayEnd(),
                            dateFormat1, dateFormat2);
                    currData.setMonthDayStart(dayStartChange);
                    currData.setMonthDayEnd(dayEndChange);
                }

                limitsList.setSelection(0);
                getSelectedLimit();
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to load data. ", e);
        }

        if (limitsList.getItemCount() > 0) {
            updateDialogState(DialogStates.NORMAL_MODE);
        } else {
            updateDialogState(DialogStates.NO_ENTRIES);
        }
    }

    /**
     * Returns the selected PEs to filter by
     * 
     * @return
     */
    private ArrayList<String> getSelectedPEs() {
        int[] selectedInd = physElemList.getSelectionIndices();
        ArrayList<String> peFilter = new ArrayList<String>();

        for (int i : selectedInd) {
            peFilter.add(physElemList.getItem(i).split(" ")[0].toUpperCase());
        }

        return peFilter;
    }

    private void newEntry() {
        // Clear selection in main list
        limitsList.deselectAll();

        // Clear form
        clearInformation();

        // Update form for new entry mode
        if (currentlyDisplayingDefaultLimits()) {
            updateDialogState(DialogStates.NEW_DEFAULT_ENTRY);
        } else {
            updateDialogState(DialogStates.NEW_LOCATION_ENTRY);
        }
    }

    private void updateDialogState(DialogStates currentDialogState) {
        switch (currentDialogState) {
        case NEW_LOCATION_ENTRY:
            // Used when the NEW button is pressed
            limitSelectedGroup.setText("NEW Item");

            // Set the Duration to 0
            durationCbo.select(0);

            limitSelectedGroup.setEnabled(true);
            physElemSelItemList.setEnabled(true);
            deleteBtn.setEnabled(false);
            break;
        case NEW_DEFAULT_ENTRY:
            // No location information for default data limits
            locationSelItemTF.setEnabled(false);

            // Used when the NEW button is pressed
            limitSelectedGroup.setText("NEW Item");

            // Set the Duration to 0
            durationCbo.select(0);

            limitSelectedGroup.setEnabled(true);
            physElemSelItemList.setEnabled(true);
            deleteBtn.setEnabled(false);
            break;
        case NORMAL_MODE:
            deleteBtn.setEnabled(true);
            limitSelectedGroup.setEnabled(true);
            physElemSelItemList.setEnabled(true);
            break;
        case NO_ENTRIES:
            deleteBtn.setEnabled(false);
            limitSelectedGroup.setEnabled(false);
            physElemSelItemList.setEnabled(false);
            break;
        case DEFAULT_LIMITS:
            locationChk.setEnabled(false);
            locationLimitTF.setEnabled(false);
            locationSelItemTF.setEnabled(false);
            break;
        case LOCATION_LIMITS:
            locationChk.setEnabled(true);
            locationLimitTF.setEnabled(true);
            locationSelItemTF.setEnabled(true);
            break;
        default:
            break;
        }
    }

    private void getSelectedLimit() {
        limitSelectedGroup.setText("Limits For Selected Item");

        if (currentlyDisplayingDefaultLimits()) {
            DataLimitData currData = QcAlertAlarmLimitsDataManager
                    .getInstance().getSelectedDefaultData(
                            limitsList.getSelectionIndex());

            updateDefaultInformationDisplay(currData);
        } else {
            LocationDataLimitData currData = QcAlertAlarmLimitsDataManager
                    .getInstance().getSelectedLocationData(
                            limitsList.getSelectionIndex());

            updateLocationInformationDisplay(currData);
        }

    }

    private boolean currentlyDisplayingDefaultLimits() {
        return limitCbo.getItem(limitCbo.getSelectionIndex()).contains(
                "Default");
    }

    private void updateDefaultInformationDisplay(DataLimitData currData) {
        String dateSringStart, dateStringEnd;
        clearInformation();

        // No Lid for default limits
        locationSelItemTF.setText("");

        // Get the Duration
        String currDur = Integer.toString(currData.getDur());
        for (int i = 0; i < durationCbo.getItemCount(); i++) {
            if (durationCbo.getItem(i).contains(currDur)) {
                durationCbo.select(i);
                break;
            }
        }

        /**
         * Get the Start/End dates and change the format from "MM-DD" to "MM/DD"
         * 
         * only for the display.
         */

        try {
            if (currData.getMonthDayStart() != null) {
                dateSringStart = changeFormat(currData.getMonthDayStart(),
                        dateFormat2, dateFormat1);
                startDateTF.setText(dateSringStart);
            }
        } catch (Exception e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Date");
            mb.setMessage("Please enter a valid Start date");

            mb.open();

        }

        try {

            if (currData.getMonthDayEnd() != null) {
                dateStringEnd = changeFormat(currData.getMonthDayEnd(),
                        dateFormat2, dateFormat1);
                endDateTF.setText(dateStringEnd);
            }
        } catch (Exception e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Date");
            mb.setMessage("Please enter a valid Start date");

            mb.open();
        }

        // Get the PE
        String currPE = currData.getPe();
        for (int i = 0; i < physElemSelItemList.getItemCount(); i++) {
            if (physElemSelItemList.getItem(i).contains(currPE)) {
                physElemSelItemList.setSelection(i);
                break;
            }

        }

        grossRangeMinTF.setText(HydroDataUtils.getDisplayString(currData
                .getGrossRangeMin()));
        grossRangeMaxTF.setText(HydroDataUtils.getDisplayString(currData
                .getGrossRangeMax()));
        reasonableRangeMinTF.setText(HydroDataUtils.getDisplayString(currData
                .getReasonRangeMin()));
        reasonableRangeMaxTF.setText(HydroDataUtils.getDisplayString(currData
                .getReasonRangeMax()));
        rateOfChangeTF.setText(HydroDataUtils.getDisplayString(currData
                .getRocMax()));

        alertUpperTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlertUpperLimit()));
        alertLowerTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlertLowerLimit()));
        alertRocTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlertRocLimit()));
        alertDiffTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlertDiffLimit()));

        alarmUpperTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlarmUpperLimit()));
        alarmLowerTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlarmLowerLimit()));
        alarmRocTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlarmRocLimit()));
        alarmDiffTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlarmDiffLimit()));
    }

    /**
     * Update the bottom portion of the screen with the data in the data passed
     * in.
     * 
     * @param currData
     */
    private void updateLocationInformationDisplay(LocationDataLimitData currData) {
        String dateSringStart, dateStringEnd;
        clearInformation();

        locationSelItemTF.setText(currData.getLid());

        // Get the Duration
        String currDur = Integer.toString(currData.getDur());
        for (int i = 0; i < durationCbo.getItemCount(); i++) {
            if (durationCbo.getItem(i).contains(currDur)) {
                durationCbo.select(i);
                break;
            }
        }

        /**
         * Get the Start/End dates and change the format from "MM-DD" to "MM/DD"
         * 
         * only for the display.
         */

        try {

            dateSringStart = changeFormat(currData.getMonthDayStart(),
                    dateFormat2, dateFormat1);
            startDateTF.setText(dateSringStart);
        } catch (Exception e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Date");
            mb.setMessage("Please enter a valid Start date");

            mb.open();

        }

        try {
            dateStringEnd = changeFormat(currData.getMonthDayEnd(),
                    dateFormat2, dateFormat1);
            endDateTF.setText(dateStringEnd);

        } catch (Exception e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Date");
            mb.setMessage("Please enter a valid Start date");

            mb.open();
        }

        // Get the PE
        String currPE = currData.getPe();
        for (int i = 0; i < physElemSelItemList.getItemCount(); i++) {
            if (physElemSelItemList.getItem(i).contains(currPE)) {
                physElemSelItemList.setSelection(i);
                break;
            }
        }

        grossRangeMinTF.setText(HydroDataUtils.getDisplayString(currData
                .getGrossRangeMin()));
        grossRangeMaxTF.setText(HydroDataUtils.getDisplayString(currData
                .getGrossRangeMax()));
        reasonableRangeMinTF.setText(HydroDataUtils.getDisplayString(currData
                .getReasonRangeMin()));
        reasonableRangeMaxTF.setText(HydroDataUtils.getDisplayString(currData
                .getReasonRangeMax()));
        rateOfChangeTF.setText(HydroDataUtils.getDisplayString(currData
                .getRocMax()));

        alertUpperTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlertUpperLimit()));
        alertLowerTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlertLowerLimit()));
        alertRocTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlertRocLimit()));
        alertDiffTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlertDiffLimit()));

        alarmUpperTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlarmUpperLimit()));
        alarmLowerTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlarmLowerLimit()));
        alarmRocTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlarmRocLimit()));
        alarmDiffTF.setText(HydroDataUtils.getDisplayString(currData
                .getAlarmDiffLimit()));
    }

    private void clearInformation() {
        locationSelItemTF.setText("");
        durationCbo.clearSelection();
        startDateTF.setText("");
        endDateTF.setText("");

        physElemSelItemList.deselectAll();

        grossRangeMinTF.setText("");
        grossRangeMaxTF.setText("");
        reasonableRangeMinTF.setText("");
        reasonableRangeMaxTF.setText("");
        rateOfChangeTF.setText("");

        alertUpperTF.setText("");
        alertLowerTF.setText("");
        alertRocTF.setText("");
        alertDiffTF.setText("");

        alarmUpperTF.setText("");
        alarmLowerTF.setText("");
        alarmRocTF.setText("");
        alarmDiffTF.setText("");
    }

    // change date format display

    private String changeFormat(String initDateFormat, SimpleDateFormat dateF1,
            SimpleDateFormat dateF2) {
        String finalDateFormat = null;

        if ((initDateFormat != null) && !(initDateFormat.equals(""))) {
                String dateformat1 = dateF1.toPattern();
                String dateformat2 = dateF2.toPattern();
                char ch1 = dateformat1.charAt(2);
                char ch2 = dateformat2.charAt(2);
                finalDateFormat = initDateFormat.replace(ch1, ch2);

        }

        return finalDateFormat;
    }

    private boolean saveRecord() {
        boolean success = false;

        if (currentlyDisplayingDefaultLimits()) {
            success = saveDefaultRecord();
        } else {
            // The lid needs to exist to add a new location data limit
            if (locationExists()) {
                success = saveLocationRecord();
            } else {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Location Does Not Exist");
                mb.setMessage("Please use Add Location from the Location Menu to add the location first.");
                mb.open();

                success = false;
            }
        }

        if (success) {
            // Reload the data keeping the selection
            selection = limitsList.getSelectionIndex();
            String currentItem=locationSelItemTF.getText();
            loadData(true);
            if (selection<0) {//must be new item
                int index = 0 ;
                for (index=0; index<limitsList.getItemCount();index++) {
                    String item = limitsList.getItem(index);
                    if (item.trim().split(" ")[0].compareToIgnoreCase(currentItem)==0)
                            break; //found the index
                }


                if (index>=limitsList.getItemCount())
                    selection=0;
                else
                    selection = index;
            }
            
            limitsList.setSelection(selection);
            getSelectedLimit();
        }

        return success;
    }

    private boolean locationExists() {
        boolean rval = false;
        String currLid = locationSelItemTF.getText();

        if (!currLid.equals("")) {
            LocationData locData = new LocationData();
            locData.setLid(currLid);
            try {
                rval = HydroDBDataManager.getInstance().checkData(locData) > 0;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, "Error in data check. ",
                        e);
            }
        }

        return rval;
    }

    private boolean saveDefaultRecord() {
        DataLimitData dataToSave = new DataLimitData();

        boolean saveSuccessful = setSaveValues(dataToSave);
        boolean rval = false;

        if (saveSuccessful) {
            try {
                HydroDBDataManager.getInstance().putData(dataToSave);

                rval = true;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, "Unable to save data. ",
                        e);
            }
        }

        return rval;
    }

    private boolean saveLocationRecord() {
        LocationDataLimitData dataToSave = new LocationDataLimitData();

        dataToSave.setLid(locationSelItemTF.getText());
        boolean saveSuccessful = setSaveValues(dataToSave);
        boolean rval = false;

        if (saveSuccessful) {
            try {
                HydroDBDataManager.getInstance().putData(dataToSave);

                rval = true;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, "Unable to save data. ",
                        e);
            }
        }

        return rval;
    }

    /**
     * Converts the value in a text field to a double.
     * 
     * @param currTF
     *            The text field to get the double value for.
     * @param tfName
     *            If there is an error in parsing the value, this name will be
     *            used to refer to the value in the error message displayed
     * @return The double value corresponding to the text field value
     */
    private Double getDoubleFromTF(Text currTF, String tfName) {
        Double rval = null;

        String tfVal = currTF.getText();

        if (!tfVal.equals("")) {
            try {
                rval = Double.parseDouble(tfVal);
            } catch (NumberFormatException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Value");
                mb.setMessage("Please enter a valid numeric value for "
                        + tfName);
                mb.open();
            }
        } else {
            rval = Double.valueOf(HydroConstants.MISSING_VALUE);
        }

        return rval;
    }

    private String getSelectedInformationPE() {
        String rval = "";

        String selectedPE = physElemSelItemList.getSelection()[0];

        rval = selectedPE.split(" ")[0];

        return rval;
    }

    private boolean setSaveValues(DataLimitData dataToSave) {

        // Make sure that PE is selected

        try {

            dataToSave.setPe(getSelectedInformationPE());

        } catch (Exception e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Physical Element");
            mb.setMessage("You must select a Physical Element.");

            mb.open();

            return false;
        }

        dataToSave.setDur(getSelectedDuration());

        // Validate date format

        String displayStartDate, displayEndDate;
        if (startDateTF.getText().equals("")) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Date");
            mb.setMessage("Please enter a valid Start date");

            mb.open();

            return false;
        }

        try {
            if (!startDateTF.getText().equals("")) {

                displayStartDate = changeFormat(startDateTF.getText(),
                        dateFormat1, dateFormat2);
                dataToSave.setMonthDayStart(displayStartDate);
            }
        } catch (Exception e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Date");
            mb.setMessage("Please enter a valid Start date");

            mb.open();

            return false;
        }

        if (endDateTF.getText().equals("")) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Date");
            mb.setMessage("Please enter a valid End date");

            mb.open();

            return false;
        }
        try {
            if (!endDateTF.getText().equals("")) {

                displayEndDate = changeFormat(endDateTF.getText(), dateFormat1,
                        dateFormat2);
                dataToSave.setMonthDayEnd(displayEndDate);
            }
        } catch (Exception e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Date");
            mb.setMessage("Please enter a valid End date");

            mb.open();

            return false;
        }

        Double temp = null;

        temp = getDoubleFromTF(grossRangeMinTF, "Gross Range Min");
        if (temp == null) {
            return false;
        }
        dataToSave.setGrossRangeMin(temp);

        temp = getDoubleFromTF(grossRangeMaxTF, "Gross Range Max");
        if (temp == null) {
            return false;
        }
        dataToSave.setGrossRangeMax(temp);

        // Verify Gross Max > Gross Min
        if (dataToSave.getGrossRangeMax() < dataToSave.getGrossRangeMin()) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Value");
            mb.setMessage("The min value must be < the max value.\n"
                    + "Please make the required changes and re-Apply them.");

            mb.open();

            return false;
        }

        temp = getDoubleFromTF(reasonableRangeMinTF, "Reasonable Range Min");
        if (temp == null) {
            return false;
        }
        dataToSave.setReasonRangeMin(temp);

        temp = getDoubleFromTF(reasonableRangeMaxTF, "Reasonable Range Max");
        if (temp == null) {
            return false;
        }
        dataToSave.setReasonRangeMax(temp);

        temp = getDoubleFromTF(rateOfChangeTF, "Quality Control Rate of Change");
        if (temp == null) {
            return false;
        }
        dataToSave.setRocMax(temp);

        temp = getDoubleFromTF(alertUpperTF, "Alert Upper Limit");
        if (temp == null) {
            return false;
        }
        dataToSave.setAlertUpperLimit(temp);

        temp = getDoubleFromTF(alertLowerTF, "Alert Lower Limit");
        if (temp == null) {
            return false;
        }
        dataToSave.setAlertLowerLimit(temp);

        temp = getDoubleFromTF(alertRocTF, "Alert ROC Limit");
        if (temp == null) {
            return false;
        }
        dataToSave.setAlertRocLimit(temp);

        temp = getDoubleFromTF(alertDiffTF, "Alert Diff Limit");
        if (temp == null) {
            return false;
        }
        dataToSave.setAlertDiffLimit(temp);

        temp = getDoubleFromTF(alarmUpperTF, "Alarm Upper Limit");
        if (temp == null) {
            return false;
        }
        dataToSave.setAlarmUpperLimit(temp);

        temp = getDoubleFromTF(alarmLowerTF, "Alarm Lower Limit");
        if (temp == null) {
            return false;
        }
        dataToSave.setAlarmLowerLimit(temp);

        temp = getDoubleFromTF(alarmRocTF, "Alarm ROC Limit");
        if (temp == null) {
            return false;
        }
        dataToSave.setAlarmRocLimit(temp);

        temp = getDoubleFromTF(alarmDiffTF, "Alarm Diff Limit");
        if (temp == null) {
            return false;
        }
        dataToSave.setAlarmDiffLimit(temp);

        return true;
    }

    private int getSelectedDuration() {
        String temp = Integer.toString(HydroConstants.MISSING_VALUE);

        if (durationCbo.getSelectionIndex() >= 0) {
            // Build regex for Mileage part of detail
            String durRegex = "\\((\\d*)\\)";
            Pattern durPattern = Pattern.compile(durRegex);

            Matcher durMatcher = durPattern.matcher(durationCbo
                    .getItem(durationCbo.getSelectionIndex()));

            // Find the Duration
            if (durMatcher.find()) {
                temp = durMatcher.group(1);
            }
        }

        return Integer.valueOf(temp);
    }

    /**
     * Confirm and delete entry.
     */
    private void deleteRecord() {
        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Delete Confirmation");
        mb.setMessage("Do you wish to delete this entry?");

        int result = mb.open();

        if (result == SWT.OK) {
            if (currentlyDisplayingDefaultLimits()) {
                DataLimitData currData = QcAlertAlarmLimitsDataManager
                        .getInstance().getSelectedDefaultData(
                                limitsList.getSelectionIndex());

                try {
                    // Delete via the Hydro Data Manager
                    HydroDBDataManager.getInstance().deleteRecord(currData);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to delete data. ", e);
                }
            } else {
                LocationDataLimitData currData = QcAlertAlarmLimitsDataManager
                        .getInstance().getSelectedLocationData(
                                limitsList.getSelectionIndex());

                try {
                    // Delete via the Hydro Data Manager
                    HydroDBDataManager.getInstance().deleteRecord(currData);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to delete entry. ", e);
                }
            }

            loadData(true);
        }
    }
}
