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
package com.raytheon.viz.hydro.stationreporting;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.stationreporting.StationReportingConstants.SortOrder;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Station Reporting Status dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation.
 * 21 Feb 2010  2915       mpduff      Fixed Time Zone problem.
 * 23 Feb 2010  4303       mpduff      Changed the &quot;missing&quot; date display to be N/A.
 * 16 Apr 2013  1790       rferrel     Make dialog non-blocking.
 * 09 Sep 2013  #2349      lvenable    Fixed Font memory leak.
 * Mar 31, 2014 #2970      lvenable    Put dispose checks in the runAsync calls.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class StationReportingStatusDlg extends CaveSWTDialog {
    /**
     * Message handler.
     */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StationReportingStatusDlg.class);

    /**
     * Font used with the list controls.
     */
    private Font font;

    /**
     * List combo box.
     */
    private Combo listCbo;

    /**
     * Hours in the past spinner.
     */
    private Spinner hoursAgoSpnr;

    /**
     * Sort order for stations combo box.
     */
    private Combo sortCbo;

    /**
     * Maximum number of stations to display.
     */
    private Combo maxCbo;

    /**
     * List to display selected stations data.
     */
    private List dataList;

    /**
     * List to display station records.
     */
    private List latestDataList;

    /**
     * Current time data text control.
     */
    private Text curTimeDataTF;

    /**
     * Telemetry data text control.
     */
    private Text telemetryDataTF;

    /**
     * DCP reports data text control.
     */
    private Text dcpReportsDataTF;

    /**
     * Minutes starting data.
     */
    private Text minutesStartDataTF;

    /**
     * latest stations being displayed.
     */
    private ArrayList<StationReportingData> latestObs;

    /**
     * Job to update the time field.
     */
    private CurrentTimeUpdateJob timeUpdateJob = new CurrentTimeUpdateJob();

    /**
     * System busy cursor.
     */
    private Cursor waitCursor = null;

    /**
     * Job to obtain station record's off the UI thread.
     */
    private LoadRecordsJob loadRecordsJob = new LoadRecordsJob();

    /**
     * Dialog font.
     */
    private Font controlFont = null;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public StationReportingStatusDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Station Reporting Status");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        if (font != null) {
            font.dispose();
        }

        if (controlFont != null) {
            controlFont.dispose();
        }
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

        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        waitCursor = shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);

        // Initialize all of the controls and layouts
        initializeComponents();
        if (timeUpdateJob.getState() == Job.NONE) {
            timeUpdateJob.schedule();
        }

    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createTopControls();
        createTopDataLabels();
        createTopDataListControl();
        createLatestListDataGroup();
        createBottonButton();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        scheduleLoadRecords();
    }

    /**
     * Update current time text field to current simulated time.
     */
    private void updateTime() {
        TimeZone GMT = TimeZone.getTimeZone("GMT");
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        format.setTimeZone(GMT);
        Calendar cal = Calendar.getInstance(GMT);
        Date date = SimulatedTime.getSystemTime().getTime();
        cal.setTime(date);
        String s = format.format(cal.getTime());
        curTimeDataTF.setText(s);

    }

    /**
     * Create the controls at the top of the dialog.
     */
    private void createTopControls() {
        Composite topComp = new Composite(shell, SWT.NONE);
        GridLayout topGl = new GridLayout(10, false);
        topComp.setLayout(topGl);

        Label listLbl = new Label(topComp, SWT.NONE);
        listLbl.setText("List");

        listCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        listCbo.add("All Locations With Latest Data");
        listCbo.add("Only Locations With Latest Data Older Than");
        listCbo.add("Locations Without Any Latest Data");
        listCbo.select(0);
        listCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (listCbo.getSelectionIndex() == 1) {
                    hoursAgoSpnr.setEnabled(true);
                } else {
                    hoursAgoSpnr.setEnabled(false);
                }
                scheduleLoadRecords();
            }
        });

        GridData gd = new GridData(100, SWT.DEFAULT);
        Label filler1 = new Label(topComp, SWT.NONE);
        filler1.setLayoutData(gd);

        gd = new GridData(35, SWT.DEFAULT);
        hoursAgoSpnr = new Spinner(topComp, SWT.BORDER);
        hoursAgoSpnr.setDigits(0);
        hoursAgoSpnr.setIncrement(1);
        hoursAgoSpnr.setPageIncrement(5);
        hoursAgoSpnr.setSelection(6);
        hoursAgoSpnr.setMinimum(1);
        hoursAgoSpnr.setLayoutData(gd);
        hoursAgoSpnr.setEnabled(false);
        hoursAgoSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scheduleLoadRecords();
            }
        });

        Label hoursAgoLbl = new Label(topComp, SWT.NONE);
        hoursAgoLbl.setText("Hours Ago");

        gd = new GridData(100, SWT.DEFAULT);
        Label filler2 = new Label(topComp, SWT.NONE);
        filler2.setLayoutData(gd);

        Label sortLbl = new Label(topComp, SWT.NONE);
        sortLbl.setText("Sort");

        sortCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        sortCbo.add("Location");
        sortCbo.add("Time");
        sortCbo.select(0);
        sortCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                scheduleLoadRecords();
            }
        });

        Label maxLbl = new Label(topComp, SWT.NONE);
        maxLbl.setText("Max # of Obs");
        maxCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        maxCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                loadRecords();
            }
        });

        maxCbo.add("100");
        maxCbo.add("50");
        maxCbo.add("10");
        maxCbo.add("All");
        maxCbo.select(1);
    }

    /**
     * Create the labels for the data list control.
     */
    private void createTopDataLabels() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        // GridLayout labelGl = new GridLayout(14, false);
        GridLayout labelGl = new GridLayout(1, false);
        labelComp.setLayout(labelGl);

        GridData gd = new GridData(780, SWT.DEFAULT);
        Label locationLbl = new Label(labelComp, SWT.NONE);
        locationLbl.setFont(controlFont);
        locationLbl.setText(getLblText());
        locationLbl.setLayoutData(gd);
    }

    /**
     * Create the data list control.
     */
    private void createTopDataListControl() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = 1050;
        gd.heightHint = 390;
        dataList = new List(shell, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        dataList.setLayoutData(gd);
        dataList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (dataList.getSelectionIndex() != -1) {
                    updateLatestDataList(dataList.getSelectionIndex());
                }
            }
        });

        dataList.setFont(font);
    }

    /**
     * Create the latest data group.
     */
    private void createLatestListDataGroup() {
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group latestGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        latestGroup.setLayout(gl);
        latestGroup.setLayoutData(mainGridData);
        latestGroup.setText(" Latest Data for Selected Location ");

        // -------------------------------------------------
        // Add the controls to the Group container
        // -------------------------------------------------
        createLatestListLabels(latestGroup);
        createLatestListDataControl(latestGroup);
        createLatestListTextControls(latestGroup);
    }

    /**
     * Create the label for the latest data list control.
     * 
     * @param parent
     *            Parent composite.
     */
    private void createLatestListLabels(Composite parent) {
        Composite labelComp = new Composite(parent, SWT.NONE);
        GridLayout labelGl = new GridLayout(13, false);

        labelComp.setLayout(labelGl);

        GridData gd = new GridData(1000, SWT.DEFAULT);
        Label locationLbl = new Label(labelComp, SWT.NONE);
        locationLbl.setText(getLatestDataLbl());
        locationLbl.setLayoutData(gd);
    }

    /**
     * Create the latest data list control.
     * 
     * @param parent
     *            Parent composite.
     */
    private void createLatestListDataControl(Composite parent) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = 1050;
        gd.heightHint = 190;
        latestDataList = new List(parent, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        latestDataList.setLayoutData(gd);

        latestDataList.setFont(font);
    }

    /**
     * Create the "Latest Data" text controls.
     * 
     * @param parent
     *            Parent composite.
     */
    private void createLatestListTextControls(Composite parent) {
        Composite dataLabelComp = new Composite(parent, SWT.NONE);
        GridLayout dataLabelGl = new GridLayout(9, false);
        dataLabelComp.setLayout(dataLabelGl);

        Label curTimeLbl = new Label(dataLabelComp, SWT.RIGHT);
        curTimeLbl.setText("Current\nTimeZ:");

        GridData gd = new GridData(150, 25);
        curTimeDataTF = new Text(dataLabelComp, SWT.BORDER);
        curTimeDataTF.setLayoutData(gd);
        curTimeDataTF.setEditable(false);

        gd = new GridData(160, SWT.DEFAULT);
        Label telemetryLbl = new Label(dataLabelComp, SWT.RIGHT);
        telemetryLbl.setText("Telemerty\nReports Every:");
        telemetryLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        telemetryDataTF = new Text(dataLabelComp, SWT.BORDER);
        telemetryDataTF.setLayoutData(gd);
        telemetryDataTF.setEditable(false);

        gd = new GridData(180, SWT.DEFAULT);
        Label minuteLbl = new Label(dataLabelComp, SWT.NONE);
        minuteLbl.setText("Minutes");
        minuteLbl.setLayoutData(gd);

        Label dcpReportLbl = new Label(dataLabelComp, SWT.RIGHT);
        dcpReportLbl.setText("DCP Reports\nEvery:");

        gd = new GridData(60, SWT.DEFAULT);
        dcpReportsDataTF = new Text(dataLabelComp, SWT.BORDER);
        dcpReportsDataTF.setLayoutData(gd);
        dcpReportsDataTF.setEditable(false);

        gd = new GridData(150, SWT.DEFAULT);
        Label minutesStartLbl = new Label(dataLabelComp, SWT.RIGHT);
        minutesStartLbl.setText("Minutes\nStarting:");
        minutesStartLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        minutesStartDataTF = new Text(dataLabelComp, SWT.BORDER);
        minutesStartDataTF.setLayoutData(gd);
        minutesStartDataTF.setEditable(false);
    }

    /**
     * Create the Close button at the bottom of the dialog.
     */
    private void createBottonButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Get the integer value associated with the current seleciton of the max
     * combo.
     * 
     * @return value
     */
    private int getMaxCount() {
        if (maxCbo.getSelectionIndex() == 3) { // ALL
            return -1;
        } else {
            return Integer.valueOf(maxCbo.getText()).intValue();
        }
    }

    /**
     * Get the list type associated with list combo current selection.
     * 
     * @return type
     */
    private StationReportingConstants.ListType getListType() {
        if (listCbo.getSelectionIndex() == 1) {
            return StationReportingConstants.ListType.DURATION;
        } else if (listCbo.getSelectionIndex() == 2) {
            return StationReportingConstants.ListType.NEVER;
        }
        return StationReportingConstants.ListType.ALL;
    }

    /**
     * Determine the sort order associated with the current sort combo
     * selection.
     * 
     * @return sortOrder
     */
    private StationReportingConstants.SortOrder getSortOrder() {
        if (sortCbo.getSelectionIndex() == 0) {
            return StationReportingConstants.SortOrder.LOCATION;
        } else {
            return StationReportingConstants.SortOrder.TIME;
        }
    }

    /**
     * Retrieve station records off the UI thread.
     */
    private void scheduleLoadRecords() {
        loadRecordsJob.listType = getListType();
        loadRecordsJob.order = getSortOrder();
        loadRecordsJob.hoursAgo = hoursAgoSpnr.getSelection();
        setBusy(true);
        latestDataList.removeAll();
        dataList.removeAll();
        loadRecordsJob.schedule();
    }

    /**
     * Display station records based on latest obs.
     * 
     */
    private void loadRecords() {
        latestDataList.removeAll();
        dataList.removeAll();
        int count = 0;
        int maxCount = getMaxCount();

        // Create a calendar object and set to '2001-01-01 00:00:00.00
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.set(2001, 0, 1, 0, 0, 0);
        cal.set(Calendar.MILLISECOND, 0);

        for (StationReportingData currLIDData : latestObs) {
            if ((maxCount == -1) || (count < getMaxCount())) {
                String lid = currLIDData.getLid();
                String name = currLIDData.getName();
                String obsTime = null;
                String postTime = null;

                if (currLIDData.getPostingtime().equals(cal.getTime())) {
                    obsTime = "      N/A";
                    postTime = "         N/A";
                } else {
                    obsTime = StationReportingData
                            .formatDBTimestamp(currLIDData.getObstime());
                    postTime = StationReportingData
                            .formatDBTimestamp(currLIDData.getPostingtime());
                }

                String entry = String.format("%-9s %-24.24s %-27s %-19s", lid,
                        name, obsTime, postTime);

                dataList.add(entry);
                count++;
            }
        }
        setBusy(false);
    }

    /**
     * Display data for the selected station record.
     * 
     * @param index
     */
    private void updateLatestDataList(int index) {
        latestDataList.removeAll();

        String id = latestObs.get(index).getLid();

        StationReportingDataManager dataManager = StationReportingDataManager
                .getInstance();

        ArrayList<StationReportingData> obsForLid = dataManager
                .getLatestObsForLid(id);
        StationReportingTimingData timingData = dataManager.getTimingData(id);

        if (!obsForLid.isEmpty()) {
            for (StationReportingData currData : obsForLid) {
                String entry = String
                        .format("%-8s %-3s %-5d %-4s %-2s %-17s  %-8.2f %-2s  %-2s %-2s %-11s %-16s %-16s", //
                                currData.getLid(), //
                                currData.getPe(),//
                                currData.getDur(), //
                                currData.getTs(), //
                                currData.getExtremum(), //
                                StationReportingData
                                        .formatInfoTimestamp(currData
                                                .getObstime()), //
                                currData.getValue(), //
                                currData.getShefQualCode(), //
                                build_qc_symbol(currData.getQualityCode()), //
                                (currData.getRevision() == Short
                                        .parseShort("0")) ? "T" : "F", //
                                currData.getProductId(), //
                                StationReportingData
                                        .formatInfoTimestamp(currData
                                                .getProducttime()), //
                                StationReportingData
                                        .formatInfoTimestamp(currData
                                                .getPostingtime()));
                latestDataList.add(entry);
            }
        } else {
            latestDataList.removeAll();

        }

        if (timingData != null) {
            if (timingData.dcpFrequency != null) {
                dcpReportsDataTF.setText(timingData.dcpFrequency);
            } else {
                dcpReportsDataTF.setText("N/A");
            }

            if (timingData.dcpTime != null) {
                minutesStartDataTF.setText(timingData.dcpTime);
            } else {
                minutesStartDataTF.setText("N/A");
            }

            if (timingData.telemFrequency != null) {
                telemetryDataTF.setText(timingData.telemFrequency);
            } else {
                telemetryDataTF.setText("N/A");
            }
        } else {
            dcpReportsDataTF.setText("N/A");
            minutesStartDataTF.setText("N/A");
            telemetryDataTF.setText("N/A");
        }
    }

    /**
     * Display string for the desired quality code.
     * 
     * @param qualityCode
     * @return value
     */
    private String build_qc_symbol(int qualityCode) {

        if (checkQCCode(StationReportingConstants.QC_PASSED, qualityCode)) {
            return "G";
        } else if (checkQCCode(StationReportingConstants.QC_QUESTIONABLE,
                qualityCode)) {
            return "Q";
        } else if (checkQCCode(StationReportingConstants.QC_FAILED, qualityCode)) {
            return "B";
        }

        return "?";

    }

    /**
     * Validate QC code and type.
     * 
     * @param type
     * @param qualityCode
     * @return
     */
    private boolean checkQCCode(String type, int qualityCode) {
        if (type.equals(StationReportingConstants.QC_PASSED)) {
            if (qualityCode > StationReportingConstants.GOOD_QUESTIONABLE_THRESHOLD) {
                return true;
            } else {
                return false;
            }
        }
        if (type.equals(StationReportingConstants.QC_QUESTIONABLE)) {
            if ((qualityCode >= StationReportingConstants.QUESTIONABLE_BAD_THRESHOLD)
                    && (qualityCode < StationReportingConstants.GOOD_QUESTIONABLE_THRESHOLD)) {
                return true;
            } else {
                return false;
            }
        }
        if (type.equals(StationReportingConstants.QC_FAILED)) {
            if (qualityCode < StationReportingConstants.QUESTIONABLE_BAD_THRESHOLD) {
                return true;
            } else {
                return false;
            }
        }

        return false;
    }

    /**
     * Job to update the time display.
     */
    class CurrentTimeUpdateJob extends Job {

        /**
         * @param name
         */
        public CurrentTimeUpdateJob() {
            super("CurrentTimeUpdateJob");
            setSystem(true);
        }

        /*
         * (non-Javadoc)
         * 
         * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            // update the state of all active displays
            Display.getDefault().syncExec(new Runnable() {

                @Override
                public void run() {
                    if (!curTimeDataTF.isDisposed()) {
                        updateTime();
                    }
                }

            });

            this.schedule(1000);

            return Status.OK_STATUS;
        }
    }

    /**
     * Formats the label for locations so it lines up with display of the
     * location records.
     * 
     * @return label
     */
    private String getLblText() {
        String format = "%S    %S                        %S              %S";

        String labelStr = String.format(format, "Location", "Name",
                "Observation Time Z", "Latest Data Posted Time Z");

        return labelStr;
    }

    /**
     * Formats the label for locations data so it lines up with the values in
     * the list.
     * 
     * @return label
     */
    private String getLatestDataLbl() {
        String format = "%S    %S     %S      %S     %S         %S              %S          %S    %S    %S          %S                   %S         %S";

        String labelStr = String.format(format, "Location", "Pe", "Dur", "Src",
                "Ex", "OBS_TIME(Z)", "Value", "Sq", "Qc", "Rv", "Id",
                "PRODUCT_TIME(Z)", "POSTING_TIME(Z)");

        return labelStr;
    }

    /**
     * Set cursor to desired state and enable/disables buttons so user cannot
     * prematurely change state.
     * 
     * @param busy
     */
    private void setBusy(boolean busy) {
        listCbo.setEnabled(!busy);
        sortCbo.setEnabled(!busy);
        maxCbo.setEnabled(!busy);
        if (busy) {
            hoursAgoSpnr.setEnabled(false);
            shell.setCursor(waitCursor);
        } else {
            hoursAgoSpnr.setEnabled(listCbo.getSelectionIndex() == 1);
            shell.setCursor(null);
        }
    }

    /**
     * A job to get station records off the UI thread.
     */
    private class LoadRecordsJob extends Job {
        /**
         * List type constraint.
         */
        protected StationReportingConstants.ListType listType;

        /**
         * sort order for the data.
         */
        protected SortOrder order;

        /**
         * Number of hours of data to retrieve.
         */
        protected int hoursAgo;

        public LoadRecordsJob() {
            super("Load RecordsJob");
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            try {
                StationReportingDataManager dataManager = StationReportingDataManager
                        .getInstance();

                ArrayList<StationReportingData> currObs = dataManager
                        .getStationReportingData(listType, order,
                                StationReportingConstants.Duration.DEFAULT,
                                hoursAgo);

                if (currObs.isEmpty()) {
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            if (isDisposed()) {
                                return;
                            }
                            setBusy(false);
                            String showErrorMsg = "No latest obs found. ";
                            MessageBox mb = new MessageBox(shell,
                                    SWT.ICON_WARNING | SWT.OK | SWT.CANCEL);
                            mb.setText("No Records Found.");
                            mb.setMessage(showErrorMsg);
                            mb.open();
                        }
                    });
                } else {
                    latestObs = currObs;
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            if (isDisposed()) {
                                return;
                            }
                            loadRecords();
                        }
                    });
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            return Status.OK_STATUS;
        }
    }
}
