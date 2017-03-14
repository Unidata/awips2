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

import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.stationreporting.StationReportingConstants.ListType;
import com.raytheon.viz.hydro.stationreporting.StationReportingConstants.SortOrder;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

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
 * May 04, 2016 #5483      dgilling    Removed fixed pixel layouts, code cleanup.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class StationReportingStatusDlg extends CaveJFACEDialog {

    /**
     * Job to update the time display.
     */
    private final class CurrentTimeUpdateJob extends UIJob {

        private final DateFormat TIME_DISPLAY_FMT = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm:ss") {
            {
                setTimeZone(TimeUtil.GMT_TIME_ZONE);
            }
        };

        public CurrentTimeUpdateJob() {
            super("CurrentTimeUpdateJob");
            setSystem(true);
        }

        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            if ((curTimeDataTF != null) && (!curTimeDataTF.isDisposed())) {
                curTimeDataTF.setText(TIME_DISPLAY_FMT.format(SimulatedTime
                        .getSystemTime().getTime()));
            }

            if (!monitor.isCanceled()) {
                schedule(TimeUtil.MILLIS_PER_SECOND);
            }

            return Status.OK_STATUS;
        }
    }

    /**
     * A job to get station records off the UI thread.
     */
    private final class RetrieveRecordsJob extends Job {

        /**
         * List type constraint.
         */
        private final StationReportingConstants.ListType listType;

        /**
         * sort order for the data.
         */
        private final SortOrder order;

        /**
         * Number of hours of data to retrieve.
         */
        private final int hoursAgo;

        public RetrieveRecordsJob(ListType listType, SortOrder order,
                int hoursAgo) {
            super("Retrieve Records Job");
            setSystem(true);
            this.listType = listType;
            this.order = order;
            this.hoursAgo = hoursAgo;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            try {
                if (monitor.isCanceled()) {
                    return Status.CANCEL_STATUS;
                }

                StationReportingDataManager dataManager = StationReportingDataManager
                        .getInstance();
                List<StationReportingData> currObs = dataManager
                        .getStationReportingData(listType, order,
                                StationReportingConstants.Duration.DEFAULT,
                                hoursAgo);

                if (monitor.isCanceled()) {
                    return Status.CANCEL_STATUS;
                }

                loadRecordsJob = new LoadRecordsJob(currObs);
                loadRecordsJob.schedule();

            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            return Status.OK_STATUS;
        }
    }

    private final class LoadRecordsJob extends UIJob {

        private final List<StationReportingData> records;

        public LoadRecordsJob(List<StationReportingData> records) {
            super("Load Records Job");
            this.records = records;
        }

        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            if (isDisposed()) {
                return Status.OK_STATUS;
            }

            if (records.isEmpty()) {
                setBusy(false);
                MessageDialog.openWarning(getShell(), "No Records Found.",
                        "No latest obs found.");
            } else {
                loadRecords(records);
            }

            return Status.OK_STATUS;
        }
    }

    /**
     * Message handler.
     */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final Map<ListType, String> LIST_COMBO_ITEMS = Collections
            .unmodifiableMap(new EnumMap<ListType, String>(ListType.class) {
                {
                    put(ListType.ALL, "All Locations With Latest Data");
                    put(ListType.DURATION,
                            "Only Locations With Latest Data Older Than");
                    put(ListType.NEVER, "Locations Without Any Latest Data");
                }
            });

    private static final Map<SortOrder, String> SORT_COMBO_ITEMS = Collections
            .unmodifiableMap(new EnumMap<SortOrder, String>(SortOrder.class) {
                {
                    put(SortOrder.LOCATION, "Location");
                    put(SortOrder.TIME, "Time");
                }
            });

    private static final Map<Integer, String> MAX_COMBO_ITEMS = Collections
            .unmodifiableMap(new LinkedHashMap<Integer, String>() {
                {
                    put(100, "100");
                    put(50, "50");
                    put(10, "10");
                    put(Integer.MAX_VALUE, "All");
                }
            });

    /**
     * Job to update the time field.
     */
    private final CurrentTimeUpdateJob timeUpdateJob;

    private final NumberFormat valueNumberFormat;

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
     * Table to display selected stations data.
     */
    private Table dataTable;

    /**
     * Table to display station records.
     */
    private Table latestDataTable;

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
    private List<StationReportingData> latestObs;

    private Cursor waitCursor;

    /**
     * Job to obtain station record's off the UI thread.
     */
    private volatile RetrieveRecordsJob retrieveJob;

    /**
     * Job to obtain station record's off the UI thread.
     */
    private volatile LoadRecordsJob loadRecordsJob;

    protected StationReportingStatusDlg(Shell parentShell) {
        super(parentShell);
        setBlockOnOpen(false);
        setShellStyle(SWT.DIALOG_TRIM | SWT.RESIZE);

        this.timeUpdateJob = new CurrentTimeUpdateJob();

        this.valueNumberFormat = NumberFormat.getNumberInstance();
        this.valueNumberFormat.setMaximumFractionDigits(2);
        this.valueNumberFormat.setMinimumFractionDigits(2);
        this.valueNumberFormat.setGroupingUsed(false);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Station Reporting Status");
        waitCursor = newShell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);
    }

    @Override
    public boolean close() {
        timeUpdateJob.cancel();
        retrieveJob.cancel();
        loadRecordsJob.cancel();
        return super.close();
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);
        createTopControls(composite);
        createTopDataTable(composite);
        createLatestDataGroup(composite);

        timeUpdateJob.schedule();
        scheduleLoadRecords();

        return composite;
    }

    /**
     * Create the controls at the top of the dialog.
     */
    private void createTopControls(Composite parent) {
        Composite topComp = new Composite(parent, SWT.NONE);
        topComp.setLayout(new GridLayout(10, false));
        topComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label listLbl = new Label(topComp, SWT.NONE);
        listLbl.setText("List");
        listLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        listCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (Entry<ListType, String> entry : LIST_COMBO_ITEMS.entrySet()) {
            ListType listType = entry.getKey();
            String displayString = entry.getValue();
            listCbo.add(displayString);
            listCbo.setData(displayString, listType);
        }
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
        listCbo.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, true));

        Label filler = new Label(topComp, SWT.NONE);
        filler.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        hoursAgoSpnr = new Spinner(topComp, SWT.BORDER);
        hoursAgoSpnr.setDigits(0);
        hoursAgoSpnr.setIncrement(1);
        hoursAgoSpnr.setPageIncrement(5);
        hoursAgoSpnr.setSelection(6);
        hoursAgoSpnr.setMinimum(1);
        hoursAgoSpnr.setEnabled(false);
        hoursAgoSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scheduleLoadRecords();
            }
        });
        hoursAgoSpnr.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        Label hoursAgoLbl = new Label(topComp, SWT.NONE);
        hoursAgoLbl.setText("Hours Ago");
        listLbl.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, true));

        Label filler2 = new Label(topComp, SWT.NONE);
        filler2.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        Label sortLbl = new Label(topComp, SWT.NONE);
        sortLbl.setText("Sort");
        listLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, true));

        sortCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (Entry<SortOrder, String> entry : SORT_COMBO_ITEMS.entrySet()) {
            SortOrder sort = entry.getKey();
            String displayString = entry.getValue();
            sortCbo.add(displayString);
            sortCbo.setData(displayString, sort);
        }
        sortCbo.select(0);
        sortCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                scheduleLoadRecords();
            }
        });
        sortCbo.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, true));

        Label maxLbl = new Label(topComp, SWT.NONE);
        maxLbl.setText("Max # of Obs");
        listLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        maxCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (Entry<Integer, String> entry : MAX_COMBO_ITEMS.entrySet()) {
            Integer maxRecords = entry.getKey();
            String displayString = entry.getValue();
            maxCbo.add(displayString);
            maxCbo.setData(displayString, maxRecords);
        }
        maxCbo.select(1);
        maxCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                loadRecords(latestObs);
            }
        });
        sortCbo.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));
    }

    private void createTopDataTable(Composite parent) {
        dataTable = new Table(parent, SWT.SINGLE | SWT.FULL_SELECTION
                | SWT.V_SCROLL | SWT.H_SCROLL);
        dataTable.setLinesVisible(false);
        dataTable.setHeaderVisible(true);
        String[] headerTitles = { "Location", "Name", "Observation Time Z",
                "Latest Data Posted Time Z" };
        for (String title : headerTitles) {
            TableColumn column = new TableColumn(dataTable, SWT.LEFT);
            column.setText(title);
        }

        dataTable.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                StationReportingData data = (StationReportingData) e.widget
                        .getData();
                if (data == null) {
                    data = (StationReportingData) dataTable.getItem(
                            dataTable.getSelectionIndex()).getData();
                }

                updateLatestDataList(data);
            }
        });

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = dataTable.getHeaderHeight()
                + (dataTable.getItemHeight() * 17);
        gd.horizontalSpan = 9;
        dataTable.setLayoutData(gd);
        for (TableColumn column : dataTable.getColumns()) {
            column.pack();
        }
    }

    /**
     * Create the latest data group.
     */
    private void createLatestDataGroup(Composite parent) {
        Group latestGroup = new Group(parent, SWT.NONE);
        latestGroup.setLayout(new GridLayout(9, false));
        latestGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        latestGroup.setText("Latest Data for Selected Location ");

        // -------------------------------------------------
        // Add the controls to the Group container
        // -------------------------------------------------
        latestDataTable = new Table(latestGroup, SWT.SINGLE
                | SWT.FULL_SELECTION | SWT.V_SCROLL | SWT.H_SCROLL);
        latestDataTable.setLinesVisible(false);
        latestDataTable.setHeaderVisible(true);
        String[] headerTitles = { "Location", "Pe", "Dur", "Src", "Ex",
                "OBS_TIME(Z)", "Value", "Sq", "Qc", "Rv", "Id",
                "PRODUCT_TIME(Z)", "POSTING_TIME(Z)" };
        for (String title : headerTitles) {
            TableColumn column = new TableColumn(latestDataTable, SWT.LEFT);
            column.setText(title);
        }
        GC gc = new GC(latestDataTable);
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        int charHeight = gc.getFontMetrics().getHeight();
        gc.dispose();
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = latestDataTable.getHeaderHeight()
                + (latestDataTable.getItemHeight() * 8);
        gd.widthHint = latestDataTable.computeTrim(0, 0, charWidth * 120,
                charHeight).width;
        gd.horizontalSpan = 9;
        latestDataTable.setLayoutData(gd);
        for (TableColumn column : latestDataTable.getColumns()) {
            column.pack();
        }

        Label curTimeLbl = new Label(latestGroup, SWT.RIGHT);
        curTimeLbl.setText("Current\nTimeZ:");
        curTimeLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        curTimeDataTF = new Text(latestGroup, SWT.BORDER);
        curTimeDataTF.setEditable(false);
        gc = new GC(curTimeDataTF);
        charWidth = gc.getFontMetrics().getAverageCharWidth();
        charHeight = gc.getFontMetrics().getHeight();
        gc.dispose();
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = curTimeDataTF.computeTrim(0, 0, charWidth * 20,
                charHeight).width;
        curTimeDataTF.setLayoutData(gd);

        Label telemetryLbl = new Label(latestGroup, SWT.RIGHT);
        telemetryLbl.setText("Telemerty\nReports Every:");
        telemetryLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true,
                true));

        telemetryDataTF = new Text(latestGroup, SWT.BORDER);
        telemetryDataTF.setEditable(false);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = telemetryDataTF.computeTrim(0, 0, charWidth * 5,
                charHeight).width;
        telemetryDataTF.setLayoutData(gd);

        Label minuteLbl = new Label(latestGroup, SWT.NONE);
        minuteLbl.setText("Minutes");
        minuteLbl
                .setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        Label dcpReportLbl = new Label(latestGroup, SWT.RIGHT);
        dcpReportLbl.setText("DCP Reports\nEvery:");
        dcpReportLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true,
                true));

        dcpReportsDataTF = new Text(latestGroup, SWT.BORDER);
        dcpReportsDataTF.setEditable(false);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = dcpReportsDataTF.computeTrim(0, 0, charWidth * 5,
                charHeight).width;
        dcpReportsDataTF.setLayoutData(gd);

        Label minutesStartLbl = new Label(latestGroup, SWT.RIGHT);
        minutesStartLbl.setText("Minutes\nStarting:");
        minutesStartLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true,
                true));

        minutesStartDataTF = new Text(latestGroup, SWT.BORDER);
        minutesStartDataTF.setEditable(false);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = minutesStartDataTF.computeTrim(0, 0, charWidth * 5,
                charHeight).width;
        minutesStartDataTF.setLayoutData(gd);
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.CLOSE_ID,
                IDialogConstants.CLOSE_LABEL, false);
    }

    @Override
    protected void buttonPressed(int buttonId) {
        switch (buttonId) {
        case IDialogConstants.CLOSE_ID:
            close();
            break;
        default:
            statusHandler.warn(String.format(
                    "Unrecognized button [%d] pressed.", buttonId));
            break;
        }
    }

    /**
     * Retrieve station records off the UI thread.
     */
    private void scheduleLoadRecords() {
        ListType listType = (ListType) listCbo.getData(listCbo.getItem(listCbo
                .getSelectionIndex()));
        SortOrder order = (SortOrder) sortCbo.getData(sortCbo.getItem(sortCbo
                .getSelectionIndex()));
        int hoursAgo = hoursAgoSpnr.getSelection();
        retrieveJob = new RetrieveRecordsJob(listType, order, hoursAgo);
        setBusy(true);
        latestDataTable.removeAll();
        dataTable.removeAll();
        retrieveJob.schedule();
    }

    /**
     * Display station records based on latest obs.
     * 
     * @param records
     * 
     */
    private void loadRecords(List<StationReportingData> records) {
        latestObs = records;

        latestDataTable.removeAll();
        dataTable.removeAll();
        Integer userMaxSelection = (Integer) maxCbo.getData(maxCbo
                .getItem(maxCbo.getSelectionIndex()));
        int maxCount = Math.min(records.size(), userMaxSelection.intValue());

        // Create a calendar object and set to '2001-01-01 00:00:00.00
        Calendar cal = TimeUtil.newGmtCalendar();
        cal.set(2001, 0, 1, 0, 0, 0);
        cal.set(Calendar.MILLISECOND, 0);

        for (StationReportingData currLIDData : records.subList(0, maxCount)) {
            String lid = currLIDData.getLid();
            String name = currLIDData.getName();
            String obsTime = null;
            String postTime = null;

            if (currLIDData.getPostingtime().equals(cal.getTime())) {
                obsTime = "N/A";
                postTime = "N/A";
            } else {
                obsTime = StationReportingData.formatDBTimestamp(currLIDData
                        .getObstime());
                postTime = StationReportingData.formatDBTimestamp(currLIDData
                        .getPostingtime());
            }

            TableItem entry = new TableItem(dataTable, SWT.NONE);
            entry.setData(currLIDData);
            entry.setFont(JFaceResources.getTextFont());
            entry.setText(0, lid);
            entry.setText(1, name);
            entry.setText(2, obsTime);
            entry.setText(3, postTime);
        }

        for (TableColumn column : dataTable.getColumns()) {
            column.pack();
        }

        setBusy(false);
    }

    /**
     * Display data for the selected station record.
     * 
     * @param data
     */
    private void updateLatestDataList(StationReportingData data) {
        latestDataTable.removeAll();

        String id = data.getLid();

        StationReportingDataManager dataManager = StationReportingDataManager
                .getInstance();

        List<StationReportingData> obsForLid = dataManager
                .getLatestObsForLid(id);
        StationReportingTimingData timingData = dataManager.getTimingData(id);

        if (!obsForLid.isEmpty()) {
            for (StationReportingData currData : obsForLid) {
                TableItem item = new TableItem(latestDataTable, SWT.NONE);
                item.setFont(JFaceResources.getTextFont());
                item.setText(0, currData.getLid());
                item.setText(1, currData.getPe());
                item.setText(2, Short.toString(currData.getDur()));
                item.setText(3, currData.getTs());
                item.setText(4, currData.getExtremum());
                item.setText(5, StationReportingData
                        .formatInfoTimestamp(currData.getObstime()));
                item.setText(6, valueNumberFormat.format(currData.getValue()));
                item.setText(7, currData.getShefQualCode());
                item.setText(8, build_qc_symbol(currData.getQualityCode()));
                item.setText(9, (currData.getRevision() == 0) ? "T" : "F");
                item.setText(10, currData.getProductId());
                item.setText(11, StationReportingData
                        .formatInfoTimestamp(currData.getProducttime()));
                item.setText(12, StationReportingData
                        .formatInfoTimestamp(currData.getPostingtime()));
            }
        }
        for (TableColumn column : latestDataTable.getColumns()) {
            column.pack();
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
            getShell().setCursor(waitCursor);
        } else {
            hoursAgoSpnr.setEnabled(listCbo.getSelectionIndex() == 1);
            getShell().setCursor(null);
        }
    }
}
