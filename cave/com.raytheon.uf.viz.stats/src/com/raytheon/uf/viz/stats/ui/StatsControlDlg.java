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
package com.raytheon.uf.viz.stats.ui;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.stats.GraphDataRequest;
import com.raytheon.uf.common.stats.GraphDataResponse;
import com.raytheon.uf.common.stats.data.GraphData;
import com.raytheon.uf.common.stats.data.StatsEventData;
import com.raytheon.uf.common.stats.xml.StatisticsConfig;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.stats.utils.StatsUiUtils;
import com.raytheon.viz.ui.dialogs.AwipsCalendar;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;
import com.raytheon.viz.ui.widgets.duallist.IUpdate;

/**
 * Stats graphing control dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 25, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StatsControlDlg extends CaveSWTDialog implements IStatsControl,
        IUpdate {
    /** Status handler. */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StatsControlDlg.class);

    /** Time Range combo box items */
    private final String[] TIME_RANGE_ITEMS = new String[] { "1 hr", "3 hr",
            "6 hr", "12 hr", "24 hr", "1 week", "2 weeks", "1 month" };

    /** Ranges corresponding to the time range combo box */
    private final long[] MILLI_RANGES = new long[] { TimeUtil.MILLIS_PER_HOUR,
            TimeUtil.MILLIS_PER_HOUR * 3, TimeUtil.MILLIS_PER_HOUR * 6,
            TimeUtil.MILLIS_PER_HOUR * 12, TimeUtil.MILLIS_PER_DAY,
            TimeUtil.MILLIS_PER_WEEK, TimeUtil.MILLIS_PER_WEEK * 2,
            TimeUtil.MILLIS_PER_MONTH };

    /** Date Format object */
    private final ThreadLocal<SimpleDateFormat> sdf = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sTemp = new SimpleDateFormat("MM/dd/yyyy HH");
            sTemp.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sTemp;
        }
    };

    /** Main composite */
    private Composite mainComp;

    /** Date time label */
    private Label dateTimeSelectedLabel;

    /** Date time button */
    private Button dateTimeBtn;

    /** Time range selection combo box */
    private Combo timeRangeCombo;

    /** Start radio button */
    private Button startRdo;

    /** Split radio button */
    private Button splitRdo;

    /** Display button */
    private Button displayBtn;

    /** Close Button */
    private Button closeBtn;

    /** Category combo box */
    private Combo categoryCombo;

    /** Event combo box */
    private Combo eventTypeCombo;

    /** Data type/attribute combo box */
    private Combo dataTypeCombo;

    /** Groups dual list config object */
    private DualListConfig groupFilterListConfig;

    /** Group dual list widget */
    private DualList groupFilterDualList;

    /** Data type label */
    private Label dataTypeLabel;

    /** Combo box size */
    private int comboSize;

    /** Selected Date */
    private Date selectedDate;

    /** List of configuration objects */
    private List<StatisticsConfig> configList;

    /** StatsUiUtils object */
    private final StatsUiUtils utils = new StatsUiUtils();

    /**
     * Constructor.
     * 
     * @param parent
     *            parent Shell
     */
    public StatsControlDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.INDEPENDENT_SHELL
                | CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("Statistics Display Control");

        Calendar cal = TimeUtil.newCalendar(TimeZone.getTimeZone("GMT"));
        cal.set(Calendar.MILLISECOND, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.add(Calendar.DAY_OF_MONTH, -1);
        this.selectedDate = cal.getTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initializeComponents(Shell shell) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        // calculateSize();

        createDateTimeComp();
        createTimeRangeComp();
        createDataChoiceComp();

        createButtons();
        generateData();
        populateCategoryCombo();
        setDataTypesAndGroups();
    }

    /**
     * Create the time composite.
     */
    private void createDateTimeComp() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);

        Group timeDateGroup = new Group(mainComp, SWT.NONE);
        timeDateGroup.setText(" Date/Time ");
        timeDateGroup.setLayout(gl);
        timeDateGroup.setLayoutData(gd);

        GridData btnData = new GridData(135, SWT.DEFAULT);
        dateTimeBtn = new Button(timeDateGroup, SWT.PUSH);
        dateTimeBtn.setLayoutData(btnData);
        dateTimeBtn.setText("Select Date/Time...");
        dateTimeBtn
                .setToolTipText("Click to select a date and time for display");
        dateTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleDateTimeSelection();
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        dateTimeSelectedLabel = new Label(timeDateGroup, SWT.NONE | SWT.CENTER);
        dateTimeSelectedLabel.setLayoutData(gd);
        dateTimeSelectedLabel.setText(getFormattedDate(this.selectedDate));

    }

    /**
     * Create the time range composite
     */
    private void createTimeRangeComp() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);

        Group rangeGroup = new Group(mainComp, SWT.NONE);
        rangeGroup.setText(" Graph Range ");
        rangeGroup.setLayout(gl);
        rangeGroup.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        Composite comboComp = new Composite(rangeGroup, SWT.NONE);
        comboComp.setLayout(gl);
        comboComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        timeRangeCombo = new Combo(comboComp, SWT.READ_ONLY);
        timeRangeCombo.setLayoutData(gd);
        timeRangeCombo.setToolTipText("Choose a Time Range");
        timeRangeCombo.setItems(this.TIME_RANGE_ITEMS);
        timeRangeCombo.select(0);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);

        Composite rdoComp = new Composite(rangeGroup, SWT.NONE);
        rdoComp.setLayout(gl);
        rdoComp.setLayoutData(gd);

        startRdo = new Button(rdoComp, SWT.RADIO);
        startRdo.setSelection(true);
        startRdo.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, true, false));
        startRdo.setText("Start");
        startRdo.setToolTipText("Start time range\nat selected time");

        splitRdo = new Button(rdoComp, SWT.RADIO);
        splitRdo.setSelection(false);
        splitRdo.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, true, false));
        splitRdo.setText("Split");
        splitRdo.setToolTipText("Split time range\nat selected time");
    }

    /**
     * Create the data choice comp
     */
    private void createDataChoiceComp() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        Group choiceGroup = new Group(mainComp, SWT.NONE);
        choiceGroup.setText(" Graph Data ");
        choiceGroup.setLayout(gl);
        choiceGroup.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        Composite comboComp = new Composite(choiceGroup, SWT.NONE);
        comboComp.setLayout(gl);
        comboComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label categoryLabel = new Label(comboComp, SWT.NONE);
        categoryLabel.setLayoutData(gd);
        categoryLabel.setText("  Category: ");

        gd = new GridData(comboSize + 50, SWT.DEFAULT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        categoryCombo = new Combo(comboComp, SWT.READ_ONLY);
        categoryCombo.setLayoutData(gd);
        categoryCombo.setSize(comboSize, SWT.DEFAULT);
        categoryCombo.setToolTipText("The Statistics Category");
        categoryCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateOptions();
                setDataTypesAndGroups();
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label eventTypeLabel = new Label(comboComp, SWT.NONE);
        eventTypeLabel.setLayoutData(gd);
        eventTypeLabel.setText("  Event Type: ");

        gd = new GridData(comboSize + 50, SWT.DEFAULT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        eventTypeCombo = new Combo(comboComp, SWT.READ_ONLY);
        eventTypeCombo.setLayoutData(gd);
        eventTypeCombo.setSize(comboSize, SWT.DEFAULT);
        eventTypeCombo.setToolTipText("The Event Type to Display");
        eventTypeCombo.select(0);
        eventTypeCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setDataTypesAndGroups();
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        dataTypeLabel = new Label(comboComp, SWT.NONE);
        dataTypeLabel.setLayoutData(gd);
        dataTypeLabel.setText("  Event Attribute: ");

        gd = new GridData(comboSize + 50, SWT.DEFAULT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dataTypeCombo = new Combo(comboComp, SWT.READ_ONLY);
        dataTypeCombo.setLayoutData(gd);
        dataTypeCombo.setSize(comboSize, SWT.DEFAULT);
        dataTypeCombo.setToolTipText("The Event Attribute to Display");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        Composite dualListComp = new Composite(choiceGroup, SWT.NONE);
        dualListComp.setLayout(gl);
        dualListComp.setLayoutData(gd);

        groupFilterListConfig = new DualListConfig();
        groupFilterListConfig.setAvailableListLabel("Available Groups:");
        groupFilterListConfig.setSelectedListLabel("Selected Groups:");
        groupFilterListConfig.setShowUpDownBtns(false);
        groupFilterListConfig.setListWidth(150);
        groupFilterListConfig.setListHeight(100);

        this.groupFilterDualList = new DualList(dualListComp, SWT.NONE,
                groupFilterListConfig, this);
    }

    /**
     * Create the buttons
     */
    private void createButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);

        Composite comp = new Composite(mainComp, SWT.NONE);
        comp.setLayout(gl);
        comp.setLayoutData(gd);

        GridData btnData = new GridData(85, SWT.DEFAULT);
        displayBtn = new Button(comp, SWT.PUSH);
        displayBtn.setLayoutData(btnData);
        displayBtn.setText("Display");
        displayBtn.setToolTipText("Display the Statistical Data");
        displayBtn.setEnabled(false);
        displayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleDisplayAction();
            }
        });

        btnData = new GridData(85, SWT.DEFAULT);
        closeBtn = new Button(comp, SWT.PUSH);
        closeBtn.setLayoutData(btnData);
        closeBtn.setText("Close");
        closeBtn.setToolTipText("Close this dialog");
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Display an informational popup message to the user.
     * 
     * @param title
     *            The title
     * @param message
     *            The message
     * @param type
     *            Type of message
     */
    public void displayPopup(String title, String message, int type) {
        MessageBox messageDialog = new MessageBox(getShell(), type);
        messageDialog.setText(title);
        messageDialog.setMessage(message);
        messageDialog.open();
    }

    /**
     * Set the data types.
     * 
     * @param dataTypes
     *            String[] of data types
     */
    public void setDataTypes(String[] dataTypes) {
        if (dataTypes != null) {
            dataTypeCombo.setItems(dataTypes);
            dataTypeCombo.select(0);
        }
    }

    /**
     * Date/Time selection handler
     */
    protected void handleDateTimeSelection() {
        AwipsCalendar ac = new AwipsCalendar(getShell(), selectedDate, 1);
        Object obj = ac.open();

        if ((obj != null) && (obj instanceof Date)) {
            selectedDate = (Date) obj;
            dateTimeSelectedLabel.setText(getFormattedDate(selectedDate));
        }
    }

    /**
     * Generate stats config data objects
     */
    private void generateData() {
        utils.generateData(this.configList);
    }

    /**
     * Populate the categore combo box
     */
    private void populateCategoryCombo() {
        List<String> items = new ArrayList<String>();
        for (StatisticsConfig config : this.configList) {
            items.addAll(config.getCategories());
        }

        categoryCombo.setItems(items.toArray(new String[items.size()]));
        categoryCombo.select(0);

        String category = this.categoryCombo.getText();
        Map<String, String> eventTypes = utils.getEventTypes(category);
        this.eventTypeCombo.setItems(eventTypes.keySet().toArray(
                new String[eventTypes.keySet().size()]));
        eventTypeCombo.select(0);
        this.eventTypeCombo.setData(eventTypes);
    }

    /**
     * Set the data types based on category selection.
     */
    public void setDataTypes() {
        String category = this.categoryCombo.getItem(categoryCombo
                .getSelectionIndex());
        String type = this.eventTypeCombo.getItem(eventTypeCombo
                .getSelectionIndex());

        @SuppressWarnings("unchecked")
        String typeID = ((Map<String, String>) eventTypeCombo.getData())
                .get(type);
        StatsEventData data = utils.getEventData(category, typeID);
        this.dataTypeCombo.setItems(data.getAttributes());
        dataTypeCombo.select(0);
        Map<String, String> attMap = utils.getEventAttributes(category, type);
        dataTypeCombo.setData(attMap);
    }

    /**
     * Update the options based on combo box selections
     */
    private void updateOptions() {
        String category = this.categoryCombo.getItem(categoryCombo
                .getSelectionIndex());
        Map<String, String> eventTypes = utils.getEventTypes(category);
        this.eventTypeCombo.setItems(eventTypes.keySet().toArray(
                new String[eventTypes.keySet().size()]));
        eventTypeCombo.select(0);
        this.eventTypeCombo.setData(eventTypes);

        String type = this.eventTypeCombo.getItem(eventTypeCombo
                .getSelectionIndex());

        @SuppressWarnings("unchecked")
        String typeID = ((Map<String, String>) eventTypeCombo.getData())
                .get(type);
        StatsEventData data = utils.getEventData(category, typeID);
        this.dataTypeCombo.setItems(data.getAttributes());
        dataTypeCombo.select(0);

        List<String> groupList = data.getGroupList();
        Collections.sort(groupList);
        groupFilterDualList.clearAvailableList(true);
        groupFilterDualList.clearSelection();
        groupFilterDualList.setAvailableItems(groupList);
        groupFilterDualList.getConfig().setFullList(groupList);
    }

    /**
     * Set the data type and groups
     */
    public void setDataTypesAndGroups() {
        String category = this.categoryCombo.getItem(categoryCombo
                .getSelectionIndex());
        String type = this.eventTypeCombo.getItem(eventTypeCombo
                .getSelectionIndex());

        @SuppressWarnings("unchecked")
        String typeID = ((Map<String, String>) eventTypeCombo.getData())
                .get(type);
        StatsEventData data = utils.getEventData(category, typeID);
        Map<String, String> attMap = utils.getEventAttributes(category, type);
        dataTypeCombo.setData(attMap);
        this.dataTypeCombo.setItems(data.getAttributes());
        dataTypeCombo.select(0);

        List<String> groupList = data.getGroupList();
        Collections.sort(groupList);
        groupFilterDualList.clearAvailableList(true);
        groupFilterDualList.clearSelection();
        groupFilterDualList.setAvailableItems(groupList);
        groupFilterDualList.getConfig().setFullList(groupList);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void hasEntries(boolean entries) {
        this.displayBtn.setEnabled(entries);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void selectionChanged() {
        // not used
    }

    /**
     * Display the graph!
     */
    private void handleDisplayAction() {
        GraphDataRequest request = new GraphDataRequest();

        String type = eventTypeCombo.getText();
        String category = categoryCombo.getText();
        String dataType = this.dataTypeCombo.getText();

        @SuppressWarnings("unchecked")
        String typeID = ((Map<String, String>) eventTypeCombo.getData())
                .get(type);
        @SuppressWarnings("unchecked")
        String dataTypeID = ((Map<String, String>) dataTypeCombo.getData())
                .get(dataType);

        String[] selectedGroups = groupFilterDualList.getSelectedListItems();
        StatsEventData conf = utils.getEventData(category, typeID);
        List<String> groupList = new ArrayList<String>();
        for (String group : selectedGroups) {
            String groupName = conf.getGroupNameFromDisplayName(group);
            if (groupName != null) {
                groupList.add(groupName);
            }
        }
        request.setField(dataTypeID);
        request.setCategory(category);
        request.setEventType(typeID);
        request.setDataType(dataTypeID);
        request.setGrouping(groupList);

        int idx = timeRangeCombo.getSelectionIndex();
        long selectedMillis = MILLI_RANGES[idx];
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(selectedDate);

        Date start;
        Date end;
        if (splitRdo.getSelection()) {
            // Go back half the time range
            cal.add(Calendar.MILLISECOND, -1 * Math.round(selectedMillis / 2));
            start = cal.getTime();
            // move forward the whole time range
            cal.add(Calendar.MILLISECOND, (int) selectedMillis);
            end = cal.getTime();
        } else {
            start = cal.getTime();
            // one month of millis is larger than an int
            if (selectedMillis > Integer.MAX_VALUE) {
                cal.add(Calendar.SECOND, (int) (selectedMillis / 1000));
            } else {
                cal.add(Calendar.MILLISECOND, (int) selectedMillis);
            }
            end = cal.getTime();
        }
        TimeRange tr = new TimeRange(start, end);
        request.setTimeRange(tr);
        request.setTimeStep(5);

        try {
            GraphDataResponse response = (GraphDataResponse) ThriftClient
                    .sendRequest(request);

            GraphData graphData = response.getGraphData();

            if (graphData != null) {
                StatsGraphDlg graphDlg = new StatsGraphDlg(getShell(), this);

                graphDlg.setGraphData(graphData);
                graphDlg.setTitle(type);
                graphDlg.setGraphTitle(dataType);
                graphDlg.setGrouping(groupList);
                graphDlg.setCategory(category);
                graphDlg.setEventType(typeID);
                graphDlg.setDataType(dataTypeID);

                graphDlg.open();
            } else {
                displayPopup(
                        "No Data",
                        "No statistical data are available for the time period selected.",
                        SWT.ICON_INFORMATION);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR, "Error Occured", e);
        }
    }

    /**
     * Format the date.
     * 
     * @param date
     *            The date to format
     * @return the formated string
     */
    private String getFormattedDate(Date date) {
        SimpleDateFormat format = sdf.get();
        return format.format(date) + "Z";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void showControl() {
        this.bringToTop();
    }

    /**
     * @return the configList
     */
    public List<StatisticsConfig> getConfigList() {
        return configList;
    }

    /**
     * @param configList
     *            the configList to set
     */
    public void setConfigList(List<StatisticsConfig> configList) {
        this.configList = configList;
    }
}
