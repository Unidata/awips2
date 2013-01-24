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
package com.raytheon.uf.viz.datadelivery.notification;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.datadelivery.common.ui.ITableChange;
import com.raytheon.uf.viz.datadelivery.common.ui.ITableFind;
import com.raytheon.uf.viz.datadelivery.common.ui.SortImages.SortDirection;
import com.raytheon.uf.viz.datadelivery.common.ui.TableComp;
import com.raytheon.uf.viz.datadelivery.common.ui.TableCompConfig;
import com.raytheon.uf.viz.datadelivery.common.ui.TableDataManager;
import com.raytheon.uf.viz.datadelivery.common.xml.ColumnXML;
import com.raytheon.uf.viz.datadelivery.notification.PriorityImages.Priority;
import com.raytheon.uf.viz.datadelivery.notification.PriorityImages.PriorityDisplay;
import com.raytheon.uf.viz.datadelivery.notification.xml.MessageLoadXML;
import com.raytheon.uf.viz.datadelivery.notification.xml.NotificationConfigXML;
import com.raytheon.uf.viz.datadelivery.notification.xml.PrioritySettingXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.NotifColumnNames;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;
import com.raytheon.uf.viz.datadelivery.utils.NotificationHandler;

/**
 * This class contains the notification table and the controls to change the
 * page that will be displayed in the table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2012   687      lvenable     Initial creation.
 * Aug 09, 2012   430      jpiatt       Modifications for sort asc & sort desc.
 * Aug 30, 2012  1120      jpiatt       Added clickSort flag.
 * Sep 06, 2012   687      mpduff       Call the table selection method of the ITableChanged interface.
 * Oct 22, 2012   1284     mpduff       Fix the start/end index for pagination of new records, code cleanup.
 * Nov 29, 2012  1285      bgonzale     Added a refresh pause button to the Notification Center Dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class NotificationTableComp extends TableComp implements ITableFind {

    /** Priority image creation class */
    private PriorityImages pImage = null;

    /** Master Table list object with users filtered only */
    private final TableDataManager<NotificationRowData> masterTableList = new TableDataManager<NotificationRowData>(
            TABLE_TYPE.NOTIFICATION);

    /** Filtered Table list object */
    private final TableDataManager<NotificationRowData> filteredTableList = new TableDataManager<NotificationRowData>(
            TABLE_TYPE.NOTIFICATION);

    /** Filtered Table list object */
    private final ArrayList<NotificationRowData> visibleTableList = new ArrayList<NotificationRowData>();

    /** Notification rows */
    private final String ROWS = "Rows ";

    /** Notification rows */
    private final String ROW = "Row ";

    private final String PAUSE_BUTTON_TEXT = "Pause";

    private final String PAUSE_MSG_TEXT = " <Paused>";

    /** Number row label */
    private Label numRowsLbl;

    /** Page label */
    private Label pageLbl;

    /** Number row label */
    private Label pageAmtLbl;

    /** Page Combo box */
    private Combo pageCbo;

    /** The start index */
    private int startIndex = 0;

    /** The end index */
    private int endIndex = 0;

    /** The selected index */
    private int selectedIndex = 0;

    /** The index of the last row of the table */
    private int lastRow = 0;

    /** Configured value per page */
    private int pageConfig = 100;

    /** The amount of necessary pages */
    private int pageAmt = 0;

    /** The number of rows in the table at that instance */
    private int numRows = 0;

    /** The index of the highlighted row */
    private int highlightIndex = 0;

    /** The number of deleted rows */
    private int deleteRows = 0;

    /** Highlight indices */
    private int[] indices = null;

    /** Dual List Object */
    private final ArrayList<String> pages = new ArrayList<String>();

    /** Find flag */
    private boolean findFlag = false;

    /** Delete flag */
    private boolean deleteFlag = false;

    /** All rows on page deleted flag */
    private boolean pageDeleteFlag = false;

    /** Init flag */
    private boolean initialized = false;

    /** The selected page */
    private int selectedPage = 1;

    /** Keeps track of present record ids **/
    private final Set<Integer> currentRecordIds = new HashSet<Integer>();

    /** Callback for the message loader */
    private final IMessageLoad msgLoadCallback;

    /** Callback for the table change */
    private final ITableChange tableChangeCallback;

    /** Notification handler */
    private NotificationHandler handler;

    /** Name of the sorted column */
    private String sortedColumnName = null;

    /** Direction of the sort */
    private SortDirection sortedDirectionName = null;

    /** Column List */
    private ArrayList<ColumnXML> columnList = null;

    /** Scroll/Refresh pause button */
    private Button pauseButton;

    /** Count of messages receieved when the dialog is paused. */
    private int messageReceivedWhilePausedCount = 0;

    /**
     * Constructor.
     *
     * Note: For the super class we are passing in a false for the notification
     * flag. This is turned off because the notification dialog is using the
     * NotificationHandler and it contains the necessary code that needs to be
     * executed.
     *
     * @param parent
     * @param tableConfig
     * @param callback
     * @param msgLoadCallback
     * @param handler
     */
    public NotificationTableComp(Composite parent, TableCompConfig tableConfig,
            ITableChange callback, IMessageLoad msgLoadCallback,
            NotificationHandler handler) {
        super(parent, tableConfig, false);

        this.tableChangeCallback = callback;
        this.msgLoadCallback = msgLoadCallback;
        this.handler = handler;

        init();
    }

    /**
     * Initialize the composite.
     */
    private void init() {

        startIndex = 0;
        endIndex = pageConfig - 1;

        pImage = new PriorityImages(this.getShell());
        pImage.setPriorityDisplay(PriorityDisplay.ColorNumName);

        createColumns();
        createBottomPageControls();

    }

    /**
     * Create the paging and row functions.
     */
    private void createBottomPageControls() {

        // Bottom Composite
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;

        Composite bottomComp = new Composite(this, SWT.NONE);
        bottomComp.setLayout(gl);
        bottomComp.setLayoutData(gd);

        Composite pageComp = new Composite(bottomComp, SWT.NONE);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gl = new GridLayout(3, false);
        pageComp.setLayout(gl);
        pageComp.setLayoutData(gd);

        // Page label
        pageLbl = new Label(pageComp, SWT.NONE);
        pageLbl.setText("Page: ");

        // Page Selection Combo Box
        GridData comboData = new GridData(65, SWT.DEFAULT);
        pageCbo = new Combo(pageComp, SWT.READ_ONLY);
        pageCbo.setLayoutData(comboData);
        pageCbo.select(0);
        pageCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                handlePageSelection();
            }
        });

        pageAmtLbl = new Label(pageComp, SWT.NONE);
        gd = new GridData(50, SWT.DEFAULT);
        pageAmtLbl.setLayoutData(gd);

        pauseButton = new Button(bottomComp, SWT.CHECK);
        pauseButton.setText(PAUSE_BUTTON_TEXT);
        pauseButton
                .setToolTipText("When checked, the Notification Table UI will"
                        + " not refresh, re-sort, or allow user modification"
                        + " until unchecked.");
        pauseButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                boolean isLocked = pauseButton.getSelection();

                messageReceivedWhilePausedCount = 0;
                pageCbo.setEnabled(!isLocked);
                tableChangeCallback.tableLock(isLocked);
                if (isLocked) {
                    pauseButton.setBackground(getDisplay().getSystemColor(
                            SWT.COLOR_RED));
                } else {
                    populateTable();
                    messageReceivedWhilePausedCount = 0;
                    pauseButton.setBackground(getDisplay().getSystemColor(
                            SWT.COLOR_WIDGET_BACKGROUND));
                }

            }
        });

        // Row Label
        Composite rowComp = new Composite(bottomComp, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gl = new GridLayout(1, false);
        rowComp.setLayout(gl);
        rowComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        numRowsLbl = new Label(rowComp, SWT.RIGHT);
        numRowsLbl.setLayoutData(gd);
    }

    /**
     * Get the entire table list.
     *
     * @return TableDataManager obj
     */
    public TableDataManager<NotificationRowData> getMasterTableList() {
        return masterTableList;
    }

    /**
     * Get the table list with filters applied.
     *
     * @return TableDataManager obj
     */
    public TableDataManager<NotificationRowData> getFilteredTableList() {
        return filteredTableList;
    }

    /**
     * Get the table list for display.
     *
     * @return TableDataManager obj
     */
    public ArrayList<NotificationRowData> getVisibleTableList() {
        return visibleTableList;
    }

    /**
     * Get the rows of data to display.
     *
     * @return list of Notification Row Data objects
     */
    private ArrayList<NotificationRowData> getTableRows() {
        visibleTableList.clear();
        numRows = filteredTableList.getDataArray().size();
        if (startIndex == 0 && numRows > endIndex) {
            endIndex = pageConfig - 1;
        }

        // Recalculate start/end indices for visible page
        if (numRows > endIndex && endIndex - startIndex < pageConfig) {
            endIndex = startIndex + pageConfig - 1;//numRows -1;
            if (endIndex - startIndex > pageConfig - 1) {
                startIndex = ((pageConfig * selectedPage) - (pageConfig - 1)) - 1;
            }
        }

        if (pageDeleteFlag) {
            startIndex = 0;
            endIndex = pageConfig - 1;
        }
        getNumberOfPages();

        // Add rows to the visible table list
        for (int i = startIndex; i <= endIndex; i++) {

            if (i < filteredTableList.getDataArray().size()) {
                NotificationRowData data = filteredTableList.getDataRow(i);
                if (data != null) {
                    visibleTableList.add(data);
                }
            } else {
                break;
            }

        }

        return visibleTableList;
    }

    /**
     * Delete table rows.
     *
     * @param deleteRecordIds
     */
    public void deleteTableDataRows(ArrayList<Integer> deleteRecordIds) {

        ArrayList<NotificationRowData> tmpDeleteArray = new ArrayList<NotificationRowData>();

        for (NotificationRowData rd : filteredTableList.getDataArray()) {
            if (deleteRecordIds.contains(rd.getId()) == true) {
                tmpDeleteArray.add(rd);
            }
        }

        filteredTableList.removeAll(tmpDeleteArray);
    }

    /**
     * Get the table obj.
     *
     * @return the table obj.
     */
    public Table getTable() {
        return table;
    }

    private void handleColumnSelection(TableColumn tc) {
        if (pauseButton.getSelection()) {
            // no table sort if paused.
            return;
        }

        boolean clickSort = true;
        updateSortDirection(tc, filteredTableList, clickSort);

        filteredTableList.sortData();

        selectedPage = pageCbo.getSelectionIndex() + 1;
        populateTable();
        updateColumnSortImage();
    }

    /**
     * Action taken when deleting a notification.
     */
    public void handleDeleteNotification() {

        deleteFlag = true;

        // Verify that at least one notification was selected.
        int[] indices = table.getSelectionIndices();

        if (indices.length == 0) {
            return;
        }

        // Extract notification ids from the table
        ArrayList<Integer> ids = new ArrayList<Integer>();
        for (int index : indices) {

            ids.add(index);
            NotificationRowData rowData = visibleTableList.get(index);
            ids.add(rowData.getId());
        }

        if (ids.size() > 0) {
            deleteRows = (ids.size() / 2);
            if (deleteRows < visibleTableList.size()) {
                selectedPage = pageCbo.getSelectionIndex() + 1;

                // If all rows on a page are deleted go to page 1
            } else {
                selectedPage = 0;
                pageCbo.select(selectedPage);
                pageDeleteFlag = true;
            }

            deleteRecords(ids);
        }
    }

    /**
     * Action taken when tool tip is selected.
     *
     * @param showToolTips
     *            true when tooltips are on
     */
    public void handleTooltipSelection(boolean showToolTips) {

        showColumnToolTips(showToolTips);

        if (showToolTips) {
            numRowsLbl.setToolTipText("Rows per page");
            pageLbl.setToolTipText("Page Selection");
            pageCbo.setToolTipText("Select a Page");
        } else {
            numRowsLbl.setToolTipText("");
            pageLbl.setToolTipText("");
            pageCbo.setToolTipText("");
        }
    }

    /**
     * Update labels on the table.
     */
    private void updateLabels() {

        int startRow = startIndex + 1;
        int endRow = endIndex + 1;
        String selection = null;

        // Total number of rows in the filteredTableList used in bottom right
        // hand corner
        numRows = filteredTableList.getDataArray().size();

        // Page calculations
        getNumberOfPages();

        // Set page label
        pageAmtLbl.setText(" of " + pageAmt);

        // Set number of pages
        pageCbo.setItems(pages.toArray(new String[0]));

        if (selectedPage > 0) {
            pageCbo.select(selectedPage - 1);
        } else {
            pageCbo.select(selectedPage);
        }

        if (pageCbo.getSelectionIndex() >= 0) {
            selection = pageCbo.getItem(pageCbo.getSelectionIndex());
        }

        if (selection == null) {
            selectedPage = 1;
        } else {
            selectedPage = pageCbo.getSelectionIndex() + 1;
        }

        if (deleteFlag) {
            if (endRow > numRows) {
                endRow = numRows;
            } else {
                endRow = (endIndex + 1) - deleteRows;
            }
        }

        if (endIndex > numRows) {
            endIndex = numRows;
            endRow = endIndex;
        }

        if (numRows < pageConfig) {
            endRow = numRows;
        }

        // Row text
        if (numRows == 0) {
            // No rows visible possibly due to filtering
            numRowsLbl
                    .setText("No rows to display. Please check the configuration and "
                            + "filtering options.");
        } else if (startIndex == endIndex) {
            numRowsLbl.setText(ROW + startRow + " of " + numRows);
            // Initial Load with over the number of configured records per page
        } else if (startIndex == 0 && (numRows > pageConfig)) {
            numRowsLbl.setText(ROWS + startRow + " - " + pageConfig + " of "
                    + numRows);
            // Number of records are less than the page config
        } else if (numRows < pageConfig) {
            numRowsLbl.setText(ROWS + startRow + " - " + endRow + " of "
                    + numRows);
        } else if (numRowsLbl != null) {
            numRowsLbl.setText(ROWS + startRow + " - " + endRow + " of "
                    + numRows);
        }

        deleteFlag = false;
        pageDeleteFlag = false;
    }

    /**
     * Refresh table after configurations have changed.
     */
    public void tableChangedAfterConfigLoad() {
        startIndex = 0;
        configChange = true;

        if (numRows > 0) {
            getTableRows();
        }

        table.setRedraw(false);

        TableColumn[] columns = table.getColumns();
        for (TableColumn column : columns) {
            column.dispose();
        }

        table.removeAll();
        createColumns();
        table.setRedraw(true);

        populateTableDataRows(null);

        populateTable();

        configChange = false;
    }

    /**
     * Delete record list.
     */
    private void deleteRecords(ArrayList<Integer> deleteList) {

        if (deleteList.size() > 0) {
            deleteTableDataRows(deleteList);
            populateTable();
        }
    }

    /**
     * Action taken when deleted notifications by time.
     */
    public void handleDeleteOlderThan() {

        deleteFlag = true;

        // Verify that at least one notification was selected.
        int[] indices = table.getSelectionIndices();

        if (indices == null || indices.length == 0) {
            return;
        }

        if (indices.length > 1) {
            DataDeliveryUtils.showMessage(getShell(), SWT.OK,
                    "Selection Error", "Please select only one row.");
            return;
        }

        NotificationRowData row = filteredTableList.getDataArray().get(
                indices[0]);
        ArrayList<Integer> deleteList = new ArrayList<Integer>();

        // Loop over rows and delete the matching rows
        for (NotificationRowData data : filteredTableList.getDataArray()) {
            // Priority is 0 based, so must subtract 1
            if (data.getDate().before(row.getDate())) {
                deleteList.add(data.getId());
            }
        }

        if (deleteList.size() > 0) {

            deleteRows = deleteList.size();
            if (deleteRows < visibleTableList.size()) {
                selectedPage = pageCbo.getSelectionIndex() + 1;

                // If all rows on a page are deleted go to page 1
            } else {
                selectedPage = 0;
                pageCbo.select(selectedPage);
                pageDeleteFlag = true;
            }

            deleteRecords(deleteList);
        }
    }

    /**
     * Calculate number of pages needed
     */
    private int getNumberOfPages() {

        // Calculate number of pages needed
        if (pageConfig != 0) {
            pageAmt = (numRows / pageConfig);
        }

        // Add an extra page if excess rows
        if (numRows > (pageAmt * pageConfig)) {
            pageAmt = pageAmt + 1;
        }

        // Clear pages array list
        if (pages != null) {
            pages.clear();
        }

        // Add necessary pages
        for (int i = 1; i <= pageAmt; i++) {
            pages.add(String.valueOf(i));
        }

        return pageAmt;
    }

    /**
     * Action taken to delete notifications by priority.
     *
     * @param priority
     *            priority indicator
     */
    public void handleDeleteByPriority(int priority) {

        deleteFlag = true;
        ArrayList<Integer> deleteList = new ArrayList<Integer>();

        // Loop over rows and delete the matching rows
        for (NotificationRowData data : filteredTableList.getDataArray()) {
            // Priority is 0 based, so must subtract 1
            if (data.getPriority() == priority - 1) {
                deleteList.add(data.getId());
            }
        }

        if (deleteList.size() > 0) {

            deleteRows = deleteList.size();
            if (deleteRows < visibleTableList.size()) {
                selectedPage = pageCbo.getSelectionIndex() + 1;

                // If all rows on a page are deleted go to page 1
            } else {
                selectedPage = 0;
                pageCbo.select(selectedPage);
                pageDeleteFlag = true;
            }

            deleteRecords(deleteList);
        }
    }

    /**
     * Get the start index.
     *
     * @return start index
     */
    public int getStartIndex() {
        return startIndex;
    }

    /**
     * Get the end index.
     *
     * @return end index
     */
    public int getEndIndex() {
        return endIndex;
    }

    /**
     * Get the selected index.
     *
     * @return selected index
     */
    public int getSelectedIndex() {
        return selectedIndex;
    }

    /**
     * Get the table cell text.
     *
     * @param name
     *            The column name
     * @param rd
     *            The data object
     * @return The text for the table cell
     */
    private String getCellText(String name, NotificationRowData rd) {
        String returnValue = null;

        if (name.equals(NotifColumnNames.TIME.getColumnName())) {
            SimpleDateFormat format = new SimpleDateFormat(
                    "yyyy-MM-dd HH:mm:ss");
            format.setTimeZone(TimeZone.getTimeZone("GMT"));
            returnValue = format.format(rd.getDate());
        } else if (name.equals(NotifColumnNames.PRIORITY.getColumnName())) {
            returnValue = String.valueOf(rd.getPriority());
        } else if (name.equals(NotifColumnNames.CATEGORY.getColumnName())) {
            returnValue = rd.getCategory();
        } else if (name.equals(NotifColumnNames.USER.getColumnName())) {
            returnValue = rd.getUser();
        } else if (name.equals(NotifColumnNames.MESSAGE.getColumnName())) {
            returnValue = rd.getMessage();
        }
        return returnValue;
    }

    /**
     * Populate the NotificationRowData objects
     *
     * @param notificationRecords
     *            list of notification records
     */
    public void populateTableDataRows(
            ArrayList<NotificationRecord> notificationRecords) {
        List<NotificationRecord> notificationList = new ArrayList<NotificationRecord>();

        NotificationConfigManager configMan = NotificationConfigManager
                .getInstance();
        ArrayList<String> users = configMan.getFilterXml().getUserFilterXml()
                .getUserList();

        if (notificationRecords == null) {
            MessageLoadXML messageLoad = msgLoadCallback.getMessageLoad();
            handler = new NotificationHandler();
            notificationList = handler.intialLoad(messageLoad, users);
            masterTableList.clearAll();
            currentRecordIds.clear();
        } else {
            for (NotificationRecord rec : notificationRecords) {
                // prevents duplicates
                if (currentRecordIds.contains(rec.getId()) == false) {
                    notificationList.add(rec);
                }
            }
        }

        if (notificationList == null || notificationList.isEmpty()) {
            return;
        }

        for (NotificationRecord record : notificationList) {
            NotificationRowData rd = new NotificationRowData();
            rd.setId(record.getId());
            rd.setDate(record.getDate().getTime());
            rd.setCategory(record.getCategory());
            rd.setMessage(record.getMessage());
            rd.setPriority(record.getPriority());
            rd.setUser(record.getUsername());
            // Master table list is filtered for user only
            masterTableList.addDataRow(rd);
            currentRecordIds.add(rd.getId());
            ++messageReceivedWhilePausedCount;
        }

        resetTable();

        filteredTableList.clearAll();

        // Apply filters to the master list to get filteredTableList
        for (NotificationRowData rd : this.masterTableList.getDataArray()) {

            // Apply filter
            if (passesFilter(rd.getUser(), rd.getPriority(), rd.getCategory())) {
                filteredTableList.addDataRow(rd);
            }
        }

        updateSortDirection(this.sortedColumn, filteredTableList, false);
    }

    /**
     * Clear the table display.
     */
    private void resetTable() {
        if (!pauseButton.getSelection()) {
            // only refresh table if not paused.
            table.clearAll();
            table.removeAll();
        }
    }

    /**
     * Pass filter information.
     *
     * @param username
     *            user name table data
     * @param priority
     *            data priority table data
     * @param subscription
     *            subscription table data
     * @return boolean true if passes filter
     */
    private boolean passesFilter(String username, int priority,
            String subscription) {

        boolean userFlag = false;
        boolean subscriptionFlag = false;
        boolean priorityFlag = false;
        boolean filterFlag = false;

        NotificationConfigManager configMan = NotificationConfigManager
                .getInstance();
        ArrayList<String> users = configMan.getFilterXml().getUserFilterXml()
                .getUserList();
        ArrayList<Priority> priorityList = configMan.getFilterXml()
                .getPriorityList();
        ArrayList<String> subscriptions = configMan.getFilterXml()
                .getSubscriptionList();
        ArrayList<Integer> num = new ArrayList<Integer>();

        for (Priority p : priorityList) {
            num.add(p.getPriorityNum());
        }

        if (username != null) {
            if (users == null || users.isEmpty() || username.equals("")
                    || users.contains(username)) {
                userFlag = true;
            }
        }

        if (subscriptions == null || subscriptions.isEmpty()
                || subscriptions.equals("")
                || subscriptions.contains(subscription)) {
            subscriptionFlag = true;
        }

        if ((num.contains(priority + 1))) {
            priorityFlag = true;
        }

        if (userFlag && subscriptionFlag && priorityFlag) {
            filterFlag = true;
        }

        return filterFlag;

    }

    /**
     * Pass filter information.
     *
     * @param records
     *            Notification record
     * @return boolean true if passes filter
     *
     */
    public boolean passesFilter(List<NotificationRecord> records) {
        for (NotificationRecord record : records) {
            if (passesFilter(record.getUsername(), record.getPriority(),
                    record.getCategory()) == false) {
                return false;
            }
        }
        return true;
    }

    /**
     * Get the column data.
     *
     * @param colName
     *            The column name of interest
     *
     * @return The populated ColumnXML object
     */
    private ColumnXML getColumnData(String colName) {

        if (columnList == null) {
            NotificationConfigManager configMan = NotificationConfigManager
                    .getInstance();

            columnList = configMan.getConfigXml().getColumnList();
        }

        for (ColumnXML column : columnList) {
            if (column.getName().equals(colName)) {
                return column;
            }
        }

        return null;
    }

    /**
     * Get the current sort direction.
     */
    @Override
    protected SortDirection getCurrentSortDirection() {
        return filteredTableList.getSortDirection();
    }

    /**
     * Create table columns.
     */
    @Override
    protected void createColumns() {
        NotificationConfigManager configMan;
        configMan = NotificationConfigManager.getInstance();

        // Get list of columns from config
        NotificationConfigXML xml = configMan.getConfigXml();

        if (columnList == null) {
            columnList = xml.getColumnList();
        }

        pageConfig = xml.getPaginationSetting();

        PrioritySettingXML pri = xml.getPrioritySetting();
        PriorityDisplay pd = PriorityDisplay.ColorNumName;
        if (pri != null) {
            if (pri.isColorNum()) {
                pd = PriorityDisplay.ColorNum;
            } else if (pri.isColor()) {
                pd = PriorityDisplay.Color;
            } else if (pri.isNum()) {
                pd = PriorityDisplay.Num;
            } else if (pri.isNumName()) {
                pd = PriorityDisplay.NumName;
            }
        }
        pImage.setPriorityDisplay(pd);

        SortDirection sortDir = SortDirection.DESCENDING;

        // Get the ones that are visible
        for (ColumnXML column : columnList) {
            if (column.isVisible()) {
                TableColumn tc = new TableColumn(table, SWT.NONE);
                String colName = column.getName();

                tc.setText(colName);
                if (colName.equals(NotifColumnNames.PRIORITY.toString())) {
                    tc.setAlignment(SWT.CENTER);
                } else {
                    tc.setAlignment(SWT.LEFT);
                }
                tc.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        handleColumnSelection((TableColumn) event.getSource());
                    }
                });

                if (column.getName()
                        .equals(NotifColumnNames.MESSAGE.toString())) {
                    tc.setResizable(true);
                } else {
                    tc.setResizable(false);
                }

                if (!initialized) {
                    // Find which column is configured to be the sort column
                    boolean sortCol = column.isSortColumn();
                    if (sortCol) {
                        sortedColumn = tc;
                    }

                    // Check if any columns set to descending
                    boolean sortAsc = column.isSortAsc();
                    if (!sortAsc) {
                        sortDir = SortDirection.ASCENDING;
                    }

                    sortDirectionMap.put(colName, sortDir);
                } else {
                    if (tc.getText().equals(sortedColumnName)) {
                        sortedColumn = tc;
                        sortDirectionMap.put(tc.getText(), sortedDirectionName);
                    }
                }

            }

        }

    }

    /**
     * Populate the table with data.
     */
    @Override
    public void populateTable() {
        if (pauseButton.getSelection()) {
            // no table repopulate if paused.
            return;
        }

        int highlightIndex = 0;
        int[] indicesArr = null;

        ArrayList<Integer> items = new ArrayList<Integer>();

        sortedColumnName = sortedColumn.getText();
        sortedDirectionName = sortDirectionMap.get(sortedColumnName);

        filteredTableList.sortData();

        TableColumn[] columns = table.getColumns();

        resetTable();

        getTableRows();

        for (NotificationRowData rd : this.visibleTableList) {
            int idx = 0;
            TableItem item = new TableItem(table, SWT.NONE);

            for (TableColumn column : columns) {
                ColumnXML columnXml = getColumnData(column.getText());
                if (columnXml != null) {
                    if (columnXml.isVisible()) {
                        if (column.getText().equals(
                                NotifColumnNames.PRIORITY.toString())) {
                            item.setImage(idx++, pImage.getImage(Priority
                                    .values()[rd.getPriority()]));
                        } else {
                            String text = getCellText(columnXml.getName(), rd);
                            if (text == null) {
                                item.setText(idx++, "");
                            } else {
                                item.setText(idx++, text);
                            }
                        }
                    }
                }
            }
        }

        // Update the bottom label values
        updateLabels();

        // check indices array for highlight all
        if (indices != null) {
            if (indices.length > 0) {
                for (int index : indices) {

                    if (index >= startIndex && index <= endIndex) {
                        if (startIndex == 0) {
                            highlightIndex = index;
                        } else if (selectedPage > 0) {
                            int extra = (selectedPage * pageConfig);
                            highlightIndex = index - extra;
                        } else {
                            highlightIndex = index - pageConfig;
                        }

                        items.add(highlightIndex);
                    }

                }

                indicesArr = new int[items.size()];

                // Rows to highlight
                for (int i = 0; i < items.size(); i++) {
                    indicesArr[i] = items.get(i);
                }

                table.select(indicesArr);
            }
        }

        updateColumnSortImage();

        initialized = true;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.viz.datadelivery.common.ui.TableComp#handleTableMouseClick
     * (org.eclipse.swt.events.MouseEvent)
     */
    @Override
    protected void handleTableMouseClick(MouseEvent event) {

    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.viz.datadelivery.common.ui.TableComp#
     * handleTableSelectionChange(org.eclipse.swt.events.SelectionEvent)
     */
    @Override
    protected void handleTableSelection(SelectionEvent e) {
        if (tableChangeCallback != null) {
            tableChangeCallback.tableSelection();
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.viz.core.notification.INotificationObserver#
     * notificationArrived
     * (com.raytheon.uf.viz.core.notification.NotificationMessage[])
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        /*
         * This method is not used. The Notification dialog is using the
         * NotificationHandler so this override method is not used.
         */
    }

    /**
     * Handle the page selection.
     */
    @Override
    public void handlePageSelection() {
        // Page selection
        if (findFlag) {
            findFlag = false;
        } else {
            String selection = pageCbo.getItem(pageCbo.getSelectionIndex());
            selectedPage = Integer.parseInt(selection);
        }

        // Calculate indices
        if (selectedPage >= 1) {

            startIndex = ((pageConfig * selectedPage) - (pageConfig - 1)) - 1;
            lastRow = filteredTableList.getDataArray().size();

            endIndex = (pageConfig * selectedPage) - 1;

            if (lastRow < endIndex) {
                endIndex = lastRow - 1;
            }

        } else {
            startIndex = 0;
            endIndex = pageConfig - 1;
        }

        populateTable();
    }

    /**
     * Find the selected table row index.
     */
    @Override
    public void selectIndex(int index) {
        findFlag = true;

        TableItem item;

        int extra = 0;
        highlightIndex = 0;
        selectedIndex = index;

        // get what page index is on
        if (index > pageConfig && (index != ((selectedPage + 1) * pageConfig))) {
            selectedPage = (index / pageConfig) + 1;
        } else {
            selectedPage = (index / pageConfig);
        }

        // switch pages
        handlePageSelection();

        selectedPage = (index / pageConfig);
        pageCbo.select(selectedPage);

        if (index > pageConfig) {

            // Calculate the index number for the current visible rows
            extra = (selectedPage) * pageConfig;
            if (index != extra) {
                highlightIndex = index - extra;
            } else {
                highlightIndex = pageConfig;
            }

            if (highlightIndex > 0) {
                item = table.getItem(highlightIndex - 1);
            } else {
                item = table.getItem(highlightIndex);
            }

        } else {
            if (index > 0) {
                item = table.getItem(index - 1);
            } else {
                item = table.getItem(index);
            }

        }

        table.setSelection(item);

    }

    /**
     * Find the selected table row indices.
     */
    @Override
    public void selectIndices(int[] indices) {
        this.indices = indices;

        // highlight table rows
        table.select(indices);
        handlePageSelection();
    }

    /**
     * Returns a string with a count of the messages received while paused.
     * 
     * @return formatted string.
     */
    public String getPauseCountLabel() {
        StringBuilder pauseString = new StringBuilder();

        if (messageReceivedWhilePausedCount > 0) {
            pauseString.append(" <Received ");
            pauseString.append(messageReceivedWhilePausedCount);
            pauseString.append(" message");
            if (messageReceivedWhilePausedCount > 1) {
                pauseString.append("s");
            }
            pauseString.append(" while Paused>");
        } else {
            pauseString.append(PAUSE_MSG_TEXT);
        }
        return pauseString.toString();
    }

    /**
     * @return true if pause is selected by the user; false otherwise.
     */
    public boolean isLocked() {
        return pauseButton.getSelection();
    }
}
