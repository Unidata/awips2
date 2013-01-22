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
package com.raytheon.uf.viz.datadelivery.subscription;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.datadelivery.service.SubscriptionNotificationResponse;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlers;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.datadelivery.common.ui.IGroupAction;
import com.raytheon.uf.viz.datadelivery.common.ui.SortImages.SortDirection;
import com.raytheon.uf.viz.datadelivery.common.ui.TableComp;
import com.raytheon.uf.viz.datadelivery.common.ui.TableCompConfig;
import com.raytheon.uf.viz.datadelivery.common.ui.TableDataManager;
import com.raytheon.uf.viz.datadelivery.common.ui.ViewDetailsDlg;
import com.raytheon.uf.viz.datadelivery.common.xml.ColumnXML;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.subscription.subset.SubsetManagerDlg;
import com.raytheon.uf.viz.datadelivery.subscription.xml.SubscriptionManagerConfigXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.SubColumnNames;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;

/**
 * Common subscription table composite that consolidates some of the common code
 * that is used by the subscription manager and the subscription viewer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2012            lvenable     Initial creation.
 * Jul 16, 2012   702      jpiatt       Modifications for subscription group.
 * Aug 21, 2012   712      mpduff       Fix problem with notifications not
 *                                      storing the subscription object.
 * Aug 30, 2012  1120      jpiatt       Added clickSort flag.
 * Aug 31, 2012  1128      mpduff       Added some typing to objects.
 * Sep 17, 2012  1157      mpduff       Check for pending subscription for proper table updates.
 * Sep 27, 2012  1202      bgonzale     Switched comparison in updateTable.
 * Oct 03, 2012  1241      djohnson     Use {@link DataDeliveryPermission} and registry handlers.
 * Dec 03, 2012  1279      mpduff       Add ability to populate from a list of subscription names.
 * Dec 12, 2012  1391      bgonzale     Added a job for subscription retrieves.
 * Jan 07, 2013  1437      bgonzale     Added sort column direction updates.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class SubscriptionTableComp extends TableComp implements IGroupAction {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionTableComp.class);

    /** Pop up menu object. */
    private Menu popupMenu;

    /**
     * Subscription action callback that is called when there is a table
     * selection.
     */
    private ISubscriptionAction subActionCallback;

    /** TableDataManager object. */
    private TableDataManager<SubscriptionManagerRowData> subManagerData;

    /**
     * Enumeration to determine the type of subscription dialog this class is
     * used with.
     */
    public static enum SubscriptionType {
        /**
         * Manager permissions.
         */
        MANAGER,
        /**
         * View permissions.
         */
        VIEWER;
    }

    /** The subscription type. */
    private SubscriptionType subType = SubscriptionType.VIEWER;

    // volatile so Eclipse job threads will see the reference that is set on the
    // UI thread
    private volatile List<String> subscriptionNameList;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param tableConfig
     *            Table configuration.
     * @param callback
     *            Subscription action callback.
     * @param subType
     *            Subscription type.
     */
    public SubscriptionTableComp(Composite parent, TableCompConfig tableConfig,
            ISubscriptionAction callback, SubscriptionType subType) {
        super(parent, tableConfig, true);

        this.subType = subType;
        this.subActionCallback = callback;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        /*
         * Setup the layout for the composite
         */
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        subManagerData = new TableDataManager<SubscriptionManagerRowData>(
                TABLE_TYPE.SUBSCRIPTION);

        createColumns();
    }

    /**
     * Handle subscription editing.
     */
    public void handleEdit() {
        if (table.getSelectionCount() == 0) {
            DataDeliveryUtils.showMessage(this.getShell(), SWT.ERROR,
                    "No Rows Selected", "Please select a row to Edit");
            return;
        }

        if (table.getSelectionCount() > 1) {
            int choice = DataDeliveryUtils
                    .showMessage(
                            this.getShell(),
                            SWT.ERROR | SWT.YES | SWT.NO,
                            "Single Selection Only",
                            "Multiple subscriptions are selected.\n"
                                    + "Only the first selected item will be edited.\n\n"
                                    + "Continue with Edit?");
            if (choice == SWT.NO) {
                return;
            }
        }

        final DataDeliveryPermission permission = DataDeliveryPermission.SUBSCRIPTION_CREATE;
        IUser user = UserController.getUserObject();
        String msg = user.uniqueId()
                + " is not authorized to access the Dataset Discovery Browser\nPermission: "
                + permission;

        try {
            if (DataDeliveryServices.getPermissionsService()
                    .checkPermissions(user, msg, permission).isAuthorized()) {
                // Get the subscription data
                int idx = table.getSelectionIndex();
                SubscriptionManagerRowData row = subManagerData.getDataRow(idx);
                SubsetManagerDlg<?, ?, ?> dlg = SubsetManagerDlg
                        .fromSubscription(this.getShell(), true,
                                row.getSubscription());

                dlg.open();
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred in authorization request", e);
        }
    }

    /**
     * Open the Add To Group dialog.
     */
    private void handleGroupAdd() {

        // Ensure a row is selected
        if (table.getSelectionCount() == 0) {
            DataDeliveryUtils.showMessage(this.getShell(), SWT.ERROR,
                    "No Rows Selected", "Please select a row to Edit");
            return;
        }

        if (table.getSelectionCount() > 1) {
            int choice = DataDeliveryUtils
                    .showMessage(
                            this.getShell(),
                            SWT.ERROR | SWT.YES | SWT.NO,
                            "Single Selection Only",
                            "Multiple subscriptions are selected.\n"
                                    + "Only the first selected item will be edited.\n\n"
                                    + "Continue with Edit?");
            if (choice == SWT.NO) {
                return;
            }
        }

        // Check permissions
        final DataDeliveryPermission permission = DataDeliveryPermission.SUBSCRIPTION_EDIT;
        IUser user = UserController.getUserObject();
        String msg = user.uniqueId()
                + " is not authorized to access Group Add\nPermission: "
                + permission;

        try {
            if (DataDeliveryServices.getPermissionsService()
                    .checkPermissions(user, msg, permission).isAuthorized()) {
                // Get the subscription data
                int idx = table.getSelectionIndex();
                SubscriptionManagerRowData row = subManagerData.getDataRow(idx);
                GroupAddDlg groupAdd = new GroupAddDlg(this.getShell(),
                        row.getSubscription(), this);
                groupAdd.open();
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred in authorization request", e);
        }
    }

    /**
     * Handle displaying the details information.
     */
    private void handleDetails() {
        StringBuilder printDetails = new StringBuilder();
        int[] selectionIndices = table.getSelectionIndices();

        for (int i = 0; i < selectionIndices.length; i++) {
            if (i > 0) {
                printDetails.append("\n\n****************************\n\n");
            }
            SubscriptionManagerRowData rowData = subManagerData
                    .getDataRow(selectionIndices[i]);

            // get the subscription details to be displayed to the user
            printDetails.append(DataDeliveryUtils.formatDetails(rowData
                    .getSubscription()));
        }

        // Pass the subscription details to be displayed
        String title;
        if (selectionIndices.length == 1) {
            title = subManagerData.getDataRow(selectionIndices[0]).getName()
                    + " Details";
        } else {
            title = "Details";
        }

        // TODO : should this open one dialog at a time or open several but they
        // are all unique?
        ViewDetailsDlg subDetails = new ViewDetailsDlg(this.getShell(),
                printDetails.toString(), title, null, null);
        subDetails.open();
    }

    /**
     * Get the table.
     * 
     * @return The table.
     */
    public Table getTable() {
        return table;
    }

    /**
     * Action performed when the column is selected.
     * 
     * @param tc
     *            Table column.
     */
    private void handleColumnSelection(TableColumn tc) {
        SortDirection sortDirection = updateSortDirection(tc, subManagerData,
                true);

        // update the xml
        SubscriptionConfigurationManager man = SubscriptionConfigurationManager
                .getInstance();
        man.setSortedColumn(tc.getText(), sortDirection);

        // Populate the data
        populateData();

        // Sort the table data.
        // sortTableData(tc);

        populateTable();

        updateColumnSortImage();
    }

    /**
     * Populate the table data according to the filter selections, .
     * 
     * @param groupName
     *            name of subscription group
     * 
     * @param officeId
     *            Office identifier, such as OAX
     */
    public void populateFilteredData(String groupName, String officeId) {
        subManagerData.clearAll();

        if ("All Subscriptions".equals(groupName)) {
            groupName = null;
        }

        if ("ALL".equals(officeId)) {
            officeId = null;
        }

        try {
            List<Subscription> lo = DataDeliveryHandlers
                    .getSubscriptionHandler().getByFilters(groupName, officeId);
            for (Subscription subscription : lo) {
                addSubscription(subscription);
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve the list of subscriptions.", e);
        }
    }

    /**
     * Populate the table data according to the filter selections, .
     * 
     * @param datasetName
     *            name of dataset. e.g. gfs
     * 
     * @param provider
     *            name of provider. e.g. NOMADS
     */
    public void populateActiveFilteredDataByDataSetAndProvider(
            String datasetName, String providerName) {
        subManagerData.clearAll();

        try {
            List<Subscription> lo = DataDeliveryHandlers
                    .getSubscriptionHandler().getActiveByDataSetAndProvider(
                            datasetName, providerName);
            for (Subscription subscription : lo) {
                addSubscription(subscription);
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve the list of subscriptions.", e);
        }
    }

    /**
     * Populate the data.
     */
    public void populateData() {
        subManagerData.clearAll();

        final ISubscriptionHandler handler = RegistryObjectHandlers
                .get(ISubscriptionHandler.class);
        final List<Subscription> subList = new ArrayList<Subscription>();
        final Shell jobShell = getShell();

        Job job = new Job("Retrieving Subscriptions...") {
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    DataDeliveryGUIUtils.markBusyInUIThread(jobShell);
                    if (subscriptionNameList == null
                            || subscriptionNameList.isEmpty()) {
                        List<Subscription> lo = handler.getAll();
                        subList.addAll(lo);
                    } else {
                        for (String subName : subscriptionNameList) {
                            Subscription sub = handler.getByName(subName);
                            if (sub != null) {
                                subList.add(sub);
                            }
                        }
                    }
                    return Status.OK_STATUS;
                } catch (RegistryHandlerException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to retrieve the list of subscriptions.", e);
                    return Status.CANCEL_STATUS;
                }
            }
        };
        job.addJobChangeListener(new JobChangeAdapter() {
            @Override
            public void done(IJobChangeEvent event) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        updateTable(subList);
                    }
                });
                DataDeliveryGUIUtils.markNotBusyInUIThread(jobShell);
            }
        });
        job.schedule();
    }

    /**
     * Add a subscription to the list of subscriptions.
     * 
     * @param subscription
     *            Subscription to add to the subscription list.
     */
    private void addSubscription(Subscription subscription) {
        SubscriptionManagerRowData rd = new SubscriptionManagerRowData();
        rd.setSubscription(subscription);
        subManagerData.addDataRow(rd);
    }

    /**
     * Get the column data XML class.
     * 
     * @param colName
     *            Column name.
     * @return The column data XML class.
     */
    private ColumnXML getColumnData(String colName) {
        SubscriptionConfigurationManager manager = SubscriptionConfigurationManager
                .getInstance();

        for (ColumnXML column : manager.getXml().getColumnList()) {
            if (column.getName().equals(colName)) {
                return column;
            }
        }

        return null;
    }

    /**
     * Get the table cell text
     * 
     * @param name
     *            The column name
     * @param rd
     *            The data object
     * @return The text for the table cell
     */
    private String getCellText(String name, SubscriptionManagerRowData rd) {
        String returnValue = null;
        if (name.equals(SubColumnNames.NAME.toString())) {
            returnValue = rd.getName();
        } else if (name.equals(SubColumnNames.OWNER.toString())) {
            returnValue = rd.getOwner();
        } else if (name.equals(SubColumnNames.STATUS.toString())) {
            returnValue = rd.getStatus();
        } else if (name.equals(SubColumnNames.PRIORITY.toString())) {
            returnValue = String.valueOf(rd.getPriority());
        } else if (name.equals(SubColumnNames.DESCRIPTION.toString())) {
            returnValue = rd.getDescription();
        } else if (name.equals(SubColumnNames.SUBSCRIPTION_START.toString())) {
            Date date = rd.getSubscriptionStart();
            if (date != null) {
                SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy HH");
                sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
                returnValue = sdf.format(date) + "Z";
            }
        } else if (name.equals(SubColumnNames.SUBSCRIPTION_EXPIRATION
                .toString())) {
            Date date = rd.getSubscriptionEnd();
            if (date == null) {
                returnValue = "No Expiration";
            } else {
                SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy HH");
                sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
                returnValue = sdf.format(date) + "Z";
            }
        } else if (name.equals(SubColumnNames.ACTIVE_START.toString())) {
            Date date = rd.getActiveStart();
            if (date != null) {
                SimpleDateFormat sdf2 = new SimpleDateFormat("MM/dd HH");
                sdf2.setTimeZone(TimeZone.getTimeZone("GMT"));

                returnValue = sdf2.format(date) + "Z";
            }
        } else if (name.equals(SubColumnNames.ACTIVE_END.toString())) {
            Date date = rd.getActiveEnd();
            if (date != null) {
                SimpleDateFormat sdf2 = new SimpleDateFormat("MM/dd HH");
                sdf2.setTimeZone(TimeZone.getTimeZone("GMT"));
                returnValue = sdf2.format(date) + "Z";
            }
        } else if (name.equals(SubColumnNames.DELIVERY.toString())) {
            returnValue = rd.getDeliveryNotify().toString();
        } else if (name.equals(SubColumnNames.OFFICE_ID.toString())) {
            returnValue = rd.getOfficeId();
        } else if (name.equals(SubColumnNames.FULL_DATA_SET.toString())) {
            returnValue = rd.getFullDataSet().toString();
        } else if (name.equals(SubColumnNames.DATA_SIZE.toString())) {
            returnValue = String.valueOf(rd.getDataSetSize());
        } else if (name.equals(SubColumnNames.GROUP_NAME.toString())) {
            returnValue = rd.getGroupName();
        }

        return returnValue;
    }

    /**
     * Update the table with the list of subscriptions.
     * 
     * @param updatedSubscriptions
     *            List of updated subscriptions.
     */
    public synchronized void updateTable(List<Subscription> updatedSubscriptions) {
        for (Subscription s : updatedSubscriptions) {
            if (s.isDeleted() == true) {
                // Delete the data from the table
                SubscriptionManagerRowData smrd;
                int removeIndex = -1;
                for (int i = 0; i < subManagerData.getDataArray().size(); i++) {
                    smrd = subManagerData.getDataArray().get(i);

                    if (smrd.getSubscription().getId().equals(s.getId())) {
                        removeIndex = i;
                        break;
                    }
                }

                if (removeIndex != -1) {
                    subManagerData.getDataArray().remove(removeIndex);
                }
            } else {
                // Update the table

                boolean foundMatch = false;
                for (SubscriptionManagerRowData smrd : subManagerData
                        .getDataArray()) {
                    if (s.getId().equals(smrd.getSubscription().getId())) {
                        smrd.setSubscription(s);
                        foundMatch = true;
                        break;
                    }
                }

                if (foundMatch == false) {
                    if (!(s instanceof PendingSubscription)) {
                        addSubscription(s);
                    }
                }
            }
        }

        populateTable();
    }

    /**
     * Get the data manager for the subscription data.
     * 
     * @return The data manager.
     */
    public TableDataManager<SubscriptionManagerRowData> getSubscriptionData() {
        return subManagerData;
    }

    /**
     * Get the sorted table column.
     * 
     * @return The table column that is sorted.
     */
    public TableColumn getSortedTableColumn() {
        return sortedColumn;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.ui.TableComp#getCurrentSortDirection
     * ()
     */
    @Override
    protected SortDirection getCurrentSortDirection() {
        return subManagerData.getSortDirection();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.common.ui.TableComp#createColumns()
     */
    @Override
    public void createColumns() {
        SubscriptionConfigurationManager configMan;
        configMan = SubscriptionConfigurationManager.getInstance();
        HashMap<String, Integer> alignmentMap = configMan.getAlignmentMap();

        // Get list of columns from config
        SubscriptionManagerConfigXML xml = configMan.getXml();
        ArrayList<ColumnXML> columnList = xml.getColumnList();

        // Get the ones that are visible
        for (ColumnXML column : columnList) {
            if (column.isVisible()) {
                TableColumn tc = new TableColumn(table, SWT.NONE);
                tc.setText(column.getName());
                tc.setAlignment(alignmentMap.get(column.getName()));
                if (column.isSortColumn()) {
                    SortDirection direction = column.isSortAsc() ? SortDirection.ASCENDING
                            : SortDirection.DESCENDING;
                    sortedColumn = tc;
                    subManagerData.setSortDirection(direction);
                    subManagerData.setSortColumn(column.getName());
                }
                tc.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        handleColumnSelection((TableColumn) event.getSource());
                    }
                });
            }
        }

        for (TableColumn tc : table.getColumns()) {
            sortDirectionMap.put(tc.getText(), SortDirection.ASCENDING);
        }

        populateTable();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.common.ui.TableComp#populateTable()
     */
    @Override
    public void populateTable() {
        TableColumn[] columns = table.getColumns();
        table.clearAll();
        table.removeAll();

        subManagerData.sortData();

        for (SubscriptionManagerRowData rd : subManagerData.getDataArray()) {
            int idx = 0;
            TableItem item = new TableItem(table, SWT.NONE);
            for (TableColumn column : columns) {
                ColumnXML columnXml = getColumnData(column.getText());
                if (columnXml != null) {
                    if (columnXml.isVisible()) {

                        String text = getCellText(columnXml.getName(), rd);
                        if (text == null) {
                            item.setText(idx++, "");
                        } else {
                            item.setText(idx++, text);
                        }
                        if (columnXml.isSortColumn()) {
                            SortDirection direction = columnXml.isSortAsc() ? SortDirection.ASCENDING
                                    : SortDirection.DESCENDING;
                            sortedColumn = column;
                            subManagerData.setSortDirection(direction);
                            subManagerData.setSortColumn(columnXml.getName());
                        }
                    }
                }
            }
        }

        if (table.getItemCount() > 0) {
            if (sortedColumn == null || sortedColumn.isDisposed()) {
                // use default sort column settings
                TableColumn column = table.getColumn(0);
                subManagerData.setSortDirection(SortDirection.ASCENDING);
                subManagerData.setSortColumn(column.getText());
            }
        }
        updateColumnSortImage();
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

        if (event.button != 3) {
            return;
        }

        if (popupMenu != null) {
            popupMenu.dispose();
        }

        // Detail popup menu
        popupMenu = new Menu(table);
        MenuItem item1 = new MenuItem(popupMenu, SWT.PUSH);
        item1.setText("Details...   ");
        item1.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleDetails();
            }
        });

        if (subType == SubscriptionType.MANAGER) {
            MenuItem editItem = new MenuItem(popupMenu, SWT.PUSH);
            editItem.setText("Edit...");
            editItem.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    handleEdit();
                }
            });

            // Add the selected row to a subscription group
            MenuItem groupItem = new MenuItem(popupMenu, SWT.PUSH);
            groupItem.setText("Add to Group...");
            groupItem.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    handleGroupAdd();
                }
            });
        }

        table.setMenu(popupMenu);

        popupMenu.setVisible(true);

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.common.ui.TableComp#
     * handleTableSelectionChange(org.eclipse.swt.events.SelectionEvent)
     */
    @Override
    protected void handleTableSelection(SelectionEvent e) {
        TableItem item = table.getSelection()[0];
        TableColumn[] columns = table.getColumns();

        for (int i = 0; i < columns.length; i++) {
            if (columns[i].getText().equals("Active")) {
                if (item.getText(i).equalsIgnoreCase("T")) {
                    subActionCallback.activateButtonUpdate("Deactivate");
                } else {
                    subActionCallback.activateButtonUpdate("Activate");
                }
            }
        }
    }

    /**
     * This method will update the subscription table with any updates,
     * deletions, or new subscriptions.
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        final ArrayList<Subscription> updatedSubscriptions = new ArrayList<Subscription>();
        try {
            for (NotificationMessage msg : messages) {
                Object obj = msg.getMessagePayload();
                if (obj instanceof SubscriptionNotificationResponse) {
                    SubscriptionNotificationResponse response = (SubscriptionNotificationResponse) obj;
                    Subscription sub = response.getSubscription();
                    if (sub != null) {
                        updatedSubscriptions.add(sub);
                    }
                }
            }
        } catch (NotificationException e) {
            statusHandler.error("Error when receiving notification", e);
        }

        if (updatedSubscriptions.isEmpty() == false) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    if (isDisposed() == false) {
                        updateTable(updatedSubscriptions);
                    }
                }
            });
        }
    }

    @Override
    public void groupSelectionUpdate(String fileName) {
        // not implemented
    }

    @Override
    public void loadGroupNames() {
        // not implemented
    }

    /**
     * Refresh the subscription table data.
     */
    @Override
    public void handleRefresh() {
        populateData();
        populateTable();

    }

    @Override
    public String getGroupNameTxt() {

        int idx = table.getSelectionIndex();
        SubscriptionManagerRowData row = subManagerData.getDataRow(idx);
        String groupName = row.getGroupName();
        return groupName;
    }

    /**
     * Set the subscription list.
     * 
     * @param subscriptionNameList
     */
    public void setSubscriptionNameList(List<String> subscriptionNameList) {
        this.subscriptionNameList = subscriptionNameList;
    }
}
