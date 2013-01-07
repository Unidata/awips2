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
package com.raytheon.uf.viz.datadelivery.subscription.approve;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.datadelivery.event.notification.ApprovedPendingSubscriptionNotificationResponse;
import com.raytheon.uf.common.datadelivery.event.notification.BaseSubscriptionNotificationResponse;
import com.raytheon.uf.common.datadelivery.event.notification.DeniedPendingSubscriptionNotificationResponse;
import com.raytheon.uf.common.datadelivery.event.notification.PendingSubscriptionNotificationResponse;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.datadelivery.common.ui.SortImages.SortDirection;
import com.raytheon.uf.viz.datadelivery.common.ui.TableComp;
import com.raytheon.uf.viz.datadelivery.common.ui.TableCompConfig;
import com.raytheon.uf.viz.datadelivery.common.ui.TableDataManager;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.PendingSubColumnNames;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;

/**
 * Table composite used for the subscription approval dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2012   687      lvenable     Initial creation
 * Aug 21, 2012   712      mpduff       Subscription ID fix.
 * Aug 30, 2012  1120      jpiatt       Added clickSort flag.
 * Sep 07, 2012  1102      djohnson     Log registry errors.
 * Sep 17, 2012  1157      mpduff       Handle notifications for table updating.
 * Oct 05, 2012 1241       djohnson     Replace RegistryManager calls with registry handler calls.
 * Nov 28, 2012 1286       djohnson     Remove sysout.
 * Dec 19, 2012 1413       bgonzale     In the notificationArrived method, check for approved or
 *                                      denied pending messages.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class SubApprovalTableComp extends TableComp {
    /**
     * The available actions
     */
    public enum Action {
        /** Create new subscription action */
        CREATE("Create"),
        /** Edit subscription action */
        EDIT("Edit"),
        /** Delete subscription action */
        DELETE("Delete");

        /** The Action */
        private String action;

        private Action(String action) {
            this.action = action;
        }
    }

    /** Status Handler */
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubApprovalTableComp.class);

    /** Pending Subscription Manager Data object */
    private TableDataManager<SubscriptionApprovalRowData> pendingSubData;

    /** Pop up menu object. */
    private Menu popupMenu;

    /** Callback to the main dialog */
    private final ISubscriptionApprovalAction callback;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent composite.
     * @param tableConfig
     *            Table configuration.
     * @param callback
     *            Reference back to the dialog
     */
    public SubApprovalTableComp(Composite parent, TableCompConfig tableConfig,
            ISubscriptionApprovalAction callback) {
        super(parent, tableConfig, true);
        this.callback = callback;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        pendingSubData = new TableDataManager<SubscriptionApprovalRowData>(
                TABLE_TYPE.PENDING_SUBSCRIPTION);

        populateData();
        createColumns();
        populateTable();
        packColumns();
    }

    /**
     * Populate data.
     */
    private void populateData() {
        pendingSubData.clearAll();

        try {
            // Get all pending subscriptions
            List<InitialPendingSubscription> results = DataDeliveryHandlers
                    .getPendingSubscriptionHandler()
                    .getAll();
            for (InitialPendingSubscription pendingSub : results) {
                addSubscription(pendingSub);
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    DataDeliveryUtils.UNABLE_TO_RETRIEVE_PENDING_SUBSCRIPTIONS,
                    e);
        }
    }

    /**
     * Add a subscription to the list of data.
     *
     * @param subscription
     *            Subscription to add.
     */
    private void addSubscription(InitialPendingSubscription subscription) {
        SubscriptionApprovalRowData rd = new SubscriptionApprovalRowData();
        rd.setSubscription(subscription);

        this.pendingSubData.addDataRow(rd);
    }

    /**
     * Get the cell text.
     *
     * @param columnName
     *            Column Name.
     * @param rd
     *            Row of approval data.
     * @return Cell text string.
     */
    private String getCellText(String columnName, SubscriptionApprovalRowData rd) {
        String returnValue = null;

        if (columnName.equals(PendingSubColumnNames.NAME.getColumnName())) {
            returnValue = rd.getSubName();
        } else if (columnName.equals(PendingSubColumnNames.OWNER
                .getColumnName())) {
            returnValue = rd.getOwner();
        } else if (columnName.equals(PendingSubColumnNames.CHANGE_ID
                .getColumnName())) {
            returnValue = rd.getChangeOwner();
        } else if (columnName.equals(PendingSubColumnNames.OFFICE
                .getColumnName())) {
            returnValue = rd.getOfficeId();
        } else if (columnName.equals(PendingSubColumnNames.DESCRIPTION
                .getColumnName())) {
            returnValue = rd.getDescription();
        } else if (columnName.equals(PendingSubColumnNames.ACTION
                .getColumnName())) {
            returnValue = rd.getAction();
        }

        return returnValue;
    }

    /**
     * Update the table.
     *
     * @param updatedSubscriptions
     *            Updated subscriptions.
     */
    public synchronized void updateTable(
            ArrayList<InitialPendingSubscription> updatedSubscriptions) {
        for (Subscription s : updatedSubscriptions) {
            if (s != null) {
                if (s.isDeleted()) {
                    // Delete the data from the table
                    SubscriptionApprovalRowData rd;
                    int removeIndex = -1;
                    for (int i = 0; i < this.pendingSubData.getDataArray()
                            .size(); i++) {
                        rd = pendingSubData.getDataArray().get(i);

                        try {
                            if (rd.getSubscription().getId().equals(s.getId())) {
                                removeIndex = i;
                                break;
                            }
                        } catch (RuntimeException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                    }

                    if (removeIndex != -1) {
                        pendingSubData.getDataArray().remove(removeIndex);
                    }
                } else {
                    // Update the table

                    boolean foundMatch = false;
                    for (SubscriptionApprovalRowData rd : pendingSubData
                            .getDataArray()) {
                        if (rd.getSubscription().getId().equals(s.getId())) {
                            rd.setSubscription((InitialPendingSubscription) s);
                            foundMatch = true;
                            break;
                        }
                    }

                    if (foundMatch == false) {
                        addSubscription((InitialPendingSubscription) s);
                    }
                }
            }
        }

        populateTable();
    }

    /**
     * Display the details for the selected table item.
     */
    public void handleDetails() {
        StringBuilder diffDetails = new StringBuilder();
        final String nl = "\n";

        SubscriptionApprovalRowData rowData = pendingSubData.getDataRow(table
                .getSelectionIndex());

        // Get the subscription object
        InitialPendingSubscription pendingSub = rowData.getSubscription();
        diffDetails.append("Subscription Name: ")
                .append(pendingSub.getName()).append(nl);
        diffDetails.append("Dataset Name: ")
                .append(pendingSub.getDataSetName()).append(nl);
        diffDetails.append("Subscription Owner: ")
                .append(pendingSub.getOwner()).append(nl);
        if (pendingSub.getChangeReason() != null) {
            diffDetails.append("Reason for Change: ")
                    .append(pendingSub.getChangeReason())
                    .append(nl);
        }

        // Only PendingSubscriptions area associated to Subscriptions, not
        // InitialPendingSubscriptions
        if (pendingSub instanceof PendingSubscription) {
            Subscription origSub;
            try {
                origSub = DataDeliveryHandlers.getSubscriptionHandler()
                        .getByPendingSubscription(
                                (PendingSubscription) pendingSub);
                if (origSub == null) {
                    throw new RegistryHandlerException(
                            new NullPointerException("origSub"));
                }

                SubscriptionDiff sd = new SubscriptionDiff(origSub, pendingSub);
                diffDetails.append(nl);
                diffDetails.append((sd.getDifferences())).append(nl);

                // Pass the subscription details to be displayed
                SubDiffDlg sdd = new SubDiffDlg(getShell(),
                        diffDetails.toString());
                sdd.open();
            } catch (RegistryHandlerException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Unable to find the subscription this pending subscription is associated with.",
                                e);
            }
        }
    }

    /**
     * Handle the column selection.
     *
     * @param event
     *            Selection event.
     */
    private void handleColumnSelection(SelectionEvent event) {

        if (pendingSubData == null) {
            return;
        }

        TableColumn tc = (TableColumn) event.getSource();

        updateSortDirection(tc, pendingSubData, true);

        sortTable();
        updateColumnSortImage();
    }

    /**
     * Sort the rows of data in the table.
     */
    private void sortTable() {
        table.removeAll();

        pendingSubData.sortData();

        ArrayList<SubscriptionApprovalRowData> sardArray = pendingSubData
                .getDataArray();

        for (SubscriptionApprovalRowData sard : sardArray) {
            TableItem ti = new TableItem(this.table, SWT.NONE);
            ti.setText(0, sard.getSubName());
            ti.setText(1, sard.getOwner());
            ti.setText(2, sard.getChangeOwner());
            ti.setText(3, sard.getOfficeId());
            ti.setText(4, sard.getDescription());
        }
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
        return pendingSubData.getSortDirection();
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.viz.datadelivery.common.ui.TableComp#createColumns()
     */
    @Override
    protected void createColumns() {
        String[] columns = new String[PendingSubColumnNames.values().length];
        TableColumn tc;

        for (int i = 0; i < columns.length; i++) {
            columns[i] = PendingSubColumnNames.values()[i].getColumnName();
        }

        for (int i = 0; i < columns.length; i++) {
            tc = new TableColumn(table, SWT.LEFT);
            tc.setText(columns[i]);

            tc.setResizable(true);

            tc.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    handleColumnSelection(event);
                }
            });

            if (i == 0) {
                sortedColumn = tc;
            }
        }
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

        pendingSubData.sortData();

        for (SubscriptionApprovalRowData rd : this.pendingSubData
                .getDataArray()) {
            int idx = 0;
            TableItem item = new TableItem(table, SWT.NONE);
            for (TableColumn column : columns) {
                String text = getCellText(column.getText(), rd);
                if (text == null) {
                    item.setText(idx++, "");
                } else {
                    item.setText(idx++, text);
                }
            }
        }

        if (sortedColumn == null) {
            sortedColumn = table.getColumn(0);
            pendingSubData.setSortColumn(sortedColumn.getText());
        } else {
            pendingSubData.setSortColumn(sortedColumn.getText());
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

        MenuItem item2 = new MenuItem(popupMenu, SWT.PUSH);
        item2.setText("Approve");
        item2.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                callback.handleApprove();
            }
        });

        MenuItem item3 = new MenuItem(popupMenu, SWT.PUSH);
        item3.setText("Deny");
        item3.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                callback.handleDeny();
            }
        });
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
        // Not used at this time.
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
        final ArrayList<InitialPendingSubscription> updatedSubscriptions = new ArrayList<InitialPendingSubscription>();
        try {
            for (NotificationMessage msg : messages) {
                Object obj = msg.getMessagePayload();
                if (obj instanceof PendingSubscription) {
                    updatedSubscriptions.add((PendingSubscription) obj);
                } else if (obj instanceof PendingSubscriptionNotificationResponse) {
                    PendingSubscriptionNotificationResponse response = (PendingSubscriptionNotificationResponse) obj;
                    InitialPendingSubscription sub = response.getSubscription();
                    updatedSubscriptions.add(sub);
                } else if (obj instanceof ApprovedPendingSubscriptionNotificationResponse) {
                    BaseSubscriptionNotificationResponse<InitialPendingSubscription> response = (BaseSubscriptionNotificationResponse<InitialPendingSubscription>) obj;
                    InitialPendingSubscription dummySubForApproved = response
                            .getSubscription();
                    dummySubForApproved.setDeleted(true);
                    updatedSubscriptions.add(dummySubForApproved);
                } else if (obj instanceof DeniedPendingSubscriptionNotificationResponse) {
                    DeniedPendingSubscriptionNotificationResponse response = (DeniedPendingSubscriptionNotificationResponse) obj;
                    InitialPendingSubscription dummySubForDenied = new InitialPendingSubscription();
                    dummySubForDenied.setId(response.getId());
                    dummySubForDenied.setDeleted(true);
                    updatedSubscriptions.add(dummySubForDenied);
                }
            }
        } catch (NotificationException e) {
            statusHandler.error("Error when receiving notification", e);
        } catch (RuntimeException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
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

    /**
     * Get a reference to the table.
     *
     * @return table
     */
    public Table getTable() {
        return table;
    }

    /**
     * Get the table data.
     *
     * @return TableDataManager<SubscriptionApprovalRowData>
     */
    public TableDataManager<SubscriptionApprovalRowData> getPendingSubData() {
        return this.pendingSubData;
    }

    /**
     * Repopulate the table.
     */
    public void repopulate() {
        populateData();
        populateTable();
    }
}