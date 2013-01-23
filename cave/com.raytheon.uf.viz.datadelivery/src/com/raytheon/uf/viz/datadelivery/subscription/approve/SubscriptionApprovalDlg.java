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
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.IPendingSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlers;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.datadelivery.common.ui.TableCompConfig;
import com.raytheon.uf.viz.datadelivery.common.ui.TableDataManager;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.subscription.CancelForceApplyAndIncreaseLatencyDisplayText;
import com.raytheon.uf.viz.datadelivery.subscription.IPermissionsService;
import com.raytheon.uf.viz.datadelivery.subscription.IPermissionsService.IAuthorizedPermissionResponse;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionNotificationService;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.ForceApplyPromptResponse;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.presenter.IDisplay;

/**
 * Pending subscription approval dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 7, 2012             mpduff      Initial creation
 * Aug 21, 2012 712        mpduff      Remove registry call to get subscription id.
 * Aug 27, 2012 0743       djohnson    Fix to not have NPE, delete associations on approval.
 * Aug 31, 2012 1128       mpudff      Notification fix.
 * Sep 06, 2012 1142       djohnson    Create deletePendingSubscription().
 * Sep 17, 2012 1157       mpduff      Add subscription to notification request.
 * Sep 14, 2012 1169       djohnson    Use storeOrReplaceRegistryObject.
 * Oct 03, 2012 1241       djohnson    Use {@link DataDeliveryPermission} and registry handlers.
 * Oct 10, 2012 1204       jpiatt      Changed GUI alert to AlertViz alert.
 * Nov 09, 2012 1286       djohnson    Consolidate duplicate subscription handling.
 * Nov 20, 2012 1286       djohnson    Implement IDisplay to display yes/no prompt.
 * Nov 28, 2012 1286       djohnson    Use subscriptionService for notification, and only notify when actually approved.
 * Dec 12, 2012 1433       bgonzale    Use new subscription copy ctor method for approval of pending subscription.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionApprovalDlg extends CaveSWTDialog implements
        INotificationObserver, ISubscriptionApprovalAction, IDisplay {

    public class ApproveSubscriptionForceApplyPromptDisplayText extends
            CancelForceApplyAndIncreaseLatencyDisplayText {
        /**
         * Constructor.
         */
        public ApproveSubscriptionForceApplyPromptDisplayText() {
            super("approve", SubscriptionApprovalDlg.this.getShell());
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String getOptionDisplayText(ForceApplyPromptResponse option,
                int requiredLatency, Subscription subscription,
                Set<String> wouldBeUnscheduledSubscriptions) {
            // Just override the cancel option to more accurately define what
            // will happen
            if (ForceApplyPromptResponse.CANCEL.equals(option)) {
                final String name = subscription.getName();
                return "Leave " + name + " as unapproved";
            } else {
                return super.getOptionDisplayText(option, requiredLatency,
                        subscription, wouldBeUnscheduledSubscriptions);
            }
        }
    }

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionApprovalDlg.class);

    private final ISubscriptionService subscriptionService = DataDeliveryServices
            .getSubscriptionService();

    private final ISubscriptionNotificationService subscriptionNotificationService = DataDeliveryServices
            .getSubscriptionNotificationService();

    private final IPermissionsService permissionsService = DataDeliveryServices
            .getPermissionsService();

    private SubApprovalTableComp tableComp;

    private String denyMessage;


    /**
     * Constructor.
     *
     * @param parent
     *            The parent Shell
     */
    public SubscriptionApprovalDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.MIN | SWT.RESIZE, CAVE.INDEPENDENT_SHELL);
        setText("Subscription Approval");
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayoutData()
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
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
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;

        return mainLayout;
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
        shell.setMinimumSize(750, 320);
        createMenus();
        createTable();
        createButtons();
    }

    /**
     * Create the menus.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        // Create the file menu
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("&Exit");
        exitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

        // Create the view menu
        MenuItem viewMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        viewMenuItem.setText("&View");

        Menu viewMenu = new Menu(menuBar);
        viewMenuItem.setMenu(viewMenu);

        MenuItem diffMI = new MenuItem(viewMenu, SWT.NONE);
        diffMI.setText("&Details...");
        diffMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                tableComp.handleDetails();
            }
        });

        // Create the Action menu
        MenuItem actionMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        actionMenuItem.setText("&Action");

        Menu actionMenu = new Menu(menuBar);
        actionMenuItem.setMenu(actionMenu);

        MenuItem approveMI = new MenuItem(actionMenu, SWT.NONE);
        approveMI.setText("&Approve Selected");
        approveMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleApprove();
            }
        });

        MenuItem denyMI = new MenuItem(actionMenu, SWT.NONE);
        denyMI.setText("&Deny Selected");
        denyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleDeny();
            }
        });

        // Create the help menu
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        MenuItem aboutMI = new MenuItem(helpMenu, SWT.NONE);
        aboutMI.setText("&About...");
        aboutMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

            }
        });

        shell.setMenuBar(menuBar);
    }

    private void createTable() {
        TableCompConfig tableConfig = new TableCompConfig(TABLE_TYPE.PENDING_SUBSCRIPTION);
        tableConfig.setTableStyle(SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI | SWT.FULL_SELECTION);
        tableConfig.setTableHeight(200);
        tableComp = new SubApprovalTableComp(shell, tableConfig, this);
    }

    private void createButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);

        Composite comp = new Composite(shell, SWT.NONE);
        comp.setLayout(gl);
        comp.setLayoutData(gd);

        int btnWidth = 90;
        GridData btnData = new GridData(btnWidth, SWT.DEFAULT);

        Button approveBtn = new Button(comp, SWT.PUSH);
        approveBtn.setText("Approve");
        approveBtn.setLayoutData(btnData);
        approveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleApprove();
            }
        });

        btnData = new GridData(btnWidth, SWT.DEFAULT);
        Button denyBtn = new Button(comp, SWT.PUSH);
        denyBtn.setText("Deny");
        denyBtn.setLayoutData(btnData);
        denyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleDeny();
            }
        });
    }

    @Override
    public void handleApprove() {
        if (tableComp.getTable().getSelectionCount() == 0) {
            DataDeliveryUtils
                    .showMessage(shell, SWT.ERROR, "No Rows Selected", "Please select a row or rows to Approve");
            return;
        }
        getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

        IUser user = UserController.getUserObject();

        IAuthorizedPermissionResponse response = allowed(user);

        if (response.isAuthorized()) {
            // Check if user or site permissions, compare to owner of sub if user permission
            boolean site = false;

            if (response
                    .hasPermission(DataDeliveryPermission.SUBSCRIPTION_APPROVE_SITE)) {
                site = true;
            }

            TableDataManager<SubscriptionApprovalRowData> pendingSubData = tableComp.getPendingSubData();
            ArrayList<SubscriptionApprovalRowData> approveList = new ArrayList<SubscriptionApprovalRowData>();
            ArrayList<String> notApprovedSubList = new ArrayList<String>();
            for (int idx : tableComp.getTable().getSelectionIndices()) {
                SubscriptionApprovalRowData approvedItem = pendingSubData.getDataRow(idx);
                if (site) {
                    approveList.add(approvedItem);
                } else {
                    if (approvedItem.isOwner(user)) {
                        approveList.add(approvedItem);
                    } else {
                        notApprovedSubList.add(approvedItem.getSubName());
                    }
                }
            }

            if (approveList.size() > 0) {
                approveSubs(approveList);
            }

            if (notApprovedSubList.size() > 0) {
                StringBuilder buffer = new StringBuilder(user.uniqueId().toString() + " is not authorized to approve pending subscriptions belonging to other users. " +
                		"\nNot authorized to approve the following subscriptions:\n\n");
                for (String name: notApprovedSubList) {
                    buffer.append(name).append("\n");
                }

                statusHandler.handle(Priority.WARN, buffer.toString());
            }
        }

        getShell().setCursor(null);
    }

    /**
     * @return
     */
    private IAuthorizedPermissionResponse allowed(IUser user) {
        try {
            String msg = user.uniqueId()
                    + " is not authorized to Approve/Deny subscriptions.";

            return permissionsService.checkPermissions(user, msg,
                    DataDeliveryPermission.SUBSCRIPTION_APPROVE_SITE,
                    DataDeliveryPermission.SUBSCRIPTION_APPROVE_USER);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to check user permissions.", e);
            return new IAuthorizedPermissionResponse() {
                @Override
                public boolean isAuthorized() {
                    return false;
                }

                @Override
                public boolean hasPermission(DataDeliveryPermission permission) {
                    return false;
                }
            };
        }
    }

    @Override
    public void handleDeny() {
        if (tableComp.getTable().getSelectionCount() == 0) {
            DataDeliveryUtils
                    .showMessage(getShell(), SWT.ERROR, "No Rows Selected", "Please select a row or rows to delete");
            return;
        }
        getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

        IUser user = UserController.getUserObject();
        IAuthorizedPermissionResponse response = allowed(user);
        if (response.isAuthorized()) {
            if (confirm()) {
                // Check if user or site permissions, compare to owner of sub if user permission
                boolean site = false;

                if (response
                        .hasPermission(DataDeliveryPermission.SUBSCRIPTION_APPROVE_SITE)) {
                    site = true;
                }

                TableDataManager<SubscriptionApprovalRowData> pendingSubData = tableComp.getPendingSubData();
                ArrayList<SubscriptionApprovalRowData> deleteList = new ArrayList<SubscriptionApprovalRowData>();
                final String username = user.uniqueId().toString();
                for (int idx : tableComp.getTable().getSelectionIndices()) {
                    SubscriptionApprovalRowData removedItem = pendingSubData.getDataRow(idx);

                    if (site) {
                        deleteList.add(removedItem);
                    } else {
                        if (removedItem.isOwner(user)) {
                            deleteList.add(removedItem);
                        }
                    }
                }

                if (!deleteList.isEmpty()) {
                    pendingSubData.removeAll(deleteList);

                    for (SubscriptionApprovalRowData rd : deleteList) {
                        InitialPendingSubscription sub = rd.getSubscription();

                        IPendingSubscriptionHandler handler = RegistryObjectHandlers
                                .get(IPendingSubscriptionHandler.class);

                        try {
                            handler.delete(username, sub);

                            subscriptionNotificationService
                                    .sendDeniedPendingSubscriptionNotification(
                                            sub, username, denyMessage);
                        } catch (RegistryHandlerException e) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "Unable to delete pending subscription.",
                                            e);
                        }
                    }

                    tableComp.repopulate();
                } else {
                    String msg = username + " is not authorized to deny pending subscriptions belonging to other users.";
                    statusHandler.handle(Priority.WARN, msg);
                }
            }
        }

        getShell().setCursor(null);
    }

    private boolean confirm() {
        DenyDlg dlg = new DenyDlg(getShell());
        Object o = dlg.open();
        if (o instanceof String) {
            this.denyMessage = (String) o;
            if (denyMessage != null && denyMessage.length() > 0) {
                return true;
            }
        }
        return false;
    }


    private void approveSubs(ArrayList<SubscriptionApprovalRowData> subList) {
        tableComp.getPendingSubData().removeAll(subList);
        String username = System.getenv().get("LOGNAME");
        for (SubscriptionApprovalRowData rd: subList) {
            InitialPendingSubscription ps = rd.getSubscription();
            Subscription s = new Subscription(ps);

            IPendingSubscriptionHandler pendingSubHandler = RegistryObjectHandlers
                    .get(IPendingSubscriptionHandler.class);

            String exceptionMessage = "Unable to update subscription.";
            try {
                ISubscriptionServiceResult result = subscriptionService
                        .update(s,
                                new ApproveSubscriptionForceApplyPromptDisplayText());
                if (result.hasMessageToDisplay()) {
                    DataDeliveryUtils.showMessage(getShell(), SWT.OK,
                            "Subscription Approved.",
                            result.getMessageToDisplay());
                }

                if (!result.isAllowFurtherEditing()) {
                    exceptionMessage = "Unable to delete pending subscription.";
                    pendingSubHandler.delete(username, ps);

                    subscriptionNotificationService
                            .sendApprovedPendingSubscriptionNotification(ps,
                                    username);
                }
            } catch (RegistryHandlerException e) {
                statusHandler.handle(Priority.PROBLEM, exceptionMessage, e);
            }
        }

        tableComp.repopulate();
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
                        tableComp.updateTable(updatedSubscriptions);
                    }
                }
            });
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean displayYesNoPopup(String title, String message) {
        return DataDeliveryUtils.showYesNoMessage(getShell(), title,
                message) == SWT.YES;
    }
}
