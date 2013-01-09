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
package com.raytheon.uf.viz.datadelivery.system;

import java.util.Collections;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryAuthRequest;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * System Management Main Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012   730       jpiatt     Initial creation.
 * Oct 03, 2012  1241       djohnson   Use {@link DataDeliveryPermission} and registry handlers.
 * Jan 04, 2012  1420       mpduff     Add delete rule function.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class SystemLatencyTab {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SystemLatencyTab.class);

    private final String notAuthorizedMsg = " is not authorized to create, edit, or delete rules using the Data Delivery System Management\nPermission: ";

    /** Parent Composite */
    private final Composite parentComp;

    /** Priority Composite */
    private Composite latencyComp;

    /** Create and edit rule dialog */
    private CreateEditRuleDlg ruleDlg;

    /** Rule type constant */
    private final String LATENCY_TYPE = "latency";

    /** Available List widget */
    private List latencyList;

    /** Flag for create and edit. */
    private boolean create;

    /** Edit rule button. */
    private Button editBtn;

    /** New rule button. */
    private Button newBtn;

    /** Delete rule button. */
    private Button deleteBtn;

    /** Button Height. */
    private final int buttonHeight = SWT.DEFAULT;

    /** Button Width. */
    private final int buttonWidth = 70;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            The Composite holding these controls
     */
    public SystemLatencyTab(Composite parentComp) {
        this.parentComp = parentComp;
        init();
    }

    /**
     * Initialize the tab.
     */
    private void init() {

        createLatencyRulesTab();
        createSideButtons();

    }

    /**
     * Create the bar that may be expanded depending on item count.
     */
    private void createLatencyRulesTab() {

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(2, false);

        latencyComp = new Composite(parentComp, SWT.NONE);
        latencyComp.setLayout(gl);
        gd.widthHint = 375;
        gd.heightHint = 200;

        latencyList = new List(latencyComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        latencyList.setLayoutData(gd);
        latencyList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                enableButtons(latencyList.getSelectionCount() > 0);
            }
        });

        loadList();
    }

    /**
     * Create the move up/down controls
     */
    private void createSideButtons() {
        GridData actionData = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        GridLayout actionLayout = new GridLayout(1, false);
        Composite actionComp = new Composite(latencyComp, SWT.NONE);
        actionComp.setLayout(actionLayout);
        actionComp.setLayoutData(actionData);

        GridData btnData = new GridData(buttonWidth, buttonHeight);

        newBtn = new Button(actionComp, SWT.PUSH);
        newBtn.setText("New...");
        newBtn.setLayoutData(btnData);
        newBtn.setEnabled(true);
        newBtn.setToolTipText("Create a new rule");
        newBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                create = true;
                handleLatencyRule();
                if (latencyList.getItemCount() > 0) {
                    latencyList.select(0);
                    enableButtons(true);
                } else {
                    enableButtons(false);
                }
            }
        });

        btnData = new GridData(buttonWidth, buttonHeight);
        editBtn = new Button(actionComp, SWT.PUSH);
        editBtn.setText("Edit...");
        editBtn.setLayoutData(btnData);
        editBtn.setEnabled(false);
        editBtn.setToolTipText("Edit item selected in the list");
        editBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                create = false;
                int idx = latencyList.getSelectionIndex();
                handleLatencyRule();
                if (!latencyList.isDisposed()) {
                    latencyList.select(idx);
                }
            }
        });

        btnData = new GridData(buttonWidth, buttonHeight);
        deleteBtn = new Button(actionComp, SWT.PUSH);
        deleteBtn.setText("Delete...");
        deleteBtn.setLayoutData(btnData);
        deleteBtn.setEnabled(false);
        deleteBtn.setToolTipText("Delete item selected in the list");
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleDeleteRule();
                if (latencyList.getItemCount() > 0) {
                    latencyList.select(0);
                }
            }
        });
    }

    private void enableButtons(boolean enable) {
        deleteBtn.setEnabled(enable);
        editBtn.setEnabled(enable);
    }

    private void handleLatencyRule() {
        final DataDeliveryPermission permission = DataDeliveryPermission.SYSTEM_MANAGEMENT_CREATE;
        IUser user = UserController.getUserObject();
        String msg = user.uniqueId() + notAuthorizedMsg + permission;
        DataDeliveryAuthRequest request = new DataDeliveryAuthRequest();
        request.setUser(user);
        request.addRequestedPermissions(permission);
        request.setNotAuthorizedMessage(msg);

        try {
            DataDeliveryAuthRequest auth = DataDeliveryUtils
                    .sendAuthorizationRequest(request);

            if (auth != null && auth.isAuthorized()) {

                if (ruleDlg == null || ruleDlg.isDisposed()) {

                    // New
                    if (create) {
                        ruleDlg = new CreateEditRuleDlg(parentComp.getShell(),
                                create, LATENCY_TYPE);
                    } else {
                        // Edit
                        String ruleName = null;
                        int selectedIdx = latencyList.getSelectionIndex();
                        if (selectedIdx > -1) {
                            ruleName = latencyList.getItem(selectedIdx);
                        } else if (latencyList.getItemCount() <= 0) {
                            DataDeliveryUtils.showMessage(
                                    parentComp.getShell(), SWT.ERROR,
                                    "Create Rule", "Please create a rule.");
                            return;
                        } else {
                            DataDeliveryUtils.showMessage(
                                    parentComp.getShell(), SWT.ERROR,
                                    "Select Rule",
                                    "Please select a rule for edit.");
                            return;
                        }
                        ruleDlg = new CreateEditRuleDlg(parentComp.getShell(),
                                create, ruleName, LATENCY_TYPE);
                    }
                    boolean reloadFlag = (Boolean) ruleDlg.open();
                    if (reloadFlag) {
                        loadList();
                    }
                } else {
                    ruleDlg.bringToTop();
                }

            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred in authorization request", e);
        }
    }

    private void handleDeleteRule() {
        final DataDeliveryPermission permission = DataDeliveryPermission.SYSTEM_MANAGEMENT_CREATE;
        IUser user = UserController.getUserObject();
        String msg = user.uniqueId() + notAuthorizedMsg + permission;
        DataDeliveryAuthRequest request = new DataDeliveryAuthRequest();
        request.setUser(user);
        request.addRequestedPermissions(permission);
        request.setNotAuthorizedMessage(msg);

        try {
            DataDeliveryAuthRequest auth = DataDeliveryUtils
                    .sendAuthorizationRequest(request);

            if (auth != null && auth.isAuthorized()) {
                String ruleName = null;
                int selectedIdx = latencyList.getSelectionIndex();
                if (selectedIdx > -1) {
                    int answer = DataDeliveryUtils.showYesNoMessage(
                            parentComp.getShell(), "Delete?",
                            "Are you sure you want to delete this rule?");
                    if (answer == SWT.YES) {
                        ruleName = latencyList.getItem(selectedIdx);
                        SystemRuleManager.getInstance().deleteLatencyRule(
                                ruleName);
                        loadList();
                        if (latencyList.getItemCount() == 0) {
                            enableButtons(false);
                        }
                    }
                } else {
                    DataDeliveryUtils.showMessage(parentComp.getShell(),
                            SWT.ERROR, "Select Rule",
                            "Please select a rule for delete.");
                    return;
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred in authorization request", e);
        }
    }

    /**
     * Load the rule list.
     */
    public void loadList() {
        latencyList.removeAll();

        // Get the list of latency rule names
        java.util.List<String> rules = null;
        rules = SystemRuleManager.getInstance().getLatencyRuleNames();

        Collections.sort(rules, String.CASE_INSENSITIVE_ORDER);
        latencyList.setItems(rules.toArray(new String[rules.size()]));
    }
}