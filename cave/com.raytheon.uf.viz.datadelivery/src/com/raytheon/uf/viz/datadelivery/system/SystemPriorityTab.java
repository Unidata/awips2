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
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * System Management Priority tab.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012   730       jpiatt     Initial creation.
 * Oct 03, 2012  1241       djohnson   Use {@link DataDeliveryPermission} and registry handlers.
 * Jan 04, 2013  1420       mpduff     Add delete function.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class SystemPriorityTab {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SystemPriorityTab.class);

    /** Parent Composite */
    private final Composite parentComp;

    /** Priority Composite */
    private Composite priorityComp;

    /** Create and edit rule dialog */
    private CreateEditRuleDlg ruleDlg;

    /** Rule type constant */
    private final String PRIORITY_TYPE = "priority";

    /** Available List widget */
    private List priorityList;

    /** Edit rule button. */
    private Button editBtn;

    /** New rule button. */
    private Button newBtn;

    /** Delete rule button */
    private Button deleteBtn;

    /** Button Height. */
    private final int buttonHeight = SWT.DEFAULT;

    /** Button Width. */
    private final int buttonWidth = 70;

    /** Flag for create and edit. */
    private boolean create;

    private final String notAuthorizedMsg = " is not authorized to create, edit, or delete rules using the Data Delivery System Management\nPermission: ";

    /**
     * Constructor.
     * 
     * @param parentComp
     *            The Composite holding these controls
     * @param systemManagementDlg
     * 
     * @param dataSet
     *            The DataSet object
     */
    public SystemPriorityTab(Composite parentComp) {
        this.parentComp = parentComp;
        init();
    }

    /**
     * Initialize the tab.
     */
    private void init() {

        createPriorityRulesTab();
        createSideButtons();

    }

    /**
     * Create the bar that may be expanded depending on item count.
     */
    private void createPriorityRulesTab() {

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(2, false);

        priorityComp = new Composite(parentComp, SWT.NONE);
        priorityComp.setLayout(gl);
        gd.widthHint = 375;
        gd.heightHint = 200;

        priorityList = new List(priorityComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        priorityList.setLayoutData(gd);
        priorityList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (priorityList.getSelectionCount() > 0) {
                    enableButtons(true);
                } else {
                    enableButtons(false);
                }
            }
        });

        loadList();
    }

    /**
     * Create the button controls.
     */
    private void createSideButtons() {

        GridData actionData = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        GridLayout actionLayout = new GridLayout(1, false);
        Composite actionComp = new Composite(priorityComp, SWT.NONE);
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
                handlePriorityRule();
                if (priorityList.getItemCount() > 0) {
                    priorityList.select(0);
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
                int idx = priorityList.getSelectionIndex();
                handlePriorityRule();
                priorityList.select(idx);
            }
        });

        btnData = new GridData(buttonWidth, buttonHeight);
        deleteBtn = new Button(actionComp, SWT.PUSH);
        deleteBtn.setText("Delete...");
        deleteBtn.setLayoutData(btnData);
        deleteBtn.setEnabled(false);
        deleteBtn.setToolTipText("Edit item selected in the list");
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleDeleteRule();
                if (priorityList.getItemCount() > 0) {
                    priorityList.select(0);
                }
            }
        });
    }

    /**
     * enable/disable the delete and edit buttons
     * 
     * @param enable
     *            setting for buttons
     */
    private void enableButtons(boolean enable) {
        deleteBtn.setEnabled(enable);
        editBtn.setEnabled(enable);
    }

    /**
     * Handle create and edit rules.
     */
    private void handlePriorityRule() {
        final DataDeliveryPermission permission = DataDeliveryPermission.SYSTEM_MANAGEMENT_CREATE;
        IUser user = UserController.getUserObject();
        String msg = user.uniqueId() + notAuthorizedMsg + permission;

        try {
            if (DataDeliveryServices.getPermissionsService()
                    .checkPermission(user, msg, permission).isAuthorized()) {

                if (ruleDlg == null || ruleDlg.isDisposed()) {
                    // New
                    if (create) {
                        ruleDlg = new CreateEditRuleDlg(parentComp.getShell(),
                                create, PRIORITY_TYPE);
                    } else {
                        // Edit
                        String ruleName = null;
                        int selectedIdx = priorityList.getSelectionIndex();
                        if (selectedIdx > -1) {
                            ruleName = priorityList.getItem(selectedIdx);
                        } else if (priorityList.getItemCount() <= 0) {
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
                                create, ruleName, PRIORITY_TYPE);
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

    /**
     * Delete action handler.
     */
    private void handleDeleteRule() {
        final DataDeliveryPermission permission = DataDeliveryPermission.SYSTEM_MANAGEMENT_CREATE;
        IUser user = UserController.getUserObject();
        String msg = user.uniqueId() + notAuthorizedMsg + permission;

        try {
            if (DataDeliveryServices.getPermissionsService()
                    .checkPermission(user, msg, permission).isAuthorized()) {

                String ruleName = null;
                int selectedIdx = priorityList.getSelectionIndex();
                if (selectedIdx > -1) {
                    int answer = DataDeliveryUtils.showYesNoMessage(
                            parentComp.getShell(), "Delete?",
                            "Are you sure you want to delete this rule?");
                    if (answer == SWT.YES) {
                        ruleName = priorityList.getItem(selectedIdx);
                        SystemRuleManager.getInstance().deletePriorityRule(
                                ruleName);
                        loadList();
                        if (priorityList.getItemCount() == 0) {
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
        priorityList.removeAll();

        // Get the list of priority rule names
        java.util.List<String> rules = SystemRuleManager.getInstance()
                .getPriorityRuleNames();

        Collections.sort(rules);

        priorityList.setItems(rules.toArray(new String[rules.size()]));
    }
}
