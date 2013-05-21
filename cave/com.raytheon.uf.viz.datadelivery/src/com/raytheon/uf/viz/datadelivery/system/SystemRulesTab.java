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
 * Abstract System Management Rules Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2013  2000       djohnson   Consolidate duplicate code from latency and priority versions.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public abstract class SystemRulesTab extends SystemTab {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SystemRulesTab.class);

    private final String notAuthorizedMsg = " is not authorized to create, edit, or delete rules using the Data Delivery System Management\nPermission: ";

    /** Priority Composite */
    private Composite listComp;

    /** Create and edit rule dialog */
    private CreateEditRuleDlg ruleDlg;

    /** Available List widget */
    private List rulesList;

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

    private final String ruleType;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            The Composite holding these controls
     */
    public SystemRulesTab(Composite parentComp, String ruleType) {
        super(parentComp);
        this.ruleType = ruleType;
    }

    /**
     * Initialize the tab.
     */
    @Override
    public void init() {
        createRulesTab();
        createSideButtons();
    }

    /**
     * Create the bar that may be expanded depending on item count.
     */
    private void createRulesTab() {

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(2, false);

        listComp = new Composite(parentComp, SWT.NONE);
        listComp.setLayout(gl);
        listComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, true, true);
        gd.widthHint = 375;
        gd.heightHint = 200;

        rulesList = new List(listComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL | SWT.SINGLE);
        rulesList.setLayoutData(gd);
        rulesList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                enableButtons(rulesList.getSelectionCount() > 0);
            }
        });

        loadList();
    }

    /**
     * Create the move up/down controls
     */
    private void createSideButtons() {
        GridData actionData = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        GridLayout actionLayout = new GridLayout(1, false);
        Composite actionComp = new Composite(listComp, SWT.NONE);
        actionComp.setLayout(actionLayout);
        actionComp.setLayoutData(actionData);

        final int horizAlign = SWT.LEFT;
        final int verticalAlign = SWT.DEFAULT;
        boolean horizExcess = true;
        boolean verticalExcess = false;

        GridData btnData = new GridData(horizAlign, verticalAlign, horizExcess,
                verticalExcess);
        btnData.widthHint = buttonWidth;
        btnData.heightHint = buttonHeight;

        newBtn = new Button(actionComp, SWT.PUSH);
        newBtn.setText("New...");
        newBtn.setLayoutData(btnData);
        newBtn.setEnabled(true);
        newBtn.setToolTipText("Create a new rule");
        newBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                create = true;
                handleRule();
                if (rulesList.getItemCount() > 0) {
                    rulesList.select(0);
                    enableButtons(true);
                } else {
                    enableButtons(false);
                }
            }
        });

        btnData = new GridData(horizAlign, verticalAlign, horizExcess,
                verticalExcess);
        btnData.widthHint = buttonWidth;
        btnData.heightHint = buttonHeight;
        editBtn = new Button(actionComp, SWT.PUSH);
        editBtn.setText("Edit...");
        editBtn.setLayoutData(btnData);
        editBtn.setEnabled(false);
        editBtn.setToolTipText("Edit item selected in the list");
        editBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                create = false;
                int idx = rulesList.getSelectionIndex();
                handleRule();
                if (!rulesList.isDisposed()) {
                    rulesList.select(idx);
                }
            }
        });

        btnData = new GridData(horizAlign, verticalAlign, horizExcess,
                verticalExcess);
        btnData.widthHint = buttonWidth;
        btnData.heightHint = buttonHeight;
        deleteBtn = new Button(actionComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(btnData);
        deleteBtn.setEnabled(false);
        deleteBtn.setToolTipText("Delete item selected in the list");
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleDeleteRule();
                if (rulesList.getItemCount() > 0) {
                    rulesList.select(0);
                }
            }
        });
    }

    /**
     * Enable the buttons?
     * 
     * @param enable
     *            setting for the buttons
     */
    private void enableButtons(boolean enable) {
        deleteBtn.setEnabled(enable);
        editBtn.setEnabled(enable);
    }

    /**
     * New/Edit rule action handler.
     */
    private void handleRule() {
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
                                create, ruleType);
                    } else {
                        // Edit
                        String ruleName = null;
                        int selectedIdx = rulesList.getSelectionIndex();
                        if (selectedIdx > -1) {
                            ruleName = rulesList.getItem(selectedIdx);
                        } else if (rulesList.getItemCount() <= 0) {
                            DataDeliveryUtils.showMessage(
                                    parentComp.getShell(), SWT.ERROR,
                                    "Create Rule", "Please create a rule.");
                            return;
                        } else {
                            DataDeliveryUtils.showMessage(
                                    parentComp.getShell(), SWT.ERROR,
                                    "Select Rule",
                                    "Please select a rule to edit.");
                            return;
                        }
                        ruleDlg = new CreateEditRuleDlg(parentComp.getShell(),
                                create, ruleName, ruleType);
                    }
                    ruleDlg.open();
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
     * Delete rule action handler.
     */
    private void handleDeleteRule() {
        final DataDeliveryPermission permission = DataDeliveryPermission.SYSTEM_MANAGEMENT_CREATE;
        IUser user = UserController.getUserObject();
        String msg = user.uniqueId() + notAuthorizedMsg + permission;

        try {
            if (DataDeliveryServices.getPermissionsService()
                    .checkPermission(user, msg, permission).isAuthorized()) {

                String ruleName = null;
                int selectedIdx = rulesList.getSelectionIndex();
                if (selectedIdx > -1) {
                    int answer = DataDeliveryUtils.showYesNoMessage(
                            parentComp.getShell(), "Delete?",
                            "Are you sure you want to delete this rule?");
                    if (answer == SWT.YES) {
                        ruleName = rulesList.getItem(selectedIdx);
                        deleteRule(ruleName);
                        loadList();
                        if (rulesList.getItemCount() == 0) {
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
        rulesList.removeAll();

        // Get the list of latency rule names
        java.util.List<String> rules = null;
        rules = getRuleNames();

        Collections.sort(rules, String.CASE_INSENSITIVE_ORDER);
        rulesList.setItems(rules.toArray(new String[rules.size()]));
    }

    /**
     * Get the rule names.
     * 
     * @return the rule names
     */
    protected abstract java.util.List<String> getRuleNames();

    /**
     * Delete the rule by name.
     * 
     * @param ruleName
     *            the rule name
     */
    protected abstract void deleteRule(String ruleName);
}