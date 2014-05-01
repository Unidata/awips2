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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;

import com.raytheon.uf.common.auth.AuthException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * System Rules Composite base class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 07, 2013   2180     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public abstract class SystemRulesComposite extends Composite implements
        INewEditDeleteAction {
    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SystemRulesComposite.class);

    /** Not authorized message */
    private final String notAuthorizedMsg = " is not authorized to create, edit, or delete rules using the Data Delivery System Management\nPermission: ";

    /** Rule type */
    private final String ruleType;

    /** List composite */
    private Composite listComp;

    /** Rules list */
    private List rulesList;

    /** Create/edit rule flag */
    private boolean create;

    /** Button composite */
    private NewEditDeleteComposite buttonComp;

    /** Create and edit rule dialog */
    private CreateEditRuleDlg ruleDlg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite
     * @param style
     *            Style bits
     * @param ruleType
     *            The rule type
     */
    public SystemRulesComposite(Composite parent, int style, String ruleType) {
        super(parent, style);
        this.ruleType = ruleType;

        init();
    }

    /**
     * Initialize
     */
    private void init() {
        GridLayout gl = new GridLayout(1, true);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        this.setLayout(gl);
        this.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gl = new GridLayout(1, false);

        listComp = new Composite(this, SWT.NONE);
        listComp.setLayout(gl);
        listComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        rulesList = new List(listComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.SINGLE);
        rulesList.setLayoutData(gd);
        rulesList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                buttonComp.enableButtons(rulesList.getSelectionCount() > 0);
            }
        });

        // Buttons
        buttonComp = new NewEditDeleteComposite(this, SWT.NONE, this);

        loadList();
    }

    /**
     * Load the rule list.
     */
    public void loadList() {
        rulesList.removeAll();

        // Get the list of latency rule names
        java.util.List<String> rules = getRuleNames();

        Collections.sort(rules, String.CASE_INSENSITIVE_ORDER);
        rulesList.setItems(rules.toArray(new String[rules.size()]));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.system.INewEditDeleteAction#newAction()
     */
    @Override
    public boolean newAction() {
        create = true;
        handleRule();
        loadList();
        if (rulesList.getItemCount() > 0) {
            rulesList.select(0);
            buttonComp.enableButtons(true);
            return true;
        }

        buttonComp.enableButtons(false);
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.system.INewEditDeleteAction#editAction
     * ()deleteAction
     */
    @Override
    public boolean editAction() {
        create = false;
        int idx = rulesList.getSelectionIndex();
        handleRule();
        if (!rulesList.isDisposed()) {
            rulesList.select(idx);
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.system.INewEditDeleteAction#deleteAction
     * ()
     */
    @Override
    public boolean deleteAction() {
        handleDeleteRule();
        if (rulesList.getItemCount() > 0) {
            rulesList.select(0);
        }

        return false;
    }

    /**
     * Delete rule action handler
     */
    private void handleDeleteRule() {
        final String permission = DataDeliveryPermission.SYSTEM_MANAGEMENT_CREATE
                .toString();
        IUser user = UserController.getUserObject();
        String msg = user.uniqueId() + notAuthorizedMsg + permission;

        try {
            if (DataDeliveryServices.getPermissionsService()
                    .checkPermission(user, msg, permission).isAuthorized()) {

                String ruleName = null;
                int selectedIdx = rulesList.getSelectionIndex();
                if (selectedIdx > -1) {
                    int answer = DataDeliveryUtils.showYesNoMessage(getShell(),
                            "Delete?",
                            "Are you sure you want to delete this rule?");
                    if (answer == SWT.YES) {
                        ruleName = rulesList.getItem(selectedIdx);
                        deleteRule(ruleName);
                        loadList();
                        if (rulesList.getItemCount() == 0) {
                            buttonComp.enableButtons(false);
                        }
                    }
                } else {
                    DataDeliveryUtils.showMessage(getShell(), SWT.ERROR,
                            "Select Rule", "Please select a rule for delete.");
                    return;
                }
            }
        } catch (AuthException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred in authorization request", e);
        }
    }

    /**
     * New/Edit rule action handler.
     */
    private void handleRule() {
        final String permission = DataDeliveryPermission.SYSTEM_MANAGEMENT_CREATE
                .toString();
        IUser user = UserController.getUserObject();
        String msg = user.uniqueId() + notAuthorizedMsg + permission;

        try {
            if (DataDeliveryServices.getPermissionsService()
                    .checkPermission(user, msg, permission).isAuthorized()) {

                if (ruleDlg == null || ruleDlg.isDisposed()) {

                    // New
                    if (create) {
                        ruleDlg = new CreateEditRuleDlg(getShell(), create,
                                ruleType);
                    } else {
                        // Edit
                        String ruleName = null;
                        int selectedIdx = rulesList.getSelectionIndex();
                        if (selectedIdx > -1) {
                            ruleName = rulesList.getItem(selectedIdx);
                        } else if (rulesList.getItemCount() <= 0) {
                            DataDeliveryUtils.showMessage(getShell(),
                                    SWT.ERROR, "Create Rule",
                                    "Please create a rule.");
                            return;
                        } else {
                            DataDeliveryUtils.showMessage(getShell(),
                                    SWT.ERROR, "Select Rule",
                                    "Please select a rule to edit.");
                            return;
                        }
                        ruleDlg = new CreateEditRuleDlg(getShell(), create,
                                ruleName, ruleType);
                    }
                    ruleDlg.open();
                } else {
                    ruleDlg.bringToTop();
                }

            }
        } catch (AuthException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred in authorization request", e);
        }
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
