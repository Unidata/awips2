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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.datadelivery.common.ui.GroupSelectComp;
import com.raytheon.uf.viz.datadelivery.common.ui.IGroupAction;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.presenter.components.ComboBoxConf;
import com.raytheon.viz.ui.presenter.components.WidgetConf;

/**
 * The Data Delivery Add To Subscription Group Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 9, 2012    702      jpiatt      Initial creation.
 * Aug 29, 2012   223      mpduff      Updated to reflect other changes.
 * Aug 31, 2012  1128      mpduff      Notification Fixes.
 * Nov 28, 2012  1286      djohnson    Use the subscription service.
 * Jan 02, 2013  1441      djohnson    Access GroupDefinitionManager in a static fashion.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class GroupAddDlg extends CaveSWTDialog {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus.getHandler(GroupAddDlg.class);

    /** The Main Composite */
    private Composite mainComp;

    /** The Subscription Group Information Composite */
    private GroupSelectComp groupSelectComp;

    /** Subscription object */
    private final Subscription subscription;

    /** IGroupAction callback */
    private final IGroupAction callback;

    private final ISubscriptionService subscriptionService = DataDeliveryServices
            .getSubscriptionService();

    /**
     * Constructor.
     * 
     * @param parent
     *           parent shell 
     * @param subscription
     *           Subscription object
     * @param callback
     *           callback to parent shell
     */
    public GroupAddDlg(Shell parent, Subscription subscription, IGroupAction callback) {
        super(parent, SWT.DIALOG_TRIM, CAVE.INDEPENDENT_SHELL);
        setText("Add To Group");
        this.subscription = subscription;
        this.callback = callback;
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
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        createSubInfo();
        
        // TODO - need to change this to not us the IGroupAction 
        groupSelectComp = new GroupSelectComp(mainComp, true);
        groupSelectComp.setGroupName(subscription.getGroupName());
        createButtons();

    }

    /* (non-Javadoc)
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        ComboBoxConf groupComboConf = new ComboBoxConf(true, "Select a Group", WidgetConf.DO_NOTHING);
        groupSelectComp.setGroupNameComboConf(groupComboConf);
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

    /**
     * Create the optional Group Name information.
     */
    private void createSubInfo() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);

        Group subInfo = new Group(mainComp, SWT.NONE);
        subInfo.setLayout(gl);
        subInfo.setLayoutData(gd);
        subInfo.setText(" Subscription Information ");

        Label subNameLbl = new Label(subInfo, SWT.NONE);
        subNameLbl.setText("Subscription Name: ");
        Label subName = new Label(subInfo, SWT.NONE);
        subName.setText(subscription.getName());

    }

    /**
     * Create the bottom buttons
     */
    private void createButtons() {
        int buttonWidth = 75;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);
        Composite btnComp = new Composite(mainComp, SWT.NONE);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        Button okBtn = new Button(btnComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(btnData);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (handleOK()) {
                    close();
                }
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button cancelBtn = new Button(btnComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(btnData);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Event handler for the OK button.
     * 
     * @return true if data are valid
     */
    private boolean handleOK() {

        //Get Group Definition
        String groupName = groupSelectComp.getGroupName();
        GroupDefinition groupDefinition = GroupDefinitionManager
                .getGroup(groupName);

        //Apply group properties to subscription definition
        if (groupDefinition != null) {
            subscription.setNotify(groupDefinition.getOption() == 1);
            subscription.setGroupName(groupName);

            //Set duration
            if (groupDefinition.getSubscriptionStart() != null) {
                subscription.setSubscriptionStart(groupDefinition.getSubscriptionStart());
                subscription.setSubscriptionEnd(groupDefinition.getSubscriptionEnd());
            } else {
                subscription.setSubscriptionStart(null);
                subscription.setSubscriptionEnd(null);
            }

            //Set active period
            if (groupDefinition.getActivePeriodStart() != null) {
                subscription.setActivePeriodStart(groupDefinition.getActivePeriodStart());
                subscription.setActivePeriodEnd(groupDefinition.getActivePeriodEnd());
            } else {
                subscription.setActivePeriodStart(null);
                subscription.setActivePeriodEnd(null);
            }
        }
        final String username = UserController.getUserObject().uniqueId()
                .toString();

        System.out.println("Fix Me:  Need to calculate data set size");
        subscription.setDataSetSize(999);
        subscription.setOfficeID(LocalizationManager.getInstance().getCurrentSite());
        subscription.setOwner(username);

        try {
            DataDeliveryHandlers.getSubscriptionHandler().store(subscription);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error saving subscription data to the registry.", e);
            return false;
        }

        subscriptionService.sendCreatedSubscriptionNotification(subscription,
                username);

        // refresh table
        callback.handleRefresh();

        return true;
    }

}

