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
package com.raytheon.uf.viz.datadelivery.common.ui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.IPendingSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryAuthRequest;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.datadelivery.retrieval.util.DataSizeUtils;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.datadelivery.filter.MetaDataManager;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.subscription.CancelForceApplyAndIncreaseLatencyDisplayText;
import com.raytheon.uf.viz.datadelivery.subscription.GroupDefinitionManager;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.presenter.IDisplay;
import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;
import com.raytheon.viz.ui.widgets.duallist.IUpdate;

/**
 * This is the user select composite. This class is intended to be extended so
 * common classes can be created and shared.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012   702      jpiatt       Initial creation.
 * Aug 08, 2012   863      jpiatt       Added new interface method.
 * Aug 22, 2012   712      mpduff       Fix notifications.
 * Aug 20, 2012  0743      djohnson     Finish making registry type-safe, AssociationQuery for pending subscriptions.
 * Aug 30, 2012   702      jpiatt       Populate selected subscriptions according to group.
 * Aug 31, 2012  1128      mpduff       Additional notification fixes, only set group related fields in subscription.
 * Sep 06, 2012   687      mpduff       Add the Subscription object back into the SubscriptionNotificationRequest object.
 * Sep 14, 2012  1169      djohnson     Use storeOrReplaceRegistryObject.
 * Sep 24, 2012  1157      mpduff       Use InitialPendingSubscription as needed.
 * Oct 03, 2012  1241      djohnson     Use {@link DataDeliveryPermission} and handlers for registry interaction.
 * Oct 24, 2012  1290      mpduff       Added check for group definition areal data being set.
 * Nov 09, 2012  1286      djohnson     Consolidate duplicate subscription handling.
 * Nov 20, 2012  1286      djohnson     Fix formatting, implement IDisplay to display yes/no prompt.
 * Dec 10, 2012  1259      bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Jan 02, 2013  1441      djohnson     Access GroupDefinitionManager in a static fashion.
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class UserSelectComp extends Composite implements IUpdate, IDisplay {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(UserSelectComp.class);

    private final String ALREADY_PENDING_SUBSCRIPTION = "There is already an edited version of this subscription.\n\nPlease "
            + "reconcile the pending subscription before making further edits.";

    /** User information composite. */
    private Composite userComp;

    /** Group Name combo box. */
    private Combo userNameCombo;

    /** User info group. */
    private Group userNameInfo;

    /** Dual List Object */
    private DualList dualList;

    /** Currently logged in user */
    private final String currentUser = LocalizationManager.getInstance()
            .getCurrentUser();

    /** User array */
    private String[] userArr;

    /** User Name array list */
    private final List<String> nameArr = new ArrayList<String>();

    /** DualListConfig object */
    private DualListConfig dualConfig;

    /** Map to hold subscriptions */
    private final Map<String, Map<String, Subscription>> addedMap = new HashMap<String, Map<String, Subscription>>();

    /** map to hold user subscriptions */
    private final Map<String, Map<String, Subscription>> userMap = new HashMap<String, Map<String, Subscription>>();

    /**
     * Registry handler for pending subscriptions.
     */
    private final IPendingSubscriptionHandler pendingSubHandler = DataDeliveryHandlers
            .getPendingSubscriptionHandler();

    /**
     * Registry handler for subscriptions.
     */
    private final ISubscriptionHandler subHandler = DataDeliveryHandlers
            .getSubscriptionHandler();

    private final ISubscriptionService subscriptionService = DataDeliveryServices
            .getSubscriptionService();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public UserSelectComp(Composite parent) {
        super(parent, SWT.NONE);

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

        createUserInfo();
        loadUsers();

    }

    /**
     * Create the user information.
     */
    private void createUserInfo() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        userNameInfo = new Group(this, SWT.NONE);
        userNameInfo.setLayout(gl);
        userNameInfo.setLayoutData(gd);
        userNameInfo.setText("  User Information  ");

        userComp = new Composite(userNameInfo, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        userComp.setLayoutData(gd);
        userComp.setLayout(gl);

        // User Combo box
        Label userName = new Label(userComp, SWT.NONE);
        userName.setText("User: ");

        gd = new GridData(150, SWT.DEFAULT);
        userNameCombo = new Combo(userComp, SWT.READ_ONLY);
        userNameCombo.setLayoutData(gd);
        userNameCombo.setToolTipText("Select a user name");
        userNameCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleUserSelect();
            }
        });

        // Available & Selected Subscriptions
        dualConfig = new DualListConfig();
        dualConfig.setListHeight(120);
        dualConfig.setListWidth(125);
        dualConfig.setShowUpDownBtns(false);
        dualConfig.setAvailableListLabel("Available Subscriptions:");
        dualConfig.setSelectedListLabel("Selected Subscriptions:");

        dualList = new DualList(userNameInfo, SWT.NONE, dualConfig, this);

    }

    /**
     * Handle a different user selected from the combo box.
     */
    private void handleUserSelect() {
        populateUserSubscriptions(userNameCombo.getText());
    }

    /**
     * Populate the Available subscriptions for a selected user.
     */
    private void populateUserSubscriptions(String owner) {

        final String ownerToUse = (owner == null) ? currentUser : owner;

        List<Subscription> results = Collections.emptyList();
        try {
            results = subHandler.getByOwner(ownerToUse);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve subscriptions for user " + ownerToUse,
                    e);
        }

        ArrayList<String> fullList = new ArrayList<String>();
        Map<String, Subscription> hMap = new HashMap<String, Subscription>();

        for (Subscription subscription : results) {

            String subName = subscription.getName();
            String user = subscription.getOwner();

            if (!nameArr.contains(user)) {
                nameArr.add(user);
            }

            fullList.add(subName);
            hMap.put(subName, subscription);
        }

        dualConfig = new DualListConfig();
        dualList.setFullList(fullList);
        userMap.put(owner, hMap);
    }

    /**
     * Change selected subscription definitions to group created properties.
     * 
     * @param groupName
     *            The name of the group
     */
    public void getSuscriptionNames(String groupName) {

        String owner = userNameCombo.getText();
        Map<String, Subscription> ownerSubs = userMap.get(owner);
        Map<String, Subscription> selectedSubs = null;

        if (addedMap.containsKey(owner)) {
            selectedSubs = addedMap.get(owner);
        } else {
            selectedSubs = new HashMap<String, Subscription>();
            addedMap.put(owner, selectedSubs);
        }

        String[] selectedList = dualList.getSelectedListItems();

        // Clear list
        selectedSubs.clear();

        // Re-add to list
        for (String subName : selectedList) {
            selectedSubs.put(subName, ownerSubs.get(subName));
        }

        if (selectedSubs != null) {
            addedMap.put(owner, selectedSubs);
        }

        // Get subscriptions that use the modified group
        List<Subscription> results = Collections.emptyList();
        try {
            results = subHandler.getByGroupName(groupName);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve subscriptions for group " + groupName,
                    e);
        }

        for (Subscription subscription : results) {
            if (addedMap.containsKey(subscription.getOwner())) {
                addedMap.get(subscription.getOwner()).put(
                        subscription.getName(), subscription);
            } else {

                HashMap<String, Subscription> ownerMap = new HashMap<String, Subscription>();
                ownerMap.put(subscription.getName(), subscription);

                addedMap.put(subscription.getOwner(), ownerMap);
            }
        }

        if (addedMap.size() > 0) {
            populateSelectedSubscriptions(groupName);
        }

    }

    /**
     * Populate the selected subscriptions
     * 
     * @param selectedList
     *            The selected subscriptions
     * @param groupName
     *            The name of the group
     */
    private void populateSelectedSubscriptions(String groupName) {

        // Get Group Definition
        GroupDefinition groupDefinition = GroupDefinitionManager
                .getGroup(groupName);

        // loop over hash map -- once over each hash map
        for (String owner : addedMap.keySet()) {

            Map<String, Subscription> ownerMap = addedMap.get(owner);

            for (String subName : ownerMap.keySet()) {

                Subscription subscription = ownerMap.get(subName);

                // Apply group properties to subscription definition
                subscription.setNotify(groupDefinition.getOption() == 1);

                subscription.setGroupName(groupName);

                // Set duration
                if (groupDefinition.getSubscriptionStart() != null) {
                    subscription.setSubscriptionStart(groupDefinition
                            .getSubscriptionStart());
                    subscription.setSubscriptionEnd(groupDefinition
                            .getSubscriptionEnd());
                } else {
                    subscription.setSubscriptionStart(null);
                    subscription.setSubscriptionEnd(null);
                }

                // Set active period
                if (groupDefinition.getActivePeriodStart() != null) {
                    subscription.setActivePeriodStart(groupDefinition
                            .getActivePeriodStart());
                    subscription.setActivePeriodEnd(groupDefinition
                            .getActivePeriodEnd());
                } else {
                    subscription.setActivePeriodStart(null);
                    subscription.setActivePeriodEnd(null);
                }

                if (subscription.getCoverage() != null
                        && groupDefinition.isArealDataSet()) {

                    DataSet dataset = MetaDataManager.getInstance().getDataSet(
                            subscription.getDataSetName(),
                            subscription.getProvider());
                    DataSizeUtils u = new DataSizeUtils(dataset);
                    u.setEnvelope(groupDefinition.getEnvelope());
                    u.setNumFcstHours(subscription.getTime()
                            .getSelectedTimeIndices().size());
                    u.setNumParameters(subscription.getParameter().size());

                    Coverage cov = new GriddedCoverage();
                    cov.setEnvelope(groupDefinition.getEnvelope());

                    subscription.setDataSetSize(u.getDataSetSize());
                    subscription.setCoverage(cov);
                }

                subscription.setOfficeID(LocalizationManager.getInstance()
                        .getCurrentSite());

                final String username = UserController.getUserObject()
                        .uniqueId().toString();

                final Shell shell = getShell();
                try {
                    InitialPendingSubscription pending = pendingSubHandler
                            .getBySubscription(subscription);

                    if (pending != null) {
                        DataDeliveryUtils.showMessage(shell,
                                SWT.ICON_INFORMATION, "Pending",
                                ALREADY_PENDING_SUBSCRIPTION);
                        return;
                    }
                } catch (RegistryHandlerException e1) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    DataDeliveryUtils.UNABLE_TO_RETRIEVE_PENDING_SUBSCRIPTIONS,
                                    e1);
                    return;
                }

                // check to see if user if authorized to approve. If so then
                // auto-approve
                IUser user = UserController.getUserObject();
                try {
                    boolean autoApprove = false;
                    DataDeliveryAuthRequest request = new DataDeliveryAuthRequest();
                    request.setUser(user);
                    request.addRequestedPermissions(
                            DataDeliveryPermission.SUBSCRIPTION_APPROVE_USER,
                            DataDeliveryPermission.SUBSCRIPTION_APPROVE_SITE);
                    request.setNotAuthorizedMessage("The subscription is awaiting approval.\n\n"
                            + "A notification message will be generated upon approval.");

                    PendingSubscription pendingSub = new PendingSubscription(
                            subscription, LocalizationManager.getInstance()
                                    .getCurrentUser());
                    pendingSub.setChangeReason("Group Definition Changed");

                    DataDeliveryAuthRequest r = DataDeliveryUtils
                            .sendAuthorizationRequest(request);
                    if (r != null && r.isAuthorized()) {
                        if (r.isAuthorized(DataDeliveryPermission.SUBSCRIPTION_APPROVE_SITE)) {
                            autoApprove = true;
                        } else if (r
                                .isAuthorized(DataDeliveryPermission.SUBSCRIPTION_APPROVE_USER)) {
                            if (subscription.getOwner().equals(
                                    user.uniqueId().toString())) {
                                autoApprove = true;
                            }
                        }

                        if (autoApprove) {
                            try {
                                ISubscriptionServiceResult response = subscriptionService
                                        .update(subscription,
                                                new CancelForceApplyAndIncreaseLatencyDisplayText(
                                                        "update", shell));
                                if (response.hasMessageToDisplay()) {
                                    DataDeliveryUtils.showMessage(shell,
                                            SWT.OK, "Subscription Updated",
                                            response.getMessageToDisplay());
                                }

                                subscriptionService
                                        .sendUpdatedSubscriptionNotification(
                                                subscription, username);
                            } catch (RegistryHandlerException e) {
                                statusHandler.handle(Priority.PROBLEM,
                                        "Unable to update subscription.", e);
                            }
                        } else {
                            savePendingSub(pendingSub, username);
                        }
                    } else {
                        savePendingSub(pendingSub, username);
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
    }

    /**
     * Save a pending subscription.
     */
    private void savePendingSub(PendingSubscription pendingSub, String username) {
        try {
            pendingSubHandler.store(pendingSub);

            final String msg = "The subscription is awaiting approval.\n\n"
                    + "A notification message will be generated upon approval.";
            DataDeliveryUtils.showMessage(getShell(), SWT.OK,
                    "Subscription Pending", msg);

            subscriptionService
                    .sendCreatedPendingSubscriptionForSubscriptionNotification(
                            pendingSub, username);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to store pending subscription.", e);
        }
    }

    /**
     * Load the User list.
     */
    private void loadUsers() {
        if (!nameArr.contains(currentUser)) {
            nameArr.add(currentUser);
        }

        populateUserSubscriptions(currentUser);

        userArr = nameArr.toArray(new String[nameArr.size()]);
        userNameCombo.setItems(userArr);
        userNameCombo.select(0);
    }

    /**
     * Check the selected list when it has changed.
     */
    @Override
    public void hasEntries(boolean entries) {

        String owner = userNameCombo.getText();
        Map<String, Subscription> ownerSubs = userMap.get(owner);
        Map<String, Subscription> selectedSubs = addedMap.get(owner);

        if (selectedSubs != null) {

            if (entries) {
                String[] selectedList = dualList.getSelectedListItems();

                // Clear list
                selectedSubs.clear();

                // Re-add to list
                for (String subName : selectedList) {
                    selectedSubs.put(subName, ownerSubs.get(subName));
                }

                addedMap.put(owner, selectedSubs);

            } else {
                selectedSubs.clear();
                addedMap.put(owner, selectedSubs);
            }
        }
    }

    @Override
    public void selectionChanged() {
        // unused
    }

    /**
     * Populate Selected Subscriptions list according to the group selection
     * 
     * @param groupName
     *            Name of the subscription group
     */
    public void selectSubscriptionsInGroup(String groupName) {

        // clear the selected list
        dualList.clearAvailableList(true);

        ArrayList<String> selectedGroupSubscriptions = new ArrayList<String>();
        Map<String, Subscription> sMap = userMap.get(userNameCombo.getText());

        for (String subscriptionName : sMap.keySet()) {

            Subscription sub = sMap.get(subscriptionName);

            if (groupName.equals(sub.getGroupName())) {
                selectedGroupSubscriptions.add(subscriptionName);
            }
        }

        // set the selected list
        dualList.selectItems(selectedGroupSubscriptions
                .toArray(new String[selectedGroupSubscriptions.size()]));

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
