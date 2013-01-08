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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

import com.google.common.collect.Sets;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.IPendingSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.datadelivery.retrieval.util.DataSizeUtils;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.datadelivery.filter.MetaDataManager;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.subscription.GroupDefinitionManager;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.ForceApplyPromptResponse;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IForceApplyPromptDisplayText;
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
public class UserSelectComp extends Composite implements IUpdate, IDisplay,
        IForceApplyPromptDisplayText {

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

    /** map to hold user subscriptions */
    private final Map<String, Map<String, Subscription>> userMap = new HashMap<String, Map<String, Subscription>>();

    private final Set<String> initiallySelectedSubscriptions = new HashSet<String>();

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
    public void getSubscriptionNames(String groupName) {

        String owner = userNameCombo.getText();
        Map<String, Subscription> ownerSubs = userMap.get(owner);

        Set<String> selectedSubscriptionNames = Sets.newHashSet(dualList
                .getSelectedListItems());

        Set<String> differences = Sets.symmetricDifference(
                selectedSubscriptionNames,
                initiallySelectedSubscriptions);

        Set<Subscription> addedToGroup = new HashSet<Subscription>();
        Set<Subscription> removedFromGroup = new HashSet<Subscription>();

        for (String subscriptionName : differences) {
            final Subscription subscription = ownerSubs.get(subscriptionName);
            if (selectedSubscriptionNames.contains(subscriptionName)) {
                addedToGroup.add(subscription);
            } else {
                removedFromGroup.add(subscription);
            }
        }

        updateGroupDefinition(groupName, addedToGroup, removedFromGroup);

    }

    /**
     * Updates the group definition, the subscriptions added to the group, and
     * the subscriptions removed from the group.
     * 
     * @param groupName
     * @param addedToGroup
     * @param removedFromGroup
     */
    private void updateGroupDefinition(String groupName,
            Set<Subscription> addedToGroup, Set<Subscription> removedFromGroup) {

        ITimer timer = TimeUtil.getPriorityEnabledTimer(statusHandler,
                Priority.DEBUG);
        timer.start();

        for (Subscription subscription : removedFromGroup) {
            subscription.setGroupName(GroupDefinition.NO_GROUP);
        }

        for (Subscription subscription : addedToGroup) {
            subscription.setGroupName(groupName);
        }

        Set<Subscription> groupSubscriptionsForUpdate = Collections.emptySet();
        try {
            groupSubscriptionsForUpdate = new HashSet<Subscription>(
                    DataDeliveryHandlers.getSubscriptionHandler()
                            .getByGroupName(groupName));

            // Remove any that are set to be removed from the group
            groupSubscriptionsForUpdate.removeAll(removedFromGroup);

            // Add those that are being added to the group
            groupSubscriptionsForUpdate.addAll(addedToGroup);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve group subscriptions.", e);
        }

        updateGroupDefinitionForSubscriptions(groupName,
                groupSubscriptionsForUpdate,
                removedFromGroup);

        timer.stop();
        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.debug("Took [" + timer.getElapsedTime()
                    + "] to update group definition");
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
    private void updateGroupDefinitionForSubscriptions(String groupName,
            Set<Subscription> groupSubscriptions,
            Set<Subscription> removeFromGroupSubscriptions) {

        // Get Group Definition
        GroupDefinition groupDefinition = GroupDefinitionManager
                .getGroup(groupName);

        for (Subscription subscription : groupSubscriptions) {

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
                u.determineNumberRequestedGrids(subscription.getParameter());

                Coverage cov = new GriddedCoverage();
                cov.setEnvelope(groupDefinition.getEnvelope());

                subscription.setDataSetSize(u.getDataSetSize());
                subscription.setCoverage(cov);
            }

            subscription.setOfficeID(LocalizationManager.getInstance()
                    .getCurrentSite());

        }
        

        try {
            final ISubscriptionServiceResult result = DataDeliveryServices
                    .getSubscriptionService().updateWithPendingCheck(
                            new ArrayList<Subscription>(Sets.union(
                                    groupSubscriptions,
                            removeFromGroupSubscriptions)),
                            this);
            if (result.hasMessageToDisplay()) {
                DataDeliveryUtils.showMessage(getShell(), SWT.ICON_INFORMATION,
                        "Edit Group", result.getMessageToDisplay());
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to update the group definition for subscriptions.",
                    e);
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
        // unused
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

        initiallySelectedSubscriptions.clear();
        Map<String, Subscription> sMap = userMap.get(userNameCombo.getText());

        for (String subscriptionName : sMap.keySet()) {

            Subscription sub = sMap.get(subscriptionName);

            if (groupName.equals(sub.getGroupName())) {
                initiallySelectedSubscriptions.add(subscriptionName);
            }
        }

        // set the selected list
        dualList.selectItems(initiallySelectedSubscriptions
                .toArray(new String[0]));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean displayYesNoPopup(String title, String message) {
        return DataDeliveryUtils.showYesNoMessage(getShell(), title, message) == SWT.YES;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getOptionDisplayText(ForceApplyPromptResponse option,
            int requiredLatency, Subscription subscription,
            Set<String> wouldBeUnscheduledSubscriptions) {
        switch (option) {
        case CANCEL:
            return "Do not update the group definition.";
        case FORCE_APPLY:
            if (wouldBeUnscheduledSubscriptions.size() == 1) {
                return "Update the group definition and unschedule "
                        + wouldBeUnscheduledSubscriptions.iterator().next();
            }
            return "Update the group definition and unschedule the subscriptions";
        case INCREASE_LATENCY:
            // Signifies it should not be an option
            return null;
        default:
            throw new IllegalArgumentException(
                    "Don't know how to handle option [" + option + "]");
        }
    }
}
