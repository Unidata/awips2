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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TimeZone;
import java.util.TreeSet;
import java.util.concurrent.Callable;

import org.eclipse.swt.widgets.Shell;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.Sets;
import com.raytheon.uf.common.auth.AuthException;
import com.raytheon.uf.common.auth.req.IPermissionsService;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthService;
import com.raytheon.uf.common.datadelivery.bandwidth.IProposeScheduleResponse;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.RecurringSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionState;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.IPendingSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;
import com.raytheon.uf.common.datadelivery.service.ISubscriptionNotificationService;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapRequest;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapResponse;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.datadelivery.actions.SubscriptionManagerAction;
import com.raytheon.uf.viz.datadelivery.system.SystemRuleManager;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * Basic implementation of the {@link ISubscriptionService}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 07, 2012 1286       djohnson     Initial creation
 * Nov 20, 2012 1286       djohnson     Use propose schedule methods to see effects of subscription scheduling.
 * Nov 28, 2012 1286       djohnson     Add more notification methods.
 * Dec 11, 2012 1404       mpduff       Add message to sendDeletedSubscriptionNotification.
 * Dec 11, 2012 1403       djohnson     Adhoc subscriptions no longer go to the registry.
 * Dec 18, 2012 1443       bgonzale     Open force apply prompt pop-up on the UI thread.
 * Dec 20, 2012 1413       bgonzale     Added new pending approve and denied request and responses.
 * Jan 04, 2013 1441       djohnson     Separated out notification methods into their own service.
 * Jan 28, 2013 1530       djohnson     Reset unscheduled flag with each update.
 * Mar 29, 2013 1841       djohnson     Subscription is now UserSubscription.
 * May 14, 2013 2000       djohnson     Check for subscription overlap/duplication.
 * May 23, 2013 1650       djohnson     Move out some presentation logic to DisplayForceApplyPromptDialog.
 * Jun 12, 2013 2038       djohnson     Launch subscription manager on the UI thread.
 * Jul 18, 2013 1653       mpduff       Add SubscriptionStatusSummary.
 * Jul 26, 2013 2232       mpduff       Refactored Data Delivery permissions.
 * Sept 25, 2013 1797      dhladky      separated time from gridded time
 * Oct 12, 2013 2460       dhladky      restored adhoc subscriptions to registry storage.
 * Oct 22, 2013  2292      mpduff       Removed subscriptionOverlapService.
 * Nov 07, 2013  2291      skorolev     Used showText() method for "Shared Subscription" message.
 * Jan 26, 2014  2259      mpduff       Turn off subs to be deactivated.
 * Feb 04, 2014  2677      mpduff       Don't do overlap checks when deactivating subs.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SubscriptionService implements ISubscriptionService {
    private static final String PENDING_SUBSCRIPTION_AWAITING_APPROVAL = "The subscription is awaiting approval.\n\n"
            + "A notification message will be generated upon approval.";

    private final String OVERLAPPING_SUBSCRIPTIONS = "The following subscriptions overlap with this one "
            + "and are candidates for a shared subscription: ";

    private final String DUPLICATE_SUBSCRIPTIONS = "This subscription is completely fulfilled by ";

    /**
     * Implementation of {@link IDisplayForceApplyPrompt} that uses an SWT
     * dialog.
     */
    private static class DisplayForceApplyPrompt implements
            IDisplayForceApplyPrompt {

        private ForceApplyPromptResponse forceApplyPromptResponse = ForceApplyPromptResponse.CANCEL;

        /**
         * {@inheritDoc}
         */
        @Override
        public ForceApplyPromptResponse displayForceApplyPrompt(
                ForceApplyPromptConfiguration configuration) {
            DisplayForceApplyPromptDialog dlg = new DisplayForceApplyPromptDialog(
                    configuration);
            forceApplyPromptResponse = (ForceApplyPromptResponse) dlg.open();
            return forceApplyPromptResponse;
        }

        /**
         * get the response from the last call to the displayForceApplyPrompt
         * method.
         */
        @Override
        public ForceApplyPromptResponse getForceApplyPromptResponse() {
            return forceApplyPromptResponse;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void displayMessage(
                IForceApplyPromptDisplayText displayTextStrategy,
                final String message) {
            final Shell shell = displayTextStrategy.getShell();
            shell.getDisplay().syncExec(new Runnable() {
                @Override
                public void run() {
                    DataDeliveryUtils.showText(shell, "Shared Subscription",
                            message);
                }
            });
        }
    }

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionService.class);

    @VisibleForTesting
    final String TITLE = "Subscription";

    private final ISubscriptionNotificationService notificationService;

    private final IBandwidthService bandwidthService;

    private final IPermissionsService permissionsService;

    private final IDisplayForceApplyPrompt forceApplyPrompt;

    /**
     * Result class used internally to denote whether the user should be
     * prompted, and any result messages.
     */
    private final class ProposeResult {
        private final boolean promptUser;

        private final ForceApplyPromptConfiguration config;

        private ProposeResult(boolean promptUser,
                ForceApplyPromptConfiguration config) {
            this.promptUser = promptUser;
            this.config = config;
        }
    }

    /**
     * A service interaction.
     */
    private interface ServiceInteraction extends Callable<String> {
        // Throws only one exception
        @Override
        String call() throws RegistryHandlerException;
    }

    /**
     * Enumeration of force apply responses.
     */
    public static enum ForceApplyPromptResponse {
        CANCEL, INCREASE_LATENCY, EDIT_SUBSCRIPTIONS, FORCE_APPLY;
    }

    /**
     * Interface representing shelling a force apply prompt.
     */
    @VisibleForTesting
    static interface IDisplayForceApplyPrompt {
        /**
         * Display the force apply prompt.
         * 
         * @param configuration
         *            the configuration
         * 
         * @return the response
         */
        ForceApplyPromptResponse displayForceApplyPrompt(
                ForceApplyPromptConfiguration configuration);

        ForceApplyPromptResponse getForceApplyPromptResponse();

        /**
         * Display a popup message to the user.
         * 
         * @param displayTextStrategy
         * @param message
         */
        void displayMessage(IForceApplyPromptDisplayText displayTextStrategy,
                String message);
    }

    /**
     * Interface that must be implemented by classes that will be showing a
     * force apply prompt message.
     */
    public static interface IForceApplyPromptDisplayText {
        /**
         * Retrieve the display text that will be displayed for each option.
         * 
         * @param option
         *            the option
         * @param requiredLatency
         *            the required latency that would be required to schedule
         *            the item(s)
         * @param subscription
         *            the subscription that would require the increased latency,
         *            or null if this is a multi-subscription operation
         * @param wouldBeUnscheduledSubscriptions
         *            the subscription names that would be unscheduled
         * @return the display text, or null if the option should not be
         *         displayed
         */
        String getOptionDisplayText(ForceApplyPromptResponse option,
                int requiredLatency, Subscription subscription,
                Set<String> wouldBeUnscheduledSubscriptions);

        /**
         * Get the shell to use.
         * 
         * @return the shell
         */
        Shell getShell();
    }

    /**
     * Private constructor. Use
     * {@link #newInstance(ISubscriptionNotificationService)} instead.
     * 
     * @param notificationService
     *            the subscription notification service
     * @param bandwidthService
     *            the bandwidth service
     */
    @VisibleForTesting
    SubscriptionService(ISubscriptionNotificationService notificationService,
            IBandwidthService bandwidthService,
            IPermissionsService permissionsService,
            IDisplayForceApplyPrompt displayForceApplyPrompt) {
        this.notificationService = notificationService;
        this.bandwidthService = bandwidthService;
        this.permissionsService = permissionsService;
        this.forceApplyPrompt = displayForceApplyPrompt;
    }

    /**
     * Factory method to create an {@link ISubscriptionService}. Allows for
     * changing to use sub-classes or different implementations later, without
     * tying specifically to the implementation class.
     * 
     * @param notificationService
     * @param bandwidthService
     * @param permissionsService
     * @param
     * @return the subscription service
     */
    public static ISubscriptionService newInstance(
            ISubscriptionNotificationService notificationService,
            IBandwidthService bandwidthService,
            IPermissionsService permissionsService) {
        return new SubscriptionService(notificationService, bandwidthService,
                permissionsService, new DisplayForceApplyPrompt());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionServiceResult store(final Subscription subscription,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {

        final List<Subscription> subscriptions = Arrays.asList(subscription);
        final String successMessage = "Subscription " + subscription.getName()
                + " has been created.";
        final ServiceInteraction action = new ServiceInteraction() {
            @Override
            public String call() throws RegistryHandlerException {
                DataDeliveryHandlers.getSubscriptionHandler().store(
                        subscription);
                return successMessage;
            }
        };

        SubscriptionServiceResult result = performAction(subscriptions, action,
                displayTextStrategy);

        if (!result.isAllowFurtherEditing()) {
            result.setSubscriptionStatusSummary(bandwidthService
                    .getSubscriptionStatusSummary(subscription));
        }

        return result;
    }

    /**
     * {@inheritDoc}
     * 
     */
    @Override
    public SubscriptionServiceResult update(final Subscription subscription,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {

        final List<Subscription> subscriptions = Arrays.asList(subscription);
        final String successMessage = "Subscription " + subscription.getName()
                + " has been updated.";
        final ServiceInteraction action = new ServiceInteraction() {
            @Override
            public String call() throws RegistryHandlerException {
                subscription.setUnscheduled(false);
                DataDeliveryHandlers.getSubscriptionHandler().update(
                        subscription);
                return successMessage;
            }
        };

        return performAction(subscriptions, action, displayTextStrategy);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionServiceResult update(final List<Subscription> subs,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {

        final String successMessage = "The subscriptions have been updated.";
        final ServiceInteraction action = new ServiceInteraction() {
            @Override
            public String call() throws RegistryHandlerException {
                for (Subscription sub : subs) {
                    sub.setUnscheduled(false);
                    DataDeliveryHandlers.getSubscriptionHandler().update(sub);
                }
                return successMessage;
            }
        };

        return performAction(subs, action, displayTextStrategy);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionServiceResult updateWithPendingCheck(
            final List<Subscription> subscriptions,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {
        final ServiceInteraction action = new ServiceInteraction() {

            @Override
            public String call() throws RegistryHandlerException {
                final SortedSet<String> alreadyPending = new TreeSet<String>();
                final SortedSet<String> pendingCreated = new TreeSet<String>();
                final SortedSet<String> unableToUpdate = new TreeSet<String>();
                final StringBuilder successMessage = new StringBuilder(
                        "The subscriptions have been updated.");

                final IPendingSubscriptionHandler pendingSubscriptionHandler = DataDeliveryHandlers
                        .getPendingSubscriptionHandler();

                for (Subscription subscription : subscriptions) {

                    try {
                        InitialPendingSubscription pending = pendingSubscriptionHandler
                                .getBySubscription(subscription);

                        if (pending != null) {
                            alreadyPending.add(subscription.getName());
                            continue;
                        }
                    } catch (RegistryHandlerException e1) {
                        statusHandler
                                .handle(Priority.INFO,
                                        DataDeliveryUtils.UNABLE_TO_RETRIEVE_PENDING_SUBSCRIPTIONS,
                                        e1);
                        unableToUpdate.add(subscription.getName());
                        continue;
                    }

                    IUser user = UserController.getUserObject();
                    final String username = user.uniqueId().toString();

                    try {
                        if (!(permissionsService instanceof RequestFromServerPermissionsService)) {
                            throw new RegistryHandlerException(
                                    "Invalid Handler "
                                            + permissionsService.getClass()
                                                    .toString());
                        }
                        boolean authorized = ((RequestFromServerPermissionsService) permissionsService)
                                .checkPermissionToChangeSubscription(user,
                                        PENDING_SUBSCRIPTION_AWAITING_APPROVAL,
                                        subscription).isAuthorized();
                        try {
                            if (authorized) {
                                subscription.setUnscheduled(false);
                                DataDeliveryHandlers.getSubscriptionHandler()
                                        .update(subscription);
                            } else {
                                PendingSubscription pendingSub = subscription
                                        .pending(username);
                                pendingSub
                                        .setChangeReason("Group Definition Changed");
                                savePendingSub(pendingSub, username);
                                pendingCreated.add(subscription.getName());
                            }
                        } catch (RegistryHandlerException e1) {
                            statusHandler
                                    .handle(Priority.INFO,
                                            DataDeliveryUtils.UNABLE_TO_RETRIEVE_PENDING_SUBSCRIPTIONS,
                                            e1);
                            unableToUpdate.add(subscription.getName());
                            continue;
                        }

                    } catch (AuthException e) {
                        statusHandler.handle(Priority.INFO,
                                e.getLocalizedMessage(), e);
                    }
                }
                appendCollectionPortion(
                        successMessage,
                        "\n\nThe following subscriptions have pending changes awaiting approval:",
                        pendingCreated);

                appendCollectionPortion(
                        successMessage,
                        "\n\nThe following subscriptions already had pending changes and were not modified:",
                        alreadyPending);

                appendCollectionPortion(
                        successMessage,
                        "\n\nThe following subscriptions were unable to be modified:",
                        unableToUpdate);

                return successMessage.toString();
            }
        };

        return performAction(subscriptions, action, displayTextStrategy);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionServiceResult store(final AdhocSubscription sub,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {

        final List<Subscription> subscriptions = Arrays
                .<Subscription> asList(sub);
        final String successMessage = "The query was successfully stored.";
        final ServiceInteraction action = new ServiceInteraction() {
            @Override
            public String call() throws RegistryHandlerException {
                DataDeliveryHandlers.getSubscriptionHandler().store(sub);
                return successMessage;
            }
        };

        SubscriptionServiceResult result = performAction(subscriptions, action,
                displayTextStrategy);
        if (!result.isAllowFurtherEditing()) {
            Date date = bandwidthService.getEstimatedCompletionTime(sub);
            if (date != null) {
                SimpleDateFormat sdf = new SimpleDateFormat(
                        "MM/dd/yyyy HH:mm zzz");
                sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
                result = new SubscriptionServiceResult(result.getMessage()
                        + "\n\nEstimated completion time:" + sdf.format(date));
            }
        }

        return result;
    }

    /**
     * Performs the action on the subscriptions. If the action would cause
     * subscriptions to be unscheduled, the user is prompted whether or not they
     * would like to continue with the action forcibly. If so, the action is
     * performed and the affected subscriptions are updated to be in the
     * unscheduled state.
     * 
     * @param subscriptions
     * @param action
     * @param displayTextStrategy
     * @return the result object
     * @throws RegistryHandlerException
     */
    private SubscriptionServiceResult performAction(
            List<Subscription> subscriptions, ServiceInteraction action,
            final IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {

        if (subscriptions == null || subscriptions.isEmpty()) {
            return new SubscriptionServiceResult(false,
                    "No subscriptions submitted.");
        }

        /*
         * If activating the subscriptions check for overlaps.
         * 
         * Only need to check one because all are being updated the same way.
         */
        if (subscriptions.get(0).getSubscriptionState() == SubscriptionState.ON) {
            SubscriptionOverlapRequest request = new SubscriptionOverlapRequest(
                    subscriptions);

            SubscriptionOverlapResponse response = null;
            try {
                response = (SubscriptionOverlapResponse) RequestRouter.route(
                        request, DataDeliveryConstants.DATA_DELIVERY_SERVER);
                if (response.isDuplicate()) {
                    return new SubscriptionServiceResult(true,
                            StringUtil.createMessage(DUPLICATE_SUBSCRIPTIONS,
                                    response.getSubscriptionNameList()));
                }

                if (response.isOverlap()) {
                    List<String> subNames = response.getSubscriptionNameList();
                    Collections.sort(subNames);
                    forceApplyPrompt.displayMessage(displayTextStrategy,
                            StringUtil.createMessage(OVERLAPPING_SUBSCRIPTIONS,
                                    subNames));
                }
            } catch (Exception e) {
                statusHandler.error("Error checking subscription overlapping",
                        e);
                return new SubscriptionServiceResult(false);
            }
        }

        try {
            final ProposeResult result = proposeScheduleAndAction(
                    subscriptions, action, displayTextStrategy);

            if (result.promptUser) {
                final Subscription subscription = (subscriptions.size() == 1) ? subscriptions
                        .get(0) : null;

                VizApp.runSync(new Runnable() {
                    @Override
                    public void run() {
                        forceApplyPrompt.displayForceApplyPrompt(result.config);
                    }
                });
                switch (forceApplyPrompt.getForceApplyPromptResponse()) {
                case INCREASE_LATENCY:
                    subscription
                            .setLatencyInMinutes(result.config.requiredLatency);
                    // Intentional fall-through
                case FORCE_APPLY:
                    // Have to make sure we set them to not be unscheduled, let
                    // the bandwidth manager decide they can't be scheduled
                    for (Subscription temp : subscriptions) {
                        temp.setUnscheduled(false);
                    }
                    String successMessage = action.call();

                    final Set<String> unscheduled = bandwidthService
                            .schedule(subscriptions);
                    updateSubscriptionsByNameToUnscheduled(unscheduled);

                    StringBuilder sb = new StringBuilder(successMessage);
                    getUnscheduledSubscriptionsPortion(sb, unscheduled);

                    return new SubscriptionServiceResult(sb.toString());
                case CANCEL:
                    return new SubscriptionServiceResult(true);
                case EDIT_SUBSCRIPTIONS:
                    if (!result.config.isNotAbleToScheduleOnlyTheSubscription()) {
                        VizApp.runSync(new Runnable() {
                            @Override
                            public void run() {
                                new SubscriptionManagerAction()
                                        .loadSubscriptionManager(SubscriptionManagerFilters
                                                .getByNames(result.config.wouldBeUnscheduledSubscriptions));
                            }
                        });
                    }
                    return new SubscriptionServiceResult(true);
                default:
                    throw new IllegalArgumentException(
                            "Unknown force apply prompt response!  Did you add a new type that must be handled?");
                }
            }

            return new SubscriptionServiceResult(result.config.message);
        } catch (RegistryHandlerException e) {
            // The in-memory objects must be corrupted since we schedule first,
            // then store to the registry, so a reinitialize is called for
            bandwidthService.reinitialize();

            throw e;
        }
    }

    /**
     * Proposes scheduling the subscriptions (with any modifications that have
     * been made) in the bandwidth manager. If subscriptions would be
     * unscheduled as a result, then a message is returned designating such.
     * 
     * @param subscriptions
     * @param serviceInteraction
     * @return the result
     * @throws RegistryHandlerException
     */
    private ProposeResult proposeScheduleAndAction(
            List<Subscription> subscriptions,
            ServiceInteraction serviceInteraction,
            IForceApplyPromptDisplayText displayTextStrategy)
            throws RegistryHandlerException {

        IProposeScheduleResponse proposeScheduleresponse = bandwidthService
                .proposeSchedule(subscriptions);
        Set<String> unscheduledSubscriptions = proposeScheduleresponse
                .getUnscheduledSubscriptions();
        boolean wouldUnscheduleSubs = !unscheduledSubscriptions.isEmpty();

        ForceApplyPromptConfiguration response = null;
        if (wouldUnscheduleSubs) {
            response = getWouldCauseUnscheduledSubscriptionsPortion(
                    unscheduledSubscriptions, subscriptions,
                    proposeScheduleresponse, displayTextStrategy);
        } else {
            response = new ForceApplyPromptConfiguration(TITLE,
                    serviceInteraction.call(), displayTextStrategy,
                    unscheduledSubscriptions);
        }

        return new ProposeResult(wouldUnscheduleSubs, response);
    }

    /**
     * Appends the unscheduled subscriptions portion to the StringBuilder.
     * 
     * @param unscheduledSubscriptions
     *            the unscheduled subscriptions
     * @param subscriptions
     *            the subscriptions which were attempting to schedule
     * @param dataSize
     */
    private ForceApplyPromptConfiguration getWouldCauseUnscheduledSubscriptionsPortion(
            Set<String> unscheduledSubscriptions,
            List<Subscription> subscriptions,
            IProposeScheduleResponse proposeScheduleResponse,
            IForceApplyPromptDisplayText displayTextStrategy) {
        StringBuilder msg = new StringBuilder();

        // Handle the case where it's just the subscription we're changing
        // itself that would not schedule
        final boolean singleSubscription = subscriptions.size() == 1;
        if ((singleSubscription && unscheduledSubscriptions.size() == 1)
                && (subscriptions.get(0).getName()
                        .equals(unscheduledSubscriptions.iterator().next()))) {
            final Subscription subscription = subscriptions.get(0);
            msg.append(
                    (subscription instanceof AdhocSubscription) ? "The query"
                            : "Subscription " + subscription.getName())
                    .append(" would not fully schedule with the bandwidth management system if this action were performed.");
        } else {
            msg.append("The following subscriptions would not fully schedule with the bandwidth management system if this action were performed:");
        }

        if (singleSubscription) {
            Subscription subscription = subscriptions.get(0);
            final int maximumLatencyFromRules = getMaximumAllowableLatency(subscription);

            return new ForceApplyPromptConfiguration(TITLE, msg.toString(),
                    proposeScheduleResponse.getRequiredLatency(),
                    maximumLatencyFromRules,
                    proposeScheduleResponse.getRequiredDataSetSize(),
                    displayTextStrategy, subscription, unscheduledSubscriptions);
        } else {
            return new ForceApplyPromptConfiguration(TITLE, msg.toString(),
                    displayTextStrategy, unscheduledSubscriptions);
        }
    }

    /**
     * Appends the unscheduled subscriptions portion to the StringBuilder.
     * 
     * @param unscheduledSubscriptions
     *            the unscheduled subscriptions
     */
    private void getUnscheduledSubscriptionsPortion(StringBuilder msg,
            Set<String> unscheduledSubscriptions) {
        appendCollectionPortion(
                msg,
                "\n\nThe following subscriptions did not fully schedule with the bandwidth management system:",
                unscheduledSubscriptions);
    }

    /**
     * Append a collection of items underneath a preamble text.
     * 
     * @param msg
     *            the current text
     * @param preamble
     *            the preamble
     * @param collection
     *            the collection of items
     */
    private void appendCollectionPortion(StringBuilder msg, String preamble,
            Collection<String> collection) {
        if (collection.isEmpty()) {
            return;
        }
        msg.append(StringUtil.createMessage(preamble, collection));
    }

    /**
     * Save a pending subscription.
     * 
     * @throws RegistryHandlerException
     */
    private void savePendingSub(PendingSubscription pendingSub, String username)
            throws RegistryHandlerException {
        DataDeliveryHandlers.getPendingSubscriptionHandler().store(pendingSub);

        notificationService
                .sendCreatedPendingSubscriptionForSubscriptionNotification(
                        pendingSub, username);
    }

    private void updateSubscriptionsByNameToUnscheduled(
            java.util.Collection<String> subscriptionNames)
            throws RegistryHandlerException {
        ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        for (String subName : subscriptionNames) {
            Subscription unscheduledSub = subscriptionHandler
                    .getByName(subName);
            if (unscheduledSub == null) {
                continue;
            }
            unscheduledSub.setUnscheduled(true);
            if (unscheduledSub instanceof RecurringSubscription) {
                ((RecurringSubscription) unscheduledSub)
                        .setSubscriptionState(SubscriptionState.OFF);
            }
            subscriptionHandler.update(unscheduledSub);
        }
    }

    /**
     * Gets the max allowed latency for this subscription from rules for it's
     * type
     * 
     * @param subscription
     * @return
     */
    private int getMaximumAllowableLatency(Subscription subscription) {

        Time subTime = subscription.getTime();

        if (subTime instanceof GriddedTime) {
            return SystemRuleManager.getInstance().getLatency(subscription,
                    Sets.newTreeSet(((GriddedTime) subTime).getCycleTimes()));
        } else if (subTime instanceof PointTime) {
            return ((PointTime) subTime).getInterval();
        } else {
            throw new IllegalArgumentException(subTime.getClass()
                    + " Not yet implemented!");
        }

    }
}
