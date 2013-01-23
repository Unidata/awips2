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
package com.raytheon.uf.viz.datadelivery.subscription.presenter;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.swt.widgets.Shell;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Strings;
import com.google.common.collect.Sets;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Utils.SubscriptionStatus;
import com.raytheon.uf.common.datadelivery.registry.handlers.IPendingSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlers;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IGuiThreadTaskExecutor;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.subscription.CancelForceApplyAndIncreaseLatencyDisplayText;
import com.raytheon.uf.viz.datadelivery.subscription.GroupDefinitionManager;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionNotificationService;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;
import com.raytheon.uf.viz.datadelivery.subscription.view.ICreateSubscriptionDlgView;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.presenter.components.ButtonConf;
import com.raytheon.viz.ui.presenter.components.CheckBoxConf;
import com.raytheon.viz.ui.presenter.components.ComboBoxConf;
import com.raytheon.viz.ui.presenter.components.WidgetConf;

/**
 * Create Subscription Dialog Presenter Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2012  223       mpduff       Initial creation
 * Aug 27, 2012 0743       djohnson     Use AssociationQuery for pending subscriptions.
 * Aug 31, 2012  702       jpiatt       Correct form population.
 * Aug 31, 2012 1128       mpduff       Notification fixes.
 * Sep 17, 2012 1157       mpduff       Pass subscription to NotificationRequest.
 * Sep 14, 2012 1169       djohnson     Remove duplicate store of the subscription.
 * Sep 18, 2012 1169       djohnson     Change to use higher-level registry API.
 * Oct 03, 2012 1241       djohnson     Use {@link DataDeliveryPermission} and registry handlers.
 * Nov 09, 2012 1286       djohnson     Consolidate duplicate subscription handling.
 * Nov 20, 2012 1286       djohnson     Pass view to subscription service.
 * Dec 12, 2012 1391       bgonzale     Added a job for subscription creation.
 * Dec 10, 2012 1259       bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Dec 17, 2012 1434       mpduff       Don't allow underscores in name.
 * Dec 18, 2012 1439       mpduff       Redo subscription name validation.
 * Jan 02, 2012 1345       djohnson     Use gui thread task executor.
 * Jan 02, 2013 1441       djohnson     Access GroupDefinitionManager in a static fashion.
 * Jan 04, 2012 1420       mpduff       Add Latency to PriorityComp.
 * Jan 11, 2013 1453       djohnson     Sets cycle times on construction.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class CreateSubscriptionDlgPresenter {
    private static final String PENDING_APPROVAL_MESSAGE = "The subscription is awaiting approval.\n\n"
            + "A notification message will be generated upon approval.";

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CreateSubscriptionDlgPresenter.class);

    @VisibleForTesting
    static final String CREATED_TITLE = "Subscription Created";

    static final String UPDATED_TITLE = "Subscription Updated";

    @VisibleForTesting
    static ISubscriptionService subscriptionService = DataDeliveryServices
            .getSubscriptionService();

    /** OK Button config object */
    protected final ButtonConf OK_CONF;

    /** Delivery combo config object */
    protected final ComboBoxConf DELIVERY_COMBO_CONF = new ComboBoxConf(true,
            "Select delivery method", WidgetConf.DO_NOTHING);

    /** Delivery options strings */
    protected final String[] DELIVERY_OPTIONS = new String[] {
            "Deliver data when available", "Notify when data are available" };

    /** Group combo config object */
    protected final ComboBoxConf GROUP_COMBO_CONF;

    /** Cycle Checkbox config object list */
    private List<CheckBoxConf> cycleChkList = new ArrayList<CheckBoxConf>();

    /** The view */
    private final ICreateSubscriptionDlgView view;

    /** The subscription object */
    private Subscription subscription;

    /** Group combo runnable action */
    private final Runnable groupComboAction;

    /** OK Button runnable action */
    private final Runnable okBtnAction;

    /** The dataset */
    private final DataSet dataSet;

    /** Group valid flag */
    boolean groupValid = false;

    /** Group Definition object */
    private GroupDefinition groupDefinition;

    /** Create subscription flag */
    private final boolean create;

    private final IGuiThreadTaskExecutor guiThreadTaskExecutor;

    private final Set<Integer> cycleTimes;

    private final ISubscriptionNotificationService subscriptionNotificationService = DataDeliveryServices
            .getSubscriptionNotificationService();

    /**
     * Constructor.
     * 
     * @param view
     *            The view
     * @param dataSet
     *            The dataset
     * @param create
     */
    public CreateSubscriptionDlgPresenter(ICreateSubscriptionDlgView view,
            DataSet dataSet, boolean create,
            IGuiThreadTaskExecutor guiThreadTaskExecutor) {
        this.view = view;
        this.dataSet = dataSet;
        this.create = create;
        this.guiThreadTaskExecutor = guiThreadTaskExecutor;

        // Get cycles
        cycleTimes = Sets.newTreeSet(((OpenDapGriddedDataSet) dataSet)
                .getCycles());

        groupComboAction = new Runnable() {
            @Override
            public void run() {
                groupNameSelectionEvent();
            }
        };
        GROUP_COMBO_CONF = new ComboBoxConf(true, "Select a group name",
                groupComboAction);

        okBtnAction = new Runnable() {
            @Override
            public void run() {
                handleOkAction();
            }
        };
        OK_CONF = new ButtonConf(true, "OK", null, okBtnAction);
    }

    /**
     * Handle OK Action
     */
    private void handleOkAction() {
        if (okAction()) {
            view.closeDlg();
        } else {
            view.bringToTop();
        }
    }

    /**
     * Open the view
     */
    public void open() {
        Runnable callback = new Runnable() {
            @Override
            public void run() {
                init();
            }
        };
        this.view.setCycleTimes(cycleTimes);
        this.view.setSubscription(this.subscription);
        this.view.setPreOpenCallback(callback);
        this.view.openDlg();
    }

    /**
     * is disposed check
     * 
     * @return true if view is disposed
     */
    public boolean isDisposed() {
        return view.isDisposed();
    }

    /**
     * Set the subscription data object.
     * 
     * @param sub
     *            The subscription object
     */
    public void setSubscriptionData(Subscription sub) {
        this.subscription = sub;
    }

    /**
     * Get the subscription.
     * 
     * @return the subscription
     */
    public Subscription getSubscription() {
        return this.subscription;
    }

    /**
     * Bring the view dialog to the top
     */
    public void bringToTop() {
        view.bringToTop();

    }

    /**
     * Initialize the view
     */
    public void init() {
        view.setDeliveryOptionsComboConf(DELIVERY_COMBO_CONF);
        view.setDeliveryOptions(DELIVERY_OPTIONS);
        view.setDeliverySelection(0);
        view.setOkConf(OK_CONF);

        final boolean hasCycleTimes = !cycleTimes.isEmpty();

        this.cycleChkList = new ArrayList<CheckBoxConf>(cycleTimes.size());
        for (int cycle : cycleTimes) {
            String hour = Strings.padStart(String.valueOf(cycle), 2, '0');

            CheckBoxConf cb = new CheckBoxConf(hour, false, "Model Cycle Time",
                    CheckBoxConf.DO_NOTHING);
            cycleChkList.add(cb);
        }

        view.setCycleConf(cycleChkList);
        ButtonConf sa = new ButtonConf(hasCycleTimes, "Select All",
                "Select all cycle times", ButtonConf.DO_NOTHING);
        ButtonConf da = new ButtonConf(hasCycleTimes, "Deselect All",
                "Deselect all cycle times", ButtonConf.DO_NOTHING);

        view.setSelectAllButton(sa);
        view.setDeselectAllButton(da);

        populate();
    }

    /**
     * Populate the dialog.
     */
    private void populate() {
        if (this.subscription == null) {
            return;
        }

        view.setSubscriptionName(subscription.getName());

        if (subscription.getDescription() != null) {
            view.setSubscriptionDescription(subscription.getDescription());
        }

        if (subscription.getGroupName() != null
                && !subscription.getGroupName().equals("None")) {
            view.setGroupName(subscription.getGroupName());
        }

        view.setDeliverySelection(subscription.isNotify() ? 0 : 1);

        if (subscription.getSubscriptionEnd() != null) {
            view.setStartDate(subscription.getSubscriptionStart());
            view.setExpirationDate(subscription.getSubscriptionEnd());
            view.setNoExpiration(false);
        } else {
            view.setNoExpiration(true);
            view.setDateTxtFieldsEnabled(false);
            view.setStartDateBtnEnabled(false);
            view.setEndDateBtnEnabled(false);
        }

        Date activePeriodStartDate = subscription.getActivePeriodStart();
        Date activePeriodEndDate = subscription.getActivePeriodEnd();

        if (activePeriodStartDate != null && activePeriodEndDate != null) {
            final Calendar now = TimeUtil.newGmtCalendar();
            int calendarYearToUse = now.get(Calendar.YEAR);

            // If currently in the window, assume starting from last year for
            // the start date
            if (subscription.getStatus().equals(
                    SubscriptionStatus.ACTIVE.toString())) {
                calendarYearToUse--;
            }

            activePeriodStartDate = calculateNextOccurenceOfMonthAndDay(
                    activePeriodStartDate, calendarYearToUse, now);

            Calendar activePeriodStartCal = TimeUtil.newGmtCalendar();
            activePeriodStartCal.setTime(activePeriodStartDate);

            activePeriodEndDate = calculateNextOccurenceOfMonthAndDay(
                    activePeriodEndDate,
                    activePeriodStartCal.get(Calendar.YEAR), now);

            view.setActiveStartDate(activePeriodStartDate);
            view.setActiveEndDate(activePeriodEndDate);
            view.setAlwaysActive(false);
        } else {
            view.setAlwaysActive(true);
            view.setActiveTextFieldsEnabled(false);
            view.setActiveEndDateBtnEnabled(false);
            view.setActiveEndDateBtnEnabled(false);
        }

        List<Integer> cycleTimes = subscription.getTime().getCycleTimes();
        if (cycleTimes != null) {
            List<String> cycleStrings = new ArrayList<String>();

            for (int cycle : cycleTimes) {
                if (cycle < 10) {
                    cycleStrings.add("0" + String.valueOf(cycle));
                } else {
                    cycleStrings.add(String.valueOf(cycle));
                }
            }

            view.selectCycles(cycleStrings);
        }

        if (!Strings.isNullOrEmpty(subscription.getGroupName())) {
            view.setGroupName(subscription.getGroupName());
        }
    }

    /**
     * Calculate the next occurrence of the month and day on the specified date
     * object.
     * 
     * @param dateWithMonthAndDay
     *            the date to retrieve the month and day from
     * @param yearToStartAt
     *            the year to start moving forward from, checking for the date
     *            to not before the current time
     * @param now
     *            the current calendar
     * 
     * @return the date object of the next occurrence
     */
    private static Date calculateNextOccurenceOfMonthAndDay(
            Date dateWithMonthAndDay, int yearToStartAt, Calendar now) {
        final Calendar cal = TimeUtil.newCalendar();
        cal.setTime(dateWithMonthAndDay);
        cal.set(Calendar.YEAR, yearToStartAt);
        if (cal.before(now)) {
            cal.add(Calendar.YEAR, 1);
        }
        return cal.getTime();
    }

    /**
     * The group name selection event
     */
    private void groupNameSelectionEvent() {
        populate(view.getGroupName());
    }

    /**
     * The OK Button action.
     * 
     * @return
     */
    boolean okAction() {
        if (!validate()) {
            return false;
        }

        // Data are valid, now add info to the subscription object and store
        // to the registry
        if (view.getDeliverySelection() == 1) {
            subscription.setNotify(true);
        } else {
            subscription.setNotify(false);
        }

        subscription.setProvider(dataSet.getProviderName());

        if (groupValid && view.isGroupSelected()) {
            subscription.setGroupName(view.getGroupName());
        }

        if (view.isNoExpirationDate()) {
            Calendar cal = TimeUtil.newGmtCalendar();
            subscription.setSubscriptionStart(cal.getTime());
            subscription.setSubscriptionEnd(null);
        } else {
            try {

                String startText = view.getStartText();
                String endText = view.getExpirationText();

                if (!startText.isEmpty()) {
                    Date startDate = DataDeliveryGUIUtils
                            .getSubscriptionFormat().parse(startText);
                    subscription.setSubscriptionStart(startDate);
                }
                if (!endText.isEmpty()) {
                    Date endDate = DataDeliveryGUIUtils.getSubscriptionFormat()
                            .parse(endText);
                    subscription.setSubscriptionEnd(endDate);
                }
            } catch (ParseException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        // active period
        if (!view.isAlwaysActive()) {
            try {
                String startText = view.getActiveStartText();
                String endText = view.getActiveEndText();

                if (!startText.isEmpty()) {
                    Date startPeriodDate = DataDeliveryGUIUtils
                            .getActiveFormat().parse(startText);
                    subscription.setActivePeriodStart(startPeriodDate);
                }
                if (!endText.isEmpty()) {
                    Date endPeriodDate = DataDeliveryGUIUtils.getActiveFormat()
                            .parse(endText);
                    subscription.setActivePeriodEnd(endPeriodDate);
                }

                subscription.setActive(true);
            } catch (ParseException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        } else {
            subscription.setActive(true);
            subscription.setActivePeriodStart(null);
            subscription.setActivePeriodEnd(null);
        }

        // priority
        int priorityInd = view.getPriority();
        subscription.setPriority(priorityInd);

        subscription.setOfficeID(LocalizationManager.getInstance()
                .getCurrentSite());

        subscription.setName(view.getSubscriptionName());

        subscription.setDescription(view.getSubscriptionDescription());

        subscription.getTime().setCycleTimes(view.getCycleTimes());

        subscription.setLatencyInMinutes(view.getLatencyValue());

        IUser user = UserController.getUserObject();
        ISubscriptionHandler handler = RegistryObjectHandlers
                .get(ISubscriptionHandler.class);

        String currentUser = LocalizationManager.getInstance().getCurrentUser();
        final String username = user.uniqueId().toString();

        if (this.create) {
            try {
                boolean autoApprove = DataDeliveryServices
                        .getPermissionsService()
                        .checkPermissionToChangeSubscription(user,
                                PENDING_APPROVAL_MESSAGE, subscription)
                        .isAuthorized();

                setSubscriptionId(subscription);

                if (autoApprove) {
                    final Shell jobShell = view.getShell();
                    Job job = new Job("Creating Subscription...") {
                        @Override
                        protected IStatus run(IProgressMonitor monitor) {
                            DataDeliveryGUIUtils.markBusyInUIThread(jobShell);
                            ISubscriptionServiceResult result = storeSubscription(
                                    subscription, username);
                            if (result.isAllowFurtherEditing()) {
                                return new Status(Status.CANCEL,
                                        CreateSubscriptionDlgPresenter.class
                                                .getName(),
                                        result.getMessageToDisplay());
                            } else {
                                return new Status(Status.OK,
                                        CreateSubscriptionDlgPresenter.class
                                                .getName(),
                                        result.getMessageToDisplay());
                            }
                        }
                    };
                    job.addJobChangeListener(new JobChangeAdapter() {
                        @Override
                        public void done(final IJobChangeEvent event) {
                            subscriptionNotificationService
                                    .sendCreatedSubscriptionNotification(
                                            subscription, username);

                            final IStatus status = event.getResult();
                            if (status.getMessage() != null) {
                                guiThreadTaskExecutor.runAsync(new Runnable() {
                                    @Override
                                    public void run() {
                                        if (!view.isDisposed()) {
                                            if (status.isOK()) {
                                                view.displayPopup(
                                                        CREATED_TITLE,
                                                        status.getMessage());
                                                view.setStatus(Status.OK);
                                                view.closeDlg();
                                            } else {
                                                view.setStatus(Status.CANCEL);
                                                view.displayPopup(
                                                        "Unable to Create Subscription",
                                                        status.getMessage());
                                            }
                                        }
                                    }
                                });
                            }
                            DataDeliveryGUIUtils
                                    .markNotBusyInUIThread(jobShell);
                        };
                    });
                    job.schedule();
                    return false;
                } else {
                    InitialPendingSubscription pendingSub = new InitialPendingSubscription(
                            subscription, currentUser);

                    try {
                        handler.store(pendingSub);

                        this.subscription = pendingSub;

                        subscriptionNotificationService
                                .sendCreatedPendingSubscriptionNotification(
                                        pendingSub, username);
                    } catch (RegistryHandlerException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to create pending subscription.", e);
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        } else {
            // Check for pending subscription, can only have one pending change
            PendingSubscription pendingSub = new PendingSubscription(
                    subscription, LocalizationManager.getInstance()
                            .getCurrentUser());
            pendingSub.setChangeReason(view.getChangeReason());

            // Create the registry ids
            setSubscriptionId(pendingSub);
            setSubscriptionId(subscription);

            IPendingSubscriptionHandler pendingSubHandler = RegistryObjectHandlers
                    .get(IPendingSubscriptionHandler.class);
            try {
                InitialPendingSubscription result = pendingSubHandler
                        .getBySubscription(subscription);
                if (result != null) {
                    String msg = "There is already an edited version of this subscription.\n\nPlease "
                            + "reconcile the pending subscription before making further edits.";
                    view.displayPopup("Pending", msg);
                    return false;
                }
            } catch (RegistryHandlerException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve pending subscriptions.", e1);
                return false;
            }

            // check to see if user is authorized to approve. If so then
            // auto-approve
            try {
                boolean autoApprove = DataDeliveryServices
                        .getPermissionsService()
                        .checkPermissionToChangeSubscription(user,
                                PENDING_APPROVAL_MESSAGE, subscription)
                        .isAuthorized();

                if (autoApprove) {
                    try {
                        final ISubscriptionServiceResult response = subscriptionService
                                .update(subscription,
                                        new CancelForceApplyAndIncreaseLatencyDisplayText(
                                                "update", view.getShell()));
                        if (response.hasMessageToDisplay()) {
                            view.displayPopup(UPDATED_TITLE,
                                    response.getMessageToDisplay());
                        }

                        // If there was a force apply prompt, and the user
                        // selects no, then we want to allow them to
                        // continue editing the subscription
                        if (response.isAllowFurtherEditing()) {
                            return false;
                        }

                        subscriptionNotificationService
                                .sendUpdatedSubscriptionNotification(
                                        subscription, username);

                    } catch (RegistryHandlerException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to update subscription.", e);
                    }
                } else {
                    setSubscriptionId(subscription);
                    try {
                        handler.update(pendingSub);

                        subscriptionNotificationService
                                .sendCreatedPendingSubscriptionForSubscriptionNotification(
                                        pendingSub, username);

                        final String msg = PENDING_APPROVAL_MESSAGE;
                        view.displayPopup("Subscription Pending", msg);
                    } catch (RegistryHandlerException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to create pending subscription.", e);
                    }
                }

            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        return true;
    }

    /**
     * Store the subscription for the user.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     * @return true if the dialog can be closed, false otherwise
     */
    @VisibleForTesting
    ISubscriptionServiceResult storeSubscription(Subscription subscription,
            String username) {
        ISubscriptionServiceResult result = null;
        try {
            result = subscriptionService.store(subscription,
                    new CancelForceApplyAndIncreaseLatencyDisplayText("create",
                            view.getShell()));
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to create subscription.", e);
        }
        return result;
    }

    /**
     * Send a notification that a subscription has been created.
     * 
     * @param subscription
     *            the subscription
     * @param username
     *            the username
     */
    @VisibleForTesting
    void sendSubscriptionNotification(Subscription subscription, String username) {
        subscriptionNotificationService.sendCreatedSubscriptionNotification(
                subscription, username);
    }

    /**
     * Validate the dialog's data
     * 
     * @return true if valid
     */
    boolean validate() {
        boolean valid = false;
        boolean datesValid = false;
        boolean activeDatesValid = false;
        boolean groupDeliverValid = false;
        boolean groupDurValid = false;
        boolean groupActiveValid = false;
        boolean latencyValid = false;

        // Validate the date entries
        datesValid = this.durationValidChk();
        activeDatesValid = this.activePeriodValidChk();
        int maxLatency = DataDeliveryUtils.getMaxLatency(subscription);
        latencyValid = DataDeliveryGUIUtils.latencyValidChk(
                view.getLatencyValue(), maxLatency);
        if (!latencyValid) {
            view.displayErrorPopup("Invalid Latency",
                    "Invalid latency value entered.\n\n"
                            + "Please enter a value in minutes between 0 and "
                            + maxLatency);
        }

        // Validate the subscription name if entered into text box
        String subscriptionName = view.getSubscriptionName();
        if (view.isCreate()) {

            // Is Subset Name entered
            if ((subscriptionName == null) || (subscriptionName.isEmpty())) {
                view.displayErrorPopup(
                        DataDeliveryGUIUtils.NAME_REQUIRED_TITLE,
                        DataDeliveryGUIUtils.NAME_REQUIRED_MESSAGE);
                return false;
            }

            if (DataDeliveryGUIUtils.INVALID_CHAR_PATTERN.matcher(
                    subscriptionName.trim()).find()) {
                view.displayErrorPopup(
                        DataDeliveryGUIUtils.INVALID_CHARS_TITLE,
                        DataDeliveryGUIUtils.INVALID_CHARS_MESSAGE);

                return false;
            }

            // Check for existing subscription
            ISubscriptionHandler handler = RegistryObjectHandlers
                    .get(ISubscriptionHandler.class);
            try {
                if (handler.getByName(subscriptionName) != null) {
                    String message = "A subscription with this name already exists.\n\nPlease enter a different subscription name.";
                    view.displayPopup("Duplicate Subscription", message);
                    view.selectAllSubscriptionName();
                    return false;
                }
            } catch (RegistryHandlerException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Unable to check for an existing subscription by name.",
                                e);
            }
        }

        if (activeDatesValid && datesValid && latencyValid) {
            valid = true;
        }

        // if group selected check if properties match saved group
        if (ComboBoxConf.NONE_AVAILABLE.equals(view.getGroupName()) && valid) {
            groupDefinition = GroupDefinitionManager.getGroup(view
                    .getGroupName());

            int deliverOption = groupDefinition.getOption();
            int formDeliver = view.getDeliverySelection();

            if (deliverOption == formDeliver) {
                groupDeliverValid = true;
            }

            // Compare the durations from the form to the group definition
            Date durStart = groupDefinition.getSubscriptionStart();
            Date durEnd = groupDefinition.getSubscriptionEnd();

            String formDurStartStr = view.getStartText();
            String formDurEndStr = view.getExpirationText();
            Date formDurStart = null;
            Date formDurEnd = null;

            if (formDurStartStr != null && formDurEndStr != null) {
                formDurStart = DataDeliveryGUIUtils
                        .getSelectedSubDate(formDurStartStr);
                formDurEnd = DataDeliveryGUIUtils
                        .getSelectedSubDate(formDurEndStr);
            }

            if (durStart != null && durEnd != null && formDurStart != null
                    && formDurEnd != null) {
                if (durStart.equals(formDurStart) && durEnd.equals(formDurEnd)) {
                    groupDurValid = true;
                }
            } else if (durStart == null && durEnd == null
                    && formDurStart == null && formDurEnd == null) {
                groupDurValid = true;
            }

            // Compare the active periods from the form to the group definition
            Date activeStart = groupDefinition.getActivePeriodStart();
            Date activeEnd = groupDefinition.getActivePeriodEnd();
            String activeStartStr = null;
            String activeEndStr = null;

            String formActiveStart = view.getActiveStartText();
            String formActiveEnd = view.getActiveEndText();

            if (!formActiveStart.isEmpty() && !formActiveStart.isEmpty()) {
                activeStartStr = DataDeliveryGUIUtils.getActiveFormat().format(
                        activeStart);
                activeEndStr = DataDeliveryGUIUtils.getActiveFormat().format(
                        activeEnd);
            }

            if (activeStartStr != null && activeEndStr != null
                    && !formActiveStart.isEmpty() && !formActiveEnd.isEmpty()) {
                if (activeStartStr.equals(formActiveStart)
                        && activeEndStr.equals(formActiveEnd)) {
                    groupActiveValid = true;
                }
            } else if (activeStartStr == null && activeEndStr == null
                    && formActiveStart.isEmpty() && formActiveEnd.isEmpty()) {
                groupActiveValid = true;
            }

        } else {
            groupDeliverValid = true;
            groupDurValid = true;
            groupActiveValid = true;
        }

        if (!groupDeliverValid || !groupDurValid || !groupActiveValid) {
            view.displayErrorPopup(
                    "Invalid Group Values",
                    "Values do not match selected group values.\n\n"
                            + "The group name will not be saved with the subscription.");
            valid = true;
        } else {
            groupValid = true;
        }

        // If valid is not set to true for any of the composites return
        if (!valid) {
            return false;
        }

        return true;
    }

    /**
     * Populate the view based on the group name selected.
     * 
     * @param groupName
     *            The selected group name
     */
    private void populate(String groupName) {
        GroupDefinition groupDefinition = GroupDefinitionManager
                .getGroup(groupName);

        if (groupDefinition == null) {
            return;
        }

        // Set deliverCombo
        int delOption = groupDefinition.getOption();
        view.setDeliverySelection(delOption);

        // Set duration info
        Date durStart = groupDefinition.getSubscriptionStart();
        Date durEnd = groupDefinition.getSubscriptionEnd();

        if (durStart != null || durEnd != null) {
            view.setStartDate(durStart);
            view.setExpirationDate(durEnd);
            view.setNoExpiration(false);

        } else {
            view.setNoExpiration(true);
            view.setSubscriptionDatesEnabled(false);
            view.setStartDateBtnEnabled(false);
            view.setEndDateBtnEnabled(false);
        }

        // Set the Active Period info
        Date activePeriodStartDate = groupDefinition.getActivePeriodStart();
        Date activePeriodEndDate = groupDefinition.getActivePeriodEnd();

        if (activePeriodStartDate != null || activePeriodEndDate != null) {
            final Calendar now = TimeUtil.newGmtCalendar();

            activePeriodStartDate = calculateNextOccurenceOfMonthAndDay(
                    activePeriodStartDate, now.get(Calendar.YEAR), now);

            Calendar activePeriodStartCal = TimeUtil.newGmtCalendar();
            activePeriodStartCal.setTime(activePeriodStartDate);

            activePeriodEndDate = calculateNextOccurenceOfMonthAndDay(
                    activePeriodEndDate,
                    activePeriodStartCal.get(Calendar.YEAR), now);

            view.setStartDate(activePeriodStartDate);
            view.setActiveEndDate(activePeriodEndDate);
            view.setAlwaysActive(false);
        } else {
            view.setAlwaysActive(true);
            view.setActiveDatesEnabled(false);
            view.setActiveStartDateBtnEnabled(false);
            view.setActiveEndDateBtnEnabled(false);
        }

        // Select the Area info
        if (groupDefinition.getEnvelope() != null) {
            // TODO add area info where applicable
        }
    }

    /**
     * Check if duration dates are valid
     * 
     * @return true if valid
     */
    public boolean durationValidChk() {
        boolean datesValid = false;
        boolean dateOrderValid = false;

        if (!view.isNoExpirationDate()) {
            boolean validateDur = DataDeliveryGUIUtils.validateDate(false,
                    view.getStartText());
            if (validateDur) {

                validateDur = DataDeliveryGUIUtils.validateDate(false,
                        view.getExpirationText());
                if (validateDur) {
                    datesValid = true;
                    dateOrderValid = DataDeliveryGUIUtils
                            .checkDateOrder(view.getStartText(),
                                    view.getExpirationText(), true);
                }
            }
        } else {
            datesValid = true;
            dateOrderValid = true;
        }

        // Display error message
        if (!datesValid) {
            view.displayErrorPopup("Invalid Date/Time",
                    "Invalid Subscription Duration values entered.\n\n"
                            + "Please use the Select Date button\n"
                            + "to select the date/time.");
        } else if (!dateOrderValid) {
            view.displayErrorPopup("Invalid Start/End Dates",
                    "Invalid Start or Expiration Duration Date entered.\n\n"
                            + "The expiration date is before the start date.");
        }

        return datesValid && dateOrderValid;
    }

    /**
     * Add the registry id to the subscription object.
     */
    private void setSubscriptionId(Subscription sub) {
        String id = RegistryUtil.getRegistryObjectKey(sub);
        sub.setId(id);
    }

    /**
     * Check if dates are valid.
     * 
     * @return true if dates are valid
     */
    public boolean activePeriodValidChk() {
        boolean activeDatesValid = false;

        if (!view.isAlwaysActive()) {
            boolean validateAct = DataDeliveryGUIUtils.validateDate(false,
                    view.getActiveStartText());
            if (validateAct) {
                validateAct = DataDeliveryGUIUtils.validateDate(false,
                        view.getActiveEndText());
                if (validateAct) {
                    activeDatesValid = true;
                }
            }
        } else {
            activeDatesValid = true;
        }

        // Display error message
        if (!activeDatesValid) {
            view.displayErrorPopup("Invalid Date",
                    "Invalid Subscription Active Period values entered.\n\n"
                            + "Please use the Select Date button\n"
                            + "to select the date.");
        }

        return activeDatesValid;
    }

    public int getStatus() {
        return view.getStatus();
    }
}
