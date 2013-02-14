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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.Collections;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlersUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datadelivery.subscription.IPermissionsService.IAuthorizedPermissionResponse;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;

/**
 * Test 
 * {@link SubscriptionService#update(java.util.List, com.raytheon.viz.ui.presenter.IDisplay).
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2012 1286       djohnson     Initial creation
 *
 * </pre>
 *
 * @author djohnson
 * @version 1.0	
 */
public class SubscriptionServiceMassUpdateTest extends
        AbstractSubscriptionServiceTest {

    private final IAuthorizedPermissionResponse authorizedPermissionsResponse = mock(IAuthorizedPermissionResponse.class);

    @Before
    public void setUpPermissionsResponse() throws VizException {
        userHasAllPermissions();
    }

    @Test
    public void testUpdateWithPendingCheckCancelingForceApplyDoesNotUpdateSubscriptions()
            throws RegistryHandlerException {
        RegistryObjectHandlersUtil.initMocks();

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsCancel();

        service.updateWithPendingCheck(subs, mockPromptDisplayText);

        verifyZeroInteractions(DataDeliveryHandlers.getSubscriptionHandler());
    }

    @Test
    public void testUpdateWithPendingCheckForceApplyUpdatesSubscriptions()
            throws RegistryHandlerException {
        RegistryObjectHandlersUtil.initMocks();

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsForceApply();

        service.updateWithPendingCheck(subs, mockPromptDisplayText);

        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        for (Subscription sub : subs) {
            verify(subscriptionHandler).update(sub);
        }
    }

    @Test
    public void testUpdateWithPendingCheckNotifiesOfSubscriptionsAlreadyPending()
            throws RegistryHandlerException {
        RegistryObjectHandlersUtil.initMocks();

        returnZeroSubscriptionNamesWhenProposeScheduleCalled();

        subscriptionAlreadyHasPendingChanges(sub1);

        final ISubscriptionServiceResult response = service
                .updateWithPendingCheck(subs, mockPromptDisplayText);

        final String expectedMessage = "The subscriptions have been updated.\n\nThe following subscriptions already had pending changes and were not modified:\n"
                + sub1.getName();

        assertThat(response.getMessageToDisplay(), is(equalTo(expectedMessage)));
    }

    @Test
    public void testUpdateWithPendingCheckNotifiesOfSubscriptionsUnableToBeUpdated()
            throws RegistryHandlerException, VizException {
        RegistryObjectHandlersUtil.initMocks();

        returnZeroSubscriptionNamesWhenProposeScheduleCalled();

        subscriptionThrowsErrorOnUpdate(sub2);

        final ISubscriptionServiceResult response = service
                .updateWithPendingCheck(subs, mockPromptDisplayText);

        final String expectedMessage = "The subscriptions have been updated.\n\nThe following subscriptions were unable to be modified:\n"
                + sub2.getName();

        assertThat(response.getMessageToDisplay(), is(equalTo(expectedMessage)));
    }

    @Test
    public void testUpdateWithPendingCheckNotifiesOfPendingSubscriptionsCreated()
            throws RegistryHandlerException, VizException {
        RegistryObjectHandlersUtil.initMocks();

        returnZeroSubscriptionNamesWhenProposeScheduleCalled();

        subscriptionCantBeChangedByUser(sub2);

        final ISubscriptionServiceResult response = service
                .updateWithPendingCheck(subs, mockPromptDisplayText);

        final String expectedMessage = "The subscriptions have been updated.\n\nThe following subscriptions have pending changes awaiting approval:\n"
                + sub2.getName();

        assertThat(response.getMessageToDisplay(), is(equalTo(expectedMessage)));
    }

    /**
     * When the user requests to change the specified subscription, the registry
     * throws an exception.
     * 
     * @param subscription
     *            the subscription
     * @throws RegistryHandlerException
     */
    private void subscriptionThrowsErrorOnUpdate(Subscription sub2)
            throws RegistryHandlerException {
        doThrow(new RegistryHandlerException("thrown on purpose")).when(
                DataDeliveryHandlers.getSubscriptionHandler()).update(sub2);
    }

    /**
     * When the user requests to change the specified subscription, they will
     * not have permission.
     * 
     * @param subscription
     *            the subscription
     * @throws VizException
     */
    private void subscriptionCantBeChangedByUser(Subscription subscription)
            throws VizException {
        IAuthorizedPermissionResponse noPermission = mock(IAuthorizedPermissionResponse.class);
        when(
                permissionsService
                        .checkPermissionToChangeSubscription(any(IUser.class),
                                any(String.class), same(subscription)))
                .thenReturn(noPermission);
        when(noPermission.isAuthorized()).thenReturn(false);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    ISubscriptionServiceResult performServiceInteraction()
            throws RegistryHandlerException {
        return service.update(subs, mockPromptDisplayText);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void returnTwoSubscriptionNamesWhenProposeScheduleCalled() {
        when(mockBandwidthService.proposeSchedule(subs)).thenReturn(
                mockProposeScheduleResponse);
        when(mockProposeScheduleResponse.getUnscheduledSubscriptions())
                .thenReturn(subNameResults);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void verifyProposeScheduleCalled() {
        verify(mockBandwidthService).proposeSchedule(subs);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void verifyScheduleCalled() {
        verify(mockBandwidthService).schedule(subs);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void returnZeroSubscriptionNamesWhenProposeScheduleCalled() {
        when(mockProposeScheduleResponse.getUnscheduledSubscriptions())
                .thenReturn(Collections.<String> emptySet());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    String getExpectedForceApplyMessage() {
        return "The following subscriptions would not fully schedule with the bandwidth management system if this action were performed:\n"
                + sub1Name + "\n" + sub2Name + "\n\nWhat would you like to do?";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    String getSuccessfulServiceInteractionMessage() {
        return "The subscriptions have been updated.";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void returnSub2NameWhenScheduleCalled() {
        when(mockBandwidthService.schedule(subs)).thenReturn(
                CollectionUtil.asSet(sub2Name));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void returnZeroSubscriptionNamesWhenScheduleCalled() {
        when(mockBandwidthService.schedule(subs)).thenReturn(
                Collections.<String> emptySet());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void verifyServiceInteractionWithRegistryHandler()
            throws RegistryHandlerException {
        for (Subscription sub : subs) {
            verify(DataDeliveryHandlers.getSubscriptionHandler()).update(sub);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    String getExpectedSuccessfulForceApplyMessageWithSub2Unscheduled() {
        return "The subscriptions have been updated.\n\nThe following subscriptions did not fully schedule with the bandwidth management system:\n"
                + sub2Name;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void whenRegistryInteractionOccursThrowException()
            throws RegistryHandlerException {
        doThrow(new RegistryHandlerException("thrown on purpose")).when(
                DataDeliveryHandlers.getSubscriptionHandler()).update(
                subs.get(0));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Subscription getExpectedDisplayForceApplyPromptSubscription() {
        // This test does expect a null argument to be passed, not an error
        return null;
    }

    private void userHasAllPermissions() throws VizException {
        when(
                permissionsService.checkPermission(any(IUser.class),
                        anyString(), any(DataDeliveryPermission.class)))
                .thenReturn(authorizedPermissionsResponse);
        when(
                permissionsService.checkPermissions(any(IUser.class),
                        anyString(), any(DataDeliveryPermission.class)))
                .thenReturn(authorizedPermissionsResponse);
        when(
                permissionsService.checkPermissionToChangeSubscription(
                        any(IUser.class), anyString(), any(Subscription.class)))
                .thenReturn(authorizedPermissionsResponse);
        when(authorizedPermissionsResponse.isAuthorized()).thenReturn(true);
    }

    /**
     * Sets a subscription to already have pending changes.
     * 
     * @param subscription
     * @throws RegistryHandlerException
     */
    private void subscriptionAlreadyHasPendingChanges(Subscription subscription)
            throws RegistryHandlerException {
        when(
                DataDeliveryHandlers.getPendingSubscriptionHandler()
                        .getBySubscription(subscription)).thenReturn(
                new InitialPendingSubscription());
    }
}
