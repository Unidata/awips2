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
package com.raytheon.uf.edex.datadelivery.service.verify;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.DataDeliveryRegistryObjectTypes;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.registry.event.InsertRegistryEvent;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlersUtil;
import com.raytheon.uf.edex.datadelivery.service.verify.SubscriptionIntegrityVerifier.IVerificationAction;
import com.raytheon.uf.edex.datadelivery.service.verify.SubscriptionIntegrityVerifier.IVerificationResponse;
import com.raytheon.uf.edex.datadelivery.service.verify.SubscriptionIntegrityVerifier.IVerificationStrategy;
import com.raytheon.uf.edex.event.EventBus;
import com.raytheon.uf.edex.event.EventBusTest;

/**
 * Test {@link SubscriptionIntegrityVerifier}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 07, 2012 1104       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionIntegrityVerifierTest {

    private static final String FAILED_VERIFICATION_MESSAGE = "Failed verification";

    private final OpenDapGriddedDataSet dataSet = new OpenDapGriddedDataSet();

    private final Subscription sub1 = SubscriptionFixture.INSTANCE.get(1);

    private final Subscription sub2 = SubscriptionFixture.INSTANCE.get(2);

    private final List<Subscription> subscriptions = Arrays.asList(sub1, sub2);

    // Initializer to setup the mock data delivery handlers
    {
        RegistryObjectHandlersUtil.initMocks();
    }

    private final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
            .getSubscriptionHandler();

    private final IVerificationStrategy verificationStrategy = mock(IVerificationStrategy.class);

    private final IVerificationAction successfulVerificationAction = mock(IVerificationAction.class);

    private final IVerificationAction failedVerificationAction = mock(IVerificationAction.class);

    private final SubscriptionIntegrityVerifier verifier = SubscriptionIntegrityVerifier
            .newInstance(verificationStrategy,
                    Arrays.asList(successfulVerificationAction),
                    Arrays.asList(failedVerificationAction));

    private final InsertRegistryEvent dataSetUpdateEvent = new InsertRegistryEvent(
            "someId", "someLid", DataDeliveryRegistryObjectTypes.DATASET);

    private final EventBus eventBus = EventBus.getInstance();
    
    @BeforeClass
    public static void classSetUp() {
        EventBusTest.initSynchronous();
    }

    @Before
    public void setUp() throws RegistryHandlerException {
        whenRegistryEventIdLookedUpReturnDataSet();
    }

    @After
    public void tearDown() {
        eventBus.unregister(verifier);
    }

    @Test
    public void testSubscriptionsForDataSetAreRetrieved()
            throws RegistryHandlerException {

        eventBus.publish(dataSetUpdateEvent);

        verifySubscriptionsAreRetrievedForDataSet();
    }

    @Test
    public void testSubscriptionsAreVerifiedAgainstDataSet()
            throws RegistryHandlerException {

        whenSubscriptionsAreRetrievedForDataSetReturnTwo();
        whenSubscriptionIsVerifiedItSucceeds(sub1);
        whenSubscriptionIsVerifiedItSucceeds(sub2);

        eventBus.publish(dataSetUpdateEvent);

        verifyEachSubscriptionIsVerifiedAgainstDataSet();
    }

    @Test
    public void testSubscriptionsAlreadyInvalidAreNotVerifiedAgainstDataSet()
            throws RegistryHandlerException {

        whenSubscriptionsAreRetrievedForDataSetReturnTwo();
        whenSubscriptionIsVerifiedItSucceeds(sub1);
        // sub2 is already invalid, should not be verified
        sub2.setValid(false);

        eventBus.publish(dataSetUpdateEvent);

        verifySubscriptionIsNotVerifiedAgainstDataSet(sub2);
    }

    @Test
    public void testFailedVerificationActionsAreInvokedForSubscriptionsFailingVerification()
            throws RegistryHandlerException {

        whenSubscriptionsAreRetrievedForDataSetReturnTwo();
        whenSubscriptionIsVerifiedItSucceeds(sub1);
        whenSubscriptionIsVerifiedItFails(sub2);

        eventBus.publish(dataSetUpdateEvent);

        verifyFailedVerificationActionInvoked(sub2);
    }

    @Test
    public void testFailedVerificationActionsAreNotInvokedForSubscriptionsPassingVerification()
            throws RegistryHandlerException {

        whenSubscriptionsAreRetrievedForDataSetReturnTwo();
        whenSubscriptionIsVerifiedItSucceeds(sub1);
        whenSubscriptionIsVerifiedItFails(sub2);

        eventBus.publish(dataSetUpdateEvent);

        verifyFailedVerificationActionNotInvoked(sub1);
    }

    @Test
    public void testSuccessfulVerificationActionsAreNotInvokedForSubscriptionsFailingVerification()
            throws RegistryHandlerException {

        whenSubscriptionsAreRetrievedForDataSetReturnTwo();
        whenSubscriptionIsVerifiedItSucceeds(sub1);
        whenSubscriptionIsVerifiedItFails(sub2);

        eventBus.publish(dataSetUpdateEvent);

        verifySuccessfulVerificationActionNotInvoked(sub2);
    }

    @Test
    public void testSuccessfulVerificationActionsAreInvokedForSubscriptionsPassingVerification()
            throws RegistryHandlerException {

        whenSubscriptionsAreRetrievedForDataSetReturnTwo();
        whenSubscriptionIsVerifiedItSucceeds(sub1);
        whenSubscriptionIsVerifiedItFails(sub2);

        eventBus.publish(dataSetUpdateEvent);

        verifySuccessfulVerificationActionInvoked(sub1);
    }

    @Test
    public void testUnableToRetrieveSubscriptionsDoesNotThrowException()
            throws RegistryHandlerException {

        whenSubscriptionsAreRetrievedForDataSetThrowException();

        eventBus.publish(dataSetUpdateEvent);

        verifySuccessfulVerificationActionNotInvoked(sub1);
        verifySuccessfulVerificationActionNotInvoked(sub2);
        verifyFailedVerificationActionNotInvoked(sub1);
        verifyFailedVerificationActionNotInvoked(sub2);
    }

    /**
     * Returns the DataSet instance when a query is looked up via the registry
     * handler.
     * 
     * @throws RegistryHandlerException
     */
    private void whenRegistryEventIdLookedUpReturnDataSet()
            throws RegistryHandlerException {
        when(
                DataDeliveryHandlers.getDataSetHandler().getById(
                        dataSetUpdateEvent.getId())).thenReturn(dataSet);

    }

    private void verifyFailedVerificationActionInvoked(Subscription subscription) {
        verify(failedVerificationAction).verificationPerformed(
                same(subscription), any(IVerificationResponse.class));
    }

    private void verifyFailedVerificationActionNotInvoked(
            Subscription subscription) {
        verify(failedVerificationAction, never()).verificationPerformed(
                same(subscription), any(IVerificationResponse.class));
    }

    private void verifySuccessfulVerificationActionInvoked(
            Subscription subscription) {
        verify(successfulVerificationAction).verificationPerformed(
                same(subscription), any(IVerificationResponse.class));
    }

    private void verifySuccessfulVerificationActionNotInvoked(
            Subscription subscription) {
        verify(successfulVerificationAction, never()).verificationPerformed(
                same(subscription), any(IVerificationResponse.class));
    }

    private void verifySubscriptionsAreRetrievedForDataSet()
            throws RegistryHandlerException {
        verify(DataDeliveryHandlers.getSubscriptionHandler())
                .getActiveByDataSetAndProvider(dataSet.getDataSetName(),
                        dataSet.getProviderName());
    }

    private void whenSubscriptionsAreRetrievedForDataSetReturnTwo()
            throws RegistryHandlerException {
        when(
                subscriptionHandler.getActiveByDataSetAndProvider(anyString(),
                        anyString())).thenReturn(subscriptions);
    }

    private void whenSubscriptionsAreRetrievedForDataSetThrowException()
            throws RegistryHandlerException {
        when(
                subscriptionHandler.getActiveByDataSetAndProvider(anyString(),
                        anyString())).thenThrow(
                new RegistryHandlerException("thrown on purpose"));
    }

    private void verifyEachSubscriptionIsVerifiedAgainstDataSet() {
        for (Subscription subscription : subscriptions) {
            verify(verificationStrategy).verify(dataSet, subscription);
        }
    }

    private void verifySubscriptionIsNotVerifiedAgainstDataSet(
            Subscription subscription) {
        verify(verificationStrategy, never()).verify(dataSet, subscription);
    }

    private void whenSubscriptionIsVerifiedItFails(Subscription subscription) {
        returnVerificationStatusWhenSubscriptionIsVerified(subscription,
                FAILED_VERIFICATION_MESSAGE);
    }

    private void whenSubscriptionIsVerifiedItSucceeds(Subscription subscription) {
        returnVerificationStatusWhenSubscriptionIsVerified(subscription, null);
    }

    private void returnVerificationStatusWhenSubscriptionIsVerified(
            Subscription subscription, String notificationMessage) {
        IVerificationResponse response = mock(IVerificationResponse.class);
        when(response.hasFailedVerification()).thenReturn(
                notificationMessage != null);
        when(response.getNotificationMessage()).thenReturn(notificationMessage);
        when(
                verificationStrategy.verify(any(DataSet.class),
                        same(subscription))).thenReturn(response);
    }
}
