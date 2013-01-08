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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthService;
import com.raytheon.uf.common.datadelivery.bandwidth.IProposeScheduleResponse;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlersUtil;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.ForceApplyPromptResponse;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IDisplayForceApplyPrompt;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IForceApplyPromptDisplayText;

/**
 * Test {@link SubscriptionService}. This test uses template methods to verify
 * that all interactions follow the same patternw when performing stores and
 * updates.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 7, 2012  1286       djohnson     Initial creation
 * Nov 20, 2012 1286       djohnson     Rewrite to support proposing subscription stores/updates and force applying.
 * Jan 02, 2012 1345       djohnson     Fix broken tests from using VizApp to move work off the UI thread.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public abstract class AbstractSubscriptionServiceTest {

    protected static final int REQUIRED_LATENCY = 2;

    final Subscription sub1 = SubscriptionFixture.INSTANCE.get(1);

    final Subscription sub2 = SubscriptionFixture.INSTANCE.get(2);

    final List<Subscription> subs = Arrays.asList(sub1, sub2);

    final AdhocSubscription adhoc = AdhocSubscriptionFixture.INSTANCE.get();

    final String sub1Name = sub1.getName();

    final String sub2Name = sub2.getName();

    final Set<String> subNameResults = new LinkedHashSet<String>();
    {
        subNameResults.add(sub1Name);
        subNameResults.add(sub2Name);
    }

    final IBandwidthService mockBandwidthService = mock(IBandwidthService.class);

    final IDisplayForceApplyPrompt mockDisplay = mock(IDisplayForceApplyPrompt.class);

    final SubscriptionService service = new SubscriptionService() {
        /**
         * {@inheritDoc}
         */
        @Override
        IBandwidthService getBandwidthService() {
            return mockBandwidthService;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        IDisplayForceApplyPrompt getForceApplyPromptDisplay() {
            return mockDisplay;
        }
    };

    final IProposeScheduleResponse mockProposeScheduleResponse = mock(IProposeScheduleResponse.class);

    final IForceApplyPromptDisplayText mockPromptDisplayText = mock(IForceApplyPromptDisplayText.class);

    @Before
    public void setUp() {
        RegistryObjectHandlersUtil.initMemory();

        when(
                mockBandwidthService
                        .proposeSchedule(anyListOf(Subscription.class)))
                .thenReturn(mockProposeScheduleResponse);
        when(mockBandwidthService.proposeSchedule(any(Subscription.class)))
                .thenReturn(mockProposeScheduleResponse);

        setupForceApplyPromptDisplayTextValues();
    }

    /**
     * Setup the expected prompt display values.
     */
    private void setupForceApplyPromptDisplayTextValues() {
        when(
                mockPromptDisplayText.getOptionDisplayText(
                        ForceApplyPromptResponse.CANCEL, REQUIRED_LATENCY,
                        sub1, subNameResults)).thenReturn("cancel");
        when(
                mockPromptDisplayText.getOptionDisplayText(
                        ForceApplyPromptResponse.FORCE_APPLY, REQUIRED_LATENCY,
                        sub1, subNameResults)).thenReturn("force apply");
        when(
                mockPromptDisplayText.getOptionDisplayText(
                        ForceApplyPromptResponse.INCREASE_LATENCY,
                        REQUIRED_LATENCY, sub1, subNameResults)).thenReturn(
                "increase latency");
    }

    @Test
    public void testFailedProposeScheduleDoesNotInvokeRegistryHandler()
            throws RegistryHandlerException {
        RegistryObjectHandlersUtil.initMocks();

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsCancel();

        performServiceInteraction();

        verifyZeroInteractions(DataDeliveryHandlers.getSubscriptionHandler());
    }

    @Test
    public void testServiceInteractionInvokesProposeSchedule()
            throws RegistryHandlerException {
        performServiceInteraction();

        verifyProposeScheduleCalled();
    }

    @Test
    public void testSuccessfulServiceInterfactionReturnsSuccessMessage()
            throws RegistryHandlerException {
        returnZeroSubscriptionNamesWhenProposeScheduleCalled();

        String message = performServiceInteraction().getMessageToDisplay();

        assertEquals("Incorrect response message returned!",
                getSuccessfulServiceInteractionMessage(), message);
    }

    @Test
    public void testFailedProposeSchedulePromptsUserForForceApply()
            throws RegistryHandlerException {
        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        returnRequiredLatencyWhenProposeScheduleCalled();

        whenForceApplyPromptedUserSelectsCancel();

        performServiceInteraction();

        final String expected = getExpectedForceApplyMessage();

        verify(mockDisplay).displayForceApplyPrompt(service.TITLE, expected,
                REQUIRED_LATENCY, mockPromptDisplayText,
                getExpectedDisplayForceApplyPromptSubscription(),
                subNameResults);
    }

    @Test
    public void testForceSchedulingCallsSchedule()
            throws RegistryHandlerException {
        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsForceApply();

        performServiceInteraction();

        verifyScheduleCalled();
    }

    @Test
    public void testForceSchedulingUpdatesUnscheduledSubscriptionsStatus()
            throws RegistryHandlerException {
        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        subscriptionHandler.store(sub2);

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsForceApply();
        returnSub2NameWhenScheduleCalled();

        performServiceInteraction();

        assertTrue("Sub2 should have been unscheduled", subscriptionHandler
                .getByName(sub2Name).isUnscheduled());
    }

    @Test
    public void testForceSchedulingInvokesRegistryHandler()
            throws RegistryHandlerException {
        RegistryObjectHandlersUtil.initMocks();

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsForceApply();

        performServiceInteraction();

        verifyServiceInteractionWithRegistryHandler();
    }

    @Test
    public void testForceSchedulingReturnsSuccessMessage()
            throws RegistryHandlerException {
        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        subscriptionHandler.store(sub2);

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsForceApply();
        returnSub2NameWhenScheduleCalled();

        String expectedMessage = getExpectedSuccessfulForceApplyMessageWithSub2Unscheduled();
        String actualMessage = performServiceInteraction()
                .getMessageToDisplay();

        assertEquals("Incorrect message returned", expectedMessage,
                actualMessage);
    }

    @Test
    public void testForceSchedulingDisallowsFurtherEdits()
            throws RegistryHandlerException {
        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        subscriptionHandler.store(sub2);

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsForceApply();
        returnSub2NameWhenScheduleCalled();

        ISubscriptionServiceResult result = performServiceInteraction();
        assertFalse("No further edits should be requested",
                result.isAllowFurtherEditing());
    }

    @Test
    public void testCancelingDoesNotCallSchedule()
            throws RegistryHandlerException {
        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsCancel();

        performServiceInteraction();

        verifyScheduleNeverCalled();
    }

    @Test
    public void testCancelingDoesNotInvokeRegistryHandler()
            throws RegistryHandlerException {
        RegistryObjectHandlersUtil.initMocks();

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsCancel();

        performServiceInteraction();

        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        verifyZeroInteractions(subscriptionHandler);
    }

    @Test
    public void testCancelingAllowsFurtherEditing()
            throws RegistryHandlerException {
        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsCancel();

        final ISubscriptionServiceResult result = performServiceInteraction();

        assertTrue("The service should request that further edits be made",
                result.isAllowFurtherEditing());
    }

    @Test
    public void testFailedRegistryActionInvokesBandwidthManagerReinitialize()
            throws RegistryHandlerException {
        RegistryObjectHandlersUtil.initMocks();

        returnZeroSubscriptionNamesWhenProposeScheduleCalled();
        whenRegistryInteractionOccursThrowException();

        try {
            performServiceInteraction();
            fail("An exception should have been thrown, why wasn't it?");
        } catch (RegistryHandlerException e) {
            // Expected path
        }

        verifyBandwidthManagerReinitializeInvoked();
    }

    /**
     * Verifies that the bandwidth manager was requested to reinitialize itself
     * from the persistent store.
     */
    private void verifyBandwidthManagerReinitializeInvoked() {
        verify(mockBandwidthService).reinitialize();
    }

    /**
     * When the force apply prompt is displayed, the user selects no.
     */
    private void whenForceApplyPromptedUserSelectsCancel() {
        when(mockDisplay.getForceApplyPromptResponse()).thenReturn(
                ForceApplyPromptResponse.CANCEL);
    }

    /**
     * When the force apply prompt is displayed, the user selects yes.
     */
    private void whenForceApplyPromptedUserSelectsForceApply() {
        when(mockDisplay.getForceApplyPromptResponse()).thenReturn(
                ForceApplyPromptResponse.FORCE_APPLY);
    }

    /**
     * Verify schedule is never called.
     */
    private void verifyScheduleNeverCalled() {
        verify(mockBandwidthService, never()).schedule(
                anyListOf(Subscription.class));
        verify(mockBandwidthService, never()).schedule(any(Subscription.class));
    }

    /**
     * Returns the value of {@link #REQUIRED_LATENCY} as the required latency
     * when propose schedule is called.
     */
    void returnRequiredLatencyWhenProposeScheduleCalled() {
        when(mockProposeScheduleResponse.getRequiredLatency()).thenReturn(
                REQUIRED_LATENCY);
    }

    /**
     * Perform the service interaction, e.g. store, update, etc.
     * 
     * @return the response
     * @throws RegistryHandlerException
     */
    abstract ISubscriptionServiceResult performServiceInteraction()
            throws RegistryHandlerException;

    /**
     * Throws an exception when the registry interaction occurs.
     */
    abstract void whenRegistryInteractionOccursThrowException()
            throws RegistryHandlerException;

    /**
     * When propose schedule is called for this interaction type, then the
     * bandwidth service will return the set of names of sub1 and sub2 as
     * unscheduled.
     */
    abstract void returnTwoSubscriptionNamesWhenProposeScheduleCalled();

    /**
     * Verify propose schedule was called with the expected arguments.
     */
    abstract void verifyProposeScheduleCalled();

    /**
     * Verify schedule was called with the expected arguments
     */
    abstract void verifyScheduleCalled();

    /**
     * When a propose schedule is called with the service interaction arguments,
     * then the bandwidth service will return an empty set.
     */
    abstract void returnZeroSubscriptionNamesWhenProposeScheduleCalled();

    /**
     * Get the expected message of a force apply.
     * 
     * @return the expected message
     */
    abstract String getExpectedForceApplyMessage();

    /**
     * Get the expected message of a successful service interaction.
     * 
     * @return the expected message
     */
    abstract String getSuccessfulServiceInteractionMessage();

    /**
     * When schedule is called for the service interaction, then the bandwidth
     * service will return the name of sub2 as unscheduled.
     */
    abstract void returnSub2NameWhenScheduleCalled();

    /**
     * When schedule is called for the service interaction, then the bandwidth
     * service will return zero subscription names.
     */
    abstract void returnZeroSubscriptionNamesWhenScheduleCalled();

    /**
     * Verify the service interaction caused the expected invocation on the
     * registry handler.
     * 
     * @throws RegistryHandlerException
     */
    abstract void verifyServiceInteractionWithRegistryHandler()
            throws RegistryHandlerException;

    /**
     * Get the expected message from a successful force apply of the service
     * interaction.
     * 
     * @return the message
     */
    abstract String getExpectedSuccessfulForceApplyMessageWithSub2Unscheduled();

    /**
     * Return the expected subscription argument to the display force apply
     * prompt.
     * 
     * @return the subscription argument
     */
    abstract Subscription getExpectedDisplayForceApplyPromptSubscription();
}
