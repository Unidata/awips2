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

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.util.CollectionUtil;
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
        when(mockBandwidthService.proposeSchedule(subs)).thenReturn(mockProposeScheduleResponse);
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
}
