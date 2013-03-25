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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

import com.raytheon.uf.edex.datadelivery.retrieval.db.IRetrievalDao;

/**
 * Test {@link RetrievalHandler}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 06, 2012 740        djohnson     Initial creation
 * Aug 09. 2012 1022       djohnson     Changes to RetrievalHandler.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * Jan 30, 2013 1543       djohnson     RetrievalTask now requires a Network.
 * Feb 05, 2013 1580       mpduff       EventBus refactor.
 * Feb 07, 2013 1543       djohnson     Move test to its proper test class, as per peer review comments.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RetrievalHandlerTest {

    private final ScheduledExecutorService executorService = mock(ScheduledExecutorService.class);

    private final IRetrievalDao mockDao = mock(IRetrievalDao.class);

    private final RetrievalTask retrievalTask = mock(RetrievalTask.class);

    private final SubscriptionNotifyTask subNotifyTask = mock(SubscriptionNotifyTask.class);

    private final IRetrievalResponseCompleter retrievalCompleter = mock(IRetrievalResponseCompleter.class);

    private final RetrievalHandler handler = new RetrievalHandler(
            executorService, mockDao, Arrays.asList(retrievalTask),
            subNotifyTask);

    @Test
    public void testAllRunningRetrievalsAreResetToPendingOnConstruction() {
        verify(mockDao).resetRunningRetrievalsToPending();
    }

    @Test
    public void testOnNotifyOfSubscriptionsARetrievalTaskIsExecuted() {
        handler.notify(Collections.<String> emptyList());

        verify(executorService).execute(retrievalTask);
    }

    @Test
    public void testRetrievalTaskIsScheduledEveryFiveMinutesWithInitialDelayOfOneMinute() {
        verify(executorService).scheduleWithFixedDelay(retrievalTask, 1, 5,
                TimeUnit.MINUTES);
    }

    @Test
    public void testSubscriptionNotifyTaskIsScheduledEveryMinuteWithInitialDelayOfOneMinute() {
        verify(executorService).scheduleWithFixedDelay(subNotifyTask, 1, 1,
                TimeUnit.MINUTES);
    }
}
