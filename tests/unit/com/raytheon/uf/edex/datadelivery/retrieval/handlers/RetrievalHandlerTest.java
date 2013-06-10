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

import com.raytheon.uf.common.time.domain.Durations;
import com.raytheon.uf.common.time.domain.api.IDuration;
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
 * Mar 04, 2013 1647       djohnson     RetrievalTasks are now scheduled via constructor parameter.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RetrievalHandlerTest {

    private static final IDuration RETRIEVAL_TASK_FREQUENCY = Durations.of(5,
            TimeUnit.MINUTES);

    private static final IDuration SUBNOTIFY_TASK_FREQUENCY = Durations.of(1,
            TimeUnit.MINUTES);

    private final ScheduledExecutorService executorService = mock(ScheduledExecutorService.class);

    private final IRetrievalDao mockDao = mock(IRetrievalDao.class);

    private final RetrievalTask retrievalTask = mock(RetrievalTask.class);

    private final SubscriptionNotifyTask subNotifyTask = mock(SubscriptionNotifyTask.class);

    private final RetrievalHandler handler = new RetrievalHandler(
            executorService, mockDao, Arrays.asList(retrievalTask),
            subNotifyTask, RETRIEVAL_TASK_FREQUENCY, SUBNOTIFY_TASK_FREQUENCY);

    @Test
    public void testAllRunningRetrievalsAreResetToPendingOnConstruction() {
        handler.executeAfterRegistryInit();
        verify(mockDao).resetRunningRetrievalsToPending();
    }

    @Test
    public void testOnNotifyOfSubscriptionsARetrievalTaskIsExecuted() {
        handler.notify(Collections.<String> emptyList());

        verify(executorService).execute(retrievalTask);
    }

    @Test
    public void testRetrievalTaskIsScheduledPerConstructorParameter() {
        handler.executeAfterRegistryInit();
        verify(executorService).scheduleWithFixedDelay(retrievalTask, 30000,
                RETRIEVAL_TASK_FREQUENCY.getMillis(), TimeUnit.MILLISECONDS);
    }

    @Test
    public void testSubscriptionNotifyTaskIsScheduledPerConstructorParameter() {
        handler.executeAfterRegistryInit();
        verify(executorService).scheduleWithFixedDelay(subNotifyTask, 30000,
                SUBNOTIFY_TASK_FREQUENCY.getMillis(), TimeUnit.MILLISECONDS);
    }
}
