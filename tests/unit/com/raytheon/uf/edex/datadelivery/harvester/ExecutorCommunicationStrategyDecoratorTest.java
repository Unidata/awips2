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
package com.raytheon.uf.edex.datadelivery.harvester;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;

import java.util.concurrent.ExecutorService;

import org.junit.Test;

import com.google.common.util.concurrent.MoreExecutors;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.CommunicationStrategy;
import com.raytheon.uf.edex.datadelivery.harvester.crawler.MainSequenceCommunicationStrategyDecorator;
import com.raytheon.uf.edex.datadelivery.retrieval.LinkStore;
import com.raytheon.uf.edex.datadelivery.retrieval.ProviderCollectionLinkStore;

/**
 * Test {@link ExecutorCommunicationStrategyDecorator}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2012 1022       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ExecutorCommunicationStrategyDecoratorTest {

    private final CommunicationStrategy decorated = mock(CommunicationStrategy.class);

    private final ExecutorService mockExecutor = mock(ExecutorService.class);

    private static final ExecutorService RUN_IN_SAME_THREAD_EXECUTOR = MoreExecutors
            .sameThreadExecutor();

    @Test
    public void testDelegatesToDecoratedObjectForSendException() {
        Exception e = new Exception();

        MainSequenceCommunicationStrategyDecorator decorator = new MainSequenceCommunicationStrategyDecorator(
                decorated, RUN_IN_SAME_THREAD_EXECUTOR);

        decorator.sendException(e);

        verify(decorated).sendException(e);
    }

    @Test
    public void testDelegatesToDecoratedObjectForSendLinkStore() {
        ProviderCollectionLinkStore linkStore = new ProviderCollectionLinkStore(
                "blah", "blah", new LinkStore());

        MainSequenceCommunicationStrategyDecorator decorator = new MainSequenceCommunicationStrategyDecorator(
                decorated, RUN_IN_SAME_THREAD_EXECUTOR);

        decorator.sendLinkStore(linkStore);

        verify(decorated).sendLinkStore(linkStore);
    }

    @Test
    public void testSubmitsRunnableToExecutorServiceForSendException() {
        MainSequenceCommunicationStrategyDecorator decorator = new MainSequenceCommunicationStrategyDecorator(
                decorated, mockExecutor);

        decorator.sendException(new Exception());

        verify(mockExecutor).execute(any(Runnable.class));
        verifyZeroInteractions(decorated);
    }

    @Test
    public void testSubmitsRunnableToExecutorServiceForSendLinkStore() {
        MainSequenceCommunicationStrategyDecorator decorator = new MainSequenceCommunicationStrategyDecorator(
                decorated, mockExecutor);

        decorator.sendLinkStore(new ProviderCollectionLinkStore("blah", "blah",
                new LinkStore()));

        verify(mockExecutor).execute(any(Runnable.class));
        verifyZeroInteractions(decorated);
    }
}
