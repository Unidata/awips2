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
package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Test;

import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalManagerNotifyEvent;

/**
 * Test {@link RetrievalManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 14, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RetrievalManagerTest {

    private static final long EVENT_ID = 1L;

    private final IBandwidthDao dao = mock(IBandwidthDao.class);

    private final RetrievalManagerNotifyEvent event = new RetrievalManagerNotifyEvent();
    {
        event.setId(Long.valueOf(EVENT_ID).toString());
    }

    @Test
    public void noSubscriptionRetrievalByEventIdDoesNotThrowException() {
        when(dao.getSubscriptionRetrieval(EVENT_ID)).thenReturn(null);

        RetrievalManager retrievalManager = new RetrievalManager(dao,
                new Object());

        retrievalManager.retrievalCompleted(event);
    }

}
