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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.handlers.SubscriptionNotifyTask.SubscriptionDelay;

/**
 * Test {@link SubscriptionNotifyTask}.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 9, 2012  1022       djohnson     Initial creation
 *
 * </pre>
 *
 * @author djohnson
 * @version 1.0	
 */

public class SubscriptionNotifyTaskTest {

    private final RetrievalRequestRecord record = new RetrievalRequestRecord(
            "subName", 0, -1L);

    private final long currentTime = System.currentTimeMillis();

    private final SubscriptionDelay delayed = SubscriptionNotifyTask
            .createSubscriptionDelay(record, currentTime);


    @Test
    public void testCreateSubscriptionDelaysSetsAllowTimeToElevenSecondsPastSpecifiedTime() {

        assertEquals(
                "Expected the delayedUntilMillis to be 11 seconds after the start time!",
                currentTime + 11000, delayed.delayedUntilMillis);
    }
    
    @Test
    public void testGetDelayWillReturnMillisRemainingUntilSubscriptionShouldBeAllowed() {
        long currentTimeMillis = System.currentTimeMillis();
        long millisRemaining = delayed.getRemainingDelay(currentTimeMillis);

        assertEquals(
                "Should have returned the remaining millis until the delay time has passed!",
                delayed.delayedUntilMillis - currentTimeMillis, millisRemaining);
    }

    @Test
    public void testGetDelayWillReturnZeroWhenDelayTimeHasElapsed() {
        long twelveSecondsPassed = currentTime + 12000;
        long millisRemaining = delayed.getRemainingDelay(twelveSecondsPassed);

        assertEquals("Should have returned 0 since the delay has elapsed!", 0,
                millisRemaining);
    }
}
