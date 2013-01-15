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
package com.raytheon.uf.viz.datadelivery.utils;

import static com.google.common.collect.Lists.newArrayList;

import java.util.List;

import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.Time;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DataDeliveryUtilsTest {

    /**
     * The method was throwing {@link IndexOutOfBoundsException}, this will
     * prevent that regression.
     */
    @Test
    public void testMaxLatencyDoesntOverrunListIndex() {
        List<Integer> cycleTimes = newArrayList();
        cycleTimes.add(0);
        cycleTimes.add(1);
        cycleTimes.add(2);

        Subscription subscription = SubscriptionFixture.INSTANCE.get();
        Time subTime = subscription.getTime();
        subTime.setCycleTimes(cycleTimes);

        DataDeliveryUtils.getMaxLatency(subscription);
    }

}
