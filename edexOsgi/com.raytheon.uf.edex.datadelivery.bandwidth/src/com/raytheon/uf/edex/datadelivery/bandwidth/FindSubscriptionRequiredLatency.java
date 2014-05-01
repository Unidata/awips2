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
package com.raytheon.uf.edex.datadelivery.bandwidth;

import java.util.SortedSet;
import java.util.TreeSet;

import com.raytheon.uf.common.datadelivery.registry.Subscription;

/**
 * Find the subscription latency that would allow it to be fully scheduled.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 20, 2013 1650       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class FindSubscriptionRequiredLatency implements
        IFindSubscriptionRequiredValue<Integer> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Integer getInitialValue(Subscription subscription) {
        int latency = subscription.getLatencyInMinutes();
        if (latency < 1) {
            latency = 1;
        }
        return latency;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Integer getNextValue(Subscription subscription, Integer currentValue) {
        return currentValue *= 2;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription setValue(Subscription subscription, Integer value) {
        subscription.setLatencyInMinutes(value.intValue());
        return subscription;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SortedSet<Integer> getPossibleValues(Integer previousValue,
            Integer currentValue) {
        SortedSet<Integer> possibleValues = new TreeSet<Integer>();
        for (int i = previousValue; i < (currentValue + 1); i++) {
            possibleValues.add(Integer.valueOf(i));
        }
        return possibleValues;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Integer getNextRestrictiveValue(Integer value) {
        // For latency, a less value would be more restrictive
        return Integer.valueOf(value.intValue() - 1);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getValueDescription() {
        return "latency";
    }

}
