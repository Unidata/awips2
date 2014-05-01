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

import java.util.Collections;
import java.util.SortedSet;
import java.util.TreeSet;

import com.raytheon.uf.common.datadelivery.registry.Subscription;

/**
 * Find the subscription size that would allow it to be fully scheduled.
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
class FindSubscriptionRequiredDataSetSize implements
        IFindSubscriptionRequiredValue<Long> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Long getInitialValue(Subscription subscription) {
        return subscription.getDataSetSize();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Long getNextValue(Subscription subscription, Long currentValue) {
        return currentValue /= 2;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription setValue(Subscription subscription, Long value) {
        subscription.setDataSetSize(value);
        return subscription;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SortedSet<Long> getPossibleValues(Long previousValue,
            Long currentValue) {
        SortedSet<Long> possibleValues = new TreeSet<Long>(
                Collections.reverseOrder());
        for (long i = currentValue; i < previousValue; i++) {
            possibleValues.add(Long.valueOf(i));
        }
        return possibleValues;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Long getNextRestrictiveValue(Long value) {
        // For DataSet size, a higher value would be more restrictive
        return Long.valueOf(value.longValue() + 1);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getValueDescription() {
        return "dataset size";
    }

}
