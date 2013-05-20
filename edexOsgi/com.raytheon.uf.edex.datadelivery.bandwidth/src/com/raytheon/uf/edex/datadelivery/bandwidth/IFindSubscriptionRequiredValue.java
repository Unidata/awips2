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

import com.raytheon.uf.common.datadelivery.registry.Subscription;

/**
 * Defines the interface to search for a required value on a subscription so
 * that it would fully schedule.
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

interface IFindSubscriptionRequiredValue<T extends Comparable<T>> {

    /**
     * Get the initial value from the subscription.
     * 
     * @param subscription
     *            the subscription
     * @return the initial value
     */
    T getInitialValue(Subscription subscription);

    /**
     * Get the next value that should be tried, when doing a large search.
     * 
     * @param subscription
     *            the subscription
     * @param currentValue
     *            the next value that should be attempted
     * @return the next value
     */
    T getNextValue(Subscription subscription, T currentValue);
    
    Subscription setValue(Subscription subscription, T value);

    /**
     * Get all values that should be included between the value that did not
     * work and the value that did work.
     * 
     * @param valueThatDidNotWork
     *            the value that did not work
     * @param valueThatDidWork
     *            the value that did work
     * @return the set of values that will be binary searched
     */
    SortedSet<T> getPossibleValues(T valueThatDidNotWork, T valueThatDidWork);

    /**
     * Get the value that would be closest to the given value, but be more
     * restrictive (i.e. more likely to not work).
     * 
     * @param value
     *            the value
     * @return the more restrictive value to attempt
     */
    T getNextRestrictiveValue(T value);

    /**
     * Get the description of the value that is being sought.
     * 
     * @return the description
     */
    String getValueDescription();
}
