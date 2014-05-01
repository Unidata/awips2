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
package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;

/**
 * Finds subscriptions that should be scheduled for bandwidth management.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2013 1543       djohnson     Initial creation
 * Jan 29, 2014 2636       mpduff       Scheduling refactor.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IFindSubscriptionsForScheduling<T extends Subscription> {
    /**
     * Finds subscriptions that should be scheduled.
     * 
     * @return subscriptions
     * @throws Exception
     */
    Map<Network, Set<T>> findSubscriptionsToSchedule() throws Exception;
}
