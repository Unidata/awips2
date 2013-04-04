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
package com.raytheon.uf.common.datadelivery.registry.handlers;

import com.raytheon.uf.common.datadelivery.registry.InitialPendingUserSubscription;
import com.raytheon.uf.common.datadelivery.registry.ebxml.PendingUserSubscriptionQuery;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;

/**
 * {@link IRegistryObjectHandler} implementation for
 * {@link InitialPendingUserSubscription}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2012 1169       djohnson     Initial creation
 * Sep 24, 2012 1157       mpduff       Changed to use InitialPendingUserSubscription.
 * Apr 04, 2013 1841       djohnson     Extracted most methods to base class.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class PendingUserSubscriptionHandler extends
        BasePendingSubscriptionHandler<InitialPendingUserSubscription, PendingUserSubscriptionQuery>
        implements IPendingUserSubscriptionHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    protected PendingUserSubscriptionQuery getQuery() {
        return new PendingUserSubscriptionQuery();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<InitialPendingUserSubscription> getRegistryObjectClass() {
        return InitialPendingUserSubscription.class;
    }
}
