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

import com.raytheon.uf.common.datadelivery.registry.InitialPendingSharedSubscription;
import com.raytheon.uf.common.datadelivery.registry.ebxml.PendingSharedSubscriptionQuery;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;

/**
 * {@link IRegistryObjectHandler} implementation for
 * {@link InitialPendingSharedSubscription}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 04, 2013 1841       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class PendingSharedSubscriptionHandler extends
        BasePendingSubscriptionHandler<InitialPendingSharedSubscription, PendingSharedSubscriptionQuery>
        implements IPendingSharedSubscriptionHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    protected PendingSharedSubscriptionQuery getQuery() {
        return new PendingSharedSubscriptionQuery();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<InitialPendingSharedSubscription> getRegistryObjectClass() {
        return InitialPendingSharedSubscription.class;
    }
}
