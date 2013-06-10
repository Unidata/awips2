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

import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.ebxml.SiteSubscriptionQuery;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;

/**
 * {@link IRegistryObjectHandler} implementation for {@link SiteSubscription}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012 1169       djohnson     Initial creation.
 * Sep 24, 2012 1157       mpduff       Change to use InitialPendingSubscription.
 * Oct 17, 2012 0726       djohnson     Add {@link #getActiveByDataSetAndProvider}.
 * Mar 29, 2013 1841       djohnson     Renamed from SubscriptionHandler.
 * Apr 05, 2013 1841       djohnson     Extracted core logic to superclass.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SiteSubscriptionHandler extends
        SubscriptionTypeHandler<SiteSubscription, SiteSubscriptionQuery>
        implements ISiteSubscriptionHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    protected SiteSubscriptionQuery getQuery() {
        return new SiteSubscriptionQuery();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<SiteSubscription> getRegistryObjectClass() {
        return SiteSubscription.class;
    }
}
