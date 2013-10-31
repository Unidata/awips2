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

import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.ebxml.AdhocSubscriptionQuery;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;

/**
 * {@link IRegistryObjectHandler} implementation for {@link AdhocSubscription}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 05, 2012 1241       djohnson     Initial creation.
 * Oct 12, 2013 2460       dhladky      restored.
 * Oct 25, 2013 2292       mpduff       Removed unused imports.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class AdhocSubscriptionHandler extends
        BaseSubscriptionHandler<AdhocSubscription, AdhocSubscriptionQuery>
        implements IAdhocSubscriptionHandler {

    /**
     * {@inheritDoc}
     */

    @Override
    protected AdhocSubscriptionQuery getQuery() {
        return new AdhocSubscriptionQuery();
    }

    /**
     * 
     * {@inheritDoc}
     */

    @Override
    protected Class<AdhocSubscription> getRegistryObjectClass() {
        return AdhocSubscription.class;
    }

}
