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
package com.raytheon.uf.common.datadelivery.registry.ebxml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Implementation of {@link SubscriptionFilterableQuery} to retrieve
 * {@link PendingSubscription}s.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 8, 2012             mpduff      Initial creation
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * Sep 24, 2012 1157       mpduff      Extends InitialPendingSubscriptionQuery.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PendingSubscriptionQuery extends
        SubscriptionFilterableQuery<InitialPendingSubscription> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<InitialPendingSubscription> getResultType() {
        return InitialPendingSubscription.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<InitialPendingSubscription> getObjectType() {
        return InitialPendingSubscription.class;
    }
}
