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

import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Implementation of {@link SubscriptionFilterableQuery} to retrieve
 * {@link SiteSubscription}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * Jul 23, 2012 702        jpiatt      Added setters for groupName & officeId.
 * Oct 03, 2012 1241       djohnson    Move query parameters to {@link SubscriptionFilterableQuery}.
 * Mar 29, 2013 1841       djohnson    Renamed from SubscriptionQuery.
 * May 21, 2013 2020       mpduff      Rename UserSubscription to SiteSubscription.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 * @param <Subscription>
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SiteSubscriptionQuery extends
        SubscriptionFilterableQuery<SiteSubscription> {
    /**
     * {@inheritDoc}
     */
    @Override
    public Class<SiteSubscription> getResultType() {
        return SiteSubscription.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<SiteSubscription> getObjectType() {
        return SiteSubscription.class;
    }
}
