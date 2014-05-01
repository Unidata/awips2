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
package com.raytheon.uf.edex.datadelivery.bandwidth.handler;

import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.ebxml.SiteSubscriptionQuery;
import com.raytheon.uf.common.datadelivery.registry.handlers.SiteSubscriptionHandler;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.edex.datadelivery.util.DataDeliveryIdUtil;

/**
 * {@link IRegistryObjectHandler} implementation for {@link SiteSubscription}.
 * Retrieves only subscritions for the given site id. If the site id is not set
 * then it retrieves all.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2013 2545       bgonzale    Initial creation.
 * Feb 11, 2014 2771       bgonzale    Use Data Delivery ID instead of Site.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
public class LocalSiteSubscriptionHandler extends SiteSubscriptionHandler {

    private String officeId;

    /**
     * Default Constructor.
     */
    public LocalSiteSubscriptionHandler() {
        this(DataDeliveryIdUtil.getId());
    }

    /**
     * Initialization Constructor.
     * 
     * @param officeId
     */
    public LocalSiteSubscriptionHandler(String officeId) {
        this.officeId = officeId;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected SiteSubscriptionQuery getQuery() {
        SiteSubscriptionQuery query = new SiteSubscriptionQuery();
        if (officeId != null) {
            query.setOfficeId(officeId);
        }
        return query;
    }

}
