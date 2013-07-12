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

import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrievalAttributes;

/**
 * * DAO that handles {@link SubscriptionRetrievalAttributes} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013 2106       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionRetrievalAttributesDao extends
        SessionManagedDao<Long, SubscriptionRetrievalAttributes> implements
        ISubscriptionRetrievalAttributesDao {

    private static final String GET_BY_SUBSCRIPTIONRETRIEVAL = "from SubscriptionRetrievalAttributes sr where "
            + "sr.subscriptionRetrieval.id = :id";

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<SubscriptionRetrievalAttributes> getEntityClass() {
        return SubscriptionRetrievalAttributes.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionRetrievalAttributes getBySubscriptionRetrieval(
            SubscriptionRetrieval retrieval) {
        return uniqueResult(GET_BY_SUBSCRIPTIONRETRIEVAL, "id",
                retrieval.getId());
    }

}
