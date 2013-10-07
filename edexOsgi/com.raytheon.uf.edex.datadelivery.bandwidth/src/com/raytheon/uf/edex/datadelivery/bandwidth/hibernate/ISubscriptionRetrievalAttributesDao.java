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

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.edex.database.dao.ISessionManagedDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrievalAttributes;

/**
 * DAO for {@link SubscriptionRetrievalAttributes} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013 2106       djohnson     Initial creation
 * Oct 2   2012  1797      dhladky      Generics
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
interface ISubscriptionRetrievalAttributesDao<T extends Time, C extends Coverage> extends
        ISessionManagedDao<Long, SubscriptionRetrievalAttributes<T, C>> {

    /**
     * @param retrieval
     * @return
     */
    SubscriptionRetrievalAttributes<T, C> getBySubscriptionRetrieval(
            SubscriptionRetrieval retrieval);
}