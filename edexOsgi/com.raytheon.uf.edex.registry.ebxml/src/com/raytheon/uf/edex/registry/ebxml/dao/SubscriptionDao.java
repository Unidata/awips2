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
package com.raytheon.uf.edex.registry.ebxml.dao;

import java.util.List;

import jakarta.xml.bind.JAXBException;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.dao.IDaoConfigFactory;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

/**
 * Data Access object for interacting with roles in the registry
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------------------------------
 * Mar 13, 2013  1082     bphillip  Initial creation
 * Sep 05, 2013  1538     bphillip  Added eagerLoadAll method
 * Feb 13, 2014  2769     bphillip  Added read only flags to query methods
 * Oct 16, 2014  3454     bphillip  Upgrading to Hibernate 4
 * Oct 27, 2020  8170     ksunil    removed references to empty tables
 * Apr 14, 2021  7849     mapeters  Refactor to use TransactionTemplate.execute instead of
 *                                  Transactional annotations
 *
 * </pre>
 *
 * @author bphillip
 */
public class SubscriptionDao extends RegistryObjectTypeDao<SubscriptionType> {

    public static final String EAGER_LOAD_QUERY = "FROM "
            + SubscriptionType.class.getName()
            + " sub fetch all properties where sub.id=:id";

    /** The jaxb manager for subscription objects */
    private JAXBManager subscriptionJaxbManager;

    /**
     * Creates a new SubscriptionDao object
     *
     * @throws JAXBException
     *             If errors occur instantiating the jaxb manager
     */
    public SubscriptionDao(IDaoConfigFactory daoConfigFactory)
            throws JAXBException {
        super(daoConfigFactory);
        subscriptionJaxbManager = new JAXBManager(SubscriptionType.class);
    }

    /**
     * Eagerly loads all the registry subscriptions
     *
     * @return All subscriptions in the registry
     * @throws EbxmlRegistryException
     *             If errors occur while querying
     */
    public List<SubscriptionType> eagerLoadAll() throws EbxmlRegistryException {
        List<SubscriptionType> subs = super.loadAll();
        for (SubscriptionType sub : subs) {
            try {
                /*
                 * FIXME: This is just a quick and dirty way of fully
                 * initializing all the fields of the subscription. Since this
                 * query happens relatively infrequently, having this operation
                 * here does not pose any sort of performance penalty.
                 * Obviously, a better solution needs to be devised in the
                 * future
                 */
                subscriptionJaxbManager.marshalToXml(sub);
            } catch (JAXBException e) {
                throw new EbxmlRegistryException("Error initializing bean!", e);
            }
        }
        return subs;
    }

    /**
     * Retrieves the fully populated subscription object
     *
     * @param subscriptionId
     *            The id of the subscription to retrieve
     * @return The fully populate subscription object
     * @throws EbxmlRegistryException
     *             If errors occur while eagerly fetching all attributes using
     *             jaxb
     */
    public SubscriptionType eagerGetById(String subscriptionId)
            throws EbxmlRegistryException {
        List<SubscriptionType> result = this.query(EAGER_LOAD_QUERY, "id",
                subscriptionId);
        if (CollectionUtil.isNullOrEmpty(result)) {
            return null;
        } else {
            SubscriptionType retVal = result.get(0);
            try {
                /*
                 * FIXME: This is just a quick and dirty way of fully
                 * initializing all the fields of the subscription. Since this
                 * query happens relatively infrequently, having this operation
                 * here does not pose any sort of performance penalty.
                 * Obviously, a better solution needs to be devised in the
                 * future
                 */
                subscriptionJaxbManager.marshalToXml(retVal);
            } catch (JAXBException e) {
                throw new EbxmlRegistryException("Error initializing bean!", e);
            }
            return retVal;
        }
    }

    @Override
    protected Class<SubscriptionType> getEntityClass() {
        return SubscriptionType.class;
    }

}
