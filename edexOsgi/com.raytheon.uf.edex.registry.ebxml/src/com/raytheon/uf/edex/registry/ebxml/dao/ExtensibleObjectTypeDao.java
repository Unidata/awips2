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

import org.hibernate.criterion.Property;

import com.raytheon.uf.edex.database.dao.IDaoConfigFactory;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;

/**
 * <pre>
 *
 * Data access object for interactions with ExtensibleObjectType objects
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Dec 02, 2013  1829     bphillip   Initial Creation
 * Apr 14, 2021  7849     mapeters   Refactor to use TransactionTemplate.execute
 *                                   instead of Transactional annotations
 *
 * </pre>
 *
 * @author bphillip
 **/
public class ExtensibleObjectTypeDao<ENTITY extends ExtensibleObjectType>
        extends SessionManagedDao<String, ENTITY> {

    public ExtensibleObjectTypeDao(IDaoConfigFactory daoConfigFactory) {
        this(daoConfigFactory, false);
    }

    public ExtensibleObjectTypeDao(IDaoConfigFactory daoConfigFactory,
            boolean admin) {
        super(daoConfigFactory, admin);
    }

    /**
     * Gets all ExtensibleObjectType objects matching (using like) the given
     * lid.
     *
     * @param lid
     *            The lid to query for
     * @return All ExtensibleObjectType objects matching the given lid
     */
    @SuppressWarnings("unchecked")
    public List<ENTITY> getByLidUsingLike(String lid) {
        return supplyInTransaction(requiredReadOnlyTransactionDef, () -> {
            return createCriteria()
                    .add(Property.forName(QueryConstants.LID).like(lid)).list();
        });
    }

    @SuppressWarnings("unchecked")
    @Override
    protected Class<ENTITY> getEntityClass() {
        return (Class<ENTITY>) ExtensibleObjectType.class;
    }

}
