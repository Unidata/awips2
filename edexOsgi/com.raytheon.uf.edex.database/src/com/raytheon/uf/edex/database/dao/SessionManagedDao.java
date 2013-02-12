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

package com.raytheon.uf.edex.database.dao;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.hibernate.SessionFactory;
import org.hibernate.dialect.Dialect;
import org.hibernate.impl.SessionFactoryImpl;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * A CoreDao mimic that is session managed. A Dao will never open its own
 * transaction, nor will it commit/rollback transactions. Any number of Daos
 * should be utilizable in the same transaction from a service that demarcates
 * the transaction boundaries.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Repository
@Transactional
public abstract class SessionManagedDao<IDENTIFIER extends Serializable, ENTITY extends IPersistableDataObject<IDENTIFIER>> implements ISessionManagedDao<IDENTIFIER, ENTITY> {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SessionManagedDao.class);

    protected HibernateTemplate template;

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSessionFactory(SessionFactory sessionFactory) {
        template = new HibernateTemplate(sessionFactory);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void create(final ENTITY obj) {
        template.save(obj);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(final ENTITY obj) {
        template.update(obj);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createOrUpdate(final ENTITY obj) {
        template.saveOrUpdate(obj);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void persistAll(final Collection<ENTITY> objs) {
        for (ENTITY obj : objs) {
            createOrUpdate(obj);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void delete(final ENTITY obj) {
        Object toDelete = template.merge(obj);
        template.delete(toDelete);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteAll(final Collection<ENTITY> objs) {
        for (ENTITY obj : objs) {
            delete(obj);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ENTITY getById(IDENTIFIER id) {
        final Class<ENTITY> entityClass = getEntityClass();
        return entityClass.cast(template.get(entityClass, id));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ENTITY> getAll() {
        return query("from " + getEntityClass().getSimpleName(),
                Collections.<String, Object> emptyMap());
    }

    /**
     * Internal convenience method for querying.
     * 
     * @param queryString
     * @param params
     * @return
     */
    @SuppressWarnings("unchecked")
    protected List<ENTITY> query(String queryString,
            Map<String, Object> params) {
        final int numberOfParams = params.size();
        String[] paramNames = new String[numberOfParams];
        Object[] paramValues = new Object[numberOfParams];
        Iterator<Map.Entry<String, Object>> iter = params.entrySet().iterator();
        for (int i = 0; i < numberOfParams; i++) {
            final Entry<String, Object> entry = iter.next();
            paramNames[i] = entry.getKey();
            paramValues[i] = entry.getValue();
        }

        return template.findByNamedParam(queryString, paramNames, paramValues);
    }

    /**
     * Internal convenience method for returning a single result.
     * 
     * @param queryString
     * @param params
     * @return
     */
    protected ENTITY uniqueResult(String queryString,
            Map<String, Object> params) {
        final List<ENTITY> results = query(queryString, params);
        if (results.isEmpty()) {
            return null;
        } else if (results.size() > 1) {
            statusHandler.warn("More than one result returned for query ["
                    + queryString + "], only returning the first!");
        }
        return results.get(0);
    }

    /**
     * Get the hibernate dialect.
     * 
     * @return the dialect.
     */
    // TODO: Remove the requirement of this method
    public Dialect getDialect() {
        return ((SessionFactoryImpl) template.getSessionFactory()).getDialect();
    }

    /**
     * Return the entity class type.
     * 
     * @return the entity class type
     */
    protected abstract Class<ENTITY> getEntityClass();
}
