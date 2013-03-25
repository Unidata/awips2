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

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.dialect.Dialect;
import org.hibernate.impl.SessionFactoryImpl;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;

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
 * 3/18/2013    1802       bphillip    Added additional database functions. Enforcing mandatory transaction propogation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Repository
@Transactional(propagation = Propagation.MANDATORY)
public abstract class SessionManagedDao<IDENTIFIER extends Serializable, ENTITY extends IPersistableDataObject<IDENTIFIER>>
        implements ISessionManagedDao<IDENTIFIER, ENTITY> {

    /** The region in the cache which stores the queries */
    private static final String QUERY_CACHE_REGION = "Queries";

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
    public ENTITY getById(Serializable id) {
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
    protected List<ENTITY> query(String queryString, Map<String, Object> params) {
        return query(queryString, params, 0);
    }

    @SuppressWarnings("unchecked")
    protected List<ENTITY> query(String queryString,
            Map<String, Object> params, int maxResults) {
        final int numberOfParams = params.size();
        String[] paramNames = new String[numberOfParams];
        Object[] paramValues = new Object[numberOfParams];
        Iterator<Map.Entry<String, Object>> iter = params.entrySet().iterator();
        for (int i = 0; i < numberOfParams; i++) {
            final Entry<String, Object> entry = iter.next();
            paramNames[i] = entry.getKey();
            paramValues[i] = entry.getValue();
        }
        HibernateTemplate templateToUse = (maxResults == 0) ? template
                : new HibernateTemplate(this.getSessionFactory());
        templateToUse.setMaxResults(maxResults);
        return templateToUse.findByNamedParam(queryString, paramNames,
                paramValues);

    }

    /**
     * Internal convenience method for returning a single result.
     * 
     * @param queryString
     * @param params
     * @return
     */
    protected ENTITY uniqueResult(String queryString, Map<String, Object> params) {
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
     * Executes an HQL query in a new Hibernate session
     * 
     * @param <T>
     *            An object type to query for
     * @param queryString
     *            The query to execute
     * @return The results of the HQL query
     * @throws DataAccessLayerException
     *             If errors are encountered during the HQL query
     */
    public <T extends Object> List<T> executeHQLQuery(String queryString)
            throws DataAccessLayerException {
        return executeHQLQuery(queryString, true, null);
    }

    /**
     * Executes an HQL query
     * 
     * @param <T>
     *            The return object type
     * @param queryString
     *            A StringBuilder instance containing an HQL query to execute
     * @return List containing the results of the query
     * @throws DataAccessLayerException
     *             If Hibernate errors occur during execution of the query
     */
    public List<ENTITY> executeHQLQuery(StringBuilder queryString)
            throws DataAccessLayerException {
        return executeHQLQuery(queryString.toString(), true, null);
    }

    /**
     * Executes an HQL query with a map of name value pairs with which to
     * substitute into the query. This method is a convenience method for
     * executing prepared statements
     * 
     * @param <T>
     *            The return object type
     * @param queryString
     *            The prepared HQL query to execute. This query contains values
     *            that will be substituted according to the names and values
     *            found in the params map
     * @param params
     *            The named parameters to substitute into the HQL query
     * @return List containing the results of the query
     * @throws DataAccessLayerException
     *             If Hibernate errors occur during the execution of the query
     */
    public List<ENTITY> executeHQLQuery(String queryString,
            Map<String, Object> params) throws DataAccessLayerException {
        return executeHQLQuery(queryString, true, params);
    }

    /**
     * Executes an HQL query in an existing Hibernate session
     * 
     * @param <T>
     *            An object type to query for
     * @param queryString
     *            The query to execute
     * @param session
     *            The existing Hibernate session
     * @return The results of the HQL query
     * @throws DataAccessLayerException
     *             if errors are encountered during the HQL query
     */
    @SuppressWarnings("unchecked")
    public <T extends Object> List<T> executeHQLQuery(final String queryString,
            boolean eager, final Map<String, Object> params)
            throws DataAccessLayerException {
        try {
            Query query = getSessionFactory().getCurrentSession()
                    .createQuery(queryString).setCacheable(true)
                    .setCacheRegion(QUERY_CACHE_REGION);
            if (params != null) {
                for (String name : params.keySet()) {
                    Object val = params.get(name);
                    query.setParameter(name, val);
                }
            }
            List<T> results = query.list();
            return results;
        } catch (Throwable e) {
            throw new DataAccessLayerException("Error executing HQLQuery ["
                    + queryString + "]", e);
        }
    }

    /**
     * Executes an HQL query in a new Hibernate session
     * 
     * @param <T>
     *            An object type to query for
     * @param queryString
     *            The query to execute
     * @return The results of the HQL query
     * @throws DataAccessLayerException
     *             If errors are encountered during the HQL query
     */
    public int executeHQLStatement(String queryString)
            throws DataAccessLayerException {
        return executeHQLStatement(queryString, true, null);
    }

    /**
     * Executes an HQL query
     * 
     * @param <T>
     *            The return object type
     * @param queryString
     *            A StringBuilder instance containing an HQL query to execute
     * @return List containing the results of the query
     * @throws DataAccessLayerException
     *             If Hibernate errors occur during execution of the query
     */
    public int executeHQLStatement(StringBuilder queryString)
            throws DataAccessLayerException {
        return executeHQLStatement(queryString.toString(), true, null);
    }

    /**
     * Executes an HQL query with a map of name value pairs with which to
     * substitute into the query. This method is a convenience method for
     * executing prepared statements
     * 
     * @param <T>
     *            The return object type
     * @param queryString
     *            The prepared HQL query to execute. This query contains values
     *            that will be substituted according to the names and values
     *            found in the params map
     * @param params
     *            The named parameters to substitute into the HQL query
     * @return List containing the results of the query
     * @throws DataAccessLayerException
     *             If Hibernate errors occur during the execution of the query
     */
    public int executeHQLStatement(String queryString,
            Map<String, Object> params) throws DataAccessLayerException {
        return executeHQLStatement(queryString, true, params);
    }

    /**
     * Executes an HQL query in an existing Hibernate session
     * 
     * @param <T>
     *            An object type to query for
     * @param queryString
     *            The query to execute
     * @param session
     *            The existing Hibernate session
     * @return The results of the HQL query
     * @throws DataAccessLayerException
     *             if errors are encountered during the HQL query
     */
    @SuppressWarnings("unchecked")
    public int executeHQLStatement(final String queryString, boolean eager,
            final Map<String, Object> params) throws DataAccessLayerException {
        try {
            Query query = getSessionFactory().getCurrentSession()
                    .createQuery(queryString).setCacheable(true)
                    .setCacheRegion(QUERY_CACHE_REGION);
            if (params != null) {
                for (String name : params.keySet()) {
                    Object val = params.get(name);
                    query.setParameter(name, val);
                }
            }
            return query.executeUpdate();
        } catch (Throwable e) {
            throw new DataAccessLayerException(
                    "Error executing HQL Statement [" + queryString + "]", e);
        }
    }

    /**
     * Executes a criteria query. This method expects a DetachedQuery instance.
     * The DetachedQuery is attached to a new session and executed
     * 
     * @param <T>
     *            An Object type
     * @param criteria
     *            The DetachedCriteria instance to execute
     * @return The results of the query
     * @throws DataAccessLayerException
     *             If errors occur in Hibernate while executing the query
     */
    @SuppressWarnings("unchecked")
    public <T extends Object> List<T> executeCriteriaQuery(
            final DetachedCriteria criteria) throws DataAccessLayerException {
        if (criteria == null) {
            return Collections.emptyList();
        }

        try {
            List<T> results = null;
            results = criteria
                    .getExecutableCriteria(
                            getSessionFactory().getCurrentSession())
                    .setCacheable(true).setCacheRegion(QUERY_CACHE_REGION)
                    .list();
            return results;
        } catch (Throwable e) {
            throw new DataAccessLayerException(
                    "Error executing Criteria Query", e);
        }
    }

    public void evict(ENTITY entity) {
        this.getSessionFactory().getCurrentSession().evict(entity);
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

    protected SessionFactory getSessionFactory() {
        return template.getSessionFactory();
    }

    /**
     * Return the entity class type.
     * 
     * @return the entity class type
     */
    protected abstract Class<ENTITY> getEntityClass();
}
