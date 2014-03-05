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

import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.dialect.Dialect;
import org.hibernate.impl.SessionFactoryImpl;
import org.hibernate.jdbc.Work;
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
 * Feb 07, 2013 1543       djohnson    Initial creation
 * 3/18/2013    1802       bphillip    Added additional database functions. Enforcing mandatory transaction propogation
 * 3/27/2013    1802       bphillip    Changed transaction propagation of query methods
 * 4/9/2013     1802       bphillip    Modified how arguments are passed in to query methods
 * May 01, 2013 1967       njensen     Fixed autoboxing for Eclipse 3.8
 * Jun 24, 2013 2106       djohnson    Use IDENTIFIER generic for method signature.
 * 10/8/2013    1682       bphillip    Added the createCriteria method
 * 12/9/2013    2613       bphillip    Added flushAndClearSession method
 * Jan 17, 2014 2459       mpduff      Added null check to prevent NPE.
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
        template.saveOrUpdateAll(objs);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void delete(final ENTITY obj) {
        if (obj != null) {
            Object toDelete = template.merge(obj);
            template.delete(toDelete);
        }
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
    @Transactional(propagation = Propagation.REQUIRED)
    public ENTITY getById(IDENTIFIER id) {
        final Class<ENTITY> entityClass = getEntityClass();
        return entityClass.cast(template.get(entityClass, id));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @Transactional(propagation = Propagation.REQUIRED)
    public List<ENTITY> getAll() {
        return query("from " + getEntityClass().getSimpleName());
    }

    public ENTITY uniqueResult(String queryString) {
        return uniqueResult(queryString, new Object[0]);
    }

    /**
     * Internal convenience method for returning a single result.
     * 
     * @param queryString
     * @param params
     * @return
     */
    @Transactional(propagation = Propagation.REQUIRED)
    protected ENTITY uniqueResult(String queryString, Object... params) {
        final List<ENTITY> results = executeHQLQuery(queryString, params);
        if (results.isEmpty()) {
            return null;
        } else if (results.size() > 1) {
            statusHandler.warn("More than one result returned for query ["
                    + queryString + "], only returning the first!");
        }
        return results.get(0);
    }

    public List<ENTITY> query(String queryString) {
        return executeHQLQuery(queryString);
    }

    /**
     * Internal convenience method for querying.
     * 
     * @param queryString
     * @param params
     * @return
     */
    @Transactional(propagation = Propagation.REQUIRED)
    public List<ENTITY> query(String queryString, Object... params) {
        return executeHQLQuery(queryString, 0, params);
    }

    @Transactional(propagation = Propagation.REQUIRED)
    public List<ENTITY> query(String queryString, Integer maxResults,
            Object... params) {
        return executeHQLQuery(queryString, maxResults, params);

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
    @Transactional(propagation = Propagation.REQUIRED)
    public <T extends Object> List<T> executeHQLQuery(String queryString) {
        return executeHQLQuery(queryString, 0);
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
    @Transactional(propagation = Propagation.REQUIRED)
    public <T extends Object> List<T> executeHQLQuery(String queryString,
            Object... params) {
        return executeHQLQuery(queryString, 0, params);
    }

    /**
     * Executes a named parameter query with the option to limit the maximum
     * number of results. The params argument contains the parameter names in
     * alternating fashion. The parameter name comes first followed by the
     * parameter value.
     * <p>
     * For example, to execute this query 'SELECT obj.field FROM object obj
     * WHERE obj.id=:id' you would call: <br>
     * executeHQLQuery("SELECT obj.field FROM object obj WHERE obj.id=:id", 0,
     * ":id",idValue)
     * 
     * @param <T>
     *            An object type to query for
     * @param queryString
     *            The query string, possibly containg name parameters
     * @param maxResults
     *            The maximum number of results to return
     * @param params
     *            The named parameter pairs
     * @return The results of the query
     */
    @SuppressWarnings("unchecked")
    @Transactional(propagation = Propagation.REQUIRED)
    public <T extends Object> List<T> executeHQLQuery(final String queryString,
            Integer maxResults, Object... params) {
        if (params.length % 2 != 0) {
            throw new IllegalArgumentException(
                    "Wrong number of arguments submitted to executeHQLQuery.");
        }
        HibernateTemplate templateToUse = (maxResults == 0) ? template
                : new HibernateTemplate(this.getSessionFactory());
        templateToUse.setMaxResults(maxResults);
        templateToUse.setQueryCacheRegion(QUERY_CACHE_REGION);
        if (params.length == 0) {
            return templateToUse.find(queryString);
        }
        String[] paramNames = new String[params.length / 2];
        Object[] paramValues = new Object[params.length / 2];
        int index = 0;
        for (int i = 0; i < params.length; i += 2) {
            paramNames[index] = (String) params[i];
            paramValues[index] = params[i + 1];
            index++;
        }
        return templateToUse.findByNamedParam(queryString, paramNames,
                paramValues);
    }

    @SuppressWarnings("unchecked")
    @Transactional(propagation = Propagation.REQUIRED)
    public <T extends Object> Iterator<T> getQueryIterator(
            final String queryString, Object... params) {
        if (params.length == 0) {
            return template.iterate(queryString);
        }
        return template.iterate(queryString, params);
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
        return executeHQLStatement(queryString, new Object[0]);
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
        return executeHQLStatement(queryString.toString(), new Object[0]);
    }

    /**
     * Executes a named parameter statement(non-query). The params argument
     * contains the parameter names in alternating fashion. The parameter name
     * comes first followed by the parameter value.
     * 
     * @param <T>
     *            An object type to query for
     * @param queryString
     *            The query string, possibly containg name parameters
     * @param maxResults
     *            The maximum number of results to return
     * @param params
     *            The named parameter pairs
     * @return The results of the query
     */
    public int executeHQLStatement(final String statement, Object... params)
            throws DataAccessLayerException {
        if (params.length % 2 != 0) {
            throw new IllegalArgumentException(
                    "Wrong number of arguments submitted to executeHQLStatement.");
        }
        try {
            Query query = getSessionFactory().getCurrentSession()
                    .createQuery(statement).setCacheable(true)
                    .setCacheRegion(QUERY_CACHE_REGION);
            for (int i = 0; i < params.length; i += 2) {
                if (params[i + 1] instanceof Collection<?>) {
                    query.setParameterList((String) params[i],
                            (Collection<?>) params[i + 1]);
                } else {
                    query.setParameter((String) params[i], params[i + 1]);
                }
            }
            return query.executeUpdate();
        } catch (Throwable e) {
            throw new DataAccessLayerException(
                    "Error executing HQL Statement [" + statement + "]", e);
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
    @Transactional(propagation = Propagation.REQUIRED)
    public <T extends Object> List<T> executeCriteriaQuery(
            final DetachedCriteria criteria) {
        if (criteria == null) {
            return Collections.emptyList();
        }
        return template.findByCriteria(criteria);
    }

    public void evict(ENTITY entity) {
        this.getSessionFactory().getCurrentSession().evict(entity);
    }

    /**
     * Low-level method to execute a unit of work.
     * 
     * @param work
     *            the work
     */
    public void executeWork(Work work) {
        this.getSessionFactory().getCurrentSession().doWork(work);
    }

    /**
     * Creates and returns a criteria instance
     * 
     * @return The criteria instance
     */
    protected Criteria createCriteria() {
        return template.getSessionFactory().getCurrentSession()
                .createCriteria(getEntityClass());
    }

    /**
     * Get the hibernate dialect.
     * 
     * @return the dialect.
     */
    public Dialect getDialect() {
        return ((SessionFactoryImpl) template.getSessionFactory()).getDialect();
    }

    /**
     * Flushes and clears the current Hibernate Session
     */
    public void flushAndClearSession() {
        this.getSessionFactory().getCurrentSession().flush();
        this.getSessionFactory().getCurrentSession().clear();
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
