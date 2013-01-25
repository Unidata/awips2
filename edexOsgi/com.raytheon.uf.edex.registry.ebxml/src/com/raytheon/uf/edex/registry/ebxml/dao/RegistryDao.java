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

import java.util.Collections;
import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;

import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.criterion.DetachedCriteria;
import org.springframework.orm.hibernate3.support.HibernateDaoSupport;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * This class is the base class from which all data access objects used by the
 * RegRep registry inherit. This class contains the necessary basic functions of
 * deleting, inserting, updating and querying.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class RegistryDao extends HibernateDaoSupport {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryDao.class);

    /**
     * The name of the session factory used by this dao
     */
    protected static final String SESSION_FACTORY_NAME = "ebxmlSessionFactory";

    /** The region in the cache which stores the queries */
    private static final String QUERY_CACHE_REGION = "Queries";

    /**
     * The class on which this data access object operates.
     */
    protected Class<?> daoClass;

    /**
     * Creates a new RegistryDao that is not assigned a class
     */
    public RegistryDao() {
        this(null);
    }

    /**
     * Creates a new data access object for the specified class
     * 
     * @param clazz
     *            The class on which this data access object will operate
     */
    public RegistryDao(Class<?> clazz) {
        super();
        this.daoClass = clazz;
        setSessionFactory((SessionFactory) EDEXUtil
                .getESBComponent(SESSION_FACTORY_NAME));
    }

    /**
     * Opens a Hibernate transaction and binds it to this thread
     */
    public void openSession() {
        this.getSessionFactory().getCurrentSession().beginTransaction();
    }

    /**
     * Closes the transaction currently bound to this thread
     */
    public void closeSession() {
        if (this.getSessionFactory().getCurrentSession().getTransaction()
                .isActive()) {
            this.getSessionFactory().getCurrentSession().getTransaction()
                    .commit();
        } else {
            statusHandler
                    .warn("Transaction is no longer active due to previous errors");
        }
    }

    public void rollback() {
        if (this.getSessionFactory().getCurrentSession().getTransaction()
                .isActive()) {
            this.getSessionFactory().getCurrentSession().getTransaction()
                    .rollback();
        } else {
            statusHandler
                    .warn("Transaction is no longer active due to previous errors");
        }

    }

    /**
     * Retrieves all objects of the specified type which are stored in the
     * database
     * 
     * @param <T>
     *            Class extending ExtensibleObjectType
     * @param type
     *            The type to load
     * @return All objects of the specified type
     * @throws EbxmlRegistryException
     *             If errors are encountered during the HQL query
     */
    public <T extends ExtensibleObjectType> List<T> getAllObjectsOfType(
            Class<T> type) throws EbxmlRegistryException {
        return executeHQLQuery("from " + type.getName());
    }

    /**
     * Executes an HQL query in a new Hibernate session
     * 
     * @param <T>
     *            An object type to query for
     * @param queryString
     *            The query to execute
     * @return The results of the HQL query
     * @throws EbxmlRegistryException
     *             If errors are encountered during the HQL query
     */
    public <T extends Object> List<T> executeHQLQuery(String queryString)
            throws EbxmlRegistryException {
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
     * @throws EbxmlRegistryException
     *             If Hibernate errors occur during execution of the query
     */
    public <T extends Object> List<T> executeHQLQuery(StringBuilder queryString)
            throws EbxmlRegistryException {
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
     * @throws EbxmlRegistryException
     *             If Hibernate errors occur during the execution of the query
     */
    public <T extends Object> List<T> executeHQLQuery(String queryString,
            Map<String, Object> params) throws EbxmlRegistryException {
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
     * @throws EbxmlRegistryException
     *             if errors are encountered during the HQL query
     */
    @SuppressWarnings("unchecked")
    public <T extends Object> List<T> executeHQLQuery(final String queryString,
            boolean eager, final Map<String, Object> params)
            throws EbxmlRegistryException {
        try {
            return (List<T>) doInTransaction(new RegistryTransactionCallback() {
                @Override
                public Object execute(Session session)
                        throws EbxmlRegistryException {
                    Query query = session.createQuery(queryString)
                            .setCacheable(true)
                            .setCacheRegion(QUERY_CACHE_REGION);
                    if (params != null) {
                        for (String name : params.keySet()) {
                            Object val = params.get(name);
                            query.setParameter(name, val);
                        }
                    }
                    List<T> results = query.list();
                    return results;
                }
            });
        } catch (Throwable e) {
            throw new EbxmlRegistryException("Error executing HQLQuery ["
                    + queryString + "]", e);
        }
    }

    /**
     * Gets a new DetachedCriteria instance for the daoClass
     * 
     * @return The new DetachedCriteria instance for the daoClass
     */
    protected DetachedCriteria getCriteriaInstance() {
        return DetachedCriteria.forClass(daoClass);
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
     * @throws EbxmlRegistryException
     *             If errors occur in Hibernate while executing the query
     */
    @SuppressWarnings("unchecked")
    public <T extends Object> List<T> executeCriteriaQuery(
            final DetachedCriteria criteria) throws EbxmlRegistryException {
        if (criteria == null) {
            return Collections.emptyList();
        }

        return (List<T>) this
                .doInTransaction(new RegistryTransactionCallback() {
                    @Override
                    public Object execute(Session session)
                            throws EbxmlRegistryException {
                        try {
                            List<T> results = null;
                            results = criteria.getExecutableCriteria(session)
                                    .setCacheable(true)
                                    .setCacheRegion(QUERY_CACHE_REGION).list();
                            return results;
                        } catch (Throwable e) {
                            throw new EbxmlRegistryException(
                                    "Error executing Criteria Query", e);
                        }
                    }
                });
    }

    /**
     * Deletes objects from the database
     * 
     * @param objects
     *            The objects to delete
     * @throws EbxmlRegistryException
     *             If the delete was unsuccessful
     */
    public void delete(Object... objects) throws EbxmlRegistryException {
        final List<Object> objectList = EbxmlObjectUtil.getList(objects);
        this.doInTransaction(new RegistryTransactionCallback() {
            @Override
            public Object execute(Session session)
                    throws EbxmlRegistryException {
                try {
                    for (Object obj : objectList) {
                        session.delete(obj);
                    }
                    return null;
                } catch (Throwable e) {
                    throw new EbxmlRegistryException(
                            "Unexpected error during delete", e);
                }
            }
        });

    }

    /**
     * Saves or updates objects
     * 
     * @param objects
     *            The objects to save or update
     * @throws EbxmlRegistryException
     *             If the save or updated failed
     */
    public void saveOrUpdate(List<Object> objects)
            throws EbxmlRegistryException {
        saveOrUpdate(objects.toArray(new Object[objects.size()]));
    }

    /**
     * Saves or updates objects
     * 
     * @param objects
     *            The objects to save or update
     * @throws EbxmlRegistryException
     *             If the save or updated failed
     */
    public void saveOrUpdate(Object... objects) throws EbxmlRegistryException {
        final List<Object> objectList = EbxmlObjectUtil.getList(objects);

        this.doInTransaction(new RegistryTransactionCallback() {

            @Override
            public Object execute(Session session)
                    throws EbxmlRegistryException {
                for (Object obj : objectList) {
                    try {
                        session.saveOrUpdate(obj);
                    } catch (Throwable e) {
                        throw new EbxmlRegistryException(
                                "Unexpected error during save or update", e);
                    }
                }
                return null;
            }
        });

    }

    /**
     * Saves objects
     * 
     * @param objects
     *            The objects to save or update
     * @throws EbxmlRegistryException
     *             If the save or updated failed
     */
    public void save(List<Object> objects) throws EbxmlRegistryException {
        save(objects.toArray(new Object[objects.size()]));
    }

    /**
     * Saves objects
     * 
     * @param objects
     *            The objects to save or update
     * @throws EbxmlRegistryException
     *             If the save or updated failed
     */
    public void save(Object... objects) throws EbxmlRegistryException {
        final List<Object> objectList = EbxmlObjectUtil.getList(objects);

        this.doInTransaction(new RegistryTransactionCallback() {

            @Override
            public Object execute(Session session)
                    throws EbxmlRegistryException {
                for (Object obj : objectList) {
                    try {
                        session.save(obj);
                    } catch (Throwable e) {
                        throw new EbxmlRegistryException(
                                "Unexpected error during save or update", e);
                    }
                }
                return null;
            }
        });

    }

    /**
     * Evicts the given objects for the current Hibernate session instance
     * 
     * @param objects
     *            The objects to evict
     * @throws EbxmlRegistryException
     *             If errors occur during the eviction process
     */
    public void evict(Object... objects) throws EbxmlRegistryException {
        final List<Object> objectList = EbxmlObjectUtil.getList(objects);

        this.doInTransaction(new RegistryTransactionCallback() {

            @Override
            public Object execute(Session session)
                    throws EbxmlRegistryException {
                for (Object obj : objectList) {
                    try {
                        // marshal the object to force associations to be
                        // eagerly initialized
                        SerializationUtil.marshalToXml(obj);
                        session.evict(obj);
                    } catch (Throwable e) {
                        throw new EbxmlRegistryException(
                                "Unexpected error during save or update", e);
                    }
                }
                return null;
            }
        });
    }

    /**
     * Gets the Hibernate statistics. This method is used for debug purposes
     * only
     * 
     * @return The String representation of the Hibernate statistics
     */
    public String getStats() {
        String[] items = this.getSessionFactory().getStatistics().toString()
                .split(",");
        StringBuffer buf = new StringBuffer();
        buf.append("_________________________________\n");
        for (int i = 0; i < items.length; i++) {
            buf.append(items[i]).append("\n");
        }
        buf.append("_________________________________\n");
        return buf.toString();
    }

    /**
     * Method used to wrap execution of a certain segment of code in a Hibernate
     * transaction. If a session is open and a transaction has already been
     * started, that session and transaction will be used. If not, a new session
     * will be created, and a new transaction will be created and begun
     * 
     * @param callback
     *            A callback object containing transactional code
     * @return The result of the transactional code
     * @throws EbxmlRegistryException
     *             If errors occur during the execution of the transactional
     *             code
     */
    protected Object doInTransaction(RegistryTransactionCallback callback)
            throws EbxmlRegistryException {
        Session session = null;
        Transaction tx = null;
        boolean txCreated = false;
        try {
            session = this.getSessionFactory().getCurrentSession();
            tx = session.getTransaction();
            if (!tx.isActive()) {
                tx = session.beginTransaction();
                txCreated = true;
            }
        } catch (HibernateException e) {
            throw new EbxmlRegistryException(
                    "Error acquiring Hibernate session.", e);
        }
        try {
            Object retVal = callback.execute(session);
            if (txCreated) {
                tx.commit();
            }
            return retVal;
        } catch (Exception e) {
            tx.rollback();
            throw new EbxmlRegistryException("Transaction failed.", e);
        } finally {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug(this.getStats());
            }

        }
    }

    /**
     * Gets the class associated with this data access object
     * 
     * @return The daoClass
     */
    public Class<?> getDaoClass() {
        return daoClass;
    }

    /**
     * Sets the class to be associated with this data access object
     * 
     * @param daoClass
     *            The daoClass
     */
    public void setDaoClass(Class<?> daoClass) {
        this.daoClass = daoClass;
    }

}
