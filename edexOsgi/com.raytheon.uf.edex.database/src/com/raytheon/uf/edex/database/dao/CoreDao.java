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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Serializable;
import java.lang.management.ManagementFactory;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.management.MBeanServer;
import javax.management.ObjectName;

import net.sf.ehcache.CacheManager;
import net.sf.ehcache.management.ManagementService;

import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Property;
import org.hibernate.jmx.StatisticsService;
import org.hibernate.metadata.ClassMetadata;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.orm.hibernate3.HibernateTransactionManager;
import org.springframework.orm.hibernate3.support.HibernateDaoSupport;
import org.springframework.transaction.TransactionException;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;
import org.springframework.transaction.support.TransactionTemplate;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * The base implementation of all daos. This implementation provides basic
 * database interaction functionality necessary to most persistable data types.
 * These functions include basic persistance and retrieval methods. Any data
 * type specific operations may be implemented by extending this class.
 * <p>
 * Data types which must be persisted to the database must have an associated
 * dao which extends this class. Each class needing a dao must also extend the
 * PersistableDataObject<T> class.
 * <p>
 * NOTE: Direct instantiation of this class is discouraged. Use
 * DaoPool.getInstance().borrowObject() for retrieving all data access objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/07      353         bphillip    Initial Check in   
 * 5/14/08      1076        brockwoo    Fix for distinct with multiple properties
 * Oct 10, 2012 1261        djohnson    Incorporate changes to DaoConfig, add generic to {@link IPersistableDataObject}.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class CoreDao extends HibernateDaoSupport {
    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CoreDao.class);

    /**
     * The Hibernate transaction manager. Methods do not directly use this
     * class. Instead, use the Transaction template
     */
    protected final HibernateTransactionManager txManager;

    /** The convenience wrapper for the Hibernate transaction manager */
    protected final TransactionTemplate txTemplate;

    /** The class associated with this dao */
    protected Class<?> daoClass;

    /**
     * Creates a new dao instance not associated with a specific class. A class
     * may be associated with this dao later by calling the setDaoClass method.
     * <p>
     * This constructor is used to create a generic dao when a Mule service bean
     * is constructed. When the service bean is constructed, the bean has no
     * knowledge about which data type it is handling. Therefore, the querying
     * capabilities provided by this class are not accessible until the daoClass
     * has been assigned
     */
    public CoreDao(DaoConfig config) {
        this.txManager = config.getTxManager();
        txTemplate = new TransactionTemplate(txManager);
        setSessionFactory(config.getSessionFactory());

        this.daoClass = config.getDaoClass();
        getHibernateTemplate().setCacheQueries(true);
    }

    /**
     * Registers the Hibernate statistics and EHCache statistics with the JMX
     * service
     */
    public void registerJMX() {
        Hashtable<String, String> tb = new Hashtable<String, String>();
        tb.put("type", "statistics");
        tb.put("sessionFactory", "HibernateSessionFactory");
        try {
            ObjectName on = new ObjectName("hibernate", tb);
            MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
            if (!mbs.isRegistered(on)) {
                StatisticsService stats = new StatisticsService();
                stats.setSessionFactory(getSessionFactory());
                mbs.registerMBean(stats, on);
                ManagementService.registerMBeans(CacheManager.getInstance(),
                        mbs, true, true, true, true);
            }
        } catch (Exception e) {
            logger.warn("Unable to register Hibernate and EHCache with JMX");
        }

    }

    /**
     * Persists an object to the database using the provided Hibernate mapping
     * file for the object
     * 
     * @param obj
     *            The object to be persisted to the database
     */
    public void persist(final Object obj) throws TransactionException {
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            public void doInTransactionWithoutResult(TransactionStatus status) {
                getHibernateTemplate().saveOrUpdate(obj);
            }
        });
    }

    /**
     * Persists or updates an object to the database using the provided
     * Hibernate mapping file for the object
     * 
     * @param obj
     *            The object to be persisted to the database
     */
    public <T> void saveOrUpdate(final PersistableDataObject<T> obj) {
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            public void doInTransactionWithoutResult(TransactionStatus status) {
                getHibernateTemplate().saveOrUpdate(obj);
            }
        });
    }

    /**
     * Creates the object entry in the database
     * 
     * @param obj
     *            The object to be created in the database
     */
    public void create(final Object obj) {
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            public void doInTransactionWithoutResult(TransactionStatus status) {
                getHibernateTemplate().save(obj);
            }
        });
    }

    /**
     * Updates an object in the database using the provided Hibernate mapping
     * file for the object
     * 
     * @param obj
     *            The object to be persisted to the database
     */
    public <T> void update(final PersistableDataObject<T> obj) {
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            public void doInTransactionWithoutResult(TransactionStatus status) {
                getHibernateTemplate().update(obj);
            }
        });
    }

    /**
     * Persists all objects in collection using a single transaction.
     * 
     * @param obj
     *            The object to be persisted to the database
     */
    public void persistAll(final Collection<? extends Object> objs) {
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            public void doInTransactionWithoutResult(TransactionStatus status) {
                HibernateTemplate ht = getHibernateTemplate();
                ht.saveOrUpdateAll(objs);
            }
        });
    }

    public List<Object> loadAll() {
        return loadAll(daoClass);
    }

    @SuppressWarnings("unchecked")
    public List<Object> loadAll(final Class<?> entity) {
        return (List<Object>) txTemplate.execute(new TransactionCallback() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                HibernateTemplate ht = getHibernateTemplate();
                return ht.loadAll(entity);
            }
        });
    }

    private static final String mergeSqlFormat = "select id from awips.%s where dataURI=:dataURI";

    public <T> List<PersistableDataObject<T>> mergeAll(
            final List<PersistableDataObject<T>> obj) {
        List<PersistableDataObject<T>> duplicates = new ArrayList<PersistableDataObject<T>>();
        Session s = this.getHibernateTemplate().getSessionFactory()
                .openSession();
        Transaction tx = s.beginTransaction();
        try {
            Map<String, Query> pluginQueryMap = new HashMap<String, Query>();
            for (PersistableDataObject<T> pdo : obj) {
                if (pdo == null) {
                    logger.error("Attempted to insert null PersistableDataObject");
                    continue;
                }
                String plugin = ((PluginDataObject) pdo).getPluginName();
                Query q = pluginQueryMap.get(plugin);
                if (q == null) {
                    q = s.createSQLQuery(String.format(mergeSqlFormat, plugin));
                    pluginQueryMap.put(plugin, q);
                }
                q.setString("dataURI", (String) pdo.getIdentifier());
                if (q.list().size() == 0) {
                    s.persist(pdo);
                } else {
                    if (!pdo.isOverwriteAllowed()) {
                        duplicates.add(pdo);
                    } else {
                        statusHandler.handle(Priority.DEBUG, "Overwriting "
                                + pdo.getIdentifier());
                    }
                }
            }
            tx.commit();
        } catch (Throwable e) {
            // TODO
            e.printStackTrace();
            tx.rollback();
        } finally {
            if (s != null) {
                s.close();
            }
        }
        return duplicates;
    }

    /**
     * Deletes an object from the database
     * 
     * @param obj
     *            The object to delete
     */
    public <T> void delete(final PersistableDataObject<T> obj) {
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            public void doInTransactionWithoutResult(TransactionStatus status) {
                getHibernateTemplate().delete(obj);
            }
        });
    }

    /**
     * Retrieves a unique object based on the id field specified in the
     * Hibernate mapping file for the specified object
     * 
     * @param id
     *            The id value of the object
     * @return The object with the matching id.<br>
     *         Null if not found
     */
    public <T> PersistableDataObject<T> queryById(final Serializable id) {
        @SuppressWarnings("unchecked")
        PersistableDataObject<T> retVal = (PersistableDataObject<T>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public PersistableDataObject<T> doInTransaction(
                            TransactionStatus status) {
                        return (PersistableDataObject<T>) getHibernateTemplate()
                                .get(daoClass, id);
                    }
                });
        return retVal;
    }

    /**
     * Retrieves a persitant object based on the given id
     * 
     * @param id
     *            The id
     * @return The object
     */
    public <T> PersistableDataObject<T> queryById(final PluginDataObject id) {
        @SuppressWarnings("unchecked")
        PersistableDataObject<T> retVal = (PersistableDataObject<T>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public PersistableDataObject<T> doInTransaction(
                            TransactionStatus status) {
                        DetachedCriteria criteria = DetachedCriteria.forClass(
                                id.getClass())
                                .add(Property.forName("dataURI").eq(
                                        id.getDataURI()));
                        List<?> list = getHibernateTemplate().findByCriteria(
                                criteria);
                        if (list.size() > 0)
                            return (PluginDataObject) list.get(0);
                        else
                            return null;
                    }
                });
        return retVal;
    }

    /**
     * Retrieves a list of objects based on a partially populated class.
     * Hibernate will find objects similar to the partially populated object
     * passed in. This method places a limit on the maximum number of results
     * returned.
     * 
     * @param obj
     *            The partially populated object
     * @param maxResults
     *            Maximum number of results to return
     * @return A list of similar objects
     */
    @SuppressWarnings("unchecked")
    public <T> List<PersistableDataObject<T>> queryByExample(
            final PersistableDataObject<T> obj, final int maxResults) {
        List<PersistableDataObject<T>> retVal = (List<PersistableDataObject<T>>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public List<PersistableDataObject<T>> doInTransaction(
                            TransactionStatus status) {
                        return getHibernateTemplate().findByExample(obj, 0,
                                maxResults);
                    }
                });
        return retVal;
    }

    /**
     * Retrieves a list of objects based on a partially populated class.
     * Hibernate will find objects similar to the partially populated object
     * passed in. This method does not place a limit on the maximum number of
     * results returned.
     * 
     * @param obj
     *            The partially populated object
     * @return A list of similar objects
     */
    public <T> List<PersistableDataObject<T>> queryByExample(
            PersistableDataObject<T> obj) {
        return queryByExample(obj, -1);
    }

    /**
     * Deletes data from the database using a DatabaseQuery object
     * 
     * @param query
     *            The query object
     * @return The results of the query
     * @throws DataAccessLayerException
     *             If the query fails
     */
    public int deleteByCriteria(final DatabaseQuery query)
            throws DataAccessLayerException {
        int rowsDeleted = 0;
        try {
            // Get a session and create a new criteria instance
            rowsDeleted = (Integer) txTemplate
                    .execute(new TransactionCallback() {
                        @Override
                        public Integer doInTransaction(TransactionStatus status) {
                            String queryString = query.createHQLDelete();
                            Query hibQuery = getSession(false).createQuery(
                                    queryString);
                            try {
                                query.populateHQLQuery(hibQuery,
                                        getSessionFactory());
                            } catch (DataAccessLayerException e) {
                                throw new org.hibernate.TransactionException(
                                        "Error populating delete statement", e);
                            }
                            return hibQuery.executeUpdate();
                        }
                    });
        } catch (TransactionException e) {
            throw new DataAccessLayerException("Transaction failed", e);
        }
        return rowsDeleted;
    }

    /**
     * Queries the database using a DatabaseQuery object
     * 
     * @param query
     *            The query object
     * @return The results of the query
     * @throws DataAccessLayerException
     *             If the query fails
     */
    public List<?> queryByCriteria(final DatabaseQuery query)
            throws DataAccessLayerException {
        List<?> queryResult = null;
        try {
            // Get a session and create a new criteria instance
            queryResult = (List<?>) txTemplate
                    .execute(new TransactionCallback() {
                        @Override
                        public List<?> doInTransaction(TransactionStatus status) {
                            String queryString = query.createHQLQuery();
                            Query hibQuery = getSession(false).createQuery(
                                    queryString);
                            try {
                                query.populateHQLQuery(hibQuery,
                                        getSessionFactory());
                            } catch (DataAccessLayerException e) {
                                throw new org.hibernate.TransactionException(
                                        "Error populating query", e);
                            }
                            // hibQuery.setCacheMode(CacheMode.NORMAL);
                            // hibQuery.setCacheRegion(QUERY_CACHE_REGION);
                            if (query.getMaxResults() != null) {
                                hibQuery.setMaxResults(query.getMaxResults());
                            }
                            List<?> results = hibQuery.list();
                            return results;
                        }
                    });

        } catch (TransactionException e) {
            throw new DataAccessLayerException("Transaction failed", e);
        }
        return queryResult;
    }

    public void deleteAll(final List<?> objs) {
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            public void doInTransactionWithoutResult(TransactionStatus status) {
                getHibernateTemplate().deleteAll(objs);
            }
        });
    }

    /**
     * 
     * @param fields
     * @param values
     * @param operands
     * @param resultCount
     * @param orderBy
     * @param orderAscending
     * @param distinctProperties
     * @return
     * @throws DataAccessLayerException
     */
    public List<?> queryByCriteria(final List<String> fields,
            final List<Object> values, final List<String> operands,
            final Integer resultCount, final String orderBy,
            final boolean orderAscending, final List<String> distinctProperties)
            throws DataAccessLayerException {

        DatabaseQuery query = new DatabaseQuery(this.daoClass.getName());
        query.addDistinctParameter(distinctProperties);
        query.addOrder(orderBy, orderAscending);
        for (int i = 0; i < fields.size(); i++) {
            QueryParam param = new QueryParam(fields.get(i), values.get(i));
            if (operands != null) {
                param.setOperand(operands.get(i));
            }
            query.addQueryParam(param);
        }

        return queryByCriteria(query);
    }

    /**
     * Retrieves a list of objects based on field names, values, and operands.<br>
     * This method is the core query method.
     * 
     * @param parameterMap
     *            A map containing <fieldName,value> pairs
     * @param operandMap
     *            A map containing <fieldName,operand> pairs. This is an
     *            optional argument and may be null. All operands will be
     *            assumed as =
     * @param resultCount
     *            The limiting number of results. This is an optional argument
     *            and may be null.
     * @param orderBy
     *            The field to order the results on. This is an optional
     *            argument and may be null.
     * @param orderAscending
     *            If an property to order by is specified, this argument must be
     *            provided. True for ascending order. False for descending
     *            order.
     * @return The list of results matching specified criteria
     */
    public List<?> queryByCriteria(final List<String> fields,
            final List<Object> values, final List<String> operands,
            final Integer resultCount, final String orderBy,
            final boolean orderAscending) throws DataAccessLayerException {
        return queryByCriteria(fields, values, operands, resultCount, orderBy,
                orderAscending, null);
    }

    /**
     * Convenience method if result limiting is not necessary.
     * 
     * @param fields
     *            The fields to query against
     * @param values
     *            The corresponding values for these fields
     * @param operands
     *            The operands to use during the query
     * @param orderBy
     *            The field to order the results on. This is an optional
     *            argument and may be null.
     * @param orderAscending
     *            If an property to order by is specified, this argument must be
     *            provided. True for ascending order. False for descending
     *            order.
     * @return The list of results matching specified criteria
     */
    public List<?> queryByCriteria(List<String> fields, List<Object> values,
            List<String> operands, String orderBy, boolean orderAscending)
            throws DataAccessLayerException {
        return queryByCriteria(fields, values, operands, null, orderBy,
                orderAscending);
    }

    /**
     * Convenience method if sorting of results is not necessary
     * 
     * @param fields
     *            The fields to query against
     * @param values
     *            The corresponding values for these fields
     * @param operands
     *            The operands to use during the query
     * @return The list of results matching specified criteria
     */
    public List<?> queryByCriteria(List<String> fields, List<Object> values,
            List<String> operands) throws DataAccessLayerException {
        return queryByCriteria(fields, values, operands, null, null, false);
    }

    /**
     * Convenience method if sorting of results and equality is assumed for all
     * operands is not necessary
     * 
     * @param fields
     *            The fields to query against
     * @param values
     *            The corresponding values for these fields
     * @param operands
     *            The operands to use during the query
     * @return The list of results matching specified criteria
     */
    public List<?> queryByCriteria(List<String> fields, List<Object> values)
            throws DataAccessLayerException {
        return queryByCriteria(fields, values, null, null, null, false);
    }

    /**
     * Retrieves a list of objects based on a single field, value, and operand.
     * 
     * @param field
     *            The field to query against
     * @param value
     *            The value to query for
     * @param operand
     *            The operand to apply
     * @return The list of results matching the specified criteria
     */
    public List<?> queryBySingleCriteria(String field, String value,
            String operand) throws DataAccessLayerException {

        DatabaseQuery query = new DatabaseQuery(this.daoClass.getName());
        query.addQueryParam(field, value, QueryParam.translateOperand(operand));
        return queryByCriteria(query);
    }

    /**
     * Retrieves a list of objects based on a single field/value pair.
     * Convenience method assuming equality operand is used.
     * 
     * @param field
     *            The field to query against
     * @param value
     *            The value to query for
     * @return The list of results matching the specified criteria
     */
    public List<?> queryBySingleCriteria(String field, String value)
            throws DataAccessLayerException {
        return queryBySingleCriteria(field, value, "=");
    }

    /**
     * Executes a catalog query
     * 
     * @param parameterMap
     *            The parameters names and values to query against
     * @param operandMap
     *            The parameter name and operands to use
     * @param distinctName
     *            The name of the parameter to search for distinct values for
     * @return The list of objects found by the query
     */
    public List<?> queryCatalog(final List<String> fields,
            final List<Object> values, final List<String> operands,
            final String distinctName) throws DataAccessLayerException {
        ArrayList<String> distinctProperties = new ArrayList<String>();
        distinctProperties.add(distinctName);
        return this.queryByCriteria(fields, values, operands, null, null,
                false, distinctProperties);
    }

    public List<?> queryCatalog(final List<String> fields,
            final List<Object> values, final List<String> operands,
            final List<String> distinctNames) throws DataAccessLayerException {
        return this.queryByCriteria(fields, values, operands, null, null,
                false, distinctNames);
    }

    /**
     * Executes an HQL query
     * 
     * @param hqlQuery
     *            The HQL query string
     * @return The list of objects returned by the query
     */
    public QueryResult executeHQLQuery(final String hqlQuery) {

        QueryResult result = (QueryResult) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public QueryResult doInTransaction(TransactionStatus status) {
                        Query hibQuery = getSession(false)
                                .createQuery(hqlQuery);
                        // hibQuery.setCacheMode(CacheMode.NORMAL);
                        // hibQuery.setCacheRegion(QUERY_CACHE_REGION);
                        hibQuery.setCacheable(true);
                        List<?> queryResult = hibQuery.list();

                        QueryResultRow[] rows = new QueryResultRow[queryResult
                                .size()];
                        if (!queryResult.isEmpty()) {
                            if (queryResult.get(0) instanceof Object[]) {
                                for (int i = 0; i < queryResult.size(); i++) {
                                    QueryResultRow row = new QueryResultRow(
                                            (Object[]) queryResult.get(i));
                                    rows[i] = row;
                                }

                            } else {
                                for (int i = 0; i < queryResult.size(); i++) {
                                    QueryResultRow row = new QueryResultRow(
                                            new Object[] { queryResult.get(i) });
                                    rows[i] = row;
                                }
                            }
                        }
                        QueryResult result = new QueryResult();
                        String[] returnAliases = hibQuery.getReturnAliases();
                        if (returnAliases == null) {
                            result.addColumnName("record", 0);
                        } else {
                            for (int i = 0; i < returnAliases.length; i++) {
                                result.addColumnName(returnAliases[i], i);
                            }
                        }
                        result.setRows(rows);
                        return result;
                    }
                });
        return result;
    }

    /**
     * Executes an HQL statement
     * 
     * @param hqlStmt
     *            The HQL statement string
     * @return The results of the statement
     */
    public int executeHQLStatement(final String hqlStmt) {

        int queryResult = (Integer) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Integer doInTransaction(TransactionStatus status) {
                        Query hibQuery = getSession(false).createQuery(hqlStmt);
                        // hibQuery.setCacheMode(CacheMode.NORMAL);
                        // hibQuery.setCacheRegion(QUERY_CACHE_REGION);
                        return hibQuery.executeUpdate();
                    }
                });

        return queryResult;
    }

    /**
     * Queries for a resultset. The query should be a select statement<br>
     * 
     * @param sql
     *            An SQL query to execute
     * @return An array objects (multiple rows are returned as Object [ Object
     *         [] ]
     */
    public Object[] executeSQLQuery(final String sql) {

        long start = System.currentTimeMillis();
        List<?> queryResult = (List<?>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public List<?> doInTransaction(TransactionStatus status) {
                        return getSession(false).createSQLQuery(sql).list();
                    }
                });
        logger.debug("executeSQLQuery took: "
                + (System.currentTimeMillis() - start) + " ms");
        return queryResult.toArray();
    }

    public List<?> executeCriteriaQuery(final List<Criterion> criterion) {

        long start = System.currentTimeMillis();
        List<?> queryResult = (List<?>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public List<?> doInTransaction(TransactionStatus status) {

                        Criteria crit = getSession(false).createCriteria(
                                daoClass);
                        for (Criterion cr : criterion) {
                            crit.add(cr);
                        }
                        return crit.list();
                    }
                });
        logger.debug("executeCriteriaQuery took: "
                + (System.currentTimeMillis() - start) + " ms");
        return queryResult;
    }

    public List<?> executeCriteriaQuery(final Criterion criterion) {
        ArrayList<Criterion> criterionList = new ArrayList<Criterion>();
        criterionList.add(criterion);
        return executeCriteriaQuery(criterionList);
    }

    /**
     * Executes a an arbitrary sql statement. The sql should not be a select
     * statement.
     * 
     * @param sql
     *            An SQL statement to execute
     * @return How many items were updated
     */
    public int executeSQLUpdate(final String sql) {

        long start = System.currentTimeMillis();
        int updateResult = (Integer) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public Integer doInTransaction(TransactionStatus status) {
                        return getSession(false).createSQLQuery(sql)
                                .executeUpdate();
                    }
                });
        logger.debug("executeSQLUpdate took: "
                + (System.currentTimeMillis() - start) + " ms");
        return updateResult;
    }

    /**
     * Executes a native SQL statement. This method completely bypasses
     * Hibernate and uses JDBC directly
     * 
     * @param sql
     *            The sql string
     * @return A QueryResultObject if it was a query, else returns the number of
     *         rows modified
     * @throws DataAccessLayerException
     *             If the statement fails
     */
    public Object executeNativeSql(String sql, boolean transactional)
            throws DataAccessLayerException {
        Session session = null;
        Transaction trans = null;
        Connection conn = null;
        Statement stmt = null;
        SQLException exception = null;
        Object results = null;

        try {
            session = getSession(true);
            if (transactional) {
                trans = session.beginTransaction();
            }
            conn = session.connection();
            stmt = conn.createStatement();

        } catch (SQLException e) {
            throw new DataAccessLayerException(
                    "Unable to create JDBC statement", e);
        }
        try {
            boolean query = false;
            if (transactional) {
                query = stmt.execute(sql);
            } else {
                query = stmt.execute("COMMIT;" + sql);
            }
            if (query) {
                results = mapResultSet(stmt.getResultSet());
            } else {
                results = stmt.getUpdateCount();
            }

        } catch (SQLException e1) {
            exception = e1;
            if (transactional) {
                trans.rollback();
            }
            logger.error("Error executing script.", e1);
        }

        try {
            stmt.close();
        } catch (SQLException e1) {
            exception = e1;
            if (transactional) {
                trans.rollback();
            }
            logger.error("Unable to close JDBC statement!", e1);
        }

        if (exception == null && transactional) {
            trans.commit();
        }
        try {
            if (!conn.isClosed()) {
                conn.close();
            }
        } catch (SQLException e) {
            exception = e;
            logger.error("Cannot close database connection!!", e);
        }
        if (session.isOpen()) {
            session.close();
        }
        if (exception != null) {
            throw new DataAccessLayerException(
                    "Unable to execute SQL update statement", exception);
        }
        return results;
    }

    public Object executeNativeSql(String sql) throws DataAccessLayerException {
        return executeNativeSql(sql, true);
    }

    /**
     * Helper method for mapping JDBC result sets
     * 
     * @param rs
     *            The raw ResultSet object
     * @return The remapped results
     * @throws SQLException
     *             If mapping fails
     */
    private QueryResult mapResultSet(ResultSet rs) throws SQLException {
        QueryResult results = new QueryResult();

        ResultSetMetaData metadata = rs.getMetaData();

        int columnCount = metadata.getColumnCount();
        for (int i = 0; i < columnCount; i++) {
            results.addColumnName(metadata.getColumnLabel(i + 1), i);
        }

        List<QueryResultRow> rows = new ArrayList<QueryResultRow>();
        while (rs.next()) {
            Object[] columnValues = new Object[columnCount];
            for (int i = 1; i <= columnCount; i++) {
                columnValues[i - 1] = rs.getObject(i);

            }
            rows.add(new QueryResultRow(columnValues));
        }
        results.setRows(rows.toArray(new QueryResultRow[] {}));
        return results;
    }

    /**
     * Runs an SQL script
     * 
     * @param sqlScript
     *            The SQL script text
     * @throws DataAccessLayerException
     *             If the script fails to execute
     */
    public void runScript(String sqlScript) throws DataAccessLayerException {
        Session session = null;
        Transaction trans = null;
        Connection conn = null;

        Statement stmt = null;
        try {
            session = getSession(true);
            trans = session.beginTransaction();
            conn = session.connection();
            stmt = conn.createStatement();
        } catch (SQLException e) {
            throw new DataAccessLayerException(
                    "Unable to create JDBC statement", e);
        }
        boolean success = true;
        String[] scriptContents = sqlScript.split(";");
        for (String line : scriptContents) {
            if (!line.isEmpty()) {
                try {
                    stmt.addBatch(line + ";");

                } catch (SQLException e1) {
                    logger.warn("Script execution failed.  Rolling back transaction");
                    trans.rollback();
                    try {
                        if (!conn.isClosed()) {
                            conn.close();
                        }
                    } catch (SQLException e2) {
                        logger.error("Cannot close database connection!!", e2);
                    }
                    if (session.isOpen()) {
                        session.close();
                    }
                    throw new DataAccessLayerException(
                            "Cannot execute SQL statement: " + line, e1);
                }
            }
        }
        try {
            stmt.executeBatch();
        } catch (SQLException e1) {
            success = false;
            trans.rollback();
            logger.error("Error executing script.", e1);
        }

        try {
            stmt.close();
        } catch (SQLException e1) {
            success = false;
            trans.rollback();
            logger.error("Unable to close JDBC statement!", e1);
        }

        if (success) {
            trans.commit();
        }
        try {
            if (!conn.isClosed()) {
                conn.close();
            }
        } catch (SQLException e) {
            logger.error("Cannot close database connection!!", e);
        }
        if (session.isOpen()) {
            session.close();
        }
    }

    /**
     * Executes an SQL script contained in a StringBuffer object
     * 
     * @param sqlScript
     *            The SQL script
     * @throws DataAccessLayerException
     *             If script execution fails
     */
    public void runScript(StringBuffer sqlScript)
            throws DataAccessLayerException {
        runScript(sqlScript.toString());
    }

    /**
     * Executes an SQL script defined in a file
     * 
     * @param script
     *            The file containing the SQL statements
     * @throws DataAccessLayerException
     *             If reading the file fails
     */
    public void runScript(File script) throws DataAccessLayerException {
        FileInputStream fileIn;
        try {
            fileIn = new FileInputStream(script);
        } catch (FileNotFoundException e) {
            throw new DataAccessLayerException(
                    "Unable to open input stream to sql script: " + script);
        }
        byte[] bytes = null;
        try {
            bytes = new byte[fileIn.available()];
            fileIn.read(bytes);
        } catch (IOException e) {
            throw new DataAccessLayerException(
                    "Unable to read script contents for script: " + script);
        }
        try {
            fileIn.close();
        } catch (IOException e) {
            throw new DataAccessLayerException(
                    "Error closing file input stream to: " + script);
        }
        runScript(new StringBuffer().append(new String(bytes)));
    }

    /**
     * Gets the object class associated with this dao
     * 
     * @return The object class associated with this dao
     */
    public Class<?> getDaoClass() {
        return daoClass;
    }

    /**
     * Sets the object class associated with this dao
     * 
     * @param daoClass
     *            The object class to assign to this dao
     */
    public void setDaoClass(Class<?> daoClass) {
        this.daoClass = daoClass;
    }

    /**
     * Sets the dao class given a fully qualified class name
     * 
     * @param fqn
     *            The fully qualified class name
     */
    public void setDaoClass(String fqn) {
        try {
            daoClass = this.getClass().getClassLoader().loadClass(fqn);
        } catch (ClassNotFoundException e) {
            logger.warn("Unable to load class: " + fqn);
        }
    }

    public ClassMetadata getDaoClassMetadata() {
        if (daoClass == null) {
            return null;
        } else {
            return getSessionFactory().getClassMetadata(daoClass);
        }
    }
}
