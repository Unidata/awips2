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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

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
public class SessionManagedDao {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SessionManagedDao.class);

    protected HibernateTemplate template;

    /**
     * Sets Hibernate session factory.
     */
    @Autowired
    public void setSessionFactory(SessionFactory sessionFactory) {
        template = new HibernateTemplate(sessionFactory);
    }

    /**
     * Creates the object entry in the database
     * 
     * @param obj
     *            The object to be created in the database
     */
    public void create(final Object obj) {
        template.save(obj);
    }

    /**
     * Updates the object entry in the database
     * 
     * @param obj
     *            The object to be created in the database
     */
    public void update(final Object obj) {
        template.update(obj);
    }

    /**
     * Creates the object entry in the database
     * 
     * @param obj
     *            The object to be created in the database
     */
    public void createOrUpdate(final Object obj) {
        template.saveOrUpdate(obj);
    }

    /**
     * Persists all objects in the collection.
     * 
     * @param objs
     *            The objects to be persisted to the database
     */
    public void persistAll(final Collection<?> objs) {
        for (Object obj : objs) {
            createOrUpdate(obj);
        }
    }

    /**
     * Deletes the object entry in the database
     * 
     * @param obj
     *            The object to be created in the database
     */
    public void delete(final Object obj) {
        Object toDelete = template.merge(obj);
        template.delete(toDelete);
    }

    /**
     * Delete all of the entities.
     * 
     * @param objs
     */
    public void deleteAll(final Collection<?> objs) {
        for (Object obj : objs) {
            delete(obj);
        }
    }

    /**
     * Internal convenience method for querying.
     * 
     * @param <T>
     * @param queryString
     * @param params
     * @return
     */
    @SuppressWarnings("unchecked")
    protected <T extends Object> List<T> query(String queryString,
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
     * @param <T>
     * @param queryString
     * @param params
     * @return
     */
    @SuppressWarnings("unchecked")
    protected <T extends Object> T uniqueResult(String queryString,
            Map<String, Object> params) {
        final List<Object> results = query(queryString, params);
        if (results.isEmpty()) {
            return null;
        } else if (results.size() > 1) {
            statusHandler.warn("More than one result returned for query ["
                    + queryString + "], only returning the first!");
        }
        return (T) results.get(0);
    }
}
