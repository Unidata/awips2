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

import org.hibernate.Criteria;
import org.hibernate.LockOptions;
import org.hibernate.Query;
import org.hibernate.dialect.Dialect;
import org.hibernate.jdbc.Work;

import com.raytheon.uf.common.util.session.SessionContext;

/**
 * Defines the public API of a DAO session context.
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
public interface IDaoSessionContext extends SessionContext {

    /**
     * Create a query for execution given the specified string.
     * 
     * @param queryString
     *            the query string
     * @return the query
     */
    Query createQuery(final String queryString);

    /**
     * Create the query criteria for the specified class.
     * 
     * 
     * 
     * @param clazz
     *            the class of the entity type
     * @return the criteria
     */
    <T> Criteria createCriteria(Class<T> clazz);

    /**
     * Get an object exclusively.
     * 
     * @param <T>
     *            the type of the object
     * @param clazz
     *            the object class
     * @param pk
     *            the primary key
     * @param lockOptions
     *            the lock options
     * @return the object, or null if it can't be locked
     */
    <T> T get(Class<T> clazz, Serializable pk, LockOptions lockOptions);

    /**
     * Create the entity in the database.
     * 
     * @param obj
     */
    void create(Object obj);

    /**
     * Update the entity in the database.
     * 
     * @param rval
     */
    <T> void update(T rval);

    /**
     * Create, or update, an entity in the database.
     * 
     * @param objs
     */
    void createOrUpdate(Object obj);

    /**
     * Create, or update, all of the entities in the database.
     * 
     * @param objs
     */
    void persistAll(Collection<? extends Object> objs);

    /**
     * Do some database work.
     * 
     * @param work
     *            the work instance
     */
    void doWork(Work work);

    /**
     * Delete the entities from the database.
     * 
     * @param entities
     */
    void deleteAll(Collection<?> entities);

    /**
     * Delete the entity from the database.
     * 
     * @param entity
     */
    void delete(Object entity);

    /**
     * Get the Hibernate dialect.
     * 
     * @return
     */
    Dialect getDialect();
}