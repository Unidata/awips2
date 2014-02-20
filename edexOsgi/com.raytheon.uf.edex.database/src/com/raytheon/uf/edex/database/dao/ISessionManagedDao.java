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
import java.util.List;

import org.hibernate.SessionFactory;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;

/**
 * Interface defining a session managed dao.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2013 1543       djohnson     Initial creation
 * 2/19/2014    2769        bphillip   Added loadById method    
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 * @param <IDENTIFIER>
 *            the entity identifier type
 * @param <ENTITY>
 *            the entity type
 */
public interface ISessionManagedDao<IDENTIFIER extends Serializable, ENTITY extends IPersistableDataObject<IDENTIFIER>> {

    /**
     * Sets Hibernate session factory.
     */
    void setSessionFactory(SessionFactory sessionFactory);

    /**
     * Creates the object entry in the database
     * 
     * @param obj
     *            The object to be created in the database
     */
    void create(final ENTITY obj);

    /**
     * Updates the object entry in the database
     * 
     * @param obj
     *            The object to be created in the database
     */
    void update(final ENTITY obj);

    /**
     * Creates the object entry in the database
     * 
     * @param obj
     *            The object to be created in the database
     */
    void createOrUpdate(final ENTITY obj);

    /**
     * Persists all objects in the collection.
     * 
     * @param objs
     *            The objects to be persisted to the database
     */
    void persistAll(final Collection<ENTITY> objs);

    /**
     * Deletes the object entry in the database
     * 
     * @param obj
     *            The object to be created in the database
     */
    void delete(final ENTITY obj);

    /**
     * Delete all of the entities.
     * 
     * @param objs
     */
    void deleteAll(final Collection<ENTITY> objs);

    /**
     * Get an entity by its id.
     * 
     * @param id
     *            the id
     * @return the entity
     */
    ENTITY getById(IDENTIFIER id);

    /**
     * Load an entity by its id. This method differs from the getById method in
     * that it only returns a proxy. The database is not actually hit until the
     * object is used.
     * 
     * @param id
     *            the id
     * @return the entity
     */
    ENTITY loadById(IDENTIFIER id);

    /**
     * Get all of the entities of this type.
     * 
     * @return the entities
     */
    List<ENTITY> getAll();
}