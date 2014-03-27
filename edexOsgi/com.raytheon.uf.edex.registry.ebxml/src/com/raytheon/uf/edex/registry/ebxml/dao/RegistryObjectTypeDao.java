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

import java.util.Arrays;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.hibernate.SessionFactory;
import org.hibernate.criterion.Property;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

/**
 * Data access object used for accessing RegistryObjectType objects from the
 * underlying database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2012 184        bphillip     Initial creation
 * 4/9/2013     1802       bphillip    Removed exception catching.  Added merge method.
 * 8/1/2013     1693       bphillip    Moved the merge method down to RegistryObjectDao
 * 10/8/2013    1682       bphillip    Added like lid method, changed to use criteria queries for simple operations
 * 2/13/2014    2769       bphillip    Added read only flags to query methods
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public abstract class RegistryObjectTypeDao<ENTITY extends RegistryObjectType>
        extends IdentifiableTypeDao<ENTITY> {

    /**
     * Creates a new RegistryObjectDao
     */
    public RegistryObjectTypeDao() {
    }

    /**
     * Retrieves registry objects based on lid values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param lids
     *            The list of lids to query for
     * @return The list of registry objects;
     */
    @SuppressWarnings("unchecked")
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<ENTITY> getByLid(List<String> lids) {
        return createCriteria().add(
                Property.forName(QueryConstants.LID).in(lids)).list();
    }

    /**
     * Retrieves registry objects based on lid values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param lids
     *            The list of lids to query for
     * @return The list of registry objects;
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<ENTITY> getByLid(String... lids) {
        return getByLid(Arrays.asList(lids));
    }

    /**
     * Gets all IdentifiableType objects matching (using like) the given lid.
     * 
     * @param lid
     *            The id to query for
     * @return All IdentifiableType objects matching the given lid
     */
    @SuppressWarnings("unchecked")
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<ENTITY> getByLidUsingLike(String lid) {
        return createCriteria().add(
                Property.forName(QueryConstants.LID).like(lid)).list();
    }

    /**
     * Retrieves registry objects based on objectType values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param objTypes
     *            The list of objectTypes to query for
     * @return The list of registry objects;
     */
    @SuppressWarnings("unchecked")
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<ENTITY> getByObjectType(List<String> objTypes) {
        return createCriteria().add(
                Property.forName(QueryConstants.OBJECT_TYPE).in(objTypes))
                .list();
    }

    /**
     * Retrieves registry objects based on status values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param status
     *            The list of statuses to query for
     * @return The list of registry objects;
     */
    @SuppressWarnings("unchecked")
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<ENTITY> getByStatus(List<String> status) {
        return createCriteria().add(
                Property.forName(QueryConstants.STATUS).in(status)).list();
    }

    /**
     * Retrieves registry objects based on owner values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param owner
     *            The list of owners to query for
     * @return The list of registry objects;
     */
    @SuppressWarnings("unchecked")
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<ENTITY> getByOwner(List<String> owner) {
        return createCriteria().add(
                Property.forName(QueryConstants.OWNER).in(owner)).list();
    }

    /**
     * Retrieves registry objects based on owner values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param owner
     *            The list of owners to query for
     * @return The list of registry objects;
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<ENTITY> getByOwner(String... owner) {
        return getByOwner(Arrays.asList(owner));
    }

    /**
     * Retrieves registry objects based on name values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param ids
     *            The list of names to query for
     * @return The list of registry objects;
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<ENTITY> getByName(List<String> names) {
        StringBuilder str = new StringBuilder(
                "select obj from RegistryObjectType obj inner join obj.name.localizedString as Strings where Strings.value in ");
        HqlQueryUtil.assembleInClause(str, "Strings.value", names);
        return this.executeHQLQuery(str.toString());
    }

    /**
     * Retrieves registry objects based on description values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param ids
     *            The list of descriptions to query for
     * @return The list of registry objects;
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<ENTITY> getByDescription(List<String> descriptions) {
        StringBuilder str = new StringBuilder(
                "select obj from RegistryObjectType obj inner join obj.description.localizedString as Strings where Strings.value in ");
        HqlQueryUtil.assembleInClause(str, "Strings.value", descriptions);
        return this.executeHQLQuery(str.toString());
    }

    /**
     * Retrieves registry objects based on classificationNode values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param ids
     *            The list of classificationNodes to query for
     * @return The list of registry objects;
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<ENTITY> getByClassification(List<String> classifications) {
        StringBuilder str = new StringBuilder(
                "select obj from RegistryObjectType obj inner join obj.classification as Classifications where ");
        HqlQueryUtil.assembleInClause(str,
                "Classifications.classificationNode", classifications);

        return this.executeHQLQuery(str.toString());
    }

    /**
     * Convenience method used by queries which return a single result. The
     * method takes a list and returns null if the list is empty
     * 
     * @param <T>
     *            Class type extending RegistryObjectType
     * @param result
     *            The result
     * @return null if the given list is empty, else returns the first element
     *         of the list
     */
    protected static RegistryObjectType getSingleResult(
            List<RegistryObjectType> result) {
        if (result.isEmpty()) {
            return null;
        } else {
            return result.get(0);
        }
    }

    @Override
    public void setSessionFactory(SessionFactory sessionFactory) {
        template = new HibernateTemplate(sessionFactory);
    }

    @Override
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public ENTITY getById(String id) {
        return super.getById(id);
    }
}
