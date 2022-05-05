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

import java.beans.PropertyDescriptor;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.beanutils.PropertyUtils;
import org.hibernate.Hibernate;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.Property;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.services.rest.response.RegObjectSubset;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

/**
 * Data access object used for accessing RegistryObjectType objects from the
 * underlying database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------------------------------
 * Jan 19, 2012  184      bphillip  Initial creation
 * Apr 09, 2013  1802     bphillip  Removed exception catching.  Added merge method.
 * Aug 01, 2013  1693     bphillip  Moved the merge method down to RegistryObjectDao
 * Oct 08, 2013  1682     bphillip  Added like lid method, changed to use criteria queries for
 *                                  simple operations
 * Feb 13, 2014  2769     bphillip  Added read only flags to query methods
 * Oct 16, 2014  3454     bphillip  Upgrading to Hibernate 4
 * Aug 25, 2016  5846     rjpeter   Remove InternationalString from DB
 * Jan 14, 2019  7238     skabasele Added getIdsOwnerUpdateTimeOfType method
 * Sep 09, 2021  8656     rjpeter   Add getByIdFullyInitialized.
 * 
 * </pre>
 * 
 * @author bphillip
 */

public abstract class RegistryObjectTypeDao<ENTITY extends RegistryObjectType>
        extends IdentifiableTypeDao<ENTITY> {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

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
        return createCriteria()
                .add(Property.forName(QueryConstants.LID).in(lids)).list();
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
        return createCriteria()
                .add(Property.forName(QueryConstants.LID).like(lid)).list();
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
        return createCriteria()
                .add(Property.forName(QueryConstants.OBJECT_TYPE).in(objTypes))
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
        return createCriteria()
                .add(Property.forName(QueryConstants.STATUS).in(status)).list();
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
        return createCriteria()
                .add(Property.forName(QueryConstants.OWNER).in(owner)).list();
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
                "select obj from RegistryObjectType obj where obj.name in ");
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
                "select obj from RegistryObjectType obj where obj.description in ");
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
        HqlQueryUtil.assembleInClause(str, "Classifications.classificationNode",
                classifications);

        return this.executeHQLQuery(str.toString());
    }

    /**
     * Gets the object ids , owner and updateTime of objects of the given object
     * type
     * 
     * @param objectType
     *            The object type to get the ids for
     * @return The list of object RegObjectSubset of objects of the given type
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<RegObjectSubset> getIdsOwnerUpdateTimeOfType(
            String objectType) {
        String query = "SELECT regObj.id, regObj.owner, regObj.updateTime FROM RegistryObjectType regObj WHERE regObj.objectType=:objectType";

        List<Object[]> queryResult = this.executeHQLQuery(query, "objectType",
                objectType);

        List<RegObjectSubset> regObSubsetList = new ArrayList<>(
                queryResult.size());
        if (!queryResult.isEmpty()) {

            for (int j = 0; j < queryResult.size(); j++) {
                Object[] columns = queryResult.get(j);
                String id = (String) columns[0];
                String owner = (String) columns[1];
                Date updateTime = (Date) columns[2];

                RegObjectSubset regObjSubset = new RegObjectSubset();
                regObjSubset.setId(id);
                regObjSubset.setOwner(owner);
                regObjSubset.setUpdateTime(updateTime);
                regObSubsetList.add(regObjSubset);

            }

        }

        return regObSubsetList;

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

    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<ENTITY> getByObjectId(List<String> Ids) {
        StringBuilder str = new StringBuilder(
                "select obj from RegistryObjectType obj where obj.id in ");
        HqlQueryUtil.assembleInClause(str, "Strings.value", Ids);

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
        this.sessionFactory = sessionFactory;
    }

    @Override
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public ENTITY getById(String id) {
        return super.getById(id);
    }

    /**
     * Fully loads an object and all its relations. Should be used when passing
     * data to other threads that will be outside the original transaction.
     *
     * @param id
     * @return
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public ENTITY getByIdFullyInitialized(String id) {
        ENTITY rval = getById(id);
        recursiveInitialize(rval);
        return rval;
    }

    /**
     * Method to walk through nested objects until all data is loaded. Only
     * necessary with objects tagged as lazy fetching so the database session
     * can be closed.
     * 
     * @param obj
     */
    protected <T> T recursiveInitialize(T obj) {
        Set<Object> dejaVu = Collections
                .newSetFromMap(new IdentityHashMap<Object, Boolean>());
        recursiveInitialize(obj, dejaVu);
        return obj;
    }

    private void recursiveInitialize(Object obj, Set<Object> dejaVu) {
        if (obj == null) {
            return;
        }

        // check for circular reference
        if (dejaVu.contains(obj)) {
            return;
        } else {
            dejaVu.add(obj);

            if (!Hibernate.isInitialized(obj)) {
                Hibernate.initialize(obj);
            }

            if (obj instanceof Collection) {
                for (Object item : (Collection<?>) obj) {
                    this.recursiveInitialize(item, dejaVu);
                }
            } else if (obj instanceof Map) {
                for (Object item : ((Map<?, ?>) obj).values()) {
                    this.recursiveInitialize(item, dejaVu);
                }
            } else if (obj.getClass().isArray()) {
                int length = Array.getLength(obj);
                for (int i = 0; i < length; i++) {
                    Object item = Array.get(obj, i);
                    this.recursiveInitialize(item, dejaVu);
                }
            } else {
                PropertyDescriptor[] properties = PropertyUtils
                        .getPropertyDescriptors(obj);
                for (PropertyDescriptor propertyDescriptor : properties) {
                    if (PropertyUtils.isReadable(obj,
                            propertyDescriptor.getName())) {
                        try {
                            Object origProp = PropertyUtils.getProperty(obj,
                                    propertyDescriptor.getName());
                            this.recursiveInitialize(origProp, dejaVu);
                        } catch (IllegalAccessException
                                | InvocationTargetException
                                | NoSuchMethodException e) {
                            // property wasn't readable, skip
                        }

                    }
                }
            }
        }
    }
}
