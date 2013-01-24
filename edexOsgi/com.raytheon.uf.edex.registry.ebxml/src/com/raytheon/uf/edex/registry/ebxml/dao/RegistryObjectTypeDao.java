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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExternalIdentifierType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExternalLinkType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.hibernate.Session;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Property;

import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class RegistryObjectTypeDao extends RegistryDao {

    /**
     * Creates a new RegistryObjectDao
     */
    public RegistryObjectTypeDao() {
        super(RegistryObjectType.class);
    }

    /**
     * Creates a new RegistryObjectDao for the provided class
     * 
     * @param <T>
     *            Class extending RegistryObjectType
     * @param clazz
     *            The class for which to create this RegistryObjectTypeDao for
     */
    public <T extends RegistryObjectType> RegistryObjectTypeDao(Class<T> clazz) {
        super(clazz);
    }

    /**
     * Queries for all ids of registry objects matching the pattern of the given
     * id. A query using 'like' will be executed.
     * 
     * @param id
     *            The id containing % or _ denoting wildcard characters
     * @return List of ids matching the given id pattern
     * @throws EbxmlRegistryException
     *             If errors occur during the query
     */
    public List<String> getMatchingIds(String id) throws EbxmlRegistryException {
        DetachedCriteria criteria = DetachedCriteria.forClass(this.daoClass);
        criteria = criteria.add(Property.forName(QueryConstants.ID).like(id));
        criteria = criteria.setProjection(Projections
                .property(QueryConstants.ID));
        return this.executeCriteriaQuery(criteria);
    }

    /**
     * Queries for all lids of registry objects matching the pattern of the
     * given id. A query using 'like' will be executed.
     * 
     * @param lid
     *            The lid containing % or _ denoting wildcard characters
     * @return List of lids matching the given id pattern
     * @throws EbxmlRegistryException
     *             If errors occur during the query
     */
    public List<String> getMatchingLids(String lid)
            throws EbxmlRegistryException {
        DetachedCriteria criteria = DetachedCriteria.forClass(this.daoClass);
        criteria = criteria.add(Property.forName(QueryConstants.LID).like(lid));
        criteria = criteria.setProjection(Projections
                .property(QueryConstants.LID));
        return this.executeCriteriaQuery(criteria);
    }

    /**
     * Retrieves all registry objects from the registry
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @return All the registry objects contained in the registry
     * @throws EbxmlRegistryException
     *             If the HQL query fails
     */
    public <T extends RegistryObjectType> List<T> getAllRegistryObjects()
            throws EbxmlRegistryException {
        return executeHQLQuery("from" + RegistryObjectType.class.getName());
    }

    /**
     * Retrieves registry objects based on id values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param ids
     *            The list of ids to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getById(List<String> ids)
            throws EbxmlRegistryException {
        return this.executeHQLQuery(HqlQueryUtil.assembleSingleParamQuery(
                daoClass, QueryConstants.ID, "in", ids));
    }

    /**
     * Retrieves registry objects based on id values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param ids
     *            The array of ids to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getById(String... ids)
            throws EbxmlRegistryException {
        return getById(Arrays.asList(ids));
    }

    /**
     * Retrieves the registry object associated with the given id value
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param id
     *            The ide to get the registry object for
     * @return The registry object with the given id
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    @SuppressWarnings("unchecked")
    public <T extends RegistryObjectType> T getById(String id)
            throws EbxmlRegistryException {
        return (T) getSingleResult(getById(Arrays.asList(id)));
    }

    /**
     * Retrieves registry objects based on lid values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param lids
     *            The list of lids to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getByLid(List<String> lids)
            throws EbxmlRegistryException {
        StringBuilder str = new StringBuilder();
        HqlQueryUtil.assembleSingleParamQuery(str, daoClass,
                QueryConstants.LID, "in", lids);
        str.append(" order by obj.lid asc,obj.versionInfo.versionNumber desc");
        return executeHQLQuery(str.toString());
    }

    /**
     * Retrieves registry objects based on lid values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param lids
     *            The list of lids to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getByLid(String... lids)
            throws EbxmlRegistryException {
        return getByLid(Arrays.asList(lids));
    }

    /**
     * Retrieves all registry objects assigned the given lid
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param lid
     *            The lid to query for
     * @return The list of registry objects containing the given lid
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    @SuppressWarnings("unchecked")
    public <T extends RegistryObjectType> T getByLid(String lid)
            throws EbxmlRegistryException {
        return (T) getSingleResult(getByLid(Arrays.asList(lid)));

    }

    /**
     * Retrieves registry objects based on objectType values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param objTypes
     *            The list of objectTypes to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getByObjectType(
            List<String> objTypes) throws EbxmlRegistryException {
        return executeHQLQuery(HqlQueryUtil.assembleSingleParamQuery(daoClass,
                QueryConstants.OBJECT_TYPE, "in", objTypes));
    }

    /**
     * Retrieves registry objects based on objectType values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param objTypes
     *            The list of objectTypes to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getByObjectType(
            String... objTypes) throws EbxmlRegistryException {
        return getByObjectType(Arrays.asList(objTypes));
    }

    /**
     * Retrieves registry objects based on objectType values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param objType
     *            The objectType to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    @SuppressWarnings("unchecked")
    public <T extends RegistryObjectType> T getByObjectType(String objType)
            throws EbxmlRegistryException {
        return (T) getSingleResult(getByObjectType(Arrays.asList(objType)));
    }

    /**
     * Retrieves registry objects based on status values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param status
     *            The list of statuses to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getByStatus(
            List<String> status) throws EbxmlRegistryException {
        return executeHQLQuery(HqlQueryUtil.assembleSingleParamQuery(daoClass,
                QueryConstants.STATUS, "in", status));
    }

    /**
     * Retrieves registry objects based on status values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param status
     *            The list of statuses to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getByStatus(String... status)
            throws EbxmlRegistryException {
        return getByStatus(Arrays.asList(status));
    }

    /**
     * Retrieves registry objects based on status values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param status
     *            The statuse to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    @SuppressWarnings("unchecked")
    public <T extends RegistryObjectType> T getByStatus(String status)
            throws EbxmlRegistryException {
        return (T) getSingleResult(getByStatus(Arrays.asList(status)));
    }

    /**
     * Retrieves registry objects based on owner values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param owner
     *            The list of owners to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getByOwner(List<String> owner)
            throws EbxmlRegistryException {
        return executeHQLQuery(HqlQueryUtil.assembleSingleParamQuery(daoClass,
                QueryConstants.OWNER, "in", owner));
    }

    /**
     * Retrieves registry objects based on owner values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param owner
     *            The list of owners to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getByOwner(String... owner)
            throws EbxmlRegistryException {
        return getByOwner(Arrays.asList(owner));
    }

    /**
     * Retrieves registry objects based on owner values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param owner
     *            The owner to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the query encounters errors
     */
    @SuppressWarnings("unchecked")
    public <T extends RegistryObjectType> T getByOwner(String owner)
            throws EbxmlRegistryException {
        return (T) getSingleResult(getByOwner(Arrays.asList(owner)));
    }

    /**
     * Retrieves registry objects based on name values
     * 
     * @param <T>
     *            A class type extending RegistryObjectType
     * @param ids
     *            The list of names to query for
     * @return The list of registry objects;
     * @throws EbxmlRegistryException
     *             If the criteria query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getByName(List<String> names)
            throws EbxmlRegistryException {
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
     * @throws EbxmlRegistryException
     *             If the criteria query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getByDescription(
            List<String> descriptions) throws EbxmlRegistryException {
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
     * @throws EbxmlRegistryException
     *             If the criteria query encounters errors
     */
    public <T extends RegistryObjectType> List<T> getByClassification(
            List<String> classifications) throws EbxmlRegistryException {
        StringBuilder str = new StringBuilder(
                "select obj from RegistryObjectType obj inner join obj.classification as Classifications where ");
        HqlQueryUtil.assembleInClause(str,
                "Classifications.classificationNode", classifications);
        return this.executeHQLQuery(str.toString());
    }

    /**
     * Deletes objects based on the reference (id field)
     * 
     * @param objRefs
     *            The objectRefs pointing to objects to be deleted
     * @throws EbxmlRegistryException
     *             If the delete fails
     */
    public void deleteByRefs(final List<ObjectRefType> objRefs)
            throws EbxmlRegistryException {

        try {
            doInTransaction(new RegistryTransactionCallback() {
                @Override
                public Object execute(Session session)
                        throws EbxmlRegistryException {
                    List<String> objIds = new ArrayList<String>();
                    for (ObjectRefType ref : objRefs) {
                        objIds.add(ref.getId());
                    }
                    List<RegistryObjectType> objs = getById(objIds);
                    for (RegistryObjectType regObj : objs) {
                        if (regObj.getClassification() != null) {
                            for (ClassificationType classification : regObj
                                    .getClassification()) {
                                delete(classification);
                            }
                        }
                    }
                    for (RegistryObjectType regObj : objs) {
                        if (regObj.getExternalIdentifier() != null) {
                            for (ExternalIdentifierType extId : regObj
                                    .getExternalIdentifier()) {
                                delete(extId);
                            }
                        }
                    }
                    for (RegistryObjectType regObj : objs) {
                        if (regObj.getExternalLink() != null) {
                            for (ExternalLinkType extLink : regObj
                                    .getExternalLink()) {
                                delete(extLink);
                            }
                        }
                    }
                    delete(objs);
                    return null;
                }
            });
        } catch (Throwable e) {
            throw new EbxmlRegistryException("Error deleting objects", e);
        }
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
    protected static <T extends RegistryObjectType> T getSingleResult(
            List<T> result) {
        if (result.isEmpty()) {
            return null;
        } else {
            return result.get(0);
        }
    }
}
