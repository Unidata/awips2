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

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.hibernate.SessionFactory;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Property;
import org.springframework.orm.hibernate3.HibernateTemplate;

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
 * 4/9/2013     1802       bphillip    Removed exception catching.  Added merge method.
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
     * Merges the state of the new object onto the persistent object
     * 
     * @param newObject
     *            The object to get the state from
     * @param existingObject
     *            The existing persistent object to copy the state on to
     */
    public void merge(RegistryObjectType newObject,
            RegistryObjectType existingObject) {
        newObject.setId(existingObject.getId());
        template.merge(newObject);
    }

    /**
     * Queries for all lids of registry objects matching the pattern of the
     * given id. A query using 'like' will be executed.
     * 
     * @param lid
     *            The lid containing % or _ denoting wildcard characters
     * @return List of lids matching the given id pattern
     */
    public List<String> getMatchingLids(String lid) {
        DetachedCriteria criteria = DetachedCriteria.forClass(this
                .getEntityClass());
        criteria = criteria.add(Property.forName(QueryConstants.LID).like(lid));
        criteria = criteria.setProjection(Projections
                .property(QueryConstants.LID));
        return this.executeCriteriaQuery(criteria);
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
    public List<ENTITY> getByLid(List<String> lids) {
        StringBuilder str = new StringBuilder();
        HqlQueryUtil.assembleSingleParamQuery(str, getEntityClass(),
                QueryConstants.LID, "in", lids);
        str.append(" order by obj.lid asc,obj.versionInfo.versionName desc");
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
     */
    public List<ENTITY> getByLid(String... lids) {
        return getByLid(Arrays.asList(lids));
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
    public List<ENTITY> getByObjectType(List<String> objTypes) {
        return executeHQLQuery(HqlQueryUtil.assembleSingleParamQuery(
                getEntityClass(), QueryConstants.OBJECT_TYPE, "in", objTypes));
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
    public List<ENTITY> getByStatus(List<String> status) {
        return executeHQLQuery(HqlQueryUtil.assembleSingleParamQuery(
                getEntityClass(), QueryConstants.STATUS, "in", status));
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
    public List<ENTITY> getByOwner(List<String> owner) {
        return executeHQLQuery(HqlQueryUtil.assembleSingleParamQuery(
                getEntityClass(), QueryConstants.OWNER, "in", owner));
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
    public List<ENTITY> getByClassification(List<String> classifications) {
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
    @SuppressWarnings("unchecked")
    public void deleteByRefs(final List<ObjectRefType> objRefs)
            throws EbxmlRegistryException {
        // TODO: FIX THIS METHOD TO ELIMINATE CASTING OR MOVE IT ELSEWHERE
        try {
            List<String> objIds = new ArrayList<String>();
            for (ObjectRefType ref : objRefs) {
                objIds.add(ref.getId());
            }
            List<ENTITY> objs = getById(objIds);
            for (RegistryObjectType regObj : objs) {
                if (regObj.getClassification() != null) {
                    for (RegistryObjectType classification : regObj
                            .getClassification()) {
                        delete((ENTITY) classification);
                    }
                }
            }
            for (RegistryObjectType regObj : objs) {
                if (regObj.getExternalIdentifier() != null) {
                    for (RegistryObjectType extId : regObj
                            .getExternalIdentifier()) {
                        delete((ENTITY) extId);
                    }
                }
            }
            for (RegistryObjectType regObj : objs) {
                if (regObj.getExternalLink() != null) {
                    for (RegistryObjectType extLink : regObj.getExternalLink()) {
                        delete((ENTITY) extLink);
                    }
                }
            }
            for (ENTITY obj : objs) {
                delete(obj);
            }
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
    public ENTITY getById(String id) {
        return super.getById(id);
    }
}
