/**
 * This software was developed and / or modifieimport java.util.Arrays;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.IdentifiableType;

import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Property;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
 B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.registry.ebxml.dao;

import java.util.Arrays;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.IdentifiableType;

import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

/**
 * Data access object for retrieving IdentifiableType objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/18/2013    1082       bphillip     Initial creation
 * 4/9/2013     1802       bphillip    Removed exception catching
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public abstract class IdentifiableTypeDao<ENTITY extends IdentifiableType>
        extends SessionManagedDao<String, ENTITY> {

    public IdentifiableTypeDao() {

    }

    @Override
    public ENTITY getById(String id) {
        return super.getById(id);
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
    public List<ENTITY> getById(String... ids) throws EbxmlRegistryException {
        return getById(Arrays.asList(ids));
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
    public List<ENTITY> getById(List<String> ids) throws EbxmlRegistryException {
        return this.executeHQLQuery(HqlQueryUtil.assembleSingleParamQuery(
                this.getEntityClass(), QueryConstants.ID, "in", ids));
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
        String hql = "select obj.id from "
                + this.getEntityClass().getSimpleName()
                + " obj where obj.id like :id";
        return this.executeHQLQuery(hql, "id", id);
    }

}
