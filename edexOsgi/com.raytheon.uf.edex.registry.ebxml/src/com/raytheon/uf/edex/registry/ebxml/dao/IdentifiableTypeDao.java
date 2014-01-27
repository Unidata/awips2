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

import org.hibernate.criterion.Property;

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
 * 10/08/2013   1682       bphillip    Added the id like query
 * 12/2/2013    1829       bphillip    Now extends ExtensibleObjectTypeDao
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class IdentifiableTypeDao<ENTITY extends IdentifiableType> extends
        ExtensibleObjectTypeDao<ENTITY> {

    public IdentifiableTypeDao() {

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
    @SuppressWarnings("unchecked")
    public List<ENTITY> getById(List<String> ids) throws EbxmlRegistryException {
        return createCriteria()
                .add(Property.forName(QueryConstants.ID).in(ids)).list();
    }

    /**
     * Gets all IdentifiableType objects matching (using like) the given id.
     * 
     * @param id
     *            The id to query for
     * @return All IdentifiableType objects matching the given id
     */
    @SuppressWarnings("unchecked")
    public List<ENTITY> getByIdUsingLike(String id) {
        return createCriteria().add(
                Property.forName(QueryConstants.ID).like(id)).list();
    }

}
