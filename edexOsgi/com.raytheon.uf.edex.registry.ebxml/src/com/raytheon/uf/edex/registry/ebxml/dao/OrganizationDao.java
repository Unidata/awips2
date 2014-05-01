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

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * Data Access object for interacting with organizations in the registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 7/30/2012    724        bphillip     Initial creation
 * 3/13/2013    1082       bphillip    Modified to use spring injection and transaction boundaries
 * 4/9/2013     1802       bphillip    Removed exception catching
 * 2/13/2014    2769       bphillip    Added read only flags to query methods
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class OrganizationDao extends RegistryObjectTypeDao<OrganizationType> {

    private static final String GET_ORGANIZATION_BY_NAME_QUERY = "select obj from OrganizationType obj inner join obj.name.localizedString as theName where lower(obj.id) like :name1 or lower(theName.value) like :name2 order by obj.id asc";

    /** The Association data access object */
    private AssociationDao associationDao;

    /**
     * Creates a new Organization Dao
     */
    public OrganizationDao() {

    }

    /**
     * Gets all organizations currently stored in the registry
     * 
     * @return List of all organizations currently stored in the registry
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<OrganizationType> getAllOrganizations() {
        return getAll();
    }

    /**
     * Gets all organizations matching the given name using a case insensitive
     * like query
     * 
     * @param name
     *            The name of the organization to retrieve
     * @return The list of organizations matching the given name using a case
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<OrganizationType> getOrganizationByName(String name) {
        List<OrganizationType> orgs = executeHQLQuery(
                GET_ORGANIZATION_BY_NAME_QUERY, "name1",
                "%" + name.toLowerCase() + "%", "name2",
                "%" + name.toLowerCase() + "%");
        return orgs;
    }

    /**
     * Gets the organization associated with the given user ID
     * 
     * @param user
     *            The user ID for which to get the organization
     * @return The organization associated with the given user ID
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public OrganizationType getOrganizationForUser(String user)
            throws EbxmlRegistryException {
        List<AssociationType> associations = associationDao.getBySourceAndType(
                user, AssociationTypes.EMPLOYEE_OF);
        if (associations.isEmpty()) {
            return null;
        } else {
            return getById(associations.get(0).getTargetObject());
        }
    }

    public void setAssociationDao(AssociationDao associationDao) {
        this.associationDao = associationDao;
    }

    @Override
    protected Class<OrganizationType> getEntityClass() {
        return OrganizationType.class;
    }

}
