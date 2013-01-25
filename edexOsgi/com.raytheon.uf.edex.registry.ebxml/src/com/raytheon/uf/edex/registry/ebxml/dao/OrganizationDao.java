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

import com.raytheon.uf.edex.registry.ebxml.constants.AssociationTypes;
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class OrganizationDao extends RegistryObjectTypeDao {

    /**
     * Creates a new organization data access object
     */
    public OrganizationDao() {
        super(OrganizationType.class);
    }

    /**
     * Gets all organizations currently stored in the registry
     * 
     * @return List of all organizations currently stored in the registry
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public List<OrganizationType> getAllOrganizations()
            throws EbxmlRegistryException {
        return executeHQLQuery("from OrganizationType");
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
    public List<OrganizationType> getOrganizationByName(String name)
            throws EbxmlRegistryException {
        List<OrganizationType> orgs = executeHQLQuery("select obj from OrganizationType obj inner join obj.name.localizedString as theName where lower(obj.id) like '%"
                + name.toLowerCase()
                + "%' or lower(theName.value) like '%"
                + name.toLowerCase() + "%' order by obj.id asc");
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
    public OrganizationType getOrganizationForUser(String user)
            throws EbxmlRegistryException {
        List<AssociationType> associations = new AssociationDao()
                .getBySourceAndType(user, AssociationTypes.EMPLOYEE_OF);
        if (associations.isEmpty()) {
            return null;
        } else {
            return getById(associations.get(0).getTargetObject());
        }
    }
}
