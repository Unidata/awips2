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
import java.util.Collections;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * Data Access object for interacting with persons in the registry
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
public class PersonDao extends RegistryObjectTypeDao<PersonType> {

    private static final String GET_BY_FIRST_NAME_QUERY = "select obj from PersonType obj where lower(obj.personName.firstName) like :firstName order by obj.personName.lastName asc, obj.personName.firstName asc";

    private static final String GET_BY_LAST_NAME_QUERY = "select obj from PersonType obj where lower(obj.personName.lastName) like :lastName order by obj.personName.lastName asc, obj.personName.firstName asc";

    private static final String GET_BY_FIRST_AND_LAST_NAME_QUERY = "select obj from PersonType obj where lower(obj.personName.firstName) like :firstname and lower(obj.personName.lastName) like :lastName order by obj.personName.lastName asc, obj.personName.firstName asc";

    private static final String GET_ALL_USER_NAMES_QUERY = "select obj.id, obj.personName.firstName, obj.personName.lastName from PersonType obj "
            + "order by obj.personName.lastName asc, obj.personName.firstName asc";

    private AssociationDao associationDao;

    /**
     * Creates a new Person data access object
     */
    public PersonDao() {
    }

    /**
     * Gets all users matching the given first name using a case insensitive
     * like query
     * 
     * @param firstName
     *            The first name
     * @return The matching users
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<PersonType> getByFirstName(String firstName) {
        if (firstName == null || firstName.trim().isEmpty()) {
            return Collections.emptyList();
        }
        return this.executeHQLQuery(GET_BY_FIRST_NAME_QUERY, ":firstName", "%"
                + firstName.toLowerCase() + "%");

    }

    /**
     * Gets all users matching the given last name using a case insensitive like
     * query
     * 
     * @param lastName
     *            The last name
     * @return The matching users
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<PersonType> getByLastName(String lastName) {
        if (lastName == null || lastName.trim().isEmpty()) {
            return Collections.emptyList();
        }
        return this.executeHQLQuery(GET_BY_LAST_NAME_QUERY, "lastName", "%"
                + lastName.toLowerCase() + "%");

    }

    /**
     * Gets all users matching the given first and last name using a case
     * insensitive like query
     * 
     * @param firstName
     *            The first name
     * @param lastName
     *            The last name
     * @return The matching users
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<PersonType> getByFirstAndLastName(String firstName,
            String lastName) {
        if (firstName.trim().isEmpty() && lastName.trim().isEmpty()) {
            return this.executeHQLQuery("from PersonType");
        }
        if (firstName == null || firstName.trim().isEmpty()) {
            return getByLastName(lastName);
        } else if (lastName == null || lastName.trim().isEmpty()) {
            return getByFirstName(firstName);
        }
        return this.executeHQLQuery(GET_BY_FIRST_AND_LAST_NAME_QUERY,
                "firstName", "%" + firstName.toLowerCase() + "%", "lastName",
                "%" + lastName.toLowerCase() + "%");

    }

    /**
     * Gets all users who are associated with the given organization
     * 
     * @param orgId
     *            The organization ID
     * @return The users associated with the organization
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<PersonType> getEmployeesOfOrganization(String orgId) {
        List<PersonType> employees = new ArrayList<PersonType>();
        List<AssociationType> associations = associationDao.getByTargetAndType(
                orgId, AssociationTypes.EMPLOYEE_OF);
        for (AssociationType association : associations) {
            employees.add((PersonType) getById(association.getSourceObject()));
        }
        return employees;
    }

    /**
     * Gets the id, first name, and last name for all PersonType objects in the
     * registry
     * 
     * @return List of object arrays (List<Object[]>) Each object array contains
     *         the id, first name, and last name for each PersonType object in
     *         the registry
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public Object getAllUserNames() throws EbxmlRegistryException {
        return this.executeHQLQuery(GET_ALL_USER_NAMES_QUERY);

    }

    /**
     * Gets all PersonType objects in the registry
     * 
     * @return All personType objects in the registry
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<PersonType> getAllUsers() throws EbxmlRegistryException {
        return getByFirstAndLastName("", "");
    }

    /**
     * Gets a specific user given the object ID
     * 
     * @param userId
     *            The ID of the person
     * @return The person with the given ID
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public PersonType getByUserId(String userId) {
        return this.getById(userId);
    }

    public void setAssociationDao(AssociationDao associationDao) {
        this.associationDao = associationDao;
    }

    @Override
    protected Class<PersonType> getEntityClass() {
        return PersonType.class;
    }

}
