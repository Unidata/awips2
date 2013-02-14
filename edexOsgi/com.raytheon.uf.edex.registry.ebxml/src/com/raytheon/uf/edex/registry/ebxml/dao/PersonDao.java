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

import com.raytheon.uf.edex.registry.ebxml.constants.AssociationTypes;
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class PersonDao extends RegistryObjectTypeDao {

    /**
     * Creates a new Person data access object
     */
    public PersonDao() {
        super(PersonType.class);
    }

    /**
     * Gets all users matching the given first name using a case insensitive
     * like query
     * 
     * @param firstName
     *            The first name
     * @return The matching users
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public List<PersonType> getByFirstName(String firstName)
            throws EbxmlRegistryException {
        if (firstName == null || firstName.trim().isEmpty()) {
            return Collections.emptyList();
        }
        return this
                .executeHQLQuery("select obj from PersonType obj where lower(obj.personName.firstName) like '%"
                        + firstName.toLowerCase()
                        + "%' order by obj.personName.lastName asc, obj.personName.firstName asc");
    }

    /**
     * Gets all users matching the given last name using a case insensitive like
     * query
     * 
     * @param lastName
     *            The last name
     * @return The matching users
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public List<PersonType> getByLastName(String lastName)
            throws EbxmlRegistryException {
        if (lastName == null || lastName.trim().isEmpty()) {
            return Collections.emptyList();
        }
        return this
                .executeHQLQuery("select obj from PersonType obj where lower(obj.personName.lastName) like '%"
                        + lastName.toLowerCase()
                        + "%' order by obj.personName.lastName asc, obj.personName.firstName asc");
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
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public List<PersonType> getByFirstAndLastName(String firstName,
            String lastName) throws EbxmlRegistryException {
        if (firstName.trim().isEmpty() && lastName.trim().isEmpty()) {
            return this.executeHQLQuery("from PersonType");
        }
        if (firstName == null || firstName.trim().isEmpty()) {
            return getByLastName(lastName);
        } else if (lastName == null || lastName.trim().isEmpty()) {
            return getByFirstName(firstName);
        }
        return this
                .executeHQLQuery("select obj from PersonType obj where lower(obj.personName.firstName) like '%"
                        + firstName.toLowerCase()
                        + "%' and lower(obj.personName.lastName) like '%"
                        + lastName.toLowerCase()
                        + "%' order by obj.personName.lastName asc, obj.personName.firstName asc");
    }

    /**
     * Gets all users who are associated with the given organization
     * 
     * @param orgId
     *            The organization ID
     * @return The users associated with the organization
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public List<PersonType> getEmployeesOfOrganization(String orgId)
            throws EbxmlRegistryException {
        List<PersonType> employees = new ArrayList<PersonType>();
        AssociationDao associationDao = new AssociationDao();
        PersonDao personDao = new PersonDao();
        List<AssociationType> associations = associationDao.getByTargetAndType(
                orgId, AssociationTypes.EMPLOYEE_OF);
        for (AssociationType association : associations) {
            employees.add((PersonType) personDao.getById(association
                    .getSourceObject()));
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
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public Object getAllUserNames() throws EbxmlRegistryException {
        return this
                .executeHQLQuery("select obj.id, obj.personName.firstName, obj.personName.lastName from PersonType obj "
                        + "order by obj.personName.lastName asc, obj.personName.firstName asc");
    }

    /**
     * Gets all PersonType objects in the registry
     * 
     * @return All personType objects in the registry
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public List<PersonType> getAllUsers() throws EbxmlRegistryException {
        return getByFirstAndLastName("", "");
    }

    /**
     * Gets a specific user given the object ID
     * 
     * @param userId
     *            The ID of the person
     * @return The person with the given ID
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public PersonType getByUserId(String userId) throws EbxmlRegistryException {
        return this.getById(userId);
    }
}
