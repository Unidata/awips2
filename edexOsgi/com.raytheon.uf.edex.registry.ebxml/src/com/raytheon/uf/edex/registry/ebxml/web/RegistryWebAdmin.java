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
package com.raytheon.uf.edex.registry.ebxml.web;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.EmailAddressType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PartyType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PostalAddressType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RoleType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.TelephoneNumberType;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.ClassificationNodeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.OrganizationDao;
import com.raytheon.uf.edex.registry.ebxml.dao.PersonDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RoleDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * Class used to get registry information for displaying information on the Data
 * Delivery Registry Admin Web Page
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
public class RegistryWebAdmin {

    /** Static list of role types */
    private static final String[] ROLE_TYPES = new String[] {
            "RegistryAdministrator", "RegistryLocalAdministrator",
            "RegistryUser", "RegistryGuest" };

    /**
     * Gets the array of address types from the registry
     * 
     * @return The array of address types from the registry
     * @throws EbxmlRegistryException
     *             If errors occur during database access
     */
    public String[] getAddressTypes() throws EbxmlRegistryException {
        List<String> result = new ClassificationNodeDao().getAddressTypes();
        return result.toArray(new String[result.size()]);
    }

    /**
     * Gets the array of telephone number types from the registry
     * 
     * @return The array of telephone number types from the registry
     * @throws EbxmlRegistryException
     *             If errors occur during database access
     */
    public String[] getTelephoneTypes() throws EbxmlRegistryException {
        List<String> result = new ClassificationNodeDao().getTelephoneTypes();
        return result.toArray(new String[result.size()]);
    }

    /**
     * Gets the array of email address types from the registry
     * 
     * @return The array of email address types from the registry
     * @throws EbxmlRegistryException
     *             If errors occur during database access
     */
    public String[] getEmailTypes() throws EbxmlRegistryException {
        List<String> result = new ClassificationNodeDao().getEmailTypes();
        return result.toArray(new String[result.size()]);
    }

    /**
     * Gets the static list of role types
     * 
     * @return The static list of role types
     */
    public String[] getRoleTypes() {
        return ROLE_TYPES;
    }

    /**
     * Gets organization names for names matching the provided search criteria
     * 
     * @param name
     *            The name (may be a partial name) of the organization to search
     *            for
     * @return The list of organization names matching the given search criteria
     * @throws EbxmlRegistryException
     *             If errors occur during database access
     */
    public String[] getOrganizationNames(String name)
            throws EbxmlRegistryException {
        List<OrganizationType> orgs = new OrganizationDao()
                .getOrganizationByName(name);
        List<String> retVal = new ArrayList<String>(orgs.size());
        for (OrganizationType org : orgs) {
            retVal.add(org.getId());
        }
        return retVal.toArray(new String[retVal.size()]);

    }

    /**
     * Gets the HTML formatted table with the details for the given organization
     * 
     * @param name
     *            The name of the organization to get details for
     * @return The HTML formatted table with the details of the given
     *         organization
     * @throws EbxmlRegistryException
     *             If errors occur during database access
     */
    public String getOrganization(String name) throws EbxmlRegistryException {
        // Instantiate the necessary data access objects
        PersonDao personDao = new PersonDao();
        OrganizationDao orgDao = new OrganizationDao();
        StringBuffer retVal = new StringBuffer(1024);

        // Query the organization table for matching organization names
        List<OrganizationType> orgs = orgDao.getOrganizationByName(name);
        if (orgs.size() == 1) {
            retVal.append("<b>" + orgs.size() + " Match</b>");
        } else {
            retVal.append("<b>" + orgs.size() + " Matches</b>");
        }

        retVal.append("<table>");
        retVal.append("<tr>");
        retVal.append("<th>ID</th>");
        retVal.append("<th>Name</th>");
        retVal.append("<th>Local Admin</th>");
        retVal.append("<th>Users</th>");
        retVal.append("</tr>");

        /*
         * Loop over the returned organizations and fill in the table rows
         */
        for (OrganizationType org : orgs) {
            String primaryContactId = org.getPrimaryContact();
            PersonType primaryContact = personDao.getById(primaryContactId);
            String pcString = "";
            if (primaryContact != null) {
                pcString = "<a id=\"" + primaryContactId
                        + "\" onclick=\"getUserDetails(" + "'"
                        + primaryContactId + "'" + ")\">"
                        + primaryContact.getPersonName().getFirstName() + " "
                        + primaryContact.getPersonName().getLastName() + "</a>";
            }

            String orgId = org.getId();
            retVal.append("<tr>");
            retVal.append("<td>")
                    .append("<a id=\"" + orgId
                            + "\" onclick=\"getOrgDetails(this.id)\">" + orgId
                            + "</a>").append("</td>");
            retVal.append("<td>")
                    .append(org.getName().getLocalizedString().get(0)
                            .getValue()).append("</td>");
            retVal.append("<td>").append(pcString).append("</td>");

            List<PersonType> users = personDao
                    .getEmployeesOfOrganization(orgId);
            retVal.append("<td style=\"word-wrap: break-word\">");
            for (int i = 0; i < users.size(); i++) {

                retVal.append("<a id=\"" + orgId
                        + "\" onclick=\"getUserDetails(" + "'"
                        + users.get(i).getId() + "'" + ")\">"
                        + users.get(i).getPersonName().getFirstName() + " "
                        + users.get(i).getPersonName().getLastName() + "</a>");
                if (i != users.size() - 1) {
                    retVal.append(", ");
                }
            }

            retVal.append("</td>").append("</tr>");
        }
        retVal.append("</table>");

        return retVal.toString();
    }

    /**
     * Gets the HTML formatted table with details for the given user name
     * 
     * @param firstName
     *            The first name of the user to search for
     * @param lastName
     *            The last name of the user to search for
     * @return The HTML formatted table with the details for the given user name
     * @throws EbxmlRegistryException
     *             If errors occur during database access
     */
    public String getUser(String firstName, String lastName)
            throws EbxmlRegistryException {

        // Instantiate the necessary data access objects
        StringBuffer retVal = new StringBuffer(1024);
        PersonDao dao = new PersonDao();
        OrganizationDao orgDao = new OrganizationDao();
        RoleDao roleDao = new RoleDao();

        // Query for users matching the given search criteria
        List<PersonType> users = dao.getByFirstAndLastName(firstName, lastName);
        for (int i = 0; i < users.size(); i++) {
            if (users.get(i).getId().equals(RegistryUtil.DEFAULT_OWNER)) {
                users.remove(i);
                break;
            }
        }
        if (users.size() == 1) {
            retVal.append("<b>" + users.size() + " Match</b>");
        } else {
            retVal.append("<b>" + users.size() + " Matches</b>");
        }
        retVal.append("<table>");
        retVal.append("<tr>");
        retVal.append("<th>ID</th>");
        retVal.append("<th>Name</th>");
        retVal.append("<th>Organization</th>");
        retVal.append("<th>Role</th>");
        retVal.append("</tr>");

        /*
         * Loop over the returned users and populate the table
         */
        for (PersonType user : users) {
            String userId = user.getId();
            OrganizationType userOrg = orgDao.getOrganizationForUser(userId);
            RoleType userRole = roleDao.getUserRole(userId);
            retVal.append("<tr>");
            retVal.append("<td>")
                    .append("<a id=\"" + userId
                            + "\" onclick=\"getUserDetails(this.id)\">"
                            + userId + "</a>").append("</td>");
            retVal.append("<td>")
                    .append(user.getPersonName().getFirstName() + " "
                            + user.getPersonName().getLastName())
                    .append("</td>");
            if (userOrg == null) {
                retVal.append("<td></td>");
            } else {
                retVal.append("<td>")
                        .append("<a id=\"" + userOrg.getId() + userId
                                + "\" onclick=\"getOrgDetails('"
                                + userOrg.getId() + "')\">" + userOrg.getId()
                                + "</a>").append("</td>");
            }
            if (userRole == null) {
                retVal.append("<td></td>");
            } else {
                retVal.append("<td>").append(userRole.getId()).append("</td>");
            }
            retVal.append("</tr>");
        }
        retVal.append("</table>");

        return retVal.toString();
    }

    /**
     * Get the details of a party(user or organization) to populate the web form
     * 
     * @param objId
     *            The id of the party to get information for
     * @return The details of the party as a property map in string format
     * @throws EbxmlRegistryException
     *             If errors occur during database access
     */
    public String getPartyDetails(String objId) throws EbxmlRegistryException {
        /*
         * Spaces need to be converted from the hex value to the actual space
         * character
         */
        objId = objId.replaceAll("%20", " ");
        Map<String, String> propMap = new HashMap<String, String>();
        RegistryObjectTypeDao dao = new RegistryObjectTypeDao(PartyType.class);
        ClassificationNodeDao nodeDao = new ClassificationNodeDao();
        try {
            dao.openSession();
            PartyType party = dao.getById(objId);

            if (party == null) {
                return "";
            }

            propMap.put(WebFields.ID.field(), party.getId());
            if (party instanceof PersonType) {
                propMap.put(WebFields.FIRST_NAME.field(), ((PersonType) party)
                        .getPersonName().getFirstName());
                propMap.put(WebFields.MIDDLE_NAME.field(), ((PersonType) party)
                        .getPersonName().getMiddleName());
                propMap.put(WebFields.LAST_NAME.field(), ((PersonType) party)
                        .getPersonName().getLastName());
                OrganizationType userOrg = new OrganizationDao()
                        .getOrganizationForUser(party.getId());
                String orgId = "";
                if (userOrg != null) {
                    orgId = userOrg.getId();
                }
                RoleType role = new RoleDao().getUserRole(party.getId());
                String roleId = "";
                if (role != null) {
                    roleId = role.getId();
                }
                propMap.put(WebFields.USER_ORG.field(), orgId);
                propMap.put(WebFields.USER_ROLE.field(), roleId);
            } else if (party instanceof OrganizationType) {
                propMap.put(WebFields.ORGANIZATION_NAME.field(), party
                        .getName().getLocalizedString().get(0).getValue());
                PersonType primaryContact = new PersonDao()
                        .getById(((OrganizationType) party).getPrimaryContact());
                if (primaryContact == null) {
                    propMap.put(WebFields.PRIMARY_CONTACT.field(),
                            "Not Specified");
                } else {
                    propMap.put(WebFields.PRIMARY_CONTACT.field(), "<a id=\""
                            + primaryContact.getId()
                            + "\" onclick=\"getUserDetails(this.id)\">"
                            + primaryContact.getPersonName().getLastName()
                            + ", "
                            + primaryContact.getPersonName().getFirstName()
                            + "</a>");
                }
            }

            if (!party.getPostalAddress().isEmpty()) {
                PostalAddressType addr = party.getPostalAddress().get(0);
                propMap.put(WebFields.ADDRESS_TYPE.field(),
                        nodeDao.getCodeFromNode(addr.getType()));
                propMap.put(WebFields.ADDRESS_1.field(), addr.getStreet());
                propMap.put(WebFields.ADDRESS_2.field(), addr.getStreetNumber());
                propMap.put(WebFields.CITY.field(), addr.getCity());
                propMap.put(WebFields.STATE.field(), addr.getStateOrProvince());
                propMap.put(WebFields.COUNTRY.field(), addr.getCountry());
                propMap.put(WebFields.POSTAL_CODE.field(), addr.getPostalCode());
            }

            if (!party.getTelephoneNumber().isEmpty()) {
                TelephoneNumberType phone = party.getTelephoneNumber().get(0);
                propMap.put(WebFields.TELEPHONE_TYPE.field(),
                        nodeDao.getCodeFromNode(phone.getType()));
                propMap.put(WebFields.AREA_CODE.field(), phone.getAreaCode());

                if (phone.getNumber().length() == 7) {
                    propMap.put(WebFields.PHONE_1.field(), phone.getNumber()
                            .substring(0, 3));
                    propMap.put(WebFields.PHONE_2.field(), phone.getNumber()
                            .substring(3));
                }
                propMap.put(WebFields.EXTENSION.field(), phone.getExtension());
            }

            if (!party.getEmailAddress().isEmpty()) {
                EmailAddressType email = party.getEmailAddress().get(0);
                propMap.put(WebFields.EMAIL_TYPE.field(),
                        nodeDao.getCodeFromNode(email.getType()));
                propMap.put(WebFields.EMAIL.field(), email.getAddress());
            }
        } finally {
            dao.closeSession();
        }
        return RegistryWebUtil.mapToString(propMap);
    }

    /**
     * Gets the array of ids associated with users. This function is used to
     * list the members of a given organization
     * 
     * @return The array of user ids and names
     * @throws EbxmlRegistryException
     *             If errors occur during database access
     */
    public String[] getUserIdsAndNames() throws EbxmlRegistryException {
        PersonDao personDao = new PersonDao();

        @SuppressWarnings("unchecked")
        List<Object[]> allUsers = (List<Object[]>) personDao.getAllUserNames();
        List<String> retVal = new ArrayList<String>(allUsers.size());
        for (Object[] userInfo : allUsers) {
            for (Object obj : userInfo) {
                retVal.add(obj.toString());
            }
        }
        return retVal.toArray(new String[] {});
    }
}
