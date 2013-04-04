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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.EmailAddressType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PartyType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonNameType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PostalAddressType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.TelephoneNumberType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.registry.ebxml.constants.AssociationTypes;
import com.raytheon.uf.edex.registry.ebxml.constants.RegistryObjectTypes;
import com.raytheon.uf.edex.registry.ebxml.constants.RegistryResponseStatus;
import com.raytheon.uf.edex.registry.ebxml.constants.StatusTypes;
import com.raytheon.uf.edex.registry.ebxml.dao.AssociationDao;
import com.raytheon.uf.edex.registry.ebxml.dao.ClassificationNodeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.OrganizationDao;
import com.raytheon.uf.edex.registry.ebxml.dao.PersonDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.util.EDEXRegistryManager;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * Utility class used to modify parties in the registry
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
public class RegistryWebUtil {

    /**
     * Creates a party (user or organization) based on parameters contained in
     * the servlet request
     * 
     * @param request
     *            The servlet request object
     * @return The registry submit object request
     * @throws EbxmlRegistryException
     *             If errors occur during registry interaction
     */
    public static SubmitObjectsRequest createParty(HttpServletRequest request)
            throws EbxmlRegistryException {

        List<RegistryObjectType> objectList = new ArrayList<RegistryObjectType>();
        ClassificationNodeDao dao = new ClassificationNodeDao();

        String partyId = request.getParameter("id");
        PartyType party = null;

        // Creating a user object
        if (request.getParameter(WebFields.OBJ_TYPE.fieldName()).equals("User")) {
            String employer = request.getParameter(WebFields.USER_ORG
                    .fieldName());
            String roleName = request.getParameter(WebFields.USER_ROLE
                    .fieldName());
            party = new PersonType();

            /*
             * Assign the person name to the person
             */
            PersonNameType personName = new PersonNameType();
            personName.setFirstName(request.getParameter(WebFields.FIRST_NAME
                    .fieldName()));
            if (request.getParameter(WebFields.MIDDLE_NAME.fieldName()) == null) {
                personName.setMiddleName("");
            } else {
                personName.setMiddleName(request
                        .getParameter(WebFields.MIDDLE_NAME.fieldName()));
            }
            personName.setLastName(request.getParameter(WebFields.LAST_NAME
                    .fieldName()));
            ((PersonType) party).setPersonName(personName);
            party.setLid(partyId);
            party.setOwner(RegistryUtil.DEFAULT_OWNER);
            party.setStatus(StatusTypes.APPROVED);
            party.setName(RegistryUtil.getInternationalString("User "
                    + personName.getFirstName() + " "
                    + personName.getMiddleName() + " "
                    + personName.getLastName()));
            party.setDescription(RegistryUtil
                    .getInternationalString("Profile description for user "
                            + personName.getFirstName() + " "
                            + personName.getMiddleName() + " "
                            + personName.getLastName()));
            party.setObjectType(RegistryObjectTypes.PERSON);

            /* Create an association to the organization if specified */
            if (!employer.trim().isEmpty()) {
                objectList.add(createEmployeeOfAssociation(partyId, employer));
            }

            /* Create an association to the role if specified */
            if (!roleName.trim().isEmpty()) {
                objectList.add(createHasRoleAssociation(partyId, roleName));
            }

        }
        // Creating an organization
        else {
            PersonDao personDao = new PersonDao();
            party = new OrganizationType();
            party.setLid(partyId);
            party.setOwner(RegistryUtil.DEFAULT_OWNER);
            party.setStatus(StatusTypes.APPROVED);
            party.setName(RegistryUtil.getInternationalString(request
                    .getParameter(WebFields.ORGANIZATION_NAME.fieldName())));
            String primaryContactStr = request
                    .getParameter(WebFields.PRIMARY_CONTACT.fieldName());

            // Setting the primary contact name
            if (!primaryContactStr.equals("Not Specified")
                    && !primaryContactStr.isEmpty()) {
                String[] nameTokens = primaryContactStr.split(", ");
                ((OrganizationType) party).setPrimaryContact(personDao
                        .getByFirstAndLastName(nameTokens[1], nameTokens[0])
                        .get(0).getId());
            }

            party.setDescription(RegistryUtil
                    .getInternationalString("Profile description for organization "
                            + partyId));
            party.setObjectType(RegistryObjectTypes.ORGANIZATION);
        }

        // Assign the postal address
        PostalAddressType address = new PostalAddressType();
        address.setType(dao.getNodeFromCode(request
                .getParameter(WebFields.ADDRESS_TYPE.fieldName())));
        address.setStreet(request.getParameter(WebFields.ADDRESS_1.fieldName()));
        address.setStreetNumber(request.getParameter(WebFields.ADDRESS_2
                .fieldName()));
        address.setCity(request.getParameter(WebFields.CITY.fieldName()));
        address.setCountry(request.getParameter(WebFields.COUNTRY.fieldName()));
        address.setStateOrProvince(request.getParameter(WebFields.STATE
                .fieldName()));
        address.setPostalCode(request.getParameter(WebFields.POSTAL_CODE
                .fieldName()));
        party.getPostalAddress().add(address);

        // Assign the telephone number
        TelephoneNumberType phone = new TelephoneNumberType();
        phone.setType(dao.getNodeFromCode(request
                .getParameter(WebFields.TELEPHONE_TYPE.fieldName())));
        phone.setAreaCode(request.getParameter(WebFields.AREA_CODE.fieldName()));
        phone.setNumber(request.getParameter(WebFields.PHONE_1.fieldName())
                + request.getParameter(WebFields.PHONE_2.fieldName()));
        phone.setExtension(request.getParameter(WebFields.EXTENSION.fieldName()));
        party.getTelephoneNumber().add(phone);

        // Assign the email address
        EmailAddressType email = new EmailAddressType();
        email.setType(dao.getNodeFromCode(request
                .getParameter(WebFields.EMAIL_TYPE.fieldName())));
        email.setAddress(request.getParameter(WebFields.EMAIL.fieldName()));
        party.getEmailAddress().add(email);

        party.setId(partyId);
        objectList.add(party);
        return getSubmitRequest(objectList);
    }

    /**
     * Creates an association from a user to an organization
     * 
     * @param user
     *            The user (The source of the association)
     * @param organization
     *            The organization (The target of the association)
     * @return The association object
     * @throws EbxmlRegistryException
     *             If errors occur during database interaction
     */
    private static AssociationType createEmployeeOfAssociation(String user,
            String organization) throws EbxmlRegistryException {

        AssociationType association = new AssociationType();
        association.setId(EbxmlObjectUtil.getUUID());
        association.setLid(association.getId());
        association.setName(RegistryUtil.getInternationalString(user + "-->"
                + organization));
        association.setDescription(RegistryUtil.getInternationalString(user
                + " is an employee of " + organization));
        association.setStatus(StatusTypes.APPROVED);
        association.setOwner(RegistryUtil.DEFAULT_OWNER);
        association.setObjectType(RegistryObjectTypes.ASSOCIATION);
        association.setSourceObject(user);
        association.setTargetObject(organization);
        association.setType(AssociationTypes.EMPLOYEE_OF);
        return association;
    }

    /**
     * Creates an association from a user to a role
     * 
     * @param user
     *            The user (The source of the association)
     * @param roleName
     *            The role (The target of the association)
     * @return The association object
     */
    private static AssociationType createHasRoleAssociation(String user,
            String roleName) {
        AssociationType association = new AssociationType();
        association.setId(EbxmlObjectUtil.getUUID());
        association.setLid(association.getId());
        association.setName(RegistryUtil.getInternationalString(user + "-->"
                + roleName));
        association.setDescription(RegistryUtil.getInternationalString(user
                + " has role of " + roleName));
        association.setStatus(StatusTypes.APPROVED);
        association.setOwner(RegistryUtil.DEFAULT_OWNER);
        association.setObjectType(RegistryObjectTypes.ASSOCIATION);
        association.setSourceObject(user);
        association.setTargetObject(roleName);
        association.setType(AssociationTypes.HAS_ROLE);
        return association;
    }

    /**
     * Sends a successful response back the servlet requester
     * 
     * @param request
     *            The servlet request
     * @param response
     *            The servlet response
     * @param responsePage
     *            The page to display
     * @param userId
     *            The requester's user ID
     * @param partyType
     *            The type of party
     * @throws ServletException
     *             If an error occurs while sending the response back
     * @throws IOException
     *             If the message cannot be forwarded
     */
    public static void sendSuccessResponse(HttpServletRequest request,
            HttpServletResponse response, String responsePage, String userId,
            String partyType) throws ServletException, IOException {
        request.setAttribute("partyType", partyType);
        request.setAttribute("userId", userId);
        request.getRequestDispatcher("/response/" + responsePage + ".jsp")
                .forward(request, response);

    }

    /**
     * Sends an error response back to the servlet requester
     * 
     * @param request
     *            The servlet request
     * @param response
     *            The servlet response
     * @param responsePage
     *            The page to display
     * @param userId
     *            The requester's user ID
     * @param partyType
     *            The type of the party
     * @param cause
     *            The cause of the error
     * @throws ServletException
     *             If an error occurs while sending the response back
     * @throws IOException
     *             If the message cannot be forwarded
     */
    public static void sendErrorResponse(HttpServletRequest request,
            HttpServletResponse response, String responsePage, String userId,
            String partyType, String cause) throws ServletException,
            IOException {
        request.setAttribute("partyType", partyType);
        request.setAttribute("userId", userId);
        request.setAttribute("cause", cause);
        request.getRequestDispatcher("/response/" + responsePage + ".jsp")
                .forward(request, response);
    }

    /**
     * Removes all associations to and from a given party
     * 
     * @param party
     *            The party for which to remove associations to and from
     * @throws EbxmlRegistryException
     *             If errors occur during database interaction
     * @throws MsgRegistryException
     *             If errors occur when submitting the remove object request
     */
    @SuppressWarnings("rawtypes")
    public static void removeAssociations(PartyType party)
            throws EbxmlRegistryException, MsgRegistryException {
        AssociationDao associationDao = new AssociationDao();
        List<AssociationType> associations = associationDao
                .getAllAssociations(party.getId());
        if (!associations.isEmpty()) {
            RemoveObjectsRequest removeRequest = getRemoveRequest(associations);
            ((EDEXRegistryManager) EDEXUtil
                    .getESBComponent("edexRegistryManager"))
                    .getLifeCycleManager().removeObjects(removeRequest);
        }
    }

    /**
     * Removes associations with the source specified as the given party
     * 
     * @param party
     *            The source of the associations to remove
     * @throws EbxmlRegistryException
     *             If errors occur during database interaction
     * @throws MsgRegistryException
     *             If errors occur when submitting the remove object request
     */
    @SuppressWarnings("rawtypes")
    public static void removeAssociationsFrom(PartyType party)
            throws EbxmlRegistryException, MsgRegistryException {
        AssociationDao associationDao = new AssociationDao();
        List<AssociationType> associations = associationDao
                .getAssociationsFrom(party.getId());
        if (!associations.isEmpty()) {
            RemoveObjectsRequest removeRequest = getRemoveRequest(associations);
            ((EDEXRegistryManager) EDEXUtil
                    .getESBComponent("edexRegistryManager"))
                    .getLifeCycleManager().removeObjects(removeRequest);
        }
    }

    /**
     * Removes associations with the target specified as the given party
     * 
     * @param party
     *            The target of the associations to remove
     * @throws EbxmlRegistryException
     *             If errors occur during database interaction
     * @throws MsgRegistryException
     *             If errors occur when submitting the remove object request
     */
    @SuppressWarnings("rawtypes")
    public static void removeAssociationsTo(PartyType party)
            throws EbxmlRegistryException, MsgRegistryException {
        AssociationDao associationDao = new AssociationDao();
        List<AssociationType> associations = associationDao
                .getAssociationsTo(party.getId());
        if (!associations.isEmpty()) {
            RemoveObjectsRequest removeRequest = getRemoveRequest(associations);
            ((EDEXRegistryManager) EDEXUtil
                    .getESBComponent("edexRegistryManager"))
                    .getLifeCycleManager().removeObjects(removeRequest);
        }
    }

    /**
     * Updates the primary contact information for a given request
     * 
     * @param request
     *            The servlet request
     * @throws EbxmlRegistryException
     *             If errors occur during database interaction
     */
    public static void updatePC(HttpServletRequest request)
            throws EbxmlRegistryException {
        if (request.getParameter(WebFields.OBJ_TYPE.fieldName()).equals("User")) {
            String user = request.getParameter(WebFields.ID.fieldName());
            String orgName = request.getParameter(WebFields.USER_ORG
                    .fieldName());
            String roleName = request.getParameter(WebFields.USER_ROLE
                    .fieldName());
            OrganizationDao orgDao = new OrganizationDao();
            OrganizationType org = orgDao.getOrganizationByName(orgName).get(0);
            String currentPrimaryContact = org.getPrimaryContact();
            boolean update = false;
            if (currentPrimaryContact == null
                    || roleName.equals("RegistryLocalAdministrator")) {
                org.setPrimaryContact(user);
                update = true;
            } else if (currentPrimaryContact.equals(user)) {
                org.setPrimaryContact(null);
                update = true;
            }
            if (update) {
                orgDao.saveOrUpdate(org);
            }
        }
    }

    /**
     * Deletes a party from the registry
     * 
     * @param party
     *            The party to delete
     * @throws EbxmlRegistryException
     *             If errors occur during database interaction
     * @throws MsgRegistryException
     *             If errors occur when submitting the remove objects request
     */
    @SuppressWarnings("rawtypes")
    public static void removeParty(PartyType party)
            throws EbxmlRegistryException, MsgRegistryException {

        EDEXRegistryManager mgr = ((EDEXRegistryManager) EDEXUtil
                .getESBComponent("edexRegistryManager"));
        RemoveObjectsRequest request = getRemoveRequest(party);
        RegistryResponseType response = mgr.getLifeCycleManager()
                .removeObjects(request);
        if (!response.getStatus().equals(RegistryResponseStatus.SUCCESS)) {
            StringBuffer exceptionText = new StringBuffer();
            exceptionText.append("Remove Objects Failed:\n");
            for (RegistryExceptionType exception : response.getException()) {
                exceptionText.append("Exception: ")
                        .append(exception.getMessage()).append(":")
                        .append(exception.getDetail()).append("\n");
                throw new EbxmlRegistryException(exceptionText.toString());
            }
        }

    }

    /**
     * Constructs a submit object request to be submitted to the
     * LifecycleManager with the given objects as the payload
     * 
     * @param <T>
     *            RegistryObjectType
     * @param obj
     *            The object to be submitted to the registry
     * @return The submit object request
     */
    public static <T extends RegistryObjectType> SubmitObjectsRequest getSubmitRequest(
            T obj) {
        List<RegistryObjectType> objs = new ArrayList<RegistryObjectType>();
        objs.add(obj);
        return getSubmitRequest(objs);
    }

    /**
     * Constructs a submit object request to be submitted to the
     * LifecycleManager with the given objects as the payload
     * 
     * @param <T>
     *            RegistryObjectType
     * @param objs
     *            The objects to be submitted to the registry
     * @return The submit object request
     */
    public static <T extends RegistryObjectType> SubmitObjectsRequest getSubmitRequest(
            List<T> objs) {
        SubmitObjectsRequest submitRequest = new SubmitObjectsRequest();
        submitRequest.setComment("Object submission");
        submitRequest.setCheckReferences(false);
        submitRequest.setMode(Mode.CREATE_OR_REPLACE);
        submitRequest.setId("User/Organization Profiles and Roles");

        submitRequest.setRegistryObjectList(EbxmlObjectUtil
                .createRegistryObjectList(objs));
        return submitRequest;
    }

    /**
     * Constructs a remove object request to be submitted to the
     * LifecycleManager with the given object as the payload
     * 
     * @param <T>
     *            RegistryObjectType
     * @param obj
     *            The object to be removed from the registry
     * @return The RemoveObjectRequest object
     */
    public static <T extends RegistryObjectType> RemoveObjectsRequest getRemoveRequest(
            T obj) {
        List<RegistryObjectType> objs = new ArrayList<RegistryObjectType>(1);
        objs.add(obj);
        return getRemoveRequest(objs);
    }

    /**
     * Constructs a remove object request to be submitted to the
     * LifecycleManager with the given objects as the payload
     * 
     * @param <T>
     *            RegistryObjectType
     * @param objs
     *            The objects to be removed from the registry
     * @return The RemoveObjectRequest object
     */
    public static <T extends RegistryObjectType> RemoveObjectsRequest getRemoveRequest(
            List<T> objs) {
        StringBuffer comment = new StringBuffer(objs.size() * 38);
        comment.append("Removing objects: ");
        for (RegistryObjectType regObj : objs) {
            comment.append("[");
            comment.append(regObj.getId());
            comment.append("] ");
        }
        RemoveObjectsRequest request = new RemoveObjectsRequest();
        request.setId(EbxmlObjectUtil.getUUID());
        request.setComment(comment.toString());
        request.setObjectRefList(EbxmlObjectUtil
                .createObjectRefListFromObjects(objs));
        return request;

    }

    /**
     * Converst a map of objects to a string representation that can be easily
     * parsed by the web client
     * 
     * @param map
     *            The map of key value pairs to convert to a string
     * @return The String representation of the map
     */
    public static String mapToString(Map<String, String> map) {
        StringBuffer retVal = new StringBuffer();
        int idx = 0;
        for (String key : map.keySet()) {
            retVal.append(key).append("===").append(map.get(key));
            if (idx != map.size() - 1) {
                retVal.append("_____");
            }
            idx++;
        }
        return retVal.toString();
    }
}
