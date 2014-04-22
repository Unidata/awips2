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
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.AssociationDao;
import com.raytheon.uf.edex.registry.ebxml.dao.ClassificationNodeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.OrganizationDao;
import com.raytheon.uf.edex.registry.ebxml.dao.PersonDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.lifecycle.LifecycleManagerImpl;
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
 * 3/13/2013    1082       bphillip     Modified to use spring injection
 * Apr 23, 2013 1910       djohnson     RegistryResponseStatus is now an enum.
 * Mar 31, 2014 2889       dhladky      Added username for notification center tracking.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class RegistryWebUtil {

    private LifecycleManagerImpl lcm;

    private ClassificationNodeDao classificationNodeDao;

    private PersonDao personDao;

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
    public SubmitObjectsRequest createParty(HttpServletRequest request)
            throws EbxmlRegistryException {

        List<RegistryObjectType> objectList = new ArrayList<RegistryObjectType>();

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
        address.setType(classificationNodeDao.getNodeFromCode(request
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
        phone.setType(classificationNodeDao.getNodeFromCode(request
                .getParameter(WebFields.TELEPHONE_TYPE.fieldName())));
        phone.setAreaCode(request.getParameter(WebFields.AREA_CODE.fieldName()));
        phone.setNumber(request.getParameter(WebFields.PHONE_1.fieldName())
                + request.getParameter(WebFields.PHONE_2.fieldName()));
        phone.setExtension(request.getParameter(WebFields.EXTENSION.fieldName()));
        party.getTelephoneNumber().add(phone);

        // Assign the email address
        EmailAddressType email = new EmailAddressType();
        email.setType(classificationNodeDao.getNodeFromCode(request
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
    private AssociationType createEmployeeOfAssociation(String user,
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
    private AssociationType createHasRoleAssociation(String user,
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
    public void sendSuccessResponse(HttpServletRequest request,
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
    public void sendErrorResponse(HttpServletRequest request,
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
    public void removeAssociations(PartyType party)
            throws EbxmlRegistryException, MsgRegistryException {
        AssociationDao associationDao = new AssociationDao();
        List<AssociationType> associations = associationDao
                .getAllAssociations(party.getId());
        if (!associations.isEmpty()) {
            RemoveObjectsRequest removeRequest = getRemoveRequest(associations);
            lcm.removeObjects(removeRequest);
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
    public void removeAssociationsFrom(PartyType party)
            throws EbxmlRegistryException, MsgRegistryException {
        AssociationDao associationDao = new AssociationDao();
        List<AssociationType> associations = associationDao
                .getAssociationsFrom(party.getId());
        if (!associations.isEmpty()) {
            RemoveObjectsRequest removeRequest = getRemoveRequest(associations);
            lcm.removeObjects(removeRequest);
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
    public void removeAssociationsTo(PartyType party)
            throws EbxmlRegistryException, MsgRegistryException {
        AssociationDao associationDao = new AssociationDao();
        List<AssociationType> associations = associationDao
                .getAssociationsTo(party.getId());
        if (!associations.isEmpty()) {
            RemoveObjectsRequest removeRequest = getRemoveRequest(associations);
            lcm.removeObjects(removeRequest);
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
    public void updatePC(HttpServletRequest request)
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
                orgDao.createOrUpdate(org);
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
    public void removeParty(PartyType party) throws EbxmlRegistryException,
            MsgRegistryException {

        RemoveObjectsRequest request = getRemoveRequest(party);
        RegistryResponseType response = lcm.removeObjects(request);
        if (!response.getStatus().equals(RegistryResponseStatus.SUCCESS)) {
            StringBuilder exceptionText = new StringBuilder();
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
    public <T extends RegistryObjectType> SubmitObjectsRequest getSubmitRequest(
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
    public <T extends RegistryObjectType> SubmitObjectsRequest getSubmitRequest(
            List<T> objs) {
        SubmitObjectsRequest submitRequest = new SubmitObjectsRequest();
        submitRequest.setComment("Object submission");
        submitRequest.setCheckReferences(false);
        submitRequest.setMode(Mode.CREATE_OR_REPLACE);
        submitRequest.setUsername(RegistryUtil.registryUser);
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
    public <T extends RegistryObjectType> RemoveObjectsRequest getRemoveRequest(
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
    public <T extends RegistryObjectType> RemoveObjectsRequest getRemoveRequest(
            List<T> objs) {
        StringBuilder comment = new StringBuilder(objs.size() * 38);
        comment.append("Removing objects: ");
        for (RegistryObjectType regObj : objs) {
            comment.append("[");
            comment.append(regObj.getId());
            comment.append("] ");
        }
        RemoveObjectsRequest request = new RemoveObjectsRequest();
        request.setId(EbxmlObjectUtil.getUUID());
        request.setComment(comment.toString());
        request.setObjectRefList(EbxmlObjectUtil.createObjectRefList(objs));
        return request;

    }

    public void setLcm(LifecycleManagerImpl lcm) {
        this.lcm = lcm;
    }

    public void setClassificationNodeDao(
            ClassificationNodeDao classificationNodeDao) {
        this.classificationNodeDao = classificationNodeDao;
    }

    public void setPersonDao(PersonDao personDao) {
        this.personDao = personDao;
    }

}
