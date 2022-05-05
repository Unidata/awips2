package com.raytheon.uf.edex.registry.ebxml;

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

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.UriInfo;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.security.encryption.AESEncryptor;
import com.raytheon.uf.edex.registry.ebxml.dao.DbInit;
import com.raytheon.uf.edex.registry.ebxml.dao.PersonDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RoleDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.security.SecurityConfiguration;

/**
 * 
 * Registry User Utilitiy class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ---------------------------------
 * Jul 28, 2014  3474     dhladky   Fixed bad ownership settings.
 * Mar 04, 2016  5388     dhladky   Changed AESEncryptor constructor
 * Aug 25, 2016  5846     rjpeter   Remove InternationalString from DB
 * Oct  1, 2018  7238     skabasele Add user info to InitialDbOjectIdsSet
 * 
 * </pre>
 * 
 * @author bphillip
 */

@Path("/registryUsers/")
@Service
@Transactional
public class RegistryUsers {

    public static final String USER_SUFFIX = "_RegistryUser";

    public static final String USER_SLOT_NAME = "user";

    public static final String PASSWORD_SLOT_NAME = "password";

    public static final String ROLE_SLOT_NAME = "role";

    private PersonDao personDao;

    private RoleDao roleDao;

    private LifecycleManager lcm;

    private SecurityConfiguration securityConfig;

    private AESEncryptor encryption;

    @GET
    @Path("getUsers")
    @Produces("text/plain")
    public String getRegistryUsers() {
        List<PersonType> users = personDao.getAll();

        StringBuilder sb = new StringBuilder();
        String name = null;
        String role = null;
        for (PersonType user : users) {
            if (user.getId().endsWith(RegistryUsers.USER_SUFFIX)) {
                name = user.getSlotValue(USER_SLOT_NAME);
                role = user.getSlotValue(ROLE_SLOT_NAME);
                if (role == null) {
                    role = "<None>";
                }
                sb.append(name).append(";");
                sb.append(role).append(";");
            }
        }
        return sb.toString();
    }

    @GET
    @Path("addUser")
    public String addRegistryUser(@Context UriInfo info)
            throws EbxmlRegistryException, MsgRegistryException {
        MultivaluedMap<String, String> params = info.getQueryParameters();
        String userName = params.getFirst("userName");
        String pwd = params.getFirst("pwd");
        String role = params.getFirst("role");

        PersonType user = new PersonType();
        user.setId(userName);
        user.setLid(userName);
        user.setObjectType(RegistryObjectTypes.PERSON);
        user.setName("User " + userName);
        user.setDescription("User" + userName);
        user.setOwner(RegistryUtil.registryUser);
        addUser(userName, pwd, role);
        return "Successfully added user " + userName + " to registry";

    }

    @GET
    @Path("deleteUser/{userName}")
    public String deleteUser(@PathParam("userName") String userName)
            throws MsgRegistryException {
        PersonType user = personDao
                .getById(userName + RegistryUsers.USER_SUFFIX);
        if (user != null) {
            remove(user);
        }
        return "Deleted user [" + userName + "]";

    }

    @GET
    @Path("changePassword")
    public String changePassword(@Context UriInfo info)
            throws MsgRegistryException, EbxmlRegistryException {
        MultivaluedMap<String, String> params = info.getQueryParameters();
        String userName = params.getFirst("userName");
        String pwd = params.getFirst("pwd");
        PersonType user = personDao.getById(userName + USER_SUFFIX);
        if (user == null) {
            return "User [" + userName + "] does not exist";
        }

        ((StringValueType) user.getSlotByName(PASSWORD_SLOT_NAME)
                .getSlotValue()).setStringValue(encryptPassword(pwd));
        submit(user);
        return "Password for user [" + userName + "] successfully updated!";
    }

    @GET
    @Path("changeRole")
    public String changeRole(@Context UriInfo info)
            throws MsgRegistryException {
        MultivaluedMap<String, String> params = info.getQueryParameters();
        String userName = params.getFirst("userName");
        String role = params.getFirst("role");
        PersonType user = personDao.getById(userName + USER_SUFFIX);
        if (user == null) {
            return "User [" + userName + "] does not exist";
        }

        ((StringValueType) user.getSlotByName(ROLE_SLOT_NAME).getSlotValue())
                .setStringValue(role);
        submit(user);
        return "Role for user [" + userName + "] successfully updated!";
    }

    public void addUser(String id, String password, String role)
            throws EbxmlRegistryException, MsgRegistryException {
        if (id == null) {
            throw new EbxmlRegistryException("User ID cannot be null");
        }
        if (userExists(id)) {
            throw new EbxmlRegistryException(
                    "User [" + id + "] already exists");
        }
        if (!roleExists(role)) {
            throw new EbxmlRegistryException(
                    "Role [" + role + "] does not exist");
        }
        PersonType user = new PersonType();
        user.setId(id + USER_SUFFIX);
        user.setLid(user.getId());
        user.setObjectType(RegistryObjectTypes.PERSON);
        user.setName("Registry User " + id);
        user.setDescription("Registry User " + id);
        user.setOwner(RegistryUtil.registryUser);

        AssociationType association = new AssociationType();
        association.setId(id + "_" + role + "_Association");
        association.setLid(association.getId());
        association.setObjectType(RegistryObjectTypes.ASSOCIATION);
        association.setOwner(RegistryUtil.registryUser);
        association.setName(role + " role assocation for user " + id);
        association.setDescription(role + " role assocation for user " + id);
        association.setSourceObject(id);
        association.setTargetObject(role);
        association.setType(AssociationTypes.HAS_ROLE);

        SlotType userSlot = new SlotType(USER_SLOT_NAME,
                new StringValueType(id));
        SlotType passwordSlot = new SlotType(PASSWORD_SLOT_NAME,
                new StringValueType(encryptPassword(password)));
        SlotType roleSlot = new SlotType(ROLE_SLOT_NAME,
                new StringValueType(role));
        user.getSlot().add(userSlot);
        user.getSlot().add(passwordSlot);
        user.getSlot().add(roleSlot);

        /*
         * Adding the user info to the InitialDbOjectIdsSet. That Set contains
         * the initial object Ids that are automatically created during the
         * initialization of the ebxml schema tables. It will used in the
         * registry synchronization process to determine the the synchronization
         * type to proceed with.
         * 
         */
        DbInit.addToInitialDbOjectIdsSet(association.getId());
        DbInit.addToInitialDbOjectIdsSet(user.getId());
        DbInit.addToInitialDbOjectIdsSet(userSlot.getId());
        DbInit.addToInitialDbOjectIdsSet(passwordSlot.getId());
        DbInit.addToInitialDbOjectIdsSet(roleSlot.getId());

        submit(association, user);
    }

    private void remove(RegistryObjectType... objs)
            throws MsgRegistryException {
        ObjectRefListType refList = new ObjectRefListType();
        for (RegistryObjectType obj : objs) {
            refList.getObjectRef().add(new ObjectRefType(obj.getId()));
        }
        RemoveObjectsRequest req = new RemoveObjectsRequest();
        req.setId("Removing registry users");
        req.setComment("Remove request to remove registry users");
        req.setDeleteChildren(true);
        req.setObjectRefList(refList);
        lcm.removeObjects(req);
    }

    private void submit(RegistryObjectType... objs)
            throws MsgRegistryException {
        SubmitObjectsRequest submitObjectsRequest = new SubmitObjectsRequest();
        submitObjectsRequest.setCheckReferences(false);
        submitObjectsRequest.setComment("Modifying registry user");
        submitObjectsRequest.setId("Submit User objects");
        submitObjectsRequest.setMode(Mode.CREATE_OR_REPLACE);
        submitObjectsRequest.setUsername(RegistryUtil.registryUser);
        submitObjectsRequest
                .setRegistryObjectList(new RegistryObjectListType());
        for (RegistryObjectType obj : objs) {
            submitObjectsRequest.getRegistryObjects().add(obj);
        }
        lcm.submitObjects(submitObjectsRequest);

    }

    public boolean userExists(String userId) {
        return personDao.getById(userId + USER_SUFFIX) != null;
    }

    private boolean roleExists(String roleId) {
        return roleDao.getById(roleId) != null;
    }

    private String encryptPassword(String password)
            throws EbxmlRegistryException {
        try {
            return getEncryption().encrypt(password);
        } catch (Exception e) {
            throw new EbxmlRegistryException("Error encrypting password", e);
        }
    }

    /**
     * @param personDao
     *            the personDao to set
     */
    public void setPersonDao(PersonDao personDao) {
        this.personDao = personDao;
    }

    /**
     * @param roleDao
     *            the roleDao to set
     */
    public void setRoleDao(RoleDao roleDao) {
        this.roleDao = roleDao;
    }

    /**
     * @param lcm
     *            the lcm to set
     */
    public void setLcm(LifecycleManager lcm) {
        this.lcm = lcm;
    }

    /**
     * @param securityConfig
     *            the securityConfig to set
     */
    public void setSecurityConfig(SecurityConfiguration securityConfig) {
        this.securityConfig = securityConfig;
    }

    /**
     * Loads the populated encryptor
     * 
     * @return
     */
    public AESEncryptor getEncryption() {

        if (securityConfig != null && encryption == null) {
            encryption = new AESEncryptor(securityConfig.getEncryptionKey());
        }

        return encryption;

    }

}
