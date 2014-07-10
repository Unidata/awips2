package com.raytheon.uf.edex.registry.ebxml;

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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.InternationalStringType;
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
import com.raytheon.uf.edex.registry.ebxml.dao.PersonDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RoleDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.security.SecurityConfiguration;

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
    public String addRegistryUser(@Context
    UriInfo info) throws EbxmlRegistryException, MsgRegistryException {
        MultivaluedMap<String, String> params = info.getQueryParameters();
        String userName = params.getFirst("userName");
        String pwd = params.getFirst("pwd");
        String role = params.getFirst("role");

        PersonType user = new PersonType();
        user.setId(userName);
        user.setLid(userName);
        user.setObjectType(RegistryObjectTypes.PERSON);
        user.setName(new InternationalStringType("User " + userName));
        user.setDescription(new InternationalStringType("User" + userName));
        user.setOwner(RegistryUtil.DEFAULT_OWNER);
        addUser(userName, pwd, role);
        return "Successfully added user " + userName + " to registry";

    }

    @GET
    @Path("deleteUser/{userName}")
    public String deleteUser(@PathParam("userName")
    String userName) throws MsgRegistryException {
        PersonType user = personDao.getById(userName
                + RegistryUsers.USER_SUFFIX);
        if (user != null) {
            remove(user);
        }
        return "Deleted user [" + userName + "]";

    }

    @GET
    @Path("changePassword")
    public String changePassword(@Context
    UriInfo info) throws MsgRegistryException, EbxmlRegistryException {
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
    public String changeRole(@Context
    UriInfo info) throws MsgRegistryException {
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
            throw new EbxmlRegistryException("User [" + id + "] already exists");
        }
        if (!roleExists(role)) {
            throw new EbxmlRegistryException("Role [" + role
                    + "] does not exist");
        }
        PersonType user = new PersonType();
        user.setId(id + USER_SUFFIX);
        user.setLid(user.getId());
        user.setObjectType(RegistryObjectTypes.PERSON);
        user.setName(new InternationalStringType("Registry User " + id));
        user.setDescription(new InternationalStringType("Registry User " + id));
        user.setOwner(RegistryUtil.DEFAULT_OWNER);

        AssociationType association = new AssociationType();
        association.setId(id + "_" + role + "_Association");
        association.setLid(association.getId());
        association.setObjectType(RegistryObjectTypes.ASSOCIATION);
        association.setOwner(RegistryUtil.DEFAULT_OWNER);
        association.setName(new InternationalStringType(role
                + " role assocation for user " + id));
        association.setDescription(new InternationalStringType(role
                + " role assocation for user " + id));
        association.setSourceObject(id);
        association.setTargetObject(role);
        association.setType(AssociationTypes.HAS_ROLE);

        SlotType userSlot = new SlotType(USER_SLOT_NAME,
                new StringValueType(id));
        SlotType passwordSlot = new SlotType(PASSWORD_SLOT_NAME,
                new StringValueType(encryptPassword(password)));
        SlotType roleSlot = new SlotType(ROLE_SLOT_NAME, new StringValueType(
                role));
        user.getSlot().add(userSlot);
        user.getSlot().add(passwordSlot);
        user.getSlot().add(roleSlot);

        submit(association, user);
    }

    private void remove(RegistryObjectType... objs) throws MsgRegistryException {
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

    private void submit(RegistryObjectType... objs) throws MsgRegistryException {
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
    
    private String encryptPassword(String password) throws EbxmlRegistryException{
        try {
            return encryption.encrypt(securityConfig.getEncryptionKey(), password);
        } catch (Exception e) {
            throw new EbxmlRegistryException("Error encrypting password",e);
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
     * @param securityConfig the securityConfig to set
     */
    public void setSecurityConfig(SecurityConfiguration securityConfig) {
        this.securityConfig = securityConfig;
    }

    /**
     * @param encryption the encryption to set
     */
    public void setEncryption(AESEncryptor encryption) {
        this.encryption = encryption;
    }
    
    

}
