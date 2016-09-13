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
package com.raytheon.uf.edex.registry.ebxml.web.security;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import javax.xml.ws.WebServiceException;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.services.RegistryServiceException;
import com.raytheon.uf.common.security.encryption.AESEncryptor;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.registry.ebxml.RegistryUsers;
import com.raytheon.uf.edex.registry.ebxml.dao.PersonDao;
import com.raytheon.uf.edex.registry.ebxml.services.RegistryRESTServices;
import com.raytheon.uf.edex.registry.ebxml.services.rest.RegistryFederationManager;
import com.raytheon.uf.edex.registry.events.CreateAuditTrailEvent;
import com.raytheon.uf.edex.security.SecurityConfiguration;

/**
 * 
 * Cache object for holding users' credentials for accessing registry web
 * services
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/10/2014    1717        bphillip    Initial creation
 * 7/24/2014    1712        bphillip    No longer singleton
 * 1/06/2015    3918        dhladky     Fixed issue where clients can't start without central registry.
 * 5/29/2015    4448        bphillip    Added default user to registry on startup
 * 10/20/2015   4992        dhladky     Improve error handling.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class CredentialCache {

    /** The registry REST services */
    private RegistryRESTServices restServices;

    /** Data access object for person type */
    private PersonDao personDao;

    /** The Hibernate Transaction template */
    private TransactionTemplate txTemplate;

    /** The security configuration */
    private SecurityConfiguration securityConfig;

    /** AESEncryptor object */
    private AESEncryptor encryption;

    /** Field denoting if this registry is running in centralRegistry mode */
    public static final boolean centralRegistry = System.getProperty(
            "edex.run.mode").equals("centralRegistry");

    /** States whether this node will join the federation */
    public static final boolean isFederationEnabled = Boolean
            .getBoolean("ebxml.registry.federation.enabled");

    /** Address of the central registry */
    private static final String CENTRAL_REGISTRY_ADDRESS = "https://"
            + (System.getProperty("ncf.host")) + ":"
            + (System.getProperty("ebxml.registry.webserver.port"));
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CredentialCache.class);
    
    /** used in non federated standalone environment */
    private static final String DEFAULT_ROLE = "RegistryLocalAdministrator";

    /** Cache holding users' credentials */
    private LoadingCache<String, String[]> credentialCache = CacheBuilder
            .newBuilder().maximumSize(1000)
            .expireAfterAccess(60, TimeUnit.MINUTES)
            .build(new CacheLoader<String, String[]>() {
                @Override
                public String[] load(final String userName)
                        throws RegistryHandlerException {
                    return txTemplate
                            .execute(new TransactionCallback<String[]>() {
                                @Override
                                public String[] doInTransaction(
                                        TransactionStatus status) {
                                    String localUserName = userName;
                                    PersonType user = null;
                                    String password = null;
                                    String role = null;
                                    /*
                                     * If we are the central registry, directly
                                     * query the registry
                                     */
                                    if (centralRegistry) {
                                        user = personDao.getById(userName
                                                + RegistryUsers.USER_SUFFIX);
                                    } else {
                                        // This is a case required if you are
                                        // connected to a central registry.
                                        if (isFederationEnabled) {
                                            String compareLocalUser = securityConfig
                                                    .getProperty(RegistryFederationManager.EDEX_SECURITY_AUTH_USER);
                                            if (!compareLocalUser
                                                    .equals(userName)) {
                                                statusHandler
                                                        .error("local registry and security.properties usernames are mismatched! registry userName: "
                                                                + userName
                                                                + " security.properties user: "
                                                                + compareLocalUser);
                                            }
                                            /*
                                             * If we are not the central
                                             * registry, query the central
                                             * registry to get the user's
                                             * information
                                             */
                                            try {
                                                user = restServices
                                                        .getRegistryObject(
                                                                CENTRAL_REGISTRY_ADDRESS,
                                                                userName
                                                                        + RegistryUsers.USER_SUFFIX);
                                            } catch (Exception e) {
                                                throw new WebServiceException(
                                                        "Unable to authenicate user with central registry! userName: "
                                                                + userName, e);
                                            }
                                        }
                                    }

                                    if (isFederationEnabled) {
                                        /*
                                         * User not found in federation mode
                                         * means unauthorized
                                         */
                                        if (user == null) {
                                            throw new WebServiceException(
                                                    "userName: "
                                                            + userName
                                                            + " Is not Authorized!!!! Check your security.properties file for a mismatch!");

                                        } else {
                                            /*
                                             * Put the user name, password, and
                                             * role in the return array. Decrypt
                                             * the password.
                                             */
                                            localUserName = user
                                                    .getSlotValue(RegistryUsers.USER_SLOT_NAME);
                                            
                                            try {
                                                password = encryption.decrypt(
                                                        securityConfig
                                                                .getEncryptionKey(),
                                                        (String) user
                                                                .getSlotValue(RegistryUsers.PASSWORD_SLOT_NAME));
                                            } catch (Exception e) {
                                                throw new RegistryServiceException(
                                                        "Error decrypting password! userName: "+localUserName,
                                                        e);
                                            }
                                            role = user
                                                    .getSlotValue(RegistryUsers.ROLE_SLOT_NAME);
                                        }
                                    } else {
                                        /*
                                         * This is a case where you are not
                                         * connected to a central registry
                                         * (Standalone server and edge
                                         * condition), use defaults.
                                         */

                                        statusHandler
                                                .handle(Priority.INFO,
                                                        "Federation not enabled! Proceeding with default user, pass, and role!");
                                        localUserName = securityConfig.getProperty(RegistryFederationManager.EDEX_SECURITY_AUTH_USER);
                                        password = securityConfig.getSecurityProperties().getProperty(
                                                RegistryFederationManager.EDEX_SECURITY_AUTH_PASSWORD);
                                        role = DEFAULT_ROLE;

                                    }

                                    return new String[] { localUserName,
                                            password, role };
                                }
                            });
                }
            });

    /**
     * Protected constructor
     */
    protected CredentialCache() {

    }

    /**
     * Listens for updates to users and invalidates their entries in the cache
     * if they have changed
     * 
     * @param event
     *            The event to examine
     */
    @Subscribe
    @Transactional(propagation = Propagation.REQUIRED)
    public void processEvent(CreateAuditTrailEvent event) {
        List<RegistryObjectType> objsAffected = event.getObjectsAffected();
        for (RegistryObjectType affectedObj : objsAffected) {
            if (RegistryObjectTypes.PERSON.equals(affectedObj.getObjectType())) {
                credentialCache.invalidate(affectedObj.getId());
            }
        }
    }

    /**
     * Gets a user from the provided user name
     * 
     * @param userName
     *            The user name of the user
     * @return An array containing the user name, password, and role of the user
     * @throws RegistryServiceException
     *             If errors occur while accessing the cache
     */
    public String[] getUser(String userName) throws RegistryServiceException {
        try {
            return credentialCache.get(userName);
        } catch (ExecutionException e) {
            throw new RegistryServiceException("Error retrieving user "
                    + userName);
        }
    }

    /**
     * Gets the role of the given user
     * 
     * @param userName
     *            The user name to get the role for
     * @return The role of the given user
     * @throws RegistryServiceException
     *             If errors occur while accessing the cache
     */
    public String getUserRole(String userName) throws RegistryServiceException {
        try {
            return credentialCache.get(userName)[0];
        } catch (ExecutionException e) {
            throw new RegistryServiceException(
                    "Error retrieving role for user " + userName);
        }
    }

    /**
     * Gets the password for the given user
     * 
     * @param userName
     *            The user to get the password for
     * @return The password for the given user
     * @throws RegistryServiceException
     *             If errors occur while accessing the cache
     */
    public String getUserPassword(String userName)
            throws RegistryServiceException {
        try {
            return credentialCache.get(userName)[1];
        } catch (ExecutionException e) {
            throw new RegistryServiceException(
                    "Error retrieving password for user " + userName);
        }
    }

    /**
     * @param restServices
     *            the restServices to set
     */
    public void setRestServices(RegistryRESTServices restServices) {
        this.restServices = restServices;
    }

    /**
     * @param personDao
     *            the personDao to set
     */
    public void setPersonDao(PersonDao personDao) {
        this.personDao = personDao;
    }

    /**
     * @param txTemplate
     *            the txTemplate to set
     */
    public void setTxTemplate(TransactionTemplate txTemplate) {
        this.txTemplate = txTemplate;
    }

    /**
     * @param securityConfig
     *            the securityConfig to set
     */
    public void setSecurityConfig(SecurityConfiguration securityConfig) {
        this.securityConfig = securityConfig;
    }

    /**
     * @param encryption
     *            the encryption to set
     */
    public void setEncryption(AESEncryptor encryption) {
        this.encryption = encryption;
    }

}
