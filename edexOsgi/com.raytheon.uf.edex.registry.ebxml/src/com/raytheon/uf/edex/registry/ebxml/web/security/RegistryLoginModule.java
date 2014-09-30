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

import java.util.ArrayList;
import java.util.List;

import javax.xml.ws.WebServiceException;

import org.eclipse.jetty.plus.jaas.spi.AbstractLoginModule;
import org.eclipse.jetty.plus.jaas.spi.UserInfo;
import org.eclipse.jetty.util.security.Credential;
import org.eclipse.jetty.util.security.Password;

import com.raytheon.uf.common.registry.services.RegistryServiceException;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * 
 * The registry login module used by the Jetty server hosting the registry
 * services
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/10/2014    1717        bphillip    Initial creation
 * 7/24/2014    1712       bphillip    Spring injection of CredentialCache
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class RegistryLoginModule extends AbstractLoginModule {

    /** Cache of user credentials */
    private CredentialCache credentialCache;

    /**
     * Creates a new RegistryLoginModule
     */
    public RegistryLoginModule() {
        super();
        /*
         * This class is instantiated via reflection by the Jetty server. Therefore
         * direct spring injection is not possible
         */
        this.credentialCache = (CredentialCache) EDEXUtil
                .getESBComponent("credentialCache");
    }

    @Override
    public UserInfo getUserInfo(final String userName) {
        String[] user = null;
        try {
            user = credentialCache.getUser(userName);
        } catch (RegistryServiceException e) {
            throw new WebServiceException("User [" + userName
                    + " Not authorized!", e);
        }
        for (String userField : user) {
            if (userField == null) {
                throw new WebServiceException("User [" + userName
                        + " Not authorized!");
            }
        }
        List<String> roleList = new ArrayList<String>(1);
        roleList.add(user[2]);
        Credential credential = new Password(user[1]);
        UserInfo userInfo = new UserInfo(userName, credential, roleList);
        return userInfo;
    }
}
