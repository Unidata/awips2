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
package com.raytheon.uf.edex.auth;

import com.raytheon.uf.edex.auth.authentication.IAuthenticationStorage;
import com.raytheon.uf.edex.auth.authentication.IAuthenticator;
import com.raytheon.uf.edex.auth.roles.IRoleStorage;

/**
 * Authentication Manager class, contains classes for storing and retrieving
 * roles, authentication/user data and the authenticator algorithm class. This
 * class should be instantiated and injected into the AuthManagerFactory using
 * spring xml in an implementing plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AuthManager {

    /** The storage for roles */
    private IRoleStorage roleStorage;

    /** Class used to authenticate a user */
    private IAuthenticator authenticator;

    /** Class used to lookup authentication data for users */
    private IAuthenticationStorage authenticationStorage;

    public IRoleStorage getRoleStorage() {
        return roleStorage;
    }

    public void setRoleStorage(IRoleStorage roleStorage) {
        this.roleStorage = roleStorage;
    }

    public IAuthenticator getAuthenticator() {
        return authenticator;
    }

    public void setAuthenticator(IAuthenticator authenticator) {
        this.authenticator = authenticator;
    }

    public IAuthenticationStorage getAuthenticationStorage() {
        return authenticationStorage;
    }

    public void setAuthenticationStorage(
            IAuthenticationStorage authenticationStorage) {
        this.authenticationStorage = authenticationStorage;
    }

}
