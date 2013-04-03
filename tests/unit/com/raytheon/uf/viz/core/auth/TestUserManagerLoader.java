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
package com.raytheon.uf.viz.core.auth;

import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.auth.user.IAuthenticationData;
import com.raytheon.uf.common.auth.user.IPermission;
import com.raytheon.uf.common.auth.user.IRole;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.plugin.nwsauth.user.User;
import com.raytheon.uf.viz.core.requests.INotAuthHandler;
import com.raytheon.uf.viz.plugin.nwsauth.NwsNotAuthHandler;

/**
 * An {@link IUserManagerLoader} that can be used in tests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2013 1441       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class TestUserManagerLoader implements IUserManagerLoader {

    public static class TestUserManager implements IUserManager {

        private static final User USER = new User("testuser");

        private static final INotAuthHandler NOT_AUTH_HANDLER = new NwsNotAuthHandler();

        /**
         * {@inheritDoc}
         */
        @Override
        public IUser getUserObject() {
            return USER;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void updateUserObject(IUser user, IAuthenticationData authData) {
            // Currently unused
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public INotAuthHandler getNotAuthHandler() {
            return NOT_AUTH_HANDLER;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public List<IPermission> getPermissions(String application) {
            return Collections.emptyList();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public List<IRole> getRoles(String application) {
            return Collections.emptyList();
        }
    }

    private static final TestUserManager USER_MANAGER = new TestUserManager();

    /**
     * {@inheritDoc}
     */
    @Override
    public IUserManager getUserManager() {
        return USER_MANAGER;
    }

}
