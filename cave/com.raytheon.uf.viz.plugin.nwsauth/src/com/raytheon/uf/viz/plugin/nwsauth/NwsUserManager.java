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
package com.raytheon.uf.viz.plugin.nwsauth;

import java.util.List;

import com.raytheon.uf.common.auth.user.IAuthenticationData;
import com.raytheon.uf.common.auth.user.IPermission;
import com.raytheon.uf.common.auth.user.IRole;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.plugin.nwsauth.user.User;
import com.raytheon.uf.viz.core.auth.IUserManager;
import com.raytheon.uf.viz.core.requests.INotAuthHandler;

/**
 * Implementation of IUserManager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2010            rgeorge     Initial creation
 * Jun 07, 2013   1981     mpduff      Add an IUser field.
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */

public class NwsUserManager implements IUserManager {

    private final NwsNotAuthHandler notAuthHandler = new NwsNotAuthHandler();

    /** Saved User Name */
    private IUser user;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.auth.IUserManager#getNotAuthHandler()
     */
    @Override
    public INotAuthHandler getNotAuthHandler() {
        return notAuthHandler;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.auth.IUserManager#getUserObject()
     */
    @Override
    public IUser getUserObject() {
        if (this.user == null) {
            String userId = System.getProperty("user.name");
            this.user = new User(userId);
            return this.user;
        } else {
            return user;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.auth.IUserManager#updateUserObject(com.raytheon
     * .uf.common.auth.user.IUser,
     * com.raytheon.uf.common.auth.user.IAuthenticationData)
     */
    @Override
    public void updateUserObject(IUser user, IAuthenticationData authData) {
        this.user = user;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<IPermission> getPermissions(String application) {
        // TODO: Should this pass through to EDEX to get this stuff?
        return NwsRoleDataManager.getInstance().getPermissions(application);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<IRole> getRoles(String application) {
        // TODO: Should this pass through to EDEX to get this stuff?
        return NwsRoleDataManager.getInstance().getRoles(application);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateUserObject(String userId, IAuthenticationData authData) {
        user = new User(userId);
    }
}
