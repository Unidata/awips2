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
package com.raytheon.uf.viz.collaboration.comm.provider.user;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
public class RosterId extends UserId implements IChatID {

    @DynamicSerializeElement
    protected String nickname;

    /**
     * 
     * @param userName
     * @param hostName
     */
    public RosterId(String userName, String hostName) {
        super(userName, hostName);
    }

    /**
     * 
     * @param userName
     * @param hostName
     * @param nickName
     * @param resource
     */
    public RosterId(String userName, String hostName, String resource) {
        super(userName, hostName, resource);
    }

    /**
     * 
     * @param userName
     * @param hostName
     * @param nickName
     * @param resource
     */
    public RosterId(String userName, String hostName, String resource,
            String nickName) {
        super(userName, hostName, resource);
        nickname = nickName;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID#setNickname(java.lang.String)
     */
    @Override
    public void setNickname(String nickname) {
        this.nickname = nickname;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID#getNickname()
     */
    @Override
    public String getNickname() {
        return nickname;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        RosterId other = (RosterId) obj;
        if (nickname == null) {
            if (other.nickname != null)
                return false;
        }
        return true;
    }

    /**
     * 
     * @param user
     * @return
     */
    public static IChatID convertFrom(org.eclipse.ecf.core.user.IUser user) {
        String name = Tools.parseName(user.getID().getName());
        String host = Tools.parseHost(user.getID().getName());
        return new RosterId(name, host, user.getNickname(), null);
    }

}
