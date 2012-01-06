package com.raytheon.uf.common.plugin.nwsauth.user;

import com.raytheon.uf.common.auth.user.IAuthenticationData;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Implementation of IUser
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 26, 2010            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
@DynamicSerialize
public class User implements IUser {

    @DynamicSerializeElement
    private UserId userId;

    @DynamicSerializeElement
    private AuthenticationData authenticationData;

    public User() {

    }

    public User(String userId) {
        setUserId(new UserId(userId));
    }

    public UserId getUserId() {
        return userId;
    }

    public void setUserId(UserId userId) {
        this.userId = userId;
    }

    public AuthenticationData getAuthenticationData() {
        return authenticationData;
    }

    public void setAuthenticationData(AuthenticationData authenticationData) {
        this.authenticationData = authenticationData;
    }

    @Override
    public UserId uniqueId() {
        return userId;
    }

    @Override
    public IAuthenticationData authenticationData() {
        // TODO Auto-generated method stub
        return this.authenticationData;
    }

    @Override
    public String toString() {
        return this.getClass().getName() + "[userId = " + userId.toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime
                * result
                + ((authenticationData == null) ? 0 : authenticationData
                        .hashCode());
        result = prime * result + ((userId == null) ? 0 : userId.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        User other = (User) obj;
        if (authenticationData == null) {
            if (other.authenticationData != null)
                return false;
        } else if (!authenticationData.equals(other.authenticationData))
            return false;
        if (userId == null) {
            if (other.userId != null)
                return false;
        } else if (!userId.equals(other.userId))
            return false;
        return true;
    }
}
