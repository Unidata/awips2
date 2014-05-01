package com.raytheon.uf.edex.plugin.nwsauth.authentication;

import com.raytheon.uf.common.auth.user.IAuthenticationData;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.plugin.nwsauth.user.AuthenticationData;
import com.raytheon.uf.edex.auth.authentication.IAuthenticationStorage;

public class AuthenticationStorage implements IAuthenticationStorage {

    @Override
    public IAuthenticationData getAuthenticationDataForUser(IUser user) {
        return new AuthenticationData();
    }

}
