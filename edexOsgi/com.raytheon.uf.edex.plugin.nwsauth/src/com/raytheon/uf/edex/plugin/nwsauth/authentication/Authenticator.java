package com.raytheon.uf.edex.plugin.nwsauth.authentication;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.plugin.nwsauth.user.AuthenticationData;
import com.raytheon.uf.edex.auth.authentication.IAuthenticator;
import com.raytheon.uf.edex.auth.resp.AuthenticationResponse;

public class Authenticator implements IAuthenticator {

    @Override
    public AuthenticationResponse authenticate(IUser user) {
        return new AuthenticationResponse(true, new AuthenticationData());
    }

}
