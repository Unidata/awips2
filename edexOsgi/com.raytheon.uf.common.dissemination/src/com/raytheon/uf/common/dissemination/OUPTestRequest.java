package com.raytheon.uf.common.dissemination;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Request to check if an OUPRequest will work
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 20, 2013  DR 16777  D. Friedman Initial creation
 * 
 * </pre>
 * 
 */
@DynamicSerialize
public class OUPTestRequest extends AbstractPrivilegedRequest {

    @DynamicSerializeElement
    OUPRequest oupRequest;

    public IUser getUser() {
        return oupRequest.getUser();
    }

    public void setUser(IUser user) {
        oupRequest.setUser(user);
    }

    public OUPRequest getOupRequest() {
        return oupRequest;
    }

    public void setOupRequest(OUPRequest oupRequest) {
        this.oupRequest = oupRequest;
    }

}
