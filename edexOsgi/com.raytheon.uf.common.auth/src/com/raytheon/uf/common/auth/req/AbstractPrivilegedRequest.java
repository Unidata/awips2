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
package com.raytheon.uf.common.auth.req;

import com.raytheon.uf.common.auth.AuthException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Abstract Class for privileged requests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2010            mschenke     Initial creation
 * Mar 06, 2014 2885       bgonzale     Fix code that is now an error in Java 1.7.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public abstract class AbstractPrivilegedRequest implements IServerRequest {

    @DynamicSerializeElement
    private IUser user;

    protected AbstractPrivilegedRequest() {
    }

    public IUser getUser() {
        return user;
    }

    public void setUser(IUser user) {
        this.user = user;
    }

    /**
     * Given the Class and User, construct the request and set the user
     * 
     * @param <T>
     * @param clazz
     * @param user
     * @return
     * @throws AuthException
     */
    public static <T extends AbstractPrivilegedRequest> T createRequest(
            Class<T> clazz, IUser user) throws AuthException {
        try {
            T request = clazz.newInstance();
            request.setUser(user);
            return request;
        } catch (Exception e) {
            throw new AuthException("Error instantiating privileged request: "
                    + clazz.getName(), e);
        }
    }
}