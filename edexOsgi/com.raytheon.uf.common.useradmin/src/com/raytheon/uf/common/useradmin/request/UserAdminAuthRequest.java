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
package com.raytheon.uf.common.useradmin.request;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2012            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */
@DynamicSerialize
public class UserAdminAuthRequest extends AbstractPrivilegedRequest implements ISerializableObject {

    @DynamicSerializeElement
    private String roleId = null;

    @DynamicSerializeElement
    private boolean authorized = false;

    @DynamicSerializeElement
    private String notAuthorizedMessage = "Not Authorized";

    /**
     * Constructor
     */
    public UserAdminAuthRequest() {

    }

    /**
     * @param roleId
     *            the roleId to set
     */
    public void setRoleId(String roleId) {
        this.roleId = roleId;
    }

    /**
     * @return the roleId
     */
    public String getRoleId() {
        return roleId;
    }

    /**
     * @return the authorized
     */
    public boolean isAuthorized() {
        return authorized;
    }

    /**
     * @param authorized
     *            the authorized to set
     */
    public void setAuthorized(boolean authorized) {
        this.authorized = authorized;
    }

    /**
     * @return the notAuthorizedMessage
     */
    public String getNotAuthorizedMessage() {
        return notAuthorizedMessage;
    }

    /**
     * @param notAuthorizedMessage
     *            the failureMessage to set
     */
    public void setNotAuthorizedMessage(String notAuthorizedMessage) {
        this.notAuthorizedMessage = notAuthorizedMessage;
    }
}
