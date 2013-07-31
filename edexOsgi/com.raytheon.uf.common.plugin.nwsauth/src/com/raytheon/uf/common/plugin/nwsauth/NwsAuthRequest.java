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
package com.raytheon.uf.common.plugin.nwsauth;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.req.IPermissionsService.IAuthorizedPermissionResponse;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Nws Authorization Request object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2013   2232     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class NwsAuthRequest extends AbstractPrivilegedRequest implements
        ISerializableObject, IAuthorizedPermissionResponse {

    /**
     * Authorized flag, true if authorized.
     */
    @DynamicSerializeElement
    private boolean authorized = false;

    /**
     * The not authorized message to display to the user.
     */
    @DynamicSerializeElement
    private String notAuthorizedMessage = "Not Authorized";

    /**
     * List of requested permissions.
     */
    @DynamicSerializeElement
    @XmlElements({ @XmlElement(type = String.class) })
    private List<String> requestList = new ArrayList<String>();

    /**
     * List of permissions that were authorized.
     */
    @DynamicSerializeElement
    @XmlElements({ @XmlElement(type = String.class) })
    private List<String> authorizedList = new ArrayList<String>();

    /**
     * Constructor
     */
    public NwsAuthRequest() {

    }

    /**
     * @return the authorized
     */
    @Override
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

    /**
     * @param permission
     *            Authorized permission
     */
    public void addAuthorized(String permission) {
        this.authorizedList.add(permission);
    }

    /**
     * Add a permission. By having this method, and a varargs version it
     * requires the creator to at least specify one permission.
     * 
     * @param permission
     */
    public void addRequestedPermissions(String permission) {
        this.addRequestedPermissions(permission);
    }

    /**
     * Add permissions.
     * 
     * @param permissions
     */
    public void addRequestedPermissions(String... permissions) {
        for (String permission : permissions) {
            this.requestList.add(permission);
        }
    }

    /**
     * Check whether the authorizations allowed for this user contain a
     * permission.
     * 
     * @param permission
     *            the permission to check for
     * @return true if the authorized list contains the permission
     */
    public boolean isAuthorized(String permission) {
        return this.authorizedList.contains(permission);
    }

    /**
     * @return the requestList
     */
    public List<String> getRequestedPermissions() {
        return this.requestList;
    }

    /**
     * @return the requestList
     */
    public List<String> getRequestList() {
        return requestList;
    }

    /**
     * @return the authorizedList
     */
    public List<String> getAuthorizedList() {
        return authorizedList;
    }

    /**
     * @param requestList
     *            the requestList to set
     */
    public void setRequestList(List<String> requestList) {
        this.requestList = requestList;
    }

    /**
     * @param authorizedList
     *            the authorizedList to set
     */
    public void setAuthorizedList(List<String> authorizedList) {
        this.authorizedList = authorizedList;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean hasPermission(String permission) {
        return isAuthorized();
    }
}
