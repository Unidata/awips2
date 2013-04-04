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
package com.raytheon.uf.common.datadelivery.request;

import java.util.List;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Authorization request for data delivery.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2012            mpduff       Initial creation
 * Oct 03, 2012 1241       djohnson     Use {@link DataDeliveryPermission}.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class DataDeliveryAuthRequest extends AbstractPrivilegedRequest implements ISerializableObject {

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

    @DynamicSerializeElement
    private DataDeliveryPermissionsContainer permissionsContainer = new DataDeliveryPermissionsContainer();

    /**
     * Constructor
     */
    public DataDeliveryAuthRequest() {

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

    /**
     * @param permission
     *            Authorized permission
     */
    public void addAuthorized(DataDeliveryPermission permission) {
        this.permissionsContainer.addAuthorized(permission);
    }

    /**
     * Add a permission. By having this method, and a varargs version it
     * requires the creator to at least specify one permission.
     * 
     * @param permission
     */
    public void addRequestedPermissions(DataDeliveryPermission permission) {
        addRequestedPermissions(new DataDeliveryPermission[] { permission });
    }

    /**
     * Add permissions.
     * 
     * @param permissions
     */
    public void addRequestedPermissions(DataDeliveryPermission... permissions) {
        for (DataDeliveryPermission permission : permissions) {
            permissionsContainer.addRequestedPermission(permission);
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
    public boolean isAuthorized(DataDeliveryPermission permission) {
        return permissionsContainer.contains(permission);
    }

    /**
     * @return
     */
    public List<DataDeliveryPermission> getRequestedPermissions() {
        return permissionsContainer.getRequestedPermissions();
    }

    /**
     * Added only to comply with dynamic serialization. DO NOT USE.
     * 
     * @return the permissionsContainer
     * @deprecated added only to comply with dynamic serialization
     */
    @Deprecated
    public DataDeliveryPermissionsContainer getPermissionsContainer() {
        return permissionsContainer;
    }

    /**
     * Added only to comply with dynamic serialization. DO NOT USE.
     * 
     * @param permissionsContainer
     *            the permissionsContainer to set
     * @deprecated added only to comply with dynamic serialization
     */
    @Deprecated
    public void setPermissionsContainer(
            DataDeliveryPermissionsContainer permissionsContainer) {
        this.permissionsContainer = permissionsContainer;
    }
}
