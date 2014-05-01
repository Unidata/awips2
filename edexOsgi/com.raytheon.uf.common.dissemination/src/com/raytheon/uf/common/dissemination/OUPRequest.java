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
package com.raytheon.uf.common.dissemination;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to disseminate an OUP
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2009            njensen     Initial creation
 * Jun 07, 2013    1981    mpduff      This is now a privileged request.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
@DynamicSerialize
public class OUPRequest extends AbstractPrivilegedRequest implements
        IServerRequest {
    /** Edex origination constant */
    public static transient final String EDEX_ORIGINATION = "edex_origination";

    @DynamicSerializeElement
    private OfficialUserProduct product;

    @DynamicSerializeElement
    private boolean checkBBB = false;

    /** User object */
    @DynamicSerializeElement
    private IUser user;

    /** A not authorized message */
    @DynamicSerializeElement
    private String NotAuthorizedMessage = "Not Authorized to Send Official User Products";

    /**
     * OUP Request permission. This should not be changed.
     */
    @DynamicSerializeElement
    private String roleId = "awips.oup";

    public OfficialUserProduct getProduct() {
        return product;
    }

    public void setProduct(OfficialUserProduct product) {
        this.product = product;
    }

    public boolean isCheckBBB() {
        return checkBBB;
    }

    public void setCheckBBB(boolean checkBBB) {
        this.checkBBB = checkBBB;
    }

    /**
     * @return the user
     */
    @Override
    public IUser getUser() {
        return user;
    }

    /**
     * @param user
     *            the user to set
     */
    @Override
    public void setUser(IUser user) {
        this.user = user;
    }

    /**
     * @return the roleId
     */
    public String getRoleId() {
        return roleId;
    }

    /**
     * @param roleId
     *            the roleId to set
     */
    public void setRoleId(String roleId) {
        this.roleId = roleId;
    }

    /**
     * @return the notAuthorizedMessage
     */
    public String getNotAuthorizedMessage() {
        return NotAuthorizedMessage;
    }

    /**
     * @param notAuthorizedMessage
     *            the notAuthorizedMessage to set
     */
    public void setNotAuthorizedMessage(String notAuthorizedMessage) {
        NotAuthorizedMessage = notAuthorizedMessage;
    }
}
