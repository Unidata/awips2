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
package com.raytheon.uf.common.remote.script;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class with common elements for the remote script requests,
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2014 2742       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
@DynamicSerialize
public class RemoteScriptRequest extends AbstractPrivilegedRequest {

    /** User making the request. */
    @DynamicSerializeElement
    private String userId;

    /** not authorized message. */
    @DynamicSerializeElement
    private String notAuthorizedMessage = "Not Authorized";

    /**
     * Getter.
     * 
     * @return userId
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Setter.
     * 
     * @param userId
     */
    public void setUserId(String userId) {
        this.userId = userId;
    }

    /**
     * Getter.
     * 
     * @return notAuthorizedMessage
     */
    public String getNotAuthorizedMessage() {
        return notAuthorizedMessage;
    }

    /**
     * Setter.
     * 
     * @param notAuthorizedMessage
     */
    public void setNotAuthorizedMessage(String notAuthorizedMessage) {
        this.notAuthorizedMessage = notAuthorizedMessage;
    }
}
