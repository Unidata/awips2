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

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response for OUPRequests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2009            njensen     Initial creation
 * Dec 15, 2009 DR3778     M. Huang    Add acknowledgment handling
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class OUPResponse implements ISerializableObject {

    @DynamicSerializeElement
    private boolean attempted;

    @DynamicSerializeElement
    private boolean sendLocalSuccess;

    @DynamicSerializeElement
    private boolean sendWANSuccess;

    @DynamicSerializeElement
    private String message;

    @DynamicSerializeElement
    private String messageId;

    @DynamicSerializeElement
    private String changedBBB;

    @DynamicSerializeElement
    private boolean needAcknowledgment;

    @DynamicSerializeElement
    private boolean acknowledged;

    /**
     * Message in the response, may be empty or null if there is no error
     * 
     * @return
     */
    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getChangedBBB() {
        return changedBBB;
    }

    public void setChangedBBB(String changedBBB) {
        this.changedBBB = changedBBB;
    }

    public String getMessageId() {
        return messageId;
    }

    public void setMessageId(String messageId) {
        this.messageId = messageId;
    }

    public void setNeedAcknowledgment(boolean ack) {
        this.needAcknowledgment = ack;
    }

    /**
     * Indicates if the request needed an acknowledgment
     * 
     * @return
     */
    public boolean getNeedAcknowledgment() {
        return needAcknowledgment;
    }

    /**
     * indicates if an acknowledgment was received
     * 
     * @return
     */
    public boolean isAcknowledged() {
        return acknowledged;
    }

    public void setAcknowledged(boolean acknowledged) {
        this.acknowledged = acknowledged;
    }

    /**
     * @param sendLocalSuccess
     *            the sendLocalSuccess to set
     */
    public void setSendLocalSuccess(boolean sendLocalSuccess) {
        this.sendLocalSuccess = sendLocalSuccess;
    }

    /**
     * indicates if the product was stored locally
     * 
     * @return the sendLocalSuccess
     */
    public boolean isSendLocalSuccess() {
        return sendLocalSuccess;
    }

    /**
     * 
     * @param sendWANSuccess
     *            the sendWANSuccess to set
     */
    public void setSendWANSuccess(boolean sendWANSuccess) {
        this.sendWANSuccess = sendWANSuccess;
    }

    /**
     * indicates if the product was sent to WAN
     * 
     * @return the sendWANSuccess
     */
    public boolean isSendWANSuccess() {
        return sendWANSuccess;
    }

    public void setAttempted(boolean attempted) {
        this.attempted = attempted;
    }

    /**
     * indicates if a store was even attempted locally
     * 
     * @return
     */
    public boolean isAttempted() {
        return attempted;
    }

    /**
     * indicates if any type of failure occurred as indicated by the status of
     * this response
     * 
     * @return
     */
    public boolean hasFailure() {
        boolean rval = false;

        if (!attempted) {
            rval = true;
        } else if (!sendLocalSuccess) {
            rval = true;
        } else if (!sendWANSuccess) {
            rval = true;
        } else if (needAcknowledgment && !acknowledged) {
            rval = true;
        }

        return rval;
    }
}
