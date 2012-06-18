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
package com.raytheon.uf.viz.collaboration.comm.identity;

import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;


/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public interface IMessage extends IPropertied {
    
    public enum MessageType { CHAT, COLLABORATION };
    
    static final String MESSAGE_TYPE = "type";
    
    static final String TIMESTAMP = "timestamp";
    
    /**
     * @return the to
     */
    IQualifiedID getTo();

    /**
     * @param to
     *            the to to set
     */
    void setTo(IQualifiedID to);

    /**
     * @return the from
     */
    IQualifiedID getFrom();

    /**
     * @param from
     *            the from to set
     */
    void setFrom(IQualifiedID from);
    
    /**
     * Get the subject of this message.
     * @return The subject of this message. The subject may be null.
     */
    String getSubject();
    
    /**
     * Set the subject of this message. If not set the
     * subject is set to null.
     * @param subject The subject of this message.
     */
    void setSubject(String subject);
    
    /**
     * 
     * @return
     */
    MessageType getMessageType();
    
    /**
     * Get the body of this message.
     * @return The body of this message. The body may be null.
     */
    String getBody();
    
    /**
     * Set the data to be transmitted in the message. If not set the
     * body is set to null.
     * @param body The data to be transmitted in the message.
     */
    void setBody(String body);

    /**
     * Returns the body of the message as a byte array.
     * @return The body of the message as binary data.
     */
    byte [] getBodyAsBinary();

    /**
     * Set the body of the message as a byte array.
     * @param body The binary data to be transmitted in the message.
     */
    void getBodyAsBinary(byte [] body);
    
    /**
     * Get the status of this message.
     * @return The message status.
     */
    String getStatus();
    
    /**
     * Set the status of this message.
     * @param The message status.
     */
    void setStatus(String status);
    
    
    /**
     * Get the receipt time for this message in milliseconds from
     * Jan 1, 1970.
     * @return The receipt time stamp. 
     */
    long getTimeStamp();
    
}
