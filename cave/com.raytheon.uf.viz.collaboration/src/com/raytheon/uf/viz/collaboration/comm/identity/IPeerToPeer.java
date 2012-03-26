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

import java.util.Collection;

import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageFilter;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageListener;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public interface IPeerToPeer extends ISession {

    /**
     * Send a Text message. Note that the recipient of the message is
     * included as an attribute of the message.
     * @param message
     */
    int sendPeerToPeer(IMessage message);

    /**
     * Send a Text message to a specific receiver.
     * @param to The intended receiver.
     * @param message The message to send.
     */
    int sendPeerToPeer(String to, String message);

    /**
     * Add a listener for incoming messages. These messages will be filtered using
     * the supplied message filter. 
     * @param listener A listener for incoming messages.
     * @param filter A filter that either accepts/rejects messages. 
     * @return The listener that was added.
     */
    IMessageListener addMessageListener(IMessageListener listener, IMessageFilter filter);
    
    /**
     * Get the message listeners defined for the session.
     * @return A not null collection of message listeners defined for the session.
     */
    Collection<IMessageListener> getMessageListeners();
    
    /**
     * Remove a message listener from the session.
     * @param listener A listener to remove.
     * @return The listener that was removed. If the listener was not
     * found, a null reference is returned.
     */
    IMessageListener removeMessageListener(IMessageListener listener);
}
