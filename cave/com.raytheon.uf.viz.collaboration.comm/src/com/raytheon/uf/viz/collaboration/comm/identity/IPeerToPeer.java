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

import com.raytheon.uf.viz.collaboration.comm.identity.event.IEventPublisher;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IUser;

/**
 * Peer to peer chat messaging interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            jkorman     Initial creation
 * Feb 13, 2014 2751       bclement    changed 'to' object to UserId
 * Jun 17, 2014 3078       bclement    changed 'to' object to IUser
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface IPeerToPeer extends ISession, IEventPublisher {

    /**
     * Send a Text message. Note that the recipient of the message is included
     * as an attribute of the message.
     * 
     * @param message
     */
    public void sendPeerToPeer(IMessage message) throws CollaborationException;

    /**
     * Send a Text message to a specific receiver.
     * 
     * @param to
     *            The intended receiver.
     * @param message
     *            The message to send.
     */
    public void sendPeerToPeer(IUser to, String message)
            throws CollaborationException;

}
