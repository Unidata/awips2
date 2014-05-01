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
package com.raytheon.uf.viz.collaboration.comm.xmpp.internal.smack;

import java.io.IOException;

import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.identity.Namespace;
import org.eclipse.ecf.provider.comm.IAsynchEventHandler;
import org.eclipse.ecf.provider.xmpp.identity.XMPPRoomID;
import org.jivesoftware.smack.packet.Presence;

/**
 * Extends ECFConnection from xmpp provider, adds ability to send presence to a
 * room using the roomId.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 20, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@SuppressWarnings("restriction")
public class ECFConnection extends
        org.eclipse.ecf.internal.provider.xmpp.smack.ECFConnection {

    public ECFConnection(boolean google, Namespace ns, IAsynchEventHandler h) {
        super(google, ns, h);
    }

    @Override
    public void sendPresenceUpdate(ID target, Presence presence)
            throws IOException {
        if (target instanceof XMPPRoomID) {
            // sendPresenceUpdate uses target.getName to determine where to send
            // the presence to, this does not work for roomId because getName
            // will return the room name without the host, so instead of using
            // the super version directly we will setTo on the presence here and
            // then send no target to super.
            presence.setTo(((XMPPRoomID) target).getMucString());
            super.sendPresenceUpdate(null, presence);
        } else {
            super.sendPresenceUpdate(target, presence);
        }
    }

}
