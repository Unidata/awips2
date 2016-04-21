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
package com.raytheon.uf.viz.collaboration.ui.actions;

import org.eclipse.jface.action.Action;
import org.jivesoftware.smack.RosterEntry;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Action to send a subscription request to a user
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 24, 2014            bclement     Initial creation
 * Mar 05, 2014 2837       bclement     added image
 * Apr 24, 2014 3070       bclement     moved contact request logic to contacts manager
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SendSubReqAction extends Action {

    private final RosterEntry entry;


        /**
     * @param entry
     *            roster entry to request a subscription to
     */
    public SendSubReqAction(RosterEntry entry) {
        super("Send Contact Request", IconUtil.getImageDescriptor(Activator
                .getDefault().getBundle(), "add_contact.gif"));
        this.entry = entry;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        CollaborationConnection connection = CollaborationConnection.getConnection();
        ContactsManager manager = connection.getContactsManager();
        try {
            manager.sendContactRequest(IDConverter.convertFrom(entry));
        } catch (CollaborationException e) {
            Activator.statusHandler.error(
                    "Unable to send subscription request", e);
        }
    }

}
