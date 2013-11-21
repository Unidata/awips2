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
package com.raytheon.uf.viz.collaboration.comm.provider.session;

import org.jivesoftware.smack.Roster;
import org.jivesoftware.smack.RosterListener;
import org.jivesoftware.smack.XMPPConnection;

/**
 * Manages roster from server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 25, 2013 2561       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class RosterManager {

    private final XMPPConnection connection;

    /**
     * @param connection
     */
    public RosterManager(XMPPConnection connection) {
        this.connection = connection;
    }

    /**
     * @return roster from server connection
     */
    public Roster getRoster() {
        return connection.getRoster();
    }

    /**
     * Add listener for roster events
     * 
     * @param listener
     */
    public void addRosterListener(RosterListener listener) {
        getRoster().addRosterListener(listener);
    }

    /**
     * Remove listener for roster events
     * 
     * @param listener
     */
    public void removeRosterListener(RosterListener listener) {
        getRoster().removeRosterListener(listener);
    }

}
