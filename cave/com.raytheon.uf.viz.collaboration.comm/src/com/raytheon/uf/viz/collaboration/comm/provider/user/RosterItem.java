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
package com.raytheon.uf.viz.collaboration.comm.provider.user;

import org.jivesoftware.smack.packet.Presence;

/**
 * Entry from local roster
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2013            bclement     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class RosterItem {

    private final UserId id;

    private Presence presence;

    /**
     * @param id
     * @param presence
     */
    public RosterItem(UserId id, Presence presence) {
        this.id = id;
        this.presence = presence;
    }

    /**
     * @return the presence
     */
    public Presence getPresence() {
        return presence;
    }

    /**
     * @param presence
     *            the presence to set
     */
    public void setPresence(Presence presence) {
        this.presence = presence;
    }

    /**
     * @return the id
     */
    public UserId getId() {
        return id;
    }

}
