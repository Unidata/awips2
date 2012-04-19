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
package com.raytheon.uf.viz.collaboration.comm.identity.roster;

import java.util.Collection;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IRosterListener;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * TODO
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 29, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface IRosterManager {

    /**
     * Get the underlying Roster.
     * 
     * @return The Roster.
     */
    IRoster getRoster();

    /**
     * 
     * @param listener
     *            A roster listener to add to the manager.
     * @return The listener that was added.
     */
    IRosterListener addRosterListener(IRosterListener listener);

    /**
     * Return a collection of all roster listeners for the manager.
     * 
     * @return
     */
    Collection<IRosterListener> getRosterListeners();

    /**
     * Removes a roster listener from the manager.
     * 
     * @param listener
     *            The listener to remove.
     * @return Was the removal successful.
     */
    IRosterListener removeRosterListener(IRosterListener listener);

    /**
     * 
     * @param account
     * @param nickName
     * @param groups
     */
    void sendRosterAdd(String account, String nickName, String[] groups)
            throws CollaborationException;

    /**
     * 
     * @param userId
     */
    void sendRosterRemove(UserId userId) throws CollaborationException;

}
