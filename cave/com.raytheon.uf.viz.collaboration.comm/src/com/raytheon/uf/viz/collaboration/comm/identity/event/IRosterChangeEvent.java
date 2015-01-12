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
package com.raytheon.uf.viz.collaboration.comm.identity.event;

import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Event fired when the roster has changed
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 06, 2012            jkorman     Initial creation.
 * Feb 24, 2014    2632    mpduff      Added getPresence, changed getItem to getEntry.
 * Apr 24, 2014    3070    bclement    getEntry() returns UserId
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface IRosterChangeEvent {

    /**
     * Get the event type.
     * 
     * @return The event type
     */
    RosterChangeType getType();

    /**
     * Get the changed entry.
     * 
     * @return The changed entry
     */
    UserId getEntry();

    /**
     * Get the Presence object.
     * 
     * @return The Presence object
     */
    Presence getPresence();

}
