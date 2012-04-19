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
package com.raytheon.uf.viz.collaboration.comm.identity.info;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface IVenueInfo {

    /**
     * 
     * @return
     */
    String getVenueDescription();

    /**
     * Get a long name for venue
     * 
     * @return
     */
    String getVenueName();

    /**
     * 
     * @return
     */
    String getVenueSubject();

    /**
     * 
     * @return
     */
    String getVenueID();

    /**
     * Get a count of the current number of room participants
     * 
     * @return Count of the current number of room participants
     */
    int getParticipantCount();

    /**
     * 
     * @return
     */
    boolean isModerated();

    /**
     * 
     * @return
     */
    boolean isPersistent();

    /**
     * 
     * @return
     */
    boolean requiresPassword();

}
