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
package com.raytheon.uf.viz.collaboration.comm.identity.user;

/**
 * Interface for users on server. Can represent user accounts or users in a
 * chatroom.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2014 2751       bclement     Initial creation
 * Jun 20, 2014 3281       bclement     added getClientIndependentId()
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public interface IUser extends IQualifiedID {

    /**
     * @param other
     * @return true if other user represents same person as this user
     */
    public boolean isSameUser(IUser other);

    /**
     * Get the user's id without any information specific to which client the
     * user is using (resource)
     * 
     * @return
     */
    public String getClientIndependentId();

}
