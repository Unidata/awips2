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
package com.raytheon.uf.viz.collaboration.display.data;

import com.raytheon.uf.viz.collaboration.comm.identity.user.IUser;

/**
 * Interface for color managers that keep track of color settings for users in a
 * session or chat.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2015 3709       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface IColorManager<T extends IUser> {

    /**
     * Get assigned color for user
     * 
     * @param user
     * @return
     */
    public UserColorInfo getColorForUser(T user);

    /**
     * Assign color to user
     * 
     * @param user
     * @param color
     */
    public void setColorForUser(T user, UserColorInfo color);

    /**
     * Clear color assignments
     */
    public void clearColors();

    /**
     * @return human readable description of color management
     */
    public String getDescription(T user);

}
