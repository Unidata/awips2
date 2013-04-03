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
package com.raytheon.uf.common.auth.user;

import java.util.List;

/**
 * Interface for representing a role. A role should be able to determine if it
 * is valid for a specific user
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2010            mschenke     Initial creation
 * Nov 06, 2012 1302       djohnson     Move back to API plugin, add getter for permissions.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IRole {

    /**
     * Get the list of permissions.
     * 
     * @return the list of permissions
     */
    List<IPermission> getPermissions();

    /**
     * Get the description of the role.
     * 
     * @return the description
     */
    String getDescription();

    /**
     * The representation of the role as a string.
     * 
     * @return
     */
    @Override
    String toString();
}
