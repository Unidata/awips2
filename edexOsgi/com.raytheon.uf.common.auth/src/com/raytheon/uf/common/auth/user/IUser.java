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

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Interface for a user object, every user should have an identifier that
 * uniquely ids them and should contain data for authentication
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IUser extends ISerializableObject {

    /**
     * Get the unique id object used to identify the user
     * 
     * @return the user identifier object
     */
    IUserId uniqueId();

    // /**
    // * Retrieve the roles assigned to the user.
    // *
    // * @return the roles
    // */
    // IRole[] getUserRoles();

    /**
     * Get the authentication data used to identify the user
     * 
     * @return data needed to authenticate the user
     */
    IAuthenticationData authenticationData();

}
