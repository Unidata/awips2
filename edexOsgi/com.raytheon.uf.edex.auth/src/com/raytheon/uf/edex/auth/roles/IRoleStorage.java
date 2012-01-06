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
package com.raytheon.uf.edex.auth.roles;

/**
 * Storage class for roles. Should have a concept of a default role which all
 * users get by default and the ability to lookup a role given an id. NOTE, ALL
 * ROLES IDS SHOULD BE TREATED AS CASE-INSENSITIVE
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

public interface IRoleStorage {

    /**
     * Given the (case insensitive) role id, return the role object
     * 
     * @param roleId
     * @return
     */
    public IRole lookupRole(String roleId);

    /**
     * Given the role, determine if it is the default role
     * 
     * @param role
     * @return
     */
    public boolean isDefaultRole(IRole role);

}
