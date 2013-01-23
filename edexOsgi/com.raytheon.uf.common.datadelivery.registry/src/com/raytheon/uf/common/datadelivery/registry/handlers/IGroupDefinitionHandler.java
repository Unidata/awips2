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
package com.raytheon.uf.common.datadelivery.registry.handlers;

import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * Handler interface for {@link GroupDefinition}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2012  1241       djohnson     Initial creation
 * Jan 02, 2013 1441       djohnson     Add deleteByName() and getGroupNames().
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IGroupDefinitionHandler extends
        IRegistryObjectHandler<GroupDefinition> {

    /**
     * Retrieve a {@link GroupDefinition} by its name.
     * 
     * @param groupName
     *            the name of the group
     * @return the group, or null if the group can't be found
     * @throws RegistryHandlerException
     *             on error
     */
    GroupDefinition getByName(String groupName) throws RegistryHandlerException;

    /**
     * Delete a {@link GroupDefinition} by its name.
     * 
     * @param groupName
     *            the name
     * @throws RegistryHandlerException
     */
    void deleteByName(String groupName) throws RegistryHandlerException;

    /**
     * Return the list of group names.
     * 
     * @return the list of group names
     * @throws RegistryHandlerException
     */
    List<String> getGroupNames() throws RegistryHandlerException;
}
