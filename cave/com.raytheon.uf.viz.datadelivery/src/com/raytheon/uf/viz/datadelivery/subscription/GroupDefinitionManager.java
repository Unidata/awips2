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
package com.raytheon.uf.viz.datadelivery.subscription;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * The Data Delivery Group Definition Manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- -------------------------
 * Jul 10, 2012    702      jpiatt      Initial creation.
 * Aug 20, 2012 0743       djohnson     Finish making registry type-safe.
 * Oct 05, 2012 1241       djohnson     Replace RegistryManager calls with registry handler calls.
 * Jan 02, 2013 1441       djohnson     Access GroupDefinitionManager in a static fashion.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class GroupDefinitionManager {

    /** Status handler */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GroupDefinitionManager.class);
    /**
     * Get all the group objects.
     * 
     * @return List of Group objects
     */
    public static List<GroupDefinition> getAllGroups() {
        List<GroupDefinition> groups = Collections.emptyList();

        try {
            groups = DataDeliveryHandlers.getGroupDefinitionHandler().getAll();
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving group data from the registry.", e);
        }

        return groups;
    }

    /**
     * Get the group object.
     * 
     * @param groupName
     *            name of the group
     * 
     * @return GroupDefinition obj
     */
    public static GroupDefinition getGroup(String groupName) {
        try {
            return DataDeliveryHandlers.getGroupDefinitionHandler().getByName(
                    groupName);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving group data from the registry.", e);
            return null;
        }
    }

    /**
     * Check whether there are groups available.
     * 
     * @return true if there are groups available
     */
    public static boolean hasGroups() {
        return !CollectionUtil.isNullOrEmpty(getGroupNames());
    }

    /**
     * Return the list of group names.
     * 
     * @return
     */
    public static List<String> getGroupNames() {
        try {
            return new ArrayList<String>(DataDeliveryHandlers
                    .getGroupDefinitionHandler().getGroupNames());
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving group names from the registry.", e);
            return new ArrayList<String>();
        }
    }

}
