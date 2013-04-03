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
import com.raytheon.uf.common.datadelivery.registry.ebxml.GroupNameQuery;
import com.raytheon.uf.common.datadelivery.registry.ebxml.GroupQuery;
import com.raytheon.uf.common.registry.RegistryManager;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.RegistryResponse;
import com.raytheon.uf.common.registry.handler.BaseRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * Group definition handler.
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

public class GroupDefinitionHandler extends
        BaseRegistryObjectHandler<GroupDefinition, GroupQuery> implements
        IGroupDefinitionHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    public GroupDefinition getByName(String groupName)
            throws RegistryHandlerException {
        GroupQuery gQuery = getQuery();
        gQuery.setGroupName(groupName);

        RegistryQueryResponse<GroupDefinition> response = RegistryManager
                .getRegistyObjects(gQuery);

        checkResponse(response, "getByName");

        return response.getSingleResult();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteByName(String groupName) throws RegistryHandlerException {
        GroupQuery gQuery = getQuery();
        gQuery.setGroupName(groupName);

        RegistryResponse<GroupDefinition> response = RegistryManager
                .removeRegistyObjects(gQuery);

        checkResponse(response, "getByName");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getGroupNames() throws RegistryHandlerException {
        GroupNameQuery groupNameQuery = new GroupNameQuery();

        RegistryQueryResponse<String> response = RegistryManager
                .getRegistyObjects(groupNameQuery);

        checkResponse(response, "getGroupNames");

        return response.getResults();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<GroupDefinition> getRegistryObjectClass() {
        return GroupDefinition.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected GroupQuery getQuery() {
        return new GroupQuery();
    }
}
