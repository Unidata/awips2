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

import java.util.ArrayList;
import java.util.List;

import com.google.common.base.Function;
import com.google.common.collect.Lists;
import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * {@link IGroupDefinitionHandler} in-memory implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2012 0726       djohnson     Initial creation
 * Jan 02, 2013 1441       djohnson     Add deleteByName() and getGroupNames().
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class MemoryGroupDefinitionHandler extends
        BaseMemoryRegistryObjectHandler<GroupDefinition> implements
        IGroupDefinitionHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    public GroupDefinition getByName(String groupName)
            throws RegistryHandlerException {
        for (GroupDefinition obj : getAll()) {
            if (matches(groupName, obj.getGroupName())) {
                return obj;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteByName(String groupName) throws RegistryHandlerException {
        GroupDefinition entity = getByName(groupName);
        if (entity != null) {
            delete(entity);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getGroupNames() throws RegistryHandlerException {
        return new ArrayList<String>(Lists.transform(getAll(),
                new Function<GroupDefinition, String>() {
                    @Override
                    public String apply(GroupDefinition arg0) {
                        return arg0.getGroupName();
                    }
                }));
    }

}
