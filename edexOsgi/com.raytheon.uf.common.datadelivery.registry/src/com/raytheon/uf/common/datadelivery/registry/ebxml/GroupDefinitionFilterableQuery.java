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
package com.raytheon.uf.common.datadelivery.registry.ebxml;

import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;
import com.raytheon.uf.common.registry.ebxml.StringAttribute;

/**
 * Base class for filtering on {@link GroupDefinition}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 2, 2013  1441      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class GroupDefinitionFilterableQuery<T> extends
        AdhocRegistryQuery<T> {
    
    @Override
    public Class<GroupDefinition> getObjectType() {
        return GroupDefinition.class;
    }

    /**
     * A setter for the queryable attribute owner equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the specified column name.
     *
     * @param owner
     *        The value of the owner attribute to search for.
     */
    public void setOwner(String owner) {
        setAttribute("owner", new StringAttribute(owner));
    }

    /**
     * A setter for the queryable attribute groupName equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the specified column name.
     *
     * @param groupName
     *        The value of the groupName attribute to search for.
     */
    public void setGroupName(String groupName) {
        setAttribute(GroupDefinition.GROUP_NAME_SLOT, new StringAttribute(
                groupName));
    }

}
