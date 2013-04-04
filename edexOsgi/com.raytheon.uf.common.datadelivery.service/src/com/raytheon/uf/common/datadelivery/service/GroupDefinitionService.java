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
package com.raytheon.uf.common.datadelivery.service;

import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.datadelivery.registry.GroupDefinitionServiceRequest;
import com.raytheon.uf.common.datadelivery.registry.GroupDefinitionServiceRequest.Type;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * Base implementation of {@link IGroupDefinitionService}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2013 1441       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class GroupDefinitionService extends
        BaseDataDeliveryService<GroupDefinitionServiceRequest> implements
        IGroupDefinitionService {

    /**
     * {@inheritDoc}
     * @throws Exception 
     */
    @Override
    public void deleteGroupDefinition(GroupDefinition group)
            throws RegistryHandlerException {
        GroupDefinitionServiceRequest request = new GroupDefinitionServiceRequest();
        request.setGroup(group);
        request.setType(Type.DELETE);

        sendRequest(request);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Object sendRequest(GroupDefinitionServiceRequest request)
            throws RegistryHandlerException {
        try {
            return super.sendRequest(request);
        } catch (Exception e) {
            throw new RegistryHandlerException(e);
        }
    }

}
