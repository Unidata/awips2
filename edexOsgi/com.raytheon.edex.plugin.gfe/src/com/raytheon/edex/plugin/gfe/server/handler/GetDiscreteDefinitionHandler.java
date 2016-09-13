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

package com.raytheon.edex.plugin.gfe.server.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.request.GetDiscreteDefinitionRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * GFE task for retrieving Discrete Definition
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/08/08     1086       dfitch      Initial Creation
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * 12/15/15     5166       kbisanz     Update logging to use SLF4J
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */
public class GetDiscreteDefinitionHandler implements
        IRequestHandler<GetDiscreteDefinitionRequest> {
    protected final transient Logger logger = LoggerFactory
            .getLogger(getClass());

    @Override
    public ServerResponse<DiscreteDefinition> handleRequest(
            GetDiscreteDefinitionRequest request) throws Exception {
        ServerResponse<DiscreteDefinition> sr = new ServerResponse<DiscreteDefinition>();
        DiscreteDefinition discreteDefinition = null;
        try {
            discreteDefinition = IFPServerConfigManager.getServerConfig(
                    request.getSiteID()).getDiscreteDefinition();
        } catch (GfeException e) {
            logger.error("Error getting discrete definition", e);
            sr.addMessage("Error getting discrete definition");
        }
        sr.setPayload(discreteDefinition);
        return sr;
    }
}
