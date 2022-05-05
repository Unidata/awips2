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

import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.request.GetWXDefinitionRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.weather.WxDefinition;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * GFE task for retrieving WX Definition
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 08, 0008  1086     dfitch    Initial Creation
 * Sep 22, 0009  3058     rjpeter   Converted to IRequestHandler
 * Dec 15, 0015  5166     kbisanz   Update logging to use SLF4J
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 *
 * </pre>
 *
 * @author dfitch
 */
public class GetWXDefinitionHandler extends BaseGfeRequestHandler
        implements IRequestHandler<GetWXDefinitionRequest> {
    protected final transient Logger logger = LoggerFactory
            .getLogger(getClass());

    @Override
    public ServerResponse<WxDefinition> handleRequest(
            GetWXDefinitionRequest request) throws Exception {
        ServerResponse<WxDefinition> sr = new ServerResponse<WxDefinition>();
        WxDefinition wxDefinition = null;
        try {
            wxDefinition = getIfpServer(request).getConfig().getWxDefinition();
        } catch (GfeException e) {
            logger.error("Error getting Wx definition", e);
            sr.addMessage("Error getting Wx definition");
        }
        sr.setPayload(wxDefinition);
        return sr;

    }

}
