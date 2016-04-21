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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.dataplugin.gfe.request.GetIscSendStatusRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetIscSendStatusRequest.IscSendStatus;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * GFE request handler for getting ISC send statuses
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2011      #4686 randerso     Initial creation
 * Jun 13, 2013     #2044  randerso     Refactored to use IFPServer
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GetIscSendStatusHandler extends BaseGfeRequestHandler implements
        IRequestHandler<GetIscSendStatusRequest> {
    protected final transient Log logger = LogFactory.getLog(getClass());

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ServerResponse<IscSendStatus> handleRequest(
            GetIscSendStatusRequest request) throws Exception {

        ServerResponse<IscSendStatus> sr = null;
        try {
            IFPServer ifpServer = getIfpServer(request);
            IFPServerConfig config = ifpServer.getConfig();

            boolean sendISConSave = config.sendiscOnSave();
            boolean sendISConPublish = config.sendiscOnPublish();
            boolean requestISC = config.requestISC();

            sr = new ServerResponse<IscSendStatus>();
            IscSendStatus iscSendStatus = new IscSendStatus(sendISConSave,
                    sendISConPublish, requestISC);
            sr.setPayload(iscSendStatus);
        } catch (Exception e) {
            logger.error("Error ISC send status", e);
            sr = new ServerResponse<IscSendStatus>();
            sr.addMessage("Error ISC send status");
        }
        return sr;
    }

}
