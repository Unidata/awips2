package com.raytheon.edex.plugin.gfe.server.handler;

import com.raytheon.edex.plugin.gfe.isc.GfeScriptExecutor;
import com.raytheon.uf.common.dataplugin.gfe.request.ExecuteIscMosaicRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2010            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
public class ExecuteIscMosaicRequestHandler implements
        IRequestHandler<ExecuteIscMosaicRequest> {

    @Override
    public ServerResponse<String> handleRequest(ExecuteIscMosaicRequest request)
            throws Exception {
        ServerResponse<String> sr = new ServerResponse<String>();
        GfeScriptExecutor scriptRunner = new GfeScriptExecutor();

        String retVal = scriptRunner.execute("iscMosaic "
                + request.getArgString());

        if (!retVal.equals(GfeScriptExecutor.SUCCESS)) {
            sr.addMessage(retVal);
        }

        return sr;
    }
}
