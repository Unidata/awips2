package com.raytheon.edex.textdb.handler;

import com.raytheon.edex.textdb.handler.RemoteRetrievalManager.RRJob;
import com.raytheon.uf.common.dataplugin.text.request.RemoteRetrievalRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

public class RemoteRetrievalHandler implements
    IRequestHandler<RemoteRetrievalRequest> {

    private RemoteRetrievalManager rrManager;
    
    @Override
    public Object handleRequest(RemoteRetrievalRequest request)
            throws Exception {
        RRJob job = rrManager.submitRequest(request);
        return job.waitResponse();
    }

    /**
     * @return the rrManager
     */
    public RemoteRetrievalManager getRrManager() {
        return rrManager;
    }

    /**
     * @param rrManager the rrManager to set
     */
    public void setRrManager(RemoteRetrievalManager rrManager) {
        this.rrManager = rrManager;
    }

}
