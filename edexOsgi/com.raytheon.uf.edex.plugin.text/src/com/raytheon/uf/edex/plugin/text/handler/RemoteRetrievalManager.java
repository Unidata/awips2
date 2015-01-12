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
package com.raytheon.uf.edex.plugin.text.handler;

import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.text.RemoteRetrievalResponse;
import com.raytheon.uf.common.dataplugin.text.request.RemoteRetrievalRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Manages remote retrieval job delegation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                                 Initial creation
 * May 20, 2014 2536       bclement    moved from edex.textdb to edex.plugin.text
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class RemoteRetrievalManager {
    public interface IRRDelegate {
        RemoteRetrievalResponse retrieve(RemoteRetrievalRequest req) throws Exception;
    }
    
    private static final transient IUFStatusHandler statusHandler = UFStatus
        .getHandler(RemoteRetrievalManager.class);

    private String remoteRetrievalExternalURI;
    private HashMap<String, RRJob> jobs = new HashMap<String, RRJob>(4);
    
    public IRRDelegate rrDelegate;
    
    public RRJob submitRequest(RemoteRetrievalRequest req) {
        RRJob job;
        synchronized (jobs) {
            job = jobs.get(req.getMatchKey());
            if (job != null)
                return job;
            job = new RRJob(req);
            jobs.put(req.getMatchKey(), job);
        }
        
        if (rrDelegate != null) {
            try {
                job.setResponse(rrDelegate.retrieve(req));
            } catch (Exception e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Remote retrieval request failed", e);
                job.setResponse(new RemoteRetrievalResponse(false, 
                        "Product Request Status", e.getMessage()));
            }
            synchronized (jobs) {
                jobs.remove(req.getMatchKey());
            }
        }
        
        return job;
    }
    
    class RRJob {
        RemoteRetrievalRequest request;
        RemoteRetrievalResponse response;
        
        public RRJob(RemoteRetrievalRequest request) {
            this.request = request;
        }

        public void setResponse(RemoteRetrievalResponse response) {
            synchronized (this) {
                this.response = response;
                notifyAll();
            }
        }

        public RemoteRetrievalResponse getResponse() {
            return response;
        }
        
        public synchronized RemoteRetrievalResponse waitResponse() throws InterruptedException {
            while (response == null) {
                wait();
            }
            return response;
        }
        
    }

    /**
     * @return the remoteRetrievalExternalURI
     */
    public String getRemoteRetrievalExternalURI() {
        return remoteRetrievalExternalURI;
    }

    /**
     * @param remoteRetrievalExternalURI the remoteRetrievalExternalURI to set
     */
    public void setRemoteRetrievalExternalURI(String remoteRetrievalExternalURI) {
        this.remoteRetrievalExternalURI = remoteRetrievalExternalURI;
    }

    /**
     * @return the rrDelegate
     */
    public IRRDelegate getRrDelegate() {
        return rrDelegate;
    }

    /**
     * @param rrDelegate the rrDelegate to set
     */
    public void setRrDelegate(IRRDelegate rrDelegate) {
        this.rrDelegate = rrDelegate;
    }
}
