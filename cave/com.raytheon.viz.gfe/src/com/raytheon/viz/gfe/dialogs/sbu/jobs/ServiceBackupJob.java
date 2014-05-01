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
package com.raytheon.viz.gfe.dialogs.sbu.jobs;

import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.gfe.request.AbstractGfeRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.site.requests.ActivateSiteRequest;
import com.raytheon.uf.common.site.requests.DeactivateSiteRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public abstract class ServiceBackupJob implements Runnable{

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ServiceBackupJob.class);

    protected String primarySite;
    
    protected boolean failed = false;
    
    protected boolean aborted = false;

    protected String name;
    
    protected static SimpleDateFormat dateFormat;
    
    protected static final long THIRTY_MINUTES = 1800000;
    
    static{
        dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * @param name
     */
    protected ServiceBackupJob(String name, String primarySite) {
        this.name = name;
        this.primarySite = primarySite;

    }

    @SuppressWarnings("unchecked")
    protected ServerResponse<String> makeRequest(IServerRequest request)
            throws GFEServerException {
        ServerResponse<String> rval = null;

        try {
            if (request instanceof AbstractGfeRequest) {
                ((AbstractGfeRequest) request).setWorkstationID(VizApp
                        .getWsId());
                ((AbstractGfeRequest) request).setSiteID(primarySite);
            }
            Object obj = ThriftClient.sendRequest(request);
            if ((request instanceof ActivateSiteRequest)
                    || (request instanceof DeactivateSiteRequest)) {
                return new ServerResponse<String>();
            }

            if (obj instanceof ServerResponse) {
                rval = (ServerResponse<String>) obj;
            } else {
                failed = true;
                throw new GFEServerException(
                        "Received invalid response object from GFE Server.  Received ["
                                + obj.getClass().getName() + "] excepted ["
                                + ServerResponse.class.getName());
            }
        } catch (VizException e) {
            failed = true;
            throw new GFEServerException(e);
        }

        if ((rval != null) && !rval.isOkay()) {
            StringBuilder msg = new StringBuilder();
            if (rval.getMessages().size() > 1) {
                msg.append("Errors ");
            } else {
                msg.append("Error ");
            }
            msg.append("occurred on GFE server -");
            Iterator<ServerMsg> iter = rval.getMessages().iterator();
            while (iter.hasNext()) {
                msg.append(iter.next().getMessage());
                if (iter.hasNext()) {
                    msg.append(", ");
                }
            }
            failed = true;
            throw new GFEServerException(msg.toString());

        }

        return rval;
    }
    
    public boolean aborted(){
        return aborted;
    }

    public boolean failed() {
        return failed;
    }

    public String getName() {
        return name;
    }
}
