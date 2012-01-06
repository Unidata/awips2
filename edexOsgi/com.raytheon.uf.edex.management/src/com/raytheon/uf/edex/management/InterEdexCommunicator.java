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
package com.raytheon.uf.edex.management;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.management.request.diagnostic.StatusRequest;
import com.raytheon.uf.common.management.response.diagnostic.StatusResponse;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class InterEdexCommunicator {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(InterEdexCommunicator.class);

    public static List<StatusResponse> discoverFriends() {
        List<StatusResponse> result = new ArrayList<StatusResponse>();
        List<ClusterTask> registered = ClusterLockUtils.getLocks(MgmtUtil.TASK);
        if (registered != null) {
            for (ClusterTask ct : registered) {
                String[] details = ct.getId().getDetails().split(":");
                String hostname = details[0];
                String runMode = details[1];
                StatusResponse resp = null;
                try {
                    String localHostname = MgmtUtil.getHostname();
                    String localMode = MgmtUtil.getRunMode();
                    if (!hostname.equals(localHostname)
                            || !runMode.equals(localMode)) {
                        String port = ct.getExtraInfo();
                        String address = MgmtUtil.buildAddress(hostname, port);
                        resp = pingStatus(address);
                    } else {
                        // the entry is this machine, no need to go out on http
                        resp = new StatusResponse();
                        resp.setHostname(localHostname);
                        resp.setJvmName(localMode);
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }

                if (resp != null) {
                    result.add(resp);
                } else {
                    // JVM is gone or unreachable, remove it from the table,
                    // it will be added whenever it is brought back up
                    ClusterLockUtils.deleteLock(MgmtUtil.TASK, ct.getId()
                            .getDetails());
                }
            }
        }

        return result;
    }

    private static StatusResponse pingStatus(String address) throws Exception {
        StatusRequest req = new StatusRequest();
        return (StatusResponse) sendRequest(req, address);
    }

    public static Object sendRequest(IServerRequest request, String address)
            throws CommunicationException, SerializationException, Exception {
        byte[] req = SerializationUtil.transformToThrift(request);
        byte[] resp = HttpClient.getInstance().postBinary(address, req);
        return SerializationUtil.transformFromThrift(resp);
    }
}
