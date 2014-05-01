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
package com.raytheon.uf.edex.management.handler.diagnostic;

import java.util.List;

import com.raytheon.uf.common.management.request.diagnostic.GetClusterMembersRequest;
import com.raytheon.uf.common.management.response.diagnostic.ClusterMembersResponse;
import com.raytheon.uf.common.management.response.diagnostic.StatusResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.management.InterEdexCommunicator;

/**
 * Connects to the other edexes it can discover for a status update
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GetClusterMembersHandler implements
        IRequestHandler<GetClusterMembersRequest> {

    @Override
    public Object handleRequest(GetClusterMembersRequest request)
            throws Exception {
        List<StatusResponse> edexes = InterEdexCommunicator.discoverFriends();
        ClusterMembersResponse container = new ClusterMembersResponse();
        container.setStatus(edexes);
        return container;
    }

}
