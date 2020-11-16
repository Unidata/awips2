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

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.request.GetProjectionsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Get Projections Handler
 *
 * Retrieves all projections defined in the GFE serverConfig.py file.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Feb 05, 2018  6493     randerso  Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class GetProjectionsHandler extends BaseGfeRequestHandler
        implements IRequestHandler<GetProjectionsRequest> {

    @Override
    public ServerResponse<List<ProjectionData>> handleRequest(
            GetProjectionsRequest request) throws Exception {
        ServerResponse<List<ProjectionData>> sr = new ServerResponse<>();
        sr.setPayload(getIfpServer(request).getConfig().projectionData());
        return sr;
    }

}
