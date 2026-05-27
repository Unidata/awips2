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

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.request.GridLocRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Class used to get the grid location of a GFE grid
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/20/08     #875       bphillip    Initial Creation
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * 11/18/15     5129       dgilling    Use better return type.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GridLocHandler extends BaseGfeRequestHandler implements
        IRequestHandler<GridLocRequest> {

    @Override
    public ServerResponse<GridLocation> handleRequest(GridLocRequest request)
            throws Exception {
        GridLocation gloc = getIfpServer(request).getConfig().dbDomain();
        ServerResponse<GridLocation> sr = new ServerResponse<>();
        sr.setPayload(gloc);
        return sr;
    }
}
