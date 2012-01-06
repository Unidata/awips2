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

import java.util.Arrays;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
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
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GridLocHandler implements IRequestHandler<GridLocRequest> {
    protected final transient Log logger = LogFactory.getLog(getClass());

    @Override
    public ServerResponse<List<GridLocation>> handleRequest(
            GridLocRequest request) throws Exception {
        ServerResponse<List<GridLocation>> sr = new ServerResponse<List<GridLocation>>();
        try {
            sr
                    .setPayload(Arrays
                            .asList(new GridLocation[] { IFPServerConfigManager
                                    .getServerConfig(request.getSiteID())
                                    .dbDomain() }));
        } catch (GfeException e) {
            logger.error("Error getting grid location", e);
            sr.addMessage("Error getting grid location: "
                    + e.getLocalizedMessage());
        }
        return sr;
    }

}
