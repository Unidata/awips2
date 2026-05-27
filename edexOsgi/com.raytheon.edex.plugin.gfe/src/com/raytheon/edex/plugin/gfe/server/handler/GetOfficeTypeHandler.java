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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.request.GetOfficeTypeRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * GFE Task for getting the office type of a site
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 15, 0009  1995     bphillip  Initial release
 * Sep 22, 0009  3058     rjpeter   Converted to IRequestHandler
 * Dec 15, 0015  5166     kbisanz   Update logging to use SLF4J
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 *
 * </pre>
 *
 * @author bphillip
 */
public class GetOfficeTypeHandler extends BaseGfeRequestHandler
        implements IRequestHandler<GetOfficeTypeRequest> {
    protected final transient Logger logger = LoggerFactory
            .getLogger(getClass());

    @Override
    public ServerResponse<String> handleRequest(GetOfficeTypeRequest request)
            throws GfeException {
        ServerResponse<String> sr = null;
        String forSite = request.getForSite();

        try {
            sr = new ServerResponse<String>();
            IFPServerConfig config = getIfpServer(request).getConfig();
            List<String> officeTypes = config.officeTypes();
            List<String> sites = config.allSites();
            int index = sites.indexOf(forSite);

            if (index == -1) {
                throw new GfeConfigurationException(
                        "Site " + forSite + " is not configured");
            } else {
                String officeType = officeTypes.get(index);
                sr.setPayload(officeType);
            }

        } catch (GfeConfigurationException e) {
            logger.error("Error getting known sites", e);
            sr = new ServerResponse<String>();
            sr.addMessage("Error getting known sites");
        }
        return sr;
    }
}
