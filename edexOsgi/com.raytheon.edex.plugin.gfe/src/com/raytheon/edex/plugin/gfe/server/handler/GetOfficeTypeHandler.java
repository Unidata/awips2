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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
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
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/15/09      1995       bphillip    Initial release
 * 09/22/09      3058       rjpeter     Converted to IRequestHandler
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GetOfficeTypeHandler implements
        IRequestHandler<GetOfficeTypeRequest> {
    protected final transient Log logger = LogFactory.getLog(getClass());

    @Override
    public ServerResponse<String> handleRequest(GetOfficeTypeRequest request)
            throws Exception {
        ServerResponse<String> sr = null;
        String siteId = request.getSiteID();
        String forSite = request.getForSite();

        try {
            sr = new ServerResponse<String>();
            List<String> officeTypes = IFPServerConfigManager.getServerConfig(
                    siteId).officeTypes();
            List<String> sites = IFPServerConfigManager.getServerConfig(siteId)
                    .allSites();
            int index = sites.indexOf(forSite);

            if (index == -1) {
                throw new GfeConfigurationException("Site " + forSite
                        + " is not configured");
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
