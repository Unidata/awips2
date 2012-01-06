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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.isc.GfeScript;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.request.IscMakeRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Processes an ISC grid request from the client
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/21/09      1995       bphillip    Initial port
 * 09/22/09      3058       rjpeter     Converted to IRequestHandler
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class IscMakeRequestHandler implements IRequestHandler<IscMakeRequest> {

    private static Map<String, GfeScript> gfeScripts;
    static {
        gfeScripts = new HashMap<String, GfeScript>();
    }

    @Override
    public ServerResponse<Object> handleRequest(IscMakeRequest request)
            throws Exception {
        ServerResponse<Object> response = new ServerResponse<Object>();
        String siteID = request.getSiteID();

        IFPServerConfig config = null;
        try {
            config = IFPServerConfigManager.getServerConfig(siteID);
        } catch (GfeConfigurationException e) {
            response.addMessage("Unable to get server config for site ["
                    + siteID + "]");
            return response;
        }
        try {
            GridLocation domain = config.dbDomain();
            List<Integer> gridDims = new ArrayList<Integer>();
            gridDims.add(domain.getNy());
            gridDims.add(domain.getNx());

            List<Double> gridBoundBox = new ArrayList<Double>();
            gridBoundBox.add(domain.getOrigin().x);
            gridBoundBox.add(domain.getOrigin().y);
            gridBoundBox.add(domain.getExtent().x);
            gridBoundBox.add(domain.getExtent().y);

            Map<String, Object> args = new HashMap<String, Object>();
            args.put("xmlRequest", request.getXml());
            args.put("gridDims", gridDims);
            args.put("gridProj", domain.getProjection().getProjectionID()
                    .toString());
            args.put("gridBoundBox", gridBoundBox);

            args.put("mhs", config.getMhsid());
            args.put("host", config.getServerHost());
            args.put("port", config.getRpcPort());
            args.put("protocol", String.valueOf(config.getProtocolVersion()));
            args.put("site", siteID);
            args.put("xmtScript", config.transmitScript());

            GfeScript script = null;
            if (!gfeScripts.containsKey(siteID)) {
                GfeScript newScript = new GfeScript("IrtServer", siteID);
                newScript.start();
                gfeScripts.put(siteID, newScript);
            }
            script = gfeScripts.get(siteID);
            while(script.isRunning()){
                Thread.sleep(100);
            }
            script.execute("makeISCrequest", args);
            response.setPayload(script.waitFor());
        } catch (Throwable e) {
            e.printStackTrace();
        }
        return response;
    }
}
