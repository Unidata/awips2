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

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.request.GetClientsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2009            randerso     Initial creation
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class GetClientsHandler implements IRequestHandler<GetClientsRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetClientsHandler.class);

    // TODO: Use better caching mechanism
    private static ThreadLocal<PythonScript> scriptCache = new ThreadLocal<PythonScript>() {

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.ThreadLocal#initialValue()
         */
        @Override
        public PythonScript initialValue() {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

            File file = pathMgr.getFile(lc, "GetBrokerConnections.py");
            if (file == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to find GetBrokerConnections.py");
                return null;
            }

            try {
                ArrayList<String> preevals = new ArrayList<String>(1);
                preevals.add("sys.argv = ['GetBrokerConnections']");
                return new PythonScript(file.getAbsolutePath(), "", this
                        .getClass().getClassLoader(), preevals);
            } catch (JepException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to set up GetBrokerConnections.py", e);
                return null;
            }
        }

    };

    @Override
    public ServerResponse<List<String>> handleRequest(GetClientsRequest request)
            throws Exception {

        ServerResponse<List<String>> sr = new ServerResponse<List<String>>();
        List<String> clients = new ArrayList<String>();
        sr.setPayload(clients);

        PythonScript ps = scriptCache.get();

        HashMap<String, Object> argsHash = new HashMap<String, Object>();
        argsHash.put("brokerHost", System.getenv("BROKER_ADDR"));
        try {
            Object obj = ps.execute("getConnections", argsHash);
            if (obj instanceof String[]) {
                for (String s : (String[]) obj) {
                    clients.add(s);
                }
            }
        } catch (JepException e) {
            sr.addMessage("Error getting client list - " + e.getMessage());
        }

        return sr;
    }
}
