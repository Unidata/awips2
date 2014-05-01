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
package com.raytheon.uf.edex.activetable;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Class to extract server info from an XML file through Python. This is done
 * server-side because the script uses IrtAccess, which the client can't import.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2010            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */
public class ServerInfoExtractor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ServerInfoExtractor.class);

    private static String filePath;

    private static String pythonPath;

    private static String vtecPath;

    private static String iscPath;

    private PythonScript python;

    static {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext commonCx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        LocalizationContext edexCx = pathManager.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
        filePath = pathManager.getFile(commonCx,
                "vtec" + File.separator + "ServerInfo.py").getPath();
        pythonPath = pathManager.getFile(commonCx, "python").getPath();
        vtecPath = pathManager.getFile(commonCx, "vtec").getPath();
        iscPath = pathManager.getFile(edexCx, "gfe" + File.separator + "isc")
                .getPath();
    }

    /**
     * Class constructor.
     */
    public ServerInfoExtractor() {
    }

    /**
     * 
     */
    public void dispose() {
        python.dispose();
    }

    /**
     * Extract server info string(s) from XML string.
     * 
     * @param xmlIncoming
     *            String with XML containing source address(es)
     * @return list of server info strings
     */
    @SuppressWarnings("unchecked")
    public List<String> extract(String xmlIncoming) {

        Map<String, Object> args = new HashMap<String, Object>();
        args.put("xmlIncoming", xmlIncoming);

        String[] result = null;
        try {
            try {
                python = new PythonScript(filePath, PyUtil.buildJepIncludePath(
                        pythonPath, vtecPath, iscPath),
                        ServerInfoExtractor.class.getClassLoader());
                try {
                    result = (String[]) python.execute("extractServerInfo",
                            args);
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error extracting server info from xml", e);
                }
            } catch (JepException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error initializing active table python", e);
            }
        } finally {
            if (python != null) {
                python.dispose();
            }
        }

        if (result == null) {
            result = new String[0];
        }
        return Arrays.asList(result);
    }
}
