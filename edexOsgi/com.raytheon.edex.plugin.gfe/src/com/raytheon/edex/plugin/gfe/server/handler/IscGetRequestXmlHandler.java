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
import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.IscGetRequestXmlRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Class used to generate the request ISC grid xml
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/21/09      1995       bphillip    Initial port
 * 09/22/09      3058       rjpeter     Converted to IRequestHandler
 * 09/05/13      2307       dgilling    Use better PythonScript constructor.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class IscGetRequestXmlHandler implements
        IRequestHandler<IscGetRequestXmlRequest> {

    @Override
    public ServerResponse<String> handleRequest(IscGetRequestXmlRequest request)
            throws Exception {
        ServerResponse<String> response = new ServerResponse<String>();
        PythonScript script = null;
        try {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext cx = pathMgr.getContext(
                    LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);

            String scriptFile = pathMgr
                    .getLocalizationFile(cx,
                            "gfe/isc" + File.separator + "iscUtil.py")
                    .getFile().getPath();
            String includePath = PyUtil.buildJepIncludePath(GfePyIncludeUtil
                    .getCommonPythonIncludePath(), GfePyIncludeUtil
                    .getIscScriptsIncludePath(), GfePyIncludeUtil
                    .getGfeConfigIncludePath(request.getSiteID()));
            script = new PythonScript(scriptFile, includePath, this.getClass()
                    .getClassLoader());
            Map<String, Object> args = new HashMap<String, Object>();
            args.put("xml", request.getXml());
            args.put("selectedServers", request.getSelectedServers());
            args.put("selectedWEList", request.getSelectedWEList());
            String xmlResponse = (String) script.execute("getRequestXML", args);
            response.setPayload(xmlResponse);

        } catch (JepException e) {
            e.printStackTrace();
        } finally {
            if (script != null) {
                script.dispose();
            }
        }

        return response;
    }
}
