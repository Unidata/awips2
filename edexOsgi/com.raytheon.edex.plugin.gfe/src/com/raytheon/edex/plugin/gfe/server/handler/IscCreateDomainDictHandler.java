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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.IscCreateDomainDictRequest;
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
 * Task used to correctly construct the ISC Request/Reply Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/06/09      1995       bphillip    Initial port
 * 09/22/09      3058       rjpeter     Converted to IRequestHandler
 * 09/05/13      2307       dgilling    Use better PythonScript constructor.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class IscCreateDomainDictHandler implements
        IRequestHandler<IscCreateDomainDictRequest> {
    protected final transient Log logger = LogFactory.getLog(getClass());

    @Override
    public ServerResponse<Object> handleRequest(
            IscCreateDomainDictRequest request) throws Exception {
        ServerResponse<Object> response = new ServerResponse<Object>();

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
            Object obj = script.execute("createDomainDict", args);
            response.setPayload(obj);

        } catch (JepException e) {
            logger.error("Error occurred creating Isc Domain Dict", e);
        } finally {
            if (script != null) {
                script.dispose();
            }
        }

        return response;
    }
}
