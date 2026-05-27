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

import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.IscCreateDomainDictRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
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
 * 07/06/09     1995        bphillip    Initial port
 * 09/22/09     3058        rjpeter     Converted to IRequestHandler
 * 09/05/13     2307        dgilling    Use better PythonScript constructor.
 * 12/15/15     5166        kbisanz     Update logging to use SLF4J
 * 07/14/16     5747        dgilling    Move edex_static to common_static.
 * </pre>
 * 
 * @author bphillip
 */
public class IscCreateDomainDictHandler implements
        IRequestHandler<IscCreateDomainDictRequest> {
    protected final transient Logger logger = LoggerFactory
            .getLogger(getClass());

    private static final String SCRIPT_PATH = LocalizationUtil.join("gfe",
            "python", "isc", "iscUtil.py");

    @Override
    public ServerResponse<Object> handleRequest(
            IscCreateDomainDictRequest request) throws Exception {
        ServerResponse<Object> response = new ServerResponse<>();

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext cx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

        String scriptFile = pathMgr.getFile(cx, SCRIPT_PATH).getPath();
        String includePath = PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getIscScriptsIncludePath(),
                GfePyIncludeUtil.getGfeConfigIncludePath(request.getSiteID()));

        try (PythonScript script = new PythonScript(scriptFile, includePath,
                this.getClass().getClassLoader())) {
            Map<String, Object> args = new HashMap<>();
            args.put("xml", request.getXml());
            Object obj = script.execute("createDomainDict", args);
            response.setPayload(obj);
        } catch (JepException e) {
            logger.error("Error occurred creating Isc Domain Dict", e);
        }

        return response;
    }
}
