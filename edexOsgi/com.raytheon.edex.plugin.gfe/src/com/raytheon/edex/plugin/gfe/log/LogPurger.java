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
package com.raytheon.edex.plugin.gfe.log;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import com.raytheon.edex.plugin.gfe.config.GFESiteActivation;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bphillip     Initial creation
 * Sep 05, 2013  #2307     dgilling     Use better PythonScript constructor.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class LogPurger {

    private Map<String, PythonScript> siteScriptMap;

    public LogPurger() {
        siteScriptMap = new HashMap<String, PythonScript>();
    }

    public void purge() throws JepException {

        for (String siteID : GFESiteActivation.getInstance().getActiveSites()) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext cx = pathMgr.getContext(
                    LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);

            String scriptFile = pathMgr
                    .getLocalizationFile(cx,
                            "gfe/isc" + File.separator + "logPurge.py")
                    .getFile().getPath();
            String includePath = PyUtil.buildJepIncludePath(
                    GfePyIncludeUtil.getCommonPythonIncludePath(),
                    GfePyIncludeUtil.getIscScriptsIncludePath(),
                    GfePyIncludeUtil.getGfeConfigIncludePath(siteID));

            PythonScript script = siteScriptMap.get(siteID);
            if (script == null) {
                script = new PythonScript(scriptFile, includePath, this
                        .getClass().getClassLoader());
                siteScriptMap.put(siteID, script);
            }
            Map<String, Object> args = new HashMap<String, Object>();
            script.execute("main", args);
        }
    }
}
