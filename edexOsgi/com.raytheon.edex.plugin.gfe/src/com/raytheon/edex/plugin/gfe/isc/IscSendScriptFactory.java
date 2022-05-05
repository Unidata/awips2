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
package com.raytheon.edex.plugin.gfe.isc;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;

/**
 * Builds ISCSendScript instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2011            dgilling     Initial creation
 * Jul 14, 2016 5747       dgilling     Moved edex_static to common_static.
 * 
 * </pre>
 * 
 * @author dgilling
 */

public class IscSendScriptFactory {

    private static final String SCRIPT_PATH = LocalizationUtil.join("gfe",
            "python", "isc", "iscExtract.py");

    private IscSendScriptFactory() {
        throw new AssertionError();
    }

    public static IscSendScript constructIscSendScript(String siteId)
            throws JepException {
        String pythonIncludePath = PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getIscScriptsIncludePath(),
                GfePyIncludeUtil.getGfeConfigIncludePath(siteId));

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext edexStaticBase = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String scriptFile = pathMgr.getFile(edexStaticBase, SCRIPT_PATH)
                .getPath();

        return new IscSendScript(scriptFile, pythonIncludePath);
    }
}
