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

import java.io.File;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.util.FileUtil;

/**
 * PythonScript object to run a GFE ISC script, like ifpnetCDF or iscDataRec.
 * This object's special feature is that it adds site-specific config paths to
 * the python include path before each execution.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2013            dgilling     Initial creation
 * May 22, 2013  #1759     dgilling     Ensure addSitePath() also adds base
 *                                      path.
 * May 31, 2013  #1759     dgilling     Ensure any site-specific paths are 
 *                                      always removed post-execution.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class IscScript extends PythonScript {

    private static final String FILEDIR = GfePyIncludeUtil.ISC;

    private final String scriptName;

    public IscScript(String scriptName) throws JepException {
        super(buildFilePath(scriptName), buildIncludePath(), IscScript.class
                .getClassLoader());
        jep.eval("import RollBackImporter");
        jep.eval("rollbackImporter = RollBackImporter.RollBackImporter()");
        this.scriptName = scriptName;
    }

    private static String buildFilePath(String scriptName) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
        File file = pathMgr.getFile(ctx, FileUtil.join(FILEDIR, scriptName));
        return file.getPath();
    }

    private static String buildIncludePath() {
        return PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getIscScriptsIncludePath());
    }

    public Object execute(String methodName, Map<String, Object> args,
            String siteId) throws JepException {
        try {
            addSiteSpecificInclude(siteId);
            Object retVal = super.execute(methodName, args);
            return retVal;
        } finally {
            // if we don't ensure these two modifications to the python include
            // path happen after every execution, site-specific paths can get
            // stuck if a JepException is thrown by the execute() method.
            jep.eval("rollbackImporter.rollback()");
            removeSiteSpecificInclude(siteId);
        }
    }

    public String getScriptName() {
        return scriptName;
    }

    private void addSiteSpecificInclude(String siteId) throws JepException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        // first we'll add config, then VTEC
        LocalizationContext baseCtx = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
        String basePath = GfePyIncludeUtil.getGfeConfigLF(baseCtx).getFile()
                .getPath();
        LocalizationContext siteCtx = pathMgr.getContextForSite(
                LocalizationType.EDEX_STATIC, siteId);
        String sitePath = GfePyIncludeUtil.getGfeConfigLF(siteCtx).getFile()
                .getPath();
        addSitePath(sitePath, basePath);

        baseCtx = pathMgr.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE);
        basePath = GfePyIncludeUtil.getVtecLF(baseCtx).getFile().getPath();
        siteCtx = pathMgr.getContextForSite(LocalizationType.COMMON_STATIC,
                siteId);
        sitePath = GfePyIncludeUtil.getVtecLF(siteCtx).getFile().getPath();
        addSitePath(sitePath, basePath);
    }

    private void addSitePath(String sitePath, String basePath)
            throws JepException {
        boolean inList = (Boolean) jep.getValue("'" + basePath
                + "' in sys.path");
        int index;
        if (inList) {
            index = (Integer) jep
                    .getValue("sys.path.index('" + basePath + "')");
        } else {
            index = (Integer) jep.getValue("len(sys.path)");
            jep.eval("sys.path.insert(" + index + ", '" + basePath + "')");
        }
        jep.eval("sys.path.insert(" + index + ", '" + sitePath + "')");
    }

    private void removeSiteSpecificInclude(String siteId) throws JepException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        // remove in reverse order
        LocalizationContext siteCtx = pathMgr.getContextForSite(
                LocalizationType.COMMON_STATIC, siteId);
        String path = GfePyIncludeUtil.getVtecLF(siteCtx).getFile().getPath();
        removeSitePath(path);

        siteCtx = pathMgr.getContextForSite(LocalizationType.EDEX_STATIC,
                siteId);
        path = GfePyIncludeUtil.getGfeConfigLF(siteCtx).getFile().getPath();
        removeSitePath(path);
    }

    private void removeSitePath(String path) throws JepException {
        jep.eval("sys.path.remove('" + path + "')");
    }
}
