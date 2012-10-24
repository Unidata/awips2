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
package com.raytheon.edex.plugin.gfe.textproducts;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.reference.MapManager;
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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 4, 2011             wldougher   Moved from MapManager
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class AreaDictionaryMaker {
    private static final Log theLogger = LogFactory
            .getLog(AreaDictionaryMaker.class);

    protected IPathManager pathMgr = PathManagerFactory.getPathManager();

    /**
     * Generate the AreaDictionary.py and CityLocation.py scripts for site,
     * using editAreaAttrs.
     * 
     * @param site
     *            The site for which the area dictionary file and city location
     *            file should be generated.
     * @param editAreaAttrs
     *            A Map from edit area names to shape file attributes
     */
    public void genAreaDictionary(String site,
            Map<String, Map<String, Object>> editAreaAttrs) {
        theLogger.info("Area Dictionary generation phase");

        if (site == null) {
            throw new IllegalArgumentException("site is null");
        }

        if ("".equals(site)) {
            throw new IllegalArgumentException("site is an empty string");
        }

        if (editAreaAttrs == null) {
            throw new IllegalArgumentException("null edit area attributes");
        }

        long t0 = System.currentTimeMillis();

        LocalizationContext cx = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
        File scriptFile = pathMgr.getLocalizationFile(cx,
                FileUtil.join("gfe", "createAreaDictionary.py")).getFile();
        String includePath = PyUtil.buildJepIncludePath(true,
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                scriptFile.getParent());
        Map<String, Object> argMap = new HashMap<String, Object>();

        LocalizationContext caveStaticConfig = pathMgr.getContext(
                LocalizationContext.LocalizationType.CAVE_STATIC,
                LocalizationContext.LocalizationLevel.CONFIGURED);
        caveStaticConfig.setContextName(site);
        File outputDirFile = pathMgr.getLocalizationFile(caveStaticConfig,
                FileUtil.join("gfe", "userPython", "textUtilities", "regular"))
                .getFile();
        outputDirFile.mkdir();
        argMap.put("outputDir", outputDirFile.getPath());

        argMap.put("mapDict", editAreaAttrs);

        PythonScript pyScript = null;
        try {
            pyScript = new PythonScript(scriptFile.getPath(), includePath,
                    MapManager.class.getClassLoader());
            pyScript.execute("createAreaDictionary", argMap);
            // createCityLocation uses script globals modified by
            // createAreaDictionary()
            pyScript.execute("createCityLocation", argMap);
        } catch (JepException e) {
            theLogger.error("Error generating area dictionary", e);
        } finally {
            if (pyScript != null) {
                pyScript.dispose();
            }
        }

        long t1 = System.currentTimeMillis();
        theLogger.info("Area Dictionary generation time: " + (t1 - t0) + " ms");
    }
}
