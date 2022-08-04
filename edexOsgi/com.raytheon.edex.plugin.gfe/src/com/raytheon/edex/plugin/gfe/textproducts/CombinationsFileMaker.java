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
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.plugin.gfe.reference.MapManager;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.util.FileUtil;

import jep.JepConfig;
import jep.JepException;

/**
 * Class to generate the combinations files. This is basically a big wrapper
 * around a Python call to createComboFiles(), in createComboFiles.py., with
 * lots of PathManager lookups.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 04, 2011            wldougher   Moved from MapManager
 * Dec 15, 2015 5166       kbisanz     Update logging to use SLF4J
 * Jul 18, 2016 5747       dgilling    Move edex_static to common_static.
 * Feb 20, 2018 6602       dgilling    Update for new text utilities path.
 * Jun 03, 2019  7852      dgilling    Update code for jep 3.8.
 *
 * </pre>
 *
 * @author wldougher
 */

public class CombinationsFileMaker {
    private static final Logger theLogger = LoggerFactory
            .getLogger(CombinationsFileMaker.class);

    // Unit tests may use a custom path manager
    protected IPathManager pathMgr = PathManagerFactory.getPathManager();

    /**
     * Generate the combinations files for site from editAreaMap.
     *
     * @param site
     *            The site for which the combinations files should be generated
     * @param editAreaMap
     *            A Map from display names to lists of edit area names
     *
     */
    public void genCombinationsFiles(String site,
            Map<String, ? extends List<String>> editAreaMap) {
        theLogger.info("Combinations Files generation phase");

        if (site == null) {
            throw new IllegalArgumentException("site is null");
        }

        if ("".equals(site)) {
            throw new IllegalArgumentException("site is an empty string");
        }

        if (editAreaMap == null) {
            throw new IllegalArgumentException("editAreaMap is null");
        }

        long t0 = System.currentTimeMillis();

        LocalizationContext cx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String scriptFile = pathMgr.getFile(cx,
                LocalizationUtil.join("gfe", "python", "createComboFiles.py"))
                .getPath();
        String includePath = GfePyIncludeUtil.getCommonPythonIncludePath();
        Map<String, Object> argMap = new HashMap<>();

        LocalizationContext caveStaticConfig = pathMgr.getContext(
                LocalizationContext.LocalizationType.CAVE_STATIC,
                LocalizationContext.LocalizationLevel.CONFIGURED);
        caveStaticConfig.setContextName(site);

        String definitionDir = pathMgr
                .getLocalizationFile(caveStaticConfig,
                        GfePyIncludeUtil.TEXT_UTILITIES)
                .getFile().getPath();
        File outputDirFile = pathMgr.getLocalizationFile(caveStaticConfig,
                FileUtil.join("gfe", "combinations")).getFile();
        outputDirFile.mkdir();
        argMap.put("outputDir", outputDirFile.getPath());
        argMap.put("definitionDir", definitionDir);
        argMap.put("mapDict", editAreaMap);

        try (PythonScript pyScript = new PythonScript(
                new JepConfig().setIncludePath(includePath).setClassLoader(
                        MapManager.class.getClassLoader()),
                scriptFile)) {
            pyScript.execute("createComboFiles", argMap);
        } catch (JepException e) {
            theLogger.error("Error generating combinations files", e);
        }

        long t1 = System.currentTimeMillis();
        theLogger.info("Combinations Files generation time: " + (t1 - t0)
                + " ms");
    }

}
