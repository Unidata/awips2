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
package com.raytheon.viz.gfe.config;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonEval;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import jep.JepConfig;
import jep.JepException;

/**
 * Catalog of gfe config files
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2010            randerso     Initial creation
 * Jul 08, 2014 3361       njensen      Consolidated code
 * Sep 29, 2014 2975       njensen      Only look up files in CAVE_STATIC
 * Feb 05, 2016 5242       dgilling     Remove calls to deprecated Localization APIs.
 * Dec 06, 2019 7989       dgilling     Check isHidden flag on arbitrary thread to
 *                                      prevent Jep thread collisions.
 *
 * </pre>
 *
 * @author randerso
 */

public class ConfigCatalog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String EXTENSION = ".py";

    private static final List<String> PRE_EVALS = Collections.unmodifiableList(
            List.of("import importlib\n", "def checkHideConfigFile(name): \n"
                    + "    module = importlib.import_module(name)\n"
                    + "    return bool(getattr(module, 'HideConfigFile', False))\n\n"));

    private PythonEval python = null;

    private boolean isHidden(LocalizationFile lf) {
        if (lf == null || !lf.exists()) {
            throw new IllegalArgumentException("No such GFE config file: "
                    + lf);
        }

        try {
            return (Boolean) python.getValue(
                    String.format("checkHideConfigFile('%s')", scriptName(lf)));
        } catch (JepException e) {
            statusHandler.warn(
                    String.format("Unable to parse GFEConfig file [%s].", lf),
                    e);
        }

        return true;
    }

    /**
     * Get all the localization files for this catalog.
     *
     * @return the localization files for the procedures.
     */
    private LocalizationFile[] getFiles() {
        LocalizationFile[] procFiles = PathManagerFactory.getPathManager()
                .listStaticFiles(LocalizationType.CAVE_STATIC,
                        GfePyIncludeUtil.CONFIG, new String[] { EXTENSION },
                        false, true);
        return procFiles;
    }

    /**
     * Get the simple names of the procedures.
     *
     * @return the simple names of the procedures.
     */
    public Map<String, Boolean> getNames() {
        LocalizationFile[] procFiles = getFiles();

        Future<Map<String, Boolean>> inventoryJob = ForkJoinPool.commonPool()
                .submit(() -> {
                    try {
                        python = new PythonEval(
                                new JepConfig().addIncludePaths(
                                        GfePyIncludeUtil.getConfigIncludePath(),
                                        GfePyIncludeUtil.getVtecIncludePath())
                                        .setClassLoader(
                                                getClass().getClassLoader()),
                                PRE_EVALS);
                        Map<String, Boolean> inventory = new HashMap<>();
                        for (LocalizationFile lf : procFiles) {
                            try {
                                inventory.put(scriptName(lf), isHidden(lf));
                            } catch (IllegalArgumentException e) {
                                statusHandler.warn(String.format(
                                        "Unable to parse GFEConfig file [%s].",
                                        lf), e);
                            }
                        }

                        return inventory;
                    } catch (JepException e) {
                        statusHandler.error(
                                "Unable to create PythonEval instance to parse GFEConfig files.",
                                e);
                    } finally {
                        if (python != null) {
                            try {
                                python.close();
                            } catch (JepException e) {
                                statusHandler.warn(
                                        "Unhandled exception thrown trying to close PythonEval instance.",
                                        e);
                            }
                            python = null;
                        }
                    }

                    return Collections.emptyMap();
                });

        try {
            return inventoryJob.get();
        } catch (InterruptedException | ExecutionException e) {
            statusHandler.error("Unable to retrieve GFEConfig inventory.", e);
        }
        return Collections.emptyMap();
    }

    /**
     * Get the script names from an array of LocalizationFiles, with any leading
     * directories and trailing ".py"s removed.
     *
     * @param scriptFiles
     *            the array of LocalizationFiles.
     * @return a Collection of simple script names with no duplicates.
     */
    private String scriptName(LocalizationFile scriptFile) {
        String fname = scriptFile.getPath();
        String[] fsplit = fname.split(IPathManager.SEPARATOR);
        return fsplit[fsplit.length - 1].replaceAll("\\.py$",
                StringUtils.EMPTY);
    }
}
