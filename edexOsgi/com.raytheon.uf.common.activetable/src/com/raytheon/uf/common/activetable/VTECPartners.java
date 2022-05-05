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
package com.raytheon.uf.common.activetable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import jep.JepConfig;
import jep.JepException;

/**
 * This wraps the VTECPartners.py file, loading it as a Map<String, Object> in
 * memory. This is a singleton; use getInstance() to obtain a reference, and
 * getattr(String) or getattr(String, Object) to obtain variables from the
 * script.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Jul 08, 2010           wldougher  Initial creation
 * May 15, 2010  3157     dgilling   Add convenience methods for retrieving TPC
 *                                   and SPC sites.
 * Aug 03, 2016  5747     dgilling   Move edex_static to common_static.
 * Jun 03, 2019  7852     dgilling   Update code for jep 3.8.
 * Jun 20, 2019  6919     randerso   Fix import of int, float, and bools from
 *                                   VTECPartners
 *
 * </pre>
 *
 * @author wldougher
 */

public class VTECPartners {

    private static final String CONFIG_PATH = LocalizationUtil.join("gfe",
            "config");

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VTECPartners.class);

    private static final ExecutorService executor = Executors
            .newCachedThreadPool();

    private static Map<String, VTECPartners> instanceMap;

    private Map<String, Object> simpleObjects;

    static {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        String localVtecPartnersPath = LocalizationUtil.join("vtec",
                "localVTECPartners.py");
        pathMgr.addLocalizationPathObserver(localVtecPartnersPath, (lf) -> {
            delete(lf.getContext().getContextName());
        });
    }

    /**
     * Private singleton constructor.
     *
     * @throws JepException
     */
    private VTECPartners(String siteId) throws JepException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String filePath = pathMgr
                .getFile(commonStaticBase,
                        LocalizationUtil.join("vtec", "VTECPartners.py"))
                .getPath();

        String pythonPath = pathMgr.getFile(commonStaticBase, "python")
                .getPath();

        LocalizationContext commonStaticSite = pathMgr.getContextForSite(
                LocalizationContext.LocalizationType.COMMON_STATIC, siteId);

        String siteDir = pathMgr.getFile(commonStaticSite, CONFIG_PATH)
                .getPath();

        String baseDir = pathMgr.getFile(commonStaticBase, "vtec")
                .getAbsolutePath();
        String vtecSiteDir = pathMgr.getFile(commonStaticSite, "vtec")
                .getAbsolutePath();


        // Create a method to get the globals from VTECPartners.py, minus
        // builtins, classes, functions, and modules, as a dict.
        List<String> stmts = new ArrayList<>();
        stmts.add("import JUtil");
        stmts.add("def globalkeys():\n" + "    return set(globals().keys())");
        stmts.add("def moduleSimpleObjects():\n"
                + "    global xxyyzz\n"
                + "    currentKeys = globalkeys()\n"
                + "    moduleKeys = currentKeys.difference(xxyyzz)\n"
                + "    globs = globals()\n"
                + "    mso = {}\n"
                + "    for k in moduleKeys:\n"
                + "        if k != 'xxyyzz' and type(globs[k]) in (int, float, str, bool, list, dict):\n"
                + "            mso[k] = globs[k]\n"
                + "    return JUtil.pyDictToJavaMap(mso)");
        stmts.add("xxyyzz = globalkeys()");

        try (PythonScript python = new PythonScript(
                new JepConfig()
                        .setIncludePath(PyUtil.buildJepIncludePath(pythonPath,
                                siteDir, vtecSiteDir, baseDir))
                        .setClassLoader(getClass().getClassLoader()),
                filePath, stmts)) {
            // Get the globals and store them until later for getattr
            Object obj = python.execute("moduleSimpleObjects", null);
            setSimpleObjects(obj);
        }
    }

    /**
     * Setter for simpleObjects (mostly to suppress unchecked cast warning)
     *
     * @param obj
     */
    @SuppressWarnings("unchecked")
    protected void setSimpleObjects(Object obj) {
        simpleObjects = (Map<String, Object>) obj;
    }

    /**
     * Get the singleton instance.
     *
     * @return the singleton instance.
     * @throws ExecutionException
     */
    public static synchronized VTECPartners getInstance(String siteId)
            throws ExecutionException {
        if (instanceMap == null) {
            instanceMap = new HashMap<>();
        }

        VTECPartners instance = instanceMap.get(siteId);
        if (instance == null) {
            try {
                instance = executor.submit(() -> {
                    return new VTECPartners(siteId);
                }).get();
            } catch (InterruptedException | RejectedExecutionException e) {
                statusHandler.warn(
                        "Failed to execute python job to read VTECPartners configuration for site ["
                                + siteId + "]",
                        e);
            }
            instanceMap.put(siteId, instance);
        }
        return instance;
    }

    public static synchronized void delete(String siteId) {
        if (instanceMap == null) {
            throw new IllegalStateException("Trying to delete site [" + siteId
                    + "] from VTECPartners cache before cache has been created.");
        }

        instanceMap.remove(siteId);
    }

    /**
     * A convenience method around getattr(String, Object); equivalent to
     * calling getattr(String, null).
     *
     * @param name
     *            The name of the attribute to retrieve
     * @return the attribute value
     */
    public Object getattr(String name) {
        return getattr(name, null);
    }

    /**
     * Get an object from the VTECPartners.py script.
     * <p>
     * The values are cached at the first call of getInstance(), so the class
     * will have to be reloaded for any changes to the script to be seen.
     *
     * @param name
     *            The name of the attribute to retrieve, i.e., "VTEC_SPC_SITE"
     * @param dft
     *            A default value to use if the attribute does not exist
     * @return the attribute value, or dft if it
     */
    public Object getattr(String name, Object dft) {
        Object obj = null;
        if (simpleObjects == null) {
            obj = dft;
        } else {
            if (simpleObjects.containsKey(name)) {
                obj = simpleObjects.get(name);
            } else {
                obj = dft;
            }
        }
        return obj;
    }

    public Collection<String> getSpcSites() {
        return getSpcSites(null);
    }

    public Collection<String> getSpcSites(String defaultSite) {
        return getSupportedIssuingSites("VTEC_SPC_SITE", defaultSite);
    }

    public Collection<String> getTpcSites() {
        return getTpcSites(null);
    }

    public Collection<String> getTpcSites(String defaultSite) {
        return getSupportedIssuingSites("VTEC_TPC_SITE", defaultSite);
    }

    protected Collection<String> getSupportedIssuingSites(String settingName,
            String defaultValue) {
        Object pyObject = getattr(settingName, defaultValue);

        if (pyObject == null) {
            String msg = String.format(
                    "VTECPartners setting [%s] not configured. Check your localVTECPartners settings.",
                    settingName);
            statusHandler.warn(msg);
        } else if (pyObject instanceof String) {
            String singleSite = (String) pyObject;
            return new HashSet<>(Arrays.asList(singleSite));
        } else if (pyObject instanceof Collection) {
            Collection<?> siteList = (Collection<?>) pyObject;
            Collection<String> retVal = new HashSet<>(siteList.size(), 1f);
            for (Object siteObj : siteList) {
                retVal.add(siteObj.toString());
            }
            return retVal;
        } else {
            String msg = String.format(
                    "Unsupported value for setting [%s]: %s. Check your localVTECPartners settings.",
                    pyObject.toString(), settingName);
            statusHandler.warn(msg);
        }

        return Collections.emptySet();
    }
}
