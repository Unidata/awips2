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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * This wraps the VTECPartners.py file, loading it as a Map<String, Object> in
 * memory. This is a singleton; use getInstance() to obtain a reference, and
 * getattr(String) or getattr(String, Object) to obtain variables from the
 * script.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 08, 2010            wldougher    Initial creation
 * May 15, 2010  #3157     dgilling     Add convenience methods for retrieving
 *                                      TPC and SPC sites.
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class VTECPartners {

    private static final String CONFIG_PATH = "config" + File.separator + "gfe";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VTECPartners.class);

    private static Map<String, VTECPartners> instanceMap;

    private Map<String, Object> simpleObjects;

    /**
     * Private singleton constructor.
     */
    private VTECPartners(String siteId) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String filePath = pathMgr.getFile(commonStaticBase,
                "vtec" + File.separator + "VTECPartners.py").getPath();

        String pythonPath = pathMgr.getFile(commonStaticBase, "python")
                .getPath();

        LocalizationContext edexStaticSite = pathMgr.getContextForSite(
                LocalizationContext.LocalizationType.EDEX_STATIC, siteId);

        String siteDir = pathMgr.getFile(edexStaticSite, CONFIG_PATH).getPath();

        LocalizationContext commonStaticSite = pathMgr.getContextForSite(
                LocalizationContext.LocalizationType.COMMON_STATIC, siteId);

        String baseDir = pathMgr.getFile(commonStaticBase, "vtec")
                .getAbsolutePath();
        String vtecSiteDir = pathMgr.getFile(commonStaticSite, "vtec")
                .getAbsolutePath();

        // Create a method to get the globals from VTECPartners.py, minus
        // builtins, classes, functions, and modules, as a dict.
        List<String> stmts = new ArrayList<String>();
        stmts.add("import JUtil");
        stmts.add("def globalkeys():\n" + "    return set(globals().keys())");
        stmts.add("def moduleSimpleObjects():\n"
                + "    global xxyyzz\n"
                + "    currentKeys = globalkeys()\n"
                + "    moduleKeys = currentKeys.difference(xxyyzz)\n"
                + "    globs = globals()\n"
                + "    mso = {}\n"
                + "    for k in moduleKeys:\n"
                + "        if k != 'xxyyzz' and type(globs[k]) in (str, list, dict):\n"
                + "            mso[k] = globs[k]\n"
                + "    return JUtil.pyDictToJavaMap(mso)");
        stmts.add("xxyyzz = globalkeys()");
        PythonScript python = null;
        try {
            python = new PythonScript(filePath, PyUtil.buildJepIncludePath(
                    pythonPath, siteDir, vtecSiteDir, baseDir),
                    VTECPartners.class.getClassLoader(), stmts);
            // Get the globals and store them until later for getattr
            Object obj = python.execute("moduleSimpleObjects", null);
            setSimpleObjects(obj);
        } catch (JepException e) {
            throw new RuntimeException("Error creating VTECPartners instance",
                    e);
        } finally {
            if (python != null) {
                python.dispose();
            }
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
     */
    public static synchronized VTECPartners getInstance(String siteId) {
        if (instanceMap == null) {
            instanceMap = new HashMap<String, VTECPartners>();
        }

        VTECPartners instance = instanceMap.get(siteId);
        if (instance == null) {
            instance = new VTECPartners(siteId);
            instanceMap.put(siteId, instance);
        }
        return instance;
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
            String msg = String
                    .format("VTECPartners setting [%s] not configured. Check your localVTECPartners settings.",
                            settingName);
            statusHandler.warn(msg);
        } else if (pyObject instanceof String) {
            String singleSite = (String) pyObject;
            return new HashSet<String>(Arrays.asList(singleSite));
        } else if (pyObject instanceof Collection) {
            Collection<?> siteList = (Collection<?>) pyObject;
            Collection<String> retVal = new HashSet<String>(siteList.size(), 1f);
            for (Object siteObj : siteList) {
                retVal.add(siteObj.toString());
            }
            return retVal;
        } else {
            String msg = String
                    .format("Unsupported value for setting [%s]: %s. Check your localVTECPartners settings.",
                            pyObject.toString(), settingName);
            statusHandler.warn(msg);
        }

        return Collections.emptySet();
    }
}
