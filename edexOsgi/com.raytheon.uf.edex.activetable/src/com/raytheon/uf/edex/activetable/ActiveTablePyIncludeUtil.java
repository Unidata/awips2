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
package com.raytheon.uf.edex.activetable;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonIncludePathUtil;

/**
 * Utility for getting python directories to include.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2013            dgilling    Initial creation
 * Dec 12, 2014  4953      randerso    Added getCommonGfeIncludePath to allow use of
 *                                     LocalizationSupport in activetable python code
 * Jul 21, 2016  5747      dgilling    Move edex_static to common_static.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class ActiveTablePyIncludeUtil extends PythonIncludePathUtil {

    public static final String GFE = "gfe";

    public static final String COMMON_GFE = LocalizationUtil.join(PYTHON, GFE);

    public static final String GFE_CONFIG = LocalizationUtil
            .join(GFE, "config");

    public static final String ISC = LocalizationUtil.join(GFE, PYTHON, "isc");

    public static final String VTEC = "vtec";

    public static String getGfeConfigIncludePath(String siteId) {
        String baseConfigDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE),
                GFE_CONFIG);
        String siteConfigDir = getPath(PATH_MANAGER.getContextForSite(
                LocalizationType.COMMON_STATIC, siteId), GFE_CONFIG);
        return PyUtil.buildJepIncludePath(siteConfigDir, baseConfigDir);
    }

    public static String getIscScriptsIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE), ISC);
    }

    public static String getVtecIncludePath(String siteId) {
        String baseDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE), VTEC);
        String siteDir = getPath(PATH_MANAGER.getContextForSite(
                LocalizationType.COMMON_STATIC, siteId), VTEC);
        return PyUtil.buildJepIncludePath(siteDir, baseDir);
    }

    public static String getCommonGfeIncludePath() {
        String pythonDir = getCommonPythonIncludePath();
        String gfeDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE),
                COMMON_GFE);
        return PyUtil.buildJepIncludePath(pythonDir, gfeDir);
    }
}
