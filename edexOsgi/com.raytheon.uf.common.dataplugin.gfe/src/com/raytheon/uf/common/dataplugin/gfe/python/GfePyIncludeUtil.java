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
package com.raytheon.uf.common.dataplugin.gfe.python;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonIncludePathUtil;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Utility for getting python directories to include
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct  9, 2008            njensen     Initial creation
 * Sep 18, 2012      #1091 randerso    added base directory to getGfeConfigIncludePath
 * Feb 27, 2013      #1447 dgilling    Re-factor based on PythonPathIncludeUtil.
 * Mar 06  2013  15717     jzeng       Change CAVE_STATIC to COMMON_STATIC
 *                                     for GFE localization files
 * Mar 11, 2013      #1759 dgilling    Add method getGfeConfigLF().
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GfePyIncludeUtil extends PythonIncludePathUtil {

    public static final String GFE = "gfe";

    private static final String COMMON_GFE = FileUtil.join(PYTHON, GFE);

    public static final String ITOOL = FileUtil.join(GFE, "itool");

    public static final String AUTO_TEST = FileUtil.join(GFE, "autotest");

    public static final String VTEC = "vtec";

    public static final String USER_PYTHON = FileUtil.join(GFE, "userPython");

    public static final String TESTS = FileUtil.join(USER_PYTHON, "tests");

    public static final String CONFIG = FileUtil.join(USER_PYTHON, "gfeConfig");

    public static final String UTILITIES = FileUtil.join(USER_PYTHON,
            "utilities");

    public static final String SMART_TOOLS = FileUtil.join(USER_PYTHON,
            "smartTools");

    public static final String PROCEDURES = FileUtil.join(USER_PYTHON,
            "procedures");

    public static final String TEXT_PRODUCTS = FileUtil.join(USER_PYTHON,
            "textProducts");

    public static final String TEXT_UTILITIES = FileUtil.join(USER_PYTHON,
            "textUtilities");

    public static final String REGULAR = FileUtil.join(TEXT_UTILITIES,
            "regular");

    public static final String HEADLINE = FileUtil.join(TEXT_UTILITIES,
            "headline");

    public static final String COMBINATIONS = FileUtil
            .join(GFE, "combinations");

    public static final String ISC = FileUtil.join(GFE, "isc");

    public static final String GFE_CONFIG = FileUtil.join("config", GFE);

    public static final String VCMODULES = FileUtil.join(GFE, "vcmodule");

    public static final String VCMOD_UTILS = FileUtil
            .join(VCMODULES, "utility");

    public static LocalizationFile getCommonGfeLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, COMMON_GFE);
    }

    public static LocalizationFile getProceduresLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, PROCEDURES);
    }

    public static LocalizationFile getTextUtilitiesLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, REGULAR);
    }

    public static LocalizationFile getTextProductsLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, TEXT_PRODUCTS);
    }

    public static LocalizationFile getSmartToolsLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, SMART_TOOLS);
    }

    public static LocalizationFile getUtilitiesLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, UTILITIES);
    }

    public static LocalizationFile getVtecLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, VTEC);
    }

    public static LocalizationFile getConfigLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, CONFIG);
    }

    public static LocalizationFile getIToolLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, ITOOL);
    }

    public static LocalizationFile getAutotestLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, AUTO_TEST);
    }

    public static LocalizationFile getHeadlineLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, HEADLINE);
    }

    public static LocalizationFile getTextProductsTemplatesLF(
            LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, TEXT_PRODUCTS);
    }

    public static LocalizationFile getCombinationsLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, COMBINATIONS);
    }

    public static LocalizationFile getTestsLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, TESTS);
    }

    public static LocalizationFile getVCModUtilsLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, VCMOD_UTILS);
    }

    // Include Path getters

    public static String getCommonGfeIncludePath() {
        String pythonDir = getCommonPythonIncludePath();
        String gfeDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE),
                COMMON_GFE);
        return PyUtil.buildJepIncludePath(pythonDir, gfeDir);
    }

    public static String getProceduresIncludePath() {
        return getProceduresIncludePath(true);
    }

    public static String getProceduresIncludePath(boolean includeUser) {
        String baseDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE),
                PROCEDURES);
        String siteDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE),
                PROCEDURES);
        if (includeUser) {
            String userDir = getPath(PATH_MANAGER.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.USER),
                    PROCEDURES);
            return PyUtil.buildJepIncludePath(userDir, siteDir, baseDir);
        } else {
            return PyUtil.buildJepIncludePath(siteDir, baseDir);
        }
    }

    public static String getTextUtilitiesIncludePath() {
        return getTextUtilitiesIncludePath(true);
    }

    public static String getTextUtilitiesIncludePath(boolean includeUser) {
        String baseDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE), REGULAR);
        String configDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED),
                REGULAR);
        String siteDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE), REGULAR);
        if (includeUser) {
            String userDir = getPath(PATH_MANAGER.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.USER),
                    REGULAR);
            return PyUtil.buildJepIncludePath(userDir, siteDir, configDir,
                    baseDir);
        } else {
            return PyUtil.buildJepIncludePath(siteDir, configDir, baseDir);
        }
    }

    public static String getTextProductsIncludePath() {
        return getTextProductsIncludePath(true);
    }

    public static String getTextProductsIncludePath(boolean includeUser) {
        String baseDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE),
                TEXT_PRODUCTS);
        String configDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED),
                TEXT_PRODUCTS);
        String siteDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE),
                TEXT_PRODUCTS);
        if (includeUser) {
            String userDir = getPath(PATH_MANAGER.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.USER),
                    TEXT_PRODUCTS);
            return PyUtil.buildJepIncludePath(userDir, siteDir, configDir,
                    baseDir);
        } else {
            return PyUtil.buildJepIncludePath(siteDir, configDir, baseDir);
        }
    }

    public static String getSmartToolsIncludePath() {
        return getSmartToolsIncludePath(true);
    }

    public static String getSmartToolsIncludePath(boolean includeUser) {
        String baseDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE),
                SMART_TOOLS);
        String siteDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE),
                SMART_TOOLS);
        if (includeUser) {
            String userDir = getPath(PATH_MANAGER.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.USER),
                    SMART_TOOLS);
            return PyUtil.buildJepIncludePath(userDir, siteDir, baseDir);
        } else {
            return PyUtil.buildJepIncludePath(siteDir, baseDir);
        }
    }

    public static String getUtilitiesIncludePath() {
        return getUtilitiesIncludePath(true);
    }

    public static String getUtilitiesIncludePath(boolean includeUser) {
        String baseDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE),
                UTILITIES);
        String siteDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE),
                UTILITIES);
        if (includeUser) {
            String userDir = getPath(PATH_MANAGER.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.USER),
                    UTILITIES);
            return PyUtil.buildJepIncludePath(userDir, siteDir, baseDir);
        } else {
            return PyUtil.buildJepIncludePath(siteDir, baseDir);
        }
    }

    public static String getVtecIncludePath() {
        String baseDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE), VTEC);
        String siteDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE), VTEC);
        return PyUtil.buildJepIncludePath(siteDir, baseDir);
    }

    public static String getVtecIncludePath(String siteId) {
        String baseDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE), VTEC);
        String siteDir = getPath(PATH_MANAGER.getContextForSite(
                LocalizationType.COMMON_STATIC, siteId), VTEC);
        return PyUtil.buildJepIncludePath(siteDir, baseDir);
    }

    public static String getConfigIncludePath() {
        return getConfigIncludePath(true);
    }

    public static String getConfigIncludePath(boolean includeUser) {
        String baseDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE), CONFIG);
        String siteDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE), CONFIG);
        if (includeUser) {
            String userDir = getPath(PATH_MANAGER.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.USER),
                    CONFIG);
            return PyUtil.buildJepIncludePath(userDir, siteDir, baseDir);
        } else {
            return PyUtil.buildJepIncludePath(siteDir, baseDir);
        }
    }

    public static String getIToolIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.BASE), ITOOL);
    }

    public static String getAutotestIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.BASE), AUTO_TEST);
    }

    public static String getHeadlineIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.BASE), HEADLINE);
    }

    public static String getTextProductsTemplatesIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE), TEXT_PRODUCTS);
    }

    public static String getCombinationsIncludePath() {
        return getCombinationsIncludePath(true);
    }

    public static String getCombinationsIncludePath(boolean includeUser) {
        String configDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED),
                COMBINATIONS);
        String siteDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE),
                COMBINATIONS);
        if (includeUser) {
            String userDir = getPath(PATH_MANAGER.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.USER),
                    COMBINATIONS);
            return PyUtil.buildJepIncludePath(userDir, siteDir, configDir);
        } else {
            return PyUtil.buildJepIncludePath(siteDir, configDir);
        }
    }

    public static String getTestsIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.BASE), TESTS);
    }

    public static String getIscScriptsIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.EDEX_STATIC,
                LocalizationLevel.BASE), ISC);
    }

    public static String getGfeConfigIncludePath(String siteId) {
        String baseConfigDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE),
                GFE_CONFIG);
        String siteConfigDir = getPath(PATH_MANAGER.getContextForSite(
                LocalizationType.EDEX_STATIC, siteId), GFE_CONFIG);
        return PyUtil.buildJepIncludePath(siteConfigDir, baseConfigDir);
    }

    public static LocalizationFile getGfeConfigLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, GFE_CONFIG);
    }

    public static String getVCModulesIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE), VCMODULES);
    }

    public static String getVCModUtilsIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE), VCMOD_UTILS);
    }
}
