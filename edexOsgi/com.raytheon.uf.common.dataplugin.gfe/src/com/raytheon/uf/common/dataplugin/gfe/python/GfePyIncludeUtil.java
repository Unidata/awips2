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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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
 * Mar 11, 2013      #1759 dgilling    Add method getGfeConfigLF().
 * Sep 16, 2013      #1759 dgilling    Move tests and autotests to GfeCavePyIncludeUtil.
 * Aug 22, 2014      #3500 bclement    added python path in getConfigIncludePath()
 * Nov 11, 2014      #4953 randerso    Changed COMMON_GFE to public
 * Jan 23, 2015      #4027 randerso    added configured and site to getCommonGfeIncludePath
 * Mar 12, 2015      #4246 randerso    Changes to support VCModules at base, site, and user levels
 * Jan 29, 2016      #5137 dgilling    Ensure all directory levels exist for 
 *                                     smart tools, procedures, text products, 
 *                                     utilities and text utilities.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GfePyIncludeUtil extends PythonIncludePathUtil {

    public static final String GFE = "gfe";

    public static final String COMMON_GFE = FileUtil.join(PYTHON, GFE);

    public static final String ITOOL = FileUtil.join(GFE, "itool");

    public static final String VTEC = "vtec";

    public static final String USER_PYTHON = FileUtil.join(GFE, "userPython");

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

    /**
     * Get all available localization levels, while optionally including or
     * excluding the {@code USER} level and levels considered to be "higher"
     * than it.
     * <p>
     * When it comes to {@code LocalizationLevel}, a higher level means one more
     * specific than the other (e.g., SITE is higher than BASE, USER is higher
     * than SITE).
     * 
     * @param includeUser
     *            If {@code true} include level {@code USER} and levels higher
     *            than it. If {@code false}, exclude those levels.
     * @return The requested list of levels.
     */
    protected static List<LocalizationLevel> getLocalizationLevels(
            boolean includeUser) {
        LocalizationLevel[] levels = PATH_MANAGER.getAvailableLevels();
        List<LocalizationLevel> retVal = new ArrayList<>(levels.length);
        for (LocalizationLevel level : levels) {
            /*
             * A reminder: the natural ordering of LocalizationLevel is such
             * that BASE < SITE < USER. So when we perform
             * USER.compareTo(level), we have 2 scenarios:
             * 
             * 1. We automatically take any level less than USER
             * 
             * 2. If the includeUser flag is set, we also include any level
             * which is greater than or equal to USER.
             */
            int compareToUser = level.compareTo(LocalizationLevel.USER);
            if ((compareToUser < 0) || (includeUser)) {
                retVal.add(0, level);
            }
        }

        return retVal;
    }

    /**
     * Builds a python-style include path for the given localization path in
     * highest to lowest order (i.e., {@code USER} level appears in the path
     * before {@code SITE} level appears before {@code BASE} level).
     * 
     * @param locType
     *            The {@code LocalizationType} to use for the include path.
     * @param locPath
     *            The path to the folder in the localization store.
     * @param includeUser
     *            If {@code true} include level {@code USER} and levels higher
     *            than it. If {@code false}, exclude those levels.
     * @return The python include path
     */
    protected static String buildIncludePath(LocalizationType locType,
            String locPath, boolean includeUser) {
        Collection<LocalizationLevel> levels = getLocalizationLevels(includeUser);
        Collection<String> paths = new ArrayList<>(levels.size());
        for (LocalizationLevel level : levels) {
            paths.add(getPath(PATH_MANAGER.getContext(locType, level), locPath));
        }

        return PyUtil.buildJepIncludePath(true, paths.toArray(new String[0]));
    }

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

    public static LocalizationFile getHeadlineLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, HEADLINE);
    }

    public static LocalizationFile getCombinationsLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, COMBINATIONS);
    }

    public static LocalizationFile getVCModUtilsLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, VCMOD_UTILS);
    }

    // Include Path getters

    public static String getCommonGfeIncludePath() {
        String commonGfeIncludePath = buildIncludePath(
                LocalizationType.COMMON_STATIC, COMMON_GFE, false);

        String pythonDir = getCommonPythonIncludePath();
        return PyUtil.buildJepIncludePath(pythonDir, commonGfeIncludePath);
    }

    public static String getProceduresIncludePath() {
        return getProceduresIncludePath(true);
    }

    public static String getProceduresIncludePath(boolean includeUser) {
        return buildIncludePath(LocalizationType.CAVE_STATIC, PROCEDURES,
                includeUser);
    }

    public static String getTextUtilitiesIncludePath() {
        return getTextUtilitiesIncludePath(true);
    }

    public static String getTextUtilitiesIncludePath(boolean includeUser) {
        return buildIncludePath(LocalizationType.CAVE_STATIC, REGULAR,
                includeUser);
    }

    public static String getTextProductsIncludePath() {
        return getTextProductsIncludePath(true);
    }

    public static String getTextProductsIncludePath(boolean includeUser) {
        return buildIncludePath(LocalizationType.CAVE_STATIC, TEXT_PRODUCTS,
                includeUser);
    }

    public static String getSmartToolsIncludePath() {
        return getSmartToolsIncludePath(true);
    }

    public static String getSmartToolsIncludePath(boolean includeUser) {
        return buildIncludePath(LocalizationType.CAVE_STATIC, SMART_TOOLS,
                includeUser);
    }

    public static String getUtilitiesIncludePath() {
        return getUtilitiesIncludePath(true);
    }

    public static String getUtilitiesIncludePath(boolean includeUser) {
        return buildIncludePath(LocalizationType.CAVE_STATIC, UTILITIES,
                includeUser);
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
        String configIncludePath = buildIncludePath(
                LocalizationType.CAVE_STATIC, CONFIG, includeUser);

        String pythonPath = GfePyIncludeUtil.getCommonPythonIncludePath();
        return PyUtil.buildJepIncludePath(configIncludePath, pythonPath);
    }

    public static String getIToolIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.BASE), ITOOL);
    }

    public static String getHeadlineIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.BASE), HEADLINE);
    }

    public static String getCombinationsIncludePath() {
        return getCombinationsIncludePath(true);
    }

    public static String getCombinationsIncludePath(boolean includeUser) {
        String configDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.CONFIGURED),
                COMBINATIONS);
        String siteDir = getPath(PATH_MANAGER.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE),
                COMBINATIONS);
        if (includeUser) {
            String userDir = getPath(PATH_MANAGER.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER),
                    COMBINATIONS);
            return PyUtil.buildJepIncludePath(userDir, siteDir, configDir);
        } else {
            return PyUtil.buildJepIncludePath(siteDir, configDir);
        }
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

    public static String getVCModulesIncludePath(String siteId) {
        return buildIncludePath(LocalizationType.COMMON_STATIC, VCMODULES, true);
    }

    public static String getVCModUtilsIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE), VCMOD_UTILS);
    }
}
