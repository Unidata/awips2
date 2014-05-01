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
/**
 * 
 */
package com.raytheon.viz.gfe.textproduct;

import java.io.File;
import java.util.Arrays;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.viz.gfe.core.script.AbstractScriptCatalog;

/**
 * @author wldougher
 * 
 */
public class TextProductCatalog extends AbstractScriptCatalog {

    private static final String[] EXTENSIONS = { ".py" };

    private LocalizationFile siteConfDir;

    private LocalizationFile siteModDir;

    private LocalizationFile userDir;

    /**
     * @see com.raytheon.viz.gfe.core.script.AbstractScriptCatalog#getExtensions()
     */
    @Override
    public String[] getExtensions() {
        return EXTENSIONS;
    }

    /**
     * @see com.raytheon.viz.gfe.core.script.AbstractScriptCatalog#getScriptTypePathPrefix()
     */
    @Override
    public String getScriptTypePathPrefix() {
        return GfePyIncludeUtil.TEXT_PRODUCTS;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.script.AbstractScriptCatalog#getFiles()
     */
    @Override
    public LocalizationFile[] getFiles() {
        LocalizationFile[] modProducts = pathManager.listStaticFiles(
                getScriptTypePathPrefix() + File.separator + "modified",
                getExtensions(), false, true);
        LocalizationFile[] confProducts = pathManager.listStaticFiles(
                getScriptTypePathPrefix() + File.separator + "configured",
                getExtensions(), false, true);
        LocalizationFile[] userProducts = pathManager.listFiles(pathManager
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.USER), getScriptTypePathPrefix(),
                getExtensions(), false, true);

        // combine arrays, setting the modified products first in order
        return concatArrays(userProducts, modProducts, confProducts);
    }

    private LocalizationFile[] concatArrays(LocalizationFile[] first,
            LocalizationFile[]... rest) {
        int totalLength = first.length;
        for (LocalizationFile[] array : rest) {
            totalLength += array.length;
        }

        LocalizationFile[] result = Arrays.copyOf(first, totalLength);
        int offset = first.length;
        for (LocalizationFile[] array : rest) {
            System.arraycopy(array, 0, result, offset, array.length);
            offset += array.length;
        }

        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.script.AbstractScriptCatalog#addObserver(com
     * .raytheon.uf.common.localization.ILocalizationFileObserver)
     */
    @Override
    public void addObserver(ILocalizationFileObserver observer) {
        userDir.addFileUpdatedObserver(observer);
        siteConfDir.addFileUpdatedObserver(observer);
        siteModDir.addFileUpdatedObserver(observer);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.script.AbstractScriptCatalog#getFile(java.lang
     * .String)
     */
    @Override
    public LocalizationFile getFile(String scriptName) {
        String path;
        LocalizationFile result;

        path = getScriptTypePathPrefix() + File.separator + scriptName;
        result = pathManager.getLocalizationFile(pathManager.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER), path);
        if (result != null && !result.exists()) {
            result = null;
        }

        if (result == null) {
            path = GfePyIncludeUtil.TEXT_PRODUCTS + File.separator + scriptName;
            result = pathManager
                    .getLocalizationFile(pathManager.getContext(
                            LocalizationType.CAVE_STATIC,
                            LocalizationLevel.SITE), path);
        }
        if (result != null && !result.exists()) {
            result = null;
        }

        if (result == null) {
            path = GfePyIncludeUtil.TEXT_PRODUCTS + File.separator + scriptName;
            result = pathManager.getLocalizationFile(
                    pathManager.getContext(LocalizationType.CAVE_STATIC,
                            LocalizationLevel.CONFIGURED), path);
        }
        if (result != null && !result.exists()) {
            result = null;
        }

        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.script.AbstractScriptCatalog#removeObserver
     * (com.raytheon.uf.common.localization.ILocalizationFileObserver)
     */
    @Override
    public void removeObserver(ILocalizationFileObserver observer) {
        userDir.removeFileUpdatedObserver(observer);
        siteConfDir.removeFileUpdatedObserver(observer);
        siteModDir.removeFileUpdatedObserver(observer);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.script.AbstractScriptCatalog#setPathManager
     * (com.raytheon.uf.common.localization.IPathManager)
     */
    @Override
    protected void setPathManager(IPathManager pathManager) {
        if (pathManager == null) {
            throw new NullPointerException("Attempt to set a null path manager");
        }

        this.pathManager = pathManager;
        LocalizationContext userCtx = pathManager.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationContext siteCtx = pathManager.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        String pathPrefix = getScriptTypePathPrefix();
        userDir = pathManager.getLocalizationFile(userCtx, pathPrefix);
        siteConfDir = pathManager.getLocalizationFile(siteCtx, pathPrefix
                + File.separator + "configured");
        siteModDir = pathManager.getLocalizationFile(siteCtx, pathPrefix
                + File.separator + "modified");
    }

}
