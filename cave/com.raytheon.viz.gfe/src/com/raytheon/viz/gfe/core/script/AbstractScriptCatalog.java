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
package com.raytheon.viz.gfe.core.script;

import java.io.File;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.util.FileUtil;

/**
 * SOFTWARE HISTORY
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 *                          wldougher   Initial creation
 * Mar 06  2013  15717      jzeng       Change CAVE_STATIC to COMMON_STATIC
 *                                      for GFE localization files
 *  @author wldougher
 * 
 */
public abstract class AbstractScriptCatalog {

    protected IPathManager pathManager;

    protected LocalizationFile baseDir;

    protected LocalizationFile procDir;

    protected LocalizationFile siteDir;

    /**
     * 
     */
    public AbstractScriptCatalog() {
        IPathManager defaultPathManager = PathManagerFactory.getPathManager();
        setPathManager(defaultPathManager);
    }

    /**
     * Set the path manager used by this catalog. This is a public method to
     * facilitate unit testing. The userCtx and procDir fields, which depend
     * entirely on static constants and the path manager, are set, too.
     * 
     * @param pathManager
     */
    protected void setPathManager(IPathManager pathManager) {
        if (pathManager == null) {
            throw new NullPointerException("Attempt to set a null path manager");
        }

        this.pathManager = pathManager;
        LocalizationContext userCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
        LocalizationContext siteCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationContext baseCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String pathPrefix = getScriptTypePathPrefix();
        procDir = pathManager.getLocalizationFile(userCtx, pathPrefix);
        siteDir = pathManager.getLocalizationFile(siteCtx, pathPrefix);
        baseDir = pathManager.getLocalizationFile(baseCtx, pathPrefix);
    }

    /**
     * Get all the localization files representing Python procedures.
     * 
     * @return the localization files for the procedures.
     */
    public LocalizationFile[] getFiles() {
        LocalizationFile[] procFiles = pathManager.listStaticFiles(
                getScriptTypePathPrefix(), getExtensions(), false, true);
        return procFiles;
    }

    /**
     * Get the simple names of the procedures.
     * 
     * @return the simple names of the procedures.
     */
    public Collection<String> getNames() {
        Collection<String> result = new HashSet<String>();
        LocalizationFile[] procFiles = getFiles();
        result = scriptNames(procFiles);
        return result;
    }

    /**
     * @param scriptName
     *            The simple name of a procedure, i.e., "Align_Grids".
     * @return The localization file for the script
     */
    public LocalizationFile getFile(String scriptName) {
        String fname = getScriptTypePathPrefix() + File.separator + scriptName
                + getExtensions()[0];
        LocalizationFile file = pathManager.getStaticLocalizationFile(fname);
        return file;
    }

    /**
     * Add an observer that will be notified when the directory of the procedure
     * catalog changes. This is just a wrapper around the appropriate method of
     * procDir. Users are responsible for removing any listeners they create.
     * 
     * @param observer
     *            the observer to add
     */
    public void addObserver(ILocalizationFileObserver observer) {
        procDir.addFileUpdatedObserver(observer);
        siteDir.addFileUpdatedObserver(observer);
        baseDir.addFileUpdatedObserver(observer);
    }

    /**
     * Remove the specified observer.
     * 
     * @param observer
     *            The observer to remove.
     */
    public void removeObserver(ILocalizationFileObserver observer) {
        procDir.removeFileUpdatedObserver(observer);
        siteDir.removeFileUpdatedObserver(observer);
        baseDir.removeFileUpdatedObserver(observer);
    }

    /**
     * Get the script names from an array of LocalizationFiles, with any leading
     * directories and trailing ".py"s removed.
     * 
     * @param scriptFiles
     *            the array of LocalizationFiles.
     * @return a Collection of simple script names with no duplicates.
     */
    protected Collection<String> scriptNames(LocalizationFile[] scriptFiles) {
        Set<String> procs = new HashSet<String>();
        String fname = null;
        String[] fsplit = null;
        String script = null;
        if (scriptFiles != null) {
            for (LocalizationFile file : scriptFiles) {
                fname = file.getName();
                fsplit = fname.split(FileUtil.fileSeparatorRegex);
                script = fsplit[fsplit.length - 1].replaceAll("\\.py$", "");
                procs.add(script);
            }
        }
        return procs;
    }

    /**
     * @return
     */
    public abstract String getScriptTypePathPrefix();

    /**
     * @return
     */
    public abstract String[] getExtensions();
}
