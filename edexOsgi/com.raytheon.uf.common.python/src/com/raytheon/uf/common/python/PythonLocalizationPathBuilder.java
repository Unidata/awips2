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
package com.raytheon.uf.common.python;

import java.io.File;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * A Builder object for creating python paths from localization hierarchies.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14, 2013       2033 mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PythonLocalizationPathBuilder {

    private static final String PATH_SEPARATOR = File.pathSeparator;

    private StringBuilder path = new StringBuilder();

    private final IPathManager pathManager;

    /**
     * Creates a {@link PythonLocalizationPathBuilder} using the default
     * {@link IPathManager}
     */
    public PythonLocalizationPathBuilder() {
        this(PathManagerFactory.getPathManager());
    }

    /**
     * Creates a path builder for the specified path manager
     * 
     * @param pathManager
     */
    public PythonLocalizationPathBuilder(IPathManager pathManager) {
        this.pathManager = pathManager;
    }

    /**
     * Appends only the highest localization level path for localizationPath
     * using the static search hierarchy
     * 
     * @param localizationPath
     */
    public PythonLocalizationPathBuilder appendLevel(String localizationPath) {
        LocalizationFile file = pathManager
                .getStaticLocalizationFile(localizationPath);
        if (file != null) {
            append(file);
        }
        return this;
    }

    /**
     * Appends only the highest localization level path for localizationPath for
     * the specified {@link LocalizationType}
     * 
     * @param localizationPath
     */
    public PythonLocalizationPathBuilder appendLevel(String localizationPath,
            LocalizationType type) {
        LocalizationContext[] searchContext = pathManager
                .getLocalSearchHierarchy(type);
        for (LocalizationContext context : searchContext) {
            LocalizationFile file = pathManager.getLocalizationFile(context,
                    localizationPath);
            if (file != null) {
                append(file);
                break;
            }
        }
        return this;
    }

    /**
     * Appends the path specified by localizationPath in a hierarchical search
     * order (USER,SITE,BASE)
     * 
     * @param localizationPath
     * @param type
     * 
     */
    public PythonLocalizationPathBuilder append(String localizationPath,
            LocalizationType type) {
        return append(localizationPath,
                pathManager.getLocalSearchHierarchy(type));
    }

    /**
     * Appends the path specified by localizationPath for each context in
     * searchHierarchy in order passed in
     * 
     * @param localizationPath
     * @param searchHierarchy
     */
    public PythonLocalizationPathBuilder append(String localizationPath,
            LocalizationContext... searchHierarchy) {
        for (LocalizationContext ctx : searchHierarchy) {
            LocalizationFile file = pathManager.getLocalizationFile(ctx,
                    localizationPath);
            if (file != null) {
                append(file);
            }
        }
        return this;
    }

    /**
     * Appends the full path of the localization file to the builder
     * 
     * @param file
     */
    public PythonLocalizationPathBuilder append(LocalizationFile file) {
        return append(file.getFile().getAbsolutePath());
    }

    /**
     * Appends a fully qualified path entry to the builder
     * 
     * @param pathEntry
     */
    public PythonLocalizationPathBuilder append(String pathEntry) {
        if (pathEntry == null) {
            pathEntry = "";
        }
        pathEntry = pathEntry.trim();
        if (pathEntry.isEmpty() == false) {
            File pathFile = new File(pathEntry);
            if (pathFile.exists() == false) {
                pathFile.mkdirs();
            }
            if (pathFile.isDirectory()) {
                path.append(pathEntry).append(PATH_SEPARATOR);
            }
        }
        return this;
    }

    /**
     * Returns the current representation of the python path.
     * 
     * @return
     */
    public String getPathString() {
        return path.toString();
    }

}
