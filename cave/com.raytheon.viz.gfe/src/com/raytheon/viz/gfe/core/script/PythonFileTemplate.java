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

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.viz.gfe.GFEException;

/**
 * An interface for generating new python files based on a template.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                     wldougher   Initial creation
 * Jan 19, 2016   4834     njensen     Rename, refactor, clean up
 * 
 * </pre>
 * 
 * @author wldougher
 * 
 */
public interface PythonFileTemplate {

    public enum Overwrite {
        OVERWRITE, SAFE
    }

    /**
     * Create a new script at the designated localization level. Implementations
     * may initialize the contents of the new script however they desire.
     * 
     * @param script
     * @param level
     * @param mode
     * @return A reference to the newly created script, or null if the script
     *         could not be created.
     */
    public LocalizationFile createNew(String script, LocalizationLevel level,
            Overwrite mode) throws GFEException;

    /**
     * The type of script this utility handles. Implementers should return this
     * in capitalized case, i.e., "Procedure", for use in error messages.
     * 
     * @return the script type string
     */
    public String getScriptType();

    /**
     * Find the named script at the designated localization level, or at the
     * most localized level at which it appears if level is null.
     * 
     * @param name
     *            the simple name of the script to find.
     * @param level
     *            the localization level of the script. If this parameter is
     *            null, the standard USER/SITE/BASE search levels will be used.
     * @return a reference to the script, or null if the script cannot be found.
     * @throws GFEException
     *             if network or file errors (other than FileNotFoundException)
     *             prevent the file from being found.
     */
    public LocalizationFile find(String name, LocalizationLevel level)
            throws GFEException;

    /**
     * Convert a simple name to the name of a script file. For example,
     * "Extrapolate" might become "Extrapolate.py".
     * 
     * @param name
     *            The undecorated name
     * @return The name of the script file derived from name.
     */
    public String normalize(String name);

    /**
     * Get the path prefix used in building localization filenames. For example,
     * the path prefix for Procedures is "gfe/userPython/procedures", or
     * GfePyIncludeUtil.PROCEDURES.
     * 
     * @return The path prefix
     */
    public abstract String getScriptTypePathPrefix();
}
