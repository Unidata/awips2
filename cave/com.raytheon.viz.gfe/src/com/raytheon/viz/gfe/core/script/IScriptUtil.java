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

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.viz.gfe.GFEException;

/**
 * An interface describing several common actions on user-defined scripts.
 * 
 * @author wldougher
 * 
 */
public interface IScriptUtil {
    public enum Overwrite {
        OVERWRITE, SAFE
    };

    /**
     * Copy script to the new file toScript at localization level toLevel. If
     * source and dest are the equal, toLevel must be different from the
     * localization level of source. If dest already exists and mode is SAFE,
     * dest will not be overwritten and the returned localization file will be
     * null.
     * 
     * @param source
     *            the simple name of the source script
     * @param dest
     *            the simple name of the destination script
     * @param mode
     *            Whether processing should proceed if dest already exists.
     * @return A reference to toScript, which may be null if the copy fails.
     * @throws GFEException
     *             if script and toScript have the same name and level or if the
     *             copy cannot complete due to file or network errors.
     */
    public LocalizationFile copy(String source, String dest,
            LocalizationLevel toLevel, Overwrite mode) throws GFEException;

    /**
     * Create a new script at the designated localization level. Implementations
     * may initialize the contents of the new script however they desire.
     * 
     * @param script
     * @return A reference to the newly created script, or null if the script
     *         could not be created.
     */
    public LocalizationFile createNew(String script, LocalizationLevel level,
            Overwrite mode) throws GFEException;

    /**
     * Delete the script with the designated name at the designated localization
     * level.
     * 
     * @param name
     *            The simple name of the script.
     * @param level
     *            the localization level at which the script should be deleted.
     * @throws GFEException
     *             if underlying file or network operations fail.
     */
    public void delete(String name, LocalizationLevel level)
            throws GFEException;

    /**
     * The type of file to pass to PythonUtil.openPythonFile() when editing
     * scripts of this type.
     * 
     * @return the file type string
     */
    public String getFileType();

    /**
     * The type of script this utility handles. Implementors should return this
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
     * Rename source to dest. This should be equivalent to calling copy()
     * followed by delete().
     * 
     * @param source
     *            the simple name of the source script
     * @param dest
     *            the simple name of the destination script
     * @param mode
     *            Whether processing should proceed if dest already exists.
     * @return A reference to the renamed file.
     */
    public LocalizationFile rename(String source, String dest,
            LocalizationLevel toLevel, Overwrite mode) throws GFEException;

    /**
     * Convert a simple name to the name of a script file. For example,
     * "Extrapolate" might become "Extrapolate.py".
     * 
     * @param name
     *            The undecorated name
     * @return The name of the script file derived from name.
     */
    public String scripted(String name);

	/**
	 * Get the path prefix used in building localization filenames. For example,
	 * the path prefix for Procedures is "gfe/userPython/procedures", or
	 * GfePyIncludeUtil.PROCEDURES.
	 * 
	 * @return The path prefix
	 */
	public abstract String getScriptTypePathPrefix();
}
