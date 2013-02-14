package com.raytheon.uf.common.localization;

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

import java.io.File;
import java.util.Map;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;

/**
 * A generalized interface for constructing LocalizationFiles. NOTE: There will
 * only exist a single reference to any LocalizationFile. It is the
 * IPathManager's responsibility to ensure multiple objects of the same
 * LocalizationFile are not returned
 * 
 * Note: Paths should use IPathManager.SEPARATOR as the separator for
 * consistency. The client OS could potentially differ from the localization
 * store's OS, and this ensures the path will be resolved correctly.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/12/2008              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface IPathManager {

    public static final String SEPARATOR = "/";

    /**
     * 
     * Checks the localization hierarchy for a static file
     * 
     * First searches user, then site and finally base to find the file.
     * 
     * Returns null if file is not found.
     * 
     * @param name
     *            the name of the file to search for
     * @return a pointer to the location on the filesystem
     */
    public abstract File getStaticFile(String name);

    /**
     * 
     * Checks the localization hierarchy for a static file
     * 
     * First searches user, then site and finally base to find the file.
     * 
     * Returns null if file is not found.
     * 
     * @param name
     *            the name of the file to search for
     * @return the file and the context it was found in
     */
    public abstract LocalizationFile getStaticLocalizationFile(String name);

    /**
     * 
     * Return a pointer to a localization file given a specific context
     * 
     * @param context
     *            the localization context to use
     * @param name
     *            the filename to search
     * @return an absolute pointer on the filesystem to the file
     */
    public abstract File getFile(LocalizationContext context, String name);

    /**
     * 
     * Return a pointer to a localization file given a specific context
     * 
     * @param context
     *            the localization context to use
     * @param name
     *            the filename to search
     * @return an absolute pointer on the filesystem to the file
     */
    public abstract LocalizationFile getLocalizationFile(
            LocalizationContext context, String name);

    /**
     * Returns a map containing a LocalizationFile for each LocalizationLevel
     * where the file exists
     * 
     * @param type
     *            desired LocalizationType
     * @param name
     *            the localization file name
     * @return map of LocalizationLevel to LocalizationFile
     */
    public abstract Map<LocalizationLevel, LocalizationFile> getTieredLocalizationFile(
            LocalizationType type, String name);

    /**
     * Lists files in the directory
     * 
     * @param context
     *            the localization context to use
     * @param name
     *            the directory name to search
     * @param extensions
     *            a list of file extensions to look for, or null if no filter
     * @param recursive
     *            true for recursive directory listing, false for a single level
     *            listing
     * @param filesOnly
     *            true if only files are listed, false to list both files and
     *            directories
     * @return the files on the filesystem in the directory
     */
    public abstract LocalizationFile[] listFiles(LocalizationContext context,
            String name, String[] extensions, boolean recursive,
            boolean filesOnly);

    /**
     * Lists files in the directory
     * 
     * @param contexts
     *            the localization contexts to search
     * @param name
     *            the directory name to search
     * @param extensions
     *            a list of file extensions to look for, or null if no filter
     * @param recursive
     *            true for recursive directory listing, false for a single level
     *            listing
     * @param filesOnly
     *            true if only files are listed, false to list both files and
     *            directories
     * @return the files on the filesystem in the directory or null in case of
     *         error
     */
    public abstract LocalizationFile[] listFiles(
            LocalizationContext[] contexts, String name, String[] extensions,
            boolean recursive, boolean filesOnly);

    /**
     * Lists all files in all localization contexts in a particular directory.
     * Note: If the same file exists in multiple localization levels, only the
     * lowest level version of the file will be returned in the array.
     * 
     * @param name
     *            the directory to look in
     * @param extensions
     *            a list of file extensions to look for, or null if no filter
     * @param recursive
     *            true for recursive directory listing, false for a single level
     *            listing
     * @param filesOnly
     *            true if only files are listed, false to list both files and
     *            directories
     * @return a list of files
     */
    public abstract LocalizationFile[] listStaticFiles(String name,
            String[] extensions, boolean recursive, boolean filesOnly);

    /**
     * Returns a localization context for the given type and level for the
     * active user.
     * 
     * @param type
     * @param level
     * @return the localization context
     */
    public abstract LocalizationContext getContext(LocalizationType type,
            LocalizationLevel level);

    /**
     * Returns the site localization context for the given type and siteId.
     * 
     * @param type
     * @param siteId
     * @return the site localization context
     */
    public abstract LocalizationContext getContextForSite(
            LocalizationType type, String siteId);

    /**
     * Return the localization contexts that should be searched given a
     * localization type
     * 
     * @param type
     *            the type
     * @return the localization contexts
     */
    public abstract LocalizationContext[] getLocalSearchHierarchy(
            LocalizationContext.LocalizationType type);

    /**
     * Get the available context strings for the given level
     * 
     * @param level
     * @return
     */
    public abstract String[] getContextList(LocalizationLevel level);

    /**
     * Returns the available levels to be used, sorted from lowest search level
     * to highest (BASE,...,SITE,...,USER)
     * 
     * @return Available sorted levels
     */
    public abstract LocalizationLevel[] getAvailableLevels();
}