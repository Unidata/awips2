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
package com.raytheon.uf.common.archive.config;

import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.IPathManager;

/**
 * Constants used by purger and GUIs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2013 #2221      rferrel     Initial creation
 * Aug 26, 2013 #2225      rferrel     Added tar extension.
 * Apr 11, 2014 #2862      rferrel     Added cluster name.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class ArchiveConstants {
    /** The value for the cluster tasks' name column. */
    public static final String CLUSTER_NAME = "Archive Shared Lock";

    /** Pattern to find slashes in a string. */
    private final static Pattern slashPattern = Pattern.compile("[/\\\\]+");

    /** Pattern to find white space in a string. */
    private final static Pattern wsPattern = Pattern.compile("\\s+");

    /** Default selection name to display. */
    public static final String defaultSelectName = "DEFAULT";

    /** Extension to use for creating a configuration's file name. */
    public static final String configFileExt = ".xml";

    /** Types of select configuration and their relative localized directory. */
    public enum Type {
        Retention("retention" + IPathManager.SEPARATOR), Case("case"
                + IPathManager.SEPARATOR);

        public final String selectionDir;

        private Type(String selectionDir) {
            this.selectionDir = selectionDir;
        }
    }

    /** Extension for compressed tar files. */
    public final static String TAR_EXTENSION = ".tgz";

    /**
     * Do not allow an instance of this class.
     */
    private ArchiveConstants() {
    }

    /**
     * Get relative path file name for a select configuration file based on type
     * and name.
     * 
     * @param type
     * @param name
     *            - when null use the default select name.
     * @return fileName
     */
    public static final String selectFileName(Type type, String name) {
        String fileName = name;
        if (fileName == null) {
            fileName = defaultSelectName;
        } else {
            fileName = convertToFileName(name);
        }
        return type.selectionDir + fileName + configFileExt;
    }

    /**
     * Convert name to fileName by trimming whitespace, converting slashes to
     * hyphens and embedded whitespace to underscore.
     * 
     * @param name
     *            - must not be null
     * @return fileName
     */
    public static final String convertToFileName(String name) {
        String fileName = name.trim();
        fileName = slashPattern.matcher(fileName).replaceAll("-");
        fileName = wsPattern.matcher(fileName).replaceAll("_");
        return fileName;
    }
}
