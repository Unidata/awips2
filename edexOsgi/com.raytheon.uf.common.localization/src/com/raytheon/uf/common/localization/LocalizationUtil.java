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
package com.raytheon.uf.common.localization;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class LocalizationUtil {

    /**
     * Extracts the name of a file or directory from a path
     * 
     * @param filePath
     * @return the name of the file/directory
     */
    public static String extractName(String filePath) {
        String[] split = splitUnique(filePath);
        if (split.length > 0) {
            return split[split.length - 1];
        }
        return filePath;
    }

    /**
     * Split the path by the file separator, getting rid of empty parts
     * 
     * @param filePath
     * @return
     */
    public static String[] splitUnique(String filePath) {
        List<String> parts = new ArrayList<String>();
        String[] split = filePath.split("[/\\\\]"); // Win32
        for (String s : split) {
            if ("".equals(s.trim()) == false) {
                parts.add(s);
            }
        }
        return parts.toArray(new String[parts.size()]);
    }

    /**
     * Split the path by the file separator, getting rid of empty parts and
     * reconstructs the file path
     * 
     * @param filePath
     * @return
     */
    public static String getSplitUnique(String filePath) {
        String[] parts = splitUnique(filePath);
        String filename = "";
        for (int i = 0; i < parts.length; ++i) {
            if (i > 0) {
                filename += IPathManager.SEPARATOR; // Win32
            }
            filename += parts[i];
        }
        return filename;
    }

    /**
     * Gets the "proper" name of the level, which capitalizes the first letter.
     * USER becomes User, SITE Site, etc
     * 
     * @param level
     * @return
     */
    public static String getProperName(LocalizationLevel level) {
        char[] chars = level.name().toLowerCase().toCharArray();
        chars[0] = Character.toUpperCase(chars[0]);
        return new String(chars);
    }
}
