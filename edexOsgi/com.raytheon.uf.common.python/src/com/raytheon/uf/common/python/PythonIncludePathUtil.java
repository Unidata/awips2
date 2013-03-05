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

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Utility for getting python directories to include.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class PythonIncludePathUtil {

    protected static final IPathManager PATH_MANAGER = PathManagerFactory
            .getPathManager();

    private static Map<LocalizationContext, Map<String, String>> pathMap = new HashMap<LocalizationContext, Map<String, String>>();

    public static final String PYTHON = "python";

    protected static String getPath(LocalizationContext ctx, String locPath) {
        Map<String, String> ctxMap = pathMap.get(ctx);
        if (ctxMap == null) {
            ctxMap = new HashMap<String, String>();
            pathMap.put(ctx, ctxMap);
        }
        String fsPath = ctxMap.get(locPath);
        if (fsPath == null) {
            LocalizationFile file = PATH_MANAGER.getLocalizationFile(ctx,
                    locPath);
            fsPath = file.getFile().getAbsolutePath();
            ctxMap.put(locPath, fsPath);
        }
        return fsPath;
    }

    public static LocalizationFile getCommonPythonLF(LocalizationContext ctx) {
        return PATH_MANAGER.getLocalizationFile(ctx, PYTHON);
    }

    public static String getCommonPythonIncludePath() {
        return getPath(PATH_MANAGER.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE), PYTHON);
    }

}
