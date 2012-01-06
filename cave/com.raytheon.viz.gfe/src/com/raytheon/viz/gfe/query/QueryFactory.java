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
package com.raytheon.viz.gfe.query;

import java.io.File;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Builds a QueryScript instance
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 10, 2008				njensen	Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class QueryFactory {

    private static final String FILEDIR = FileUtil.join("gfe", "query");

    private static final String FILENAME = "Evaluator.py";

    private static QueryScript cachedScript;

    /**
     * Builds a query script
     * 
     * @param dm
     *            the data manager for the script
     * @return an initialized script object
     * @throws JepException
     */
    public static QueryScript buildQueryScript(DataManager dm)
            throws JepException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);

        File file = pathMgr.getFile(ctx, FileUtil.join(FILEDIR, FILENAME));
        File includeDir = pathMgr.getFile(ctx, FILEDIR);

        return new QueryScript(file.getPath(), PyUtil.buildJepIncludePath(
                includeDir.getPath(),
                GfePyIncludeUtil.getCommonGfeIncludePath()),
                QueryScript.class.getClassLoader(), dm);
    }

    public static QueryScript getCachedScript(DataManager dm)
            throws JepException {
        if (cachedScript == null) {
            cachedScript = buildQueryScript(dm);
        }
        return cachedScript;
    }

}
