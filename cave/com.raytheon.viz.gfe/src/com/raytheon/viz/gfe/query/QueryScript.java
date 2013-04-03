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
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Interface to run a python reference set query
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 9, 2008				njensen	    Initial creation
 * 2/14/2013                mnash       Add QueryScript instantiation logic
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class QueryScript extends PythonScript {

    private static final String INSTANCE = "evaluatorInstance";

    private DataManager dataMgr;

    private static final String FILEDIR = FileUtil.join("gfe", "query");

    private static final String FILENAME = "Evaluator.py";

    public QueryScript(DataManager dm) throws JepException {
        super(buildFilePath(), buildIncludePath(), QueryScript.class
                .getClassLoader());
        dataMgr = dm;
        init();
    }

    private static String buildFilePath() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);

        File file = pathMgr.getFile(ctx, FileUtil.join(FILEDIR, FILENAME));
        return file.getPath();
    }

    private static String buildIncludePath() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);

        File includeDir = pathMgr.getFile(ctx, FILEDIR);
        return PyUtil.buildJepIncludePath(includeDir.getPath(),
                GfePyIncludeUtil.getCommonGfeIncludePath());
    }

    private void init() throws JepException {
        jep.set("dataMgr", dataMgr);
        jep.eval(INSTANCE + " = Evaluator(dataMgr)");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.python.PythonScript#execute(java.lang.String,
     * java.util.Map)
     */
    @Override
    public Object execute(String methodName, Map<String, Object> args)
            throws JepException {
        this.internalExecute(methodName, INSTANCE, args);
        return getExecutionResult();
    }

}
