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
package com.raytheon.viz.gfe.textformatter;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.concurrent.AbstractPythonScriptFactory;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.python.GfeCavePyIncludeUtil;

/**
 * Builds text formatter script objects
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 02, 2008            njensen     Initial creation
 * Apr 20, 2015  4027      randerso    Remove unused TextProductsTemplates path and added 
 *                                     Tests path for GFE formatter auto tests
 * Jul 28, 2015  4263      dgilling    Refactor based on AbstractPythonScriptFactory.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FormatterScriptFactory extends
        AbstractPythonScriptFactory<FormatterScript> {

    private static final String SCRIPT_EXECUTOR_NAME = "text-product-metadata";

    private static final int EXECUTOR_NUM_THREADS = 1;

    public FormatterScriptFactory() {
        super(SCRIPT_EXECUTOR_NAME, EXECUTOR_NUM_THREADS);
    }

    private static String buildScriptPath() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext baseContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);
        String headlineDir = GfePyIncludeUtil.getHeadlineLF(baseContext)
                .getFile().getPath();
        String runnerPath = FileUtil.join(headlineDir, "FormatterRunner.py");
        return runnerPath;
    }

    private static String buildIncludePath() {
        String include = PyUtil.buildJepIncludePath(true,
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getVtecIncludePath(),
                GfePyIncludeUtil.getCommonGfeIncludePath(),
                GfePyIncludeUtil.getHeadlineIncludePath(),
                GfePyIncludeUtil.getTextUtilitiesIncludePath(),
                GfePyIncludeUtil.getTextProductsIncludePath(),
                GfePyIncludeUtil.getUtilitiesIncludePath(),
                GfePyIncludeUtil.getCombinationsIncludePath(),
                GfeCavePyIncludeUtil.getTestsIncludePath());
        return include;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.python.concurrent.AbstractPythonScriptFactory#
     * createPythonScript()
     */
    @Override
    public FormatterScript createPythonScript() throws JepException {
        return new FormatterScript(buildScriptPath(), buildIncludePath(),
                getClass().getClassLoader());
    }
}
