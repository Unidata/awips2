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
import com.raytheon.uf.common.python.PythonIncludePathUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.exception.VizException;
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
 * Aug 21, 2015  4509      dgilling    Added time and dataaccess to include path.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FormatterScriptFactory {

    public static FormatterScript buildFormatterScript() throws JepException,
            VizException {

        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationContext baseContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);

        String headlineDir = GfePyIncludeUtil.getHeadlineLF(baseContext)
                .getFile().getPath();
        String runnerPath = FileUtil.join(headlineDir, "FormatterRunner.py");

        String include = PyUtil.buildJepIncludePath(true, PythonIncludePathUtil
                .getCommonPythonIncludePath("time", "dataaccess"),
                GfePyIncludeUtil.getCommonPythonIncludePath(), GfePyIncludeUtil
                        .getVtecIncludePath(), GfePyIncludeUtil
                        .getCommonGfeIncludePath(), GfePyIncludeUtil
                        .getHeadlineIncludePath(), GfePyIncludeUtil
                        .getTextUtilitiesIncludePath(), GfePyIncludeUtil
                        .getTextProductsIncludePath(), GfePyIncludeUtil
                        .getUtilitiesIncludePath(), GfePyIncludeUtil
                        .getCombinationsIncludePath(), GfeCavePyIncludeUtil
                        .getTestsIncludePath());

        return new FormatterScript(runnerPath, include,
                FormatterScript.class.getClassLoader());
    }
}
