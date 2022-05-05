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
package com.raytheon.viz.gfe.procedures;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonIncludePathUtil;
import com.raytheon.uf.common.python.concurrent.PythonInterpreterFactory;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.core.DataManager;

import jep.JepException;

/**
 * Builds the procedure controller instance
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 05, 2008           njensen   Initial creation
 * Feb 25, 2010  4108     ryu       Add user/site directories to include path
 * May 20, 2015  4509     njensen   Added time and dataaccess to include path
 * Jul 27, 2015  4263     dgilling  Refactor and make abstract.
 * Dec 14, 2015  4816     dgilling  Support refactored PythonJobCoordinator API.
 * Feb 13, 2018  6906     randerso  Renamed and updated for merged
 *                                  ProcedureController
 *
 * </pre>
 *
 * @author njensen
 */

public class ProcedureScriptFactory
        implements PythonInterpreterFactory<ProcedureController> {

    protected final DataManager dataMgr;

    /**
     * Constructor
     *
     * @param dataMgr
     */
    public ProcedureScriptFactory(final DataManager dataMgr) {
        this.dataMgr = dataMgr;
    }

    protected static String buildScriptPath() {
        LocalizationContext baseCtx = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE);
        String baseUtil = GfePyIncludeUtil.getUtilitiesLF(baseCtx).getFile()
                .getPath();
        return FileUtil.join(baseUtil, "ProcedureInterface.py");
    }

    protected static String buildIncludePath() {
        return PyUtil.buildJepIncludePath(
                PythonIncludePathUtil.getCommonPythonIncludePath("time",
                        "dataaccess"),
                GfePyIncludeUtil.getVtecIncludePath(),
                GfePyIncludeUtil.getCommonGfeIncludePath(),
                GfePyIncludeUtil.getProceduresIncludePath(),
                GfePyIncludeUtil.getUtilitiesIncludePath());
    }

    @Override
    public ProcedureController createPythonScript() throws JepException {
        return new ProcedureController(buildScriptPath(), buildIncludePath(),
                getClass().getClassLoader(), dataMgr);
    }
}
