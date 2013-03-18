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

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Builds the procedure controller instance
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2008            njensen     Initial creation
 * Feb 25, 2010  4108     ryu         Add user/site directories to include path
 * Mar 06, 2013  15717    jzeng       Change CAVE_STATIC to COMMON_STATIC
 *                                    for GFE localization files
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ProcedureFactory {

    private static ProcedureController buildInstance(DataManager dataMgr,
            boolean ui) throws JepException {
        LocalizationContext baseCtx = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.BASE);

        String baseUtil = GfePyIncludeUtil.getUtilitiesLF(baseCtx).getFile()
                .getPath();
        String scriptPath = FileUtil.join(baseUtil, "ProcedureInterface.py");

        String includePath = PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getVtecIncludePath(),
                GfePyIncludeUtil.getCommonGfeIncludePath(),
                GfePyIncludeUtil.getProceduresIncludePath(),
                GfePyIncludeUtil.getUtilitiesIncludePath());

        ProcedureController procCont = null;
        if (ui) {
            procCont = new ProcedureUIController(scriptPath, includePath,
                    ProcedureFactory.class.getClassLoader(), dataMgr);
        } else {
            procCont = new ProcedureController(scriptPath, includePath,
                    ProcedureFactory.class.getClassLoader(), dataMgr);
        }

        return procCont;
    }

    public static ProcedureController buildController(DataManager dataMgr)
            throws JepException {
        return buildInstance(dataMgr, false);
    }

    public static ProcedureUIController buildUIController(DataManager dataMgr)
            throws JepException {
        return (ProcedureUIController) buildInstance(dataMgr, true);
    }

}
