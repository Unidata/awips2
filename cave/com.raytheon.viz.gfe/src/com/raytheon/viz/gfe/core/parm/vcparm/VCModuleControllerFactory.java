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
package com.raytheon.viz.gfe.core.parm.vcparm;

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
 * Factory for the <code>VCModuleController</code> class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2011            dgilling     Initial creation
 * Jul 08, 2014 3361       njensen      Only build include path once
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class VCModuleControllerFactory {

    private static String includePath;

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     */
    private VCModuleControllerFactory() {
        throw new AssertionError();
    }

    private static String getScriptPath() {
        LocalizationContext baseCtx = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.BASE);
        String scriptPath = GfePyIncludeUtil.getVCModUtilsLF(baseCtx).getFile()
                .getPath();
        return FileUtil.join(scriptPath, "VCModuleInterface.py");
    }

    public static VCModuleController buildInstance(DataManager dataMgr)
            throws JepException {
        synchronized (VCModuleController.class) {
            if (includePath == null) {
            includePath = PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getVCModUtilsIncludePath(),
                GfePyIncludeUtil.getVCModulesIncludePath(),
                GfePyIncludeUtil.getCommonPythonIncludePath());
            }
        }

        return new VCModuleController(getScriptPath(), includePath,
                VCModuleControllerFactory.class.getClassLoader(), dataMgr);
    }
}
