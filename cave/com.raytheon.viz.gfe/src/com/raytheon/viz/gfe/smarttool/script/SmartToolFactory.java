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
package com.raytheon.viz.gfe.smarttool.script;

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
 * Factory for smart tools
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	 Description
 * ------------	----------	-----------	 --------------------------
 * Mar 21, 2008				njensen	     Initial creation
 * Jul 9, 2009  2454        ryu          Put user and site's python scripts on path for import
 * Mar 06,2013  15717       jzeng        Change CAVE_STATIC to COMMON_STATIC 
 *                                       for GFE localization files 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolFactory {

    private static String getScriptPath() {
        LocalizationContext baseCtx = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.BASE);
        String scriptPath = GfePyIncludeUtil.getUtilitiesLF(baseCtx).getFile()
                .getPath();
        return FileUtil.join(scriptPath, "SmartToolInterface.py");
    }

    private static SmartToolController buildInstance(DataManager dataMgr,
            boolean ui) throws JepException {
        String includePath = PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getVtecIncludePath(),
                GfePyIncludeUtil.getCommonGfeIncludePath(),
                GfePyIncludeUtil.getSmartToolsIncludePath(),
                GfePyIncludeUtil.getUtilitiesIncludePath());

        SmartToolController smartCont = null;
        if (ui) {
            smartCont = new SmartToolUIController(getScriptPath(), includePath,
                    SmartToolFactory.class.getClassLoader(), dataMgr);
        } else {
            smartCont = new SmartToolController(getScriptPath(), includePath,
                    SmartToolFactory.class.getClassLoader(), dataMgr);
        }

        return smartCont;
    }

    public static SmartToolUIController buildUIController(DataManager dataMgr)
            throws JepException {
        return (SmartToolUIController) buildInstance(dataMgr, true);
    }

    public static SmartToolController buildController(DataManager dataMgr)
            throws JepException {
        return buildInstance(dataMgr, false);
    }

}
