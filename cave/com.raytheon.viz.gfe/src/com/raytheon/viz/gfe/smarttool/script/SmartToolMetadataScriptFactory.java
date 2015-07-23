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
import com.raytheon.uf.common.python.PythonIncludePathUtil;
import com.raytheon.uf.common.python.concurrent.AbstractPythonScriptFactory;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Script factory for {@code SmartToolUIController} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2015  #4263     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SmartToolMetadataScriptFactory extends
        AbstractPythonScriptFactory<SmartToolMetadataController> {

    private final DataManager dataMgr;

    public SmartToolMetadataScriptFactory(final DataManager dataMgr) {
        super("smart-tool", 1);
        this.dataMgr = dataMgr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.python.concurrent.AbstractPythonScriptFactory#
     * createPythonScript()
     */
    @Override
    public SmartToolMetadataController createPythonScript() throws JepException {
        return new SmartToolMetadataController(getScriptPath(),
                getIncludePath(), getClass().getClassLoader(), dataMgr);
    }

    private static String getScriptPath() {
        LocalizationContext baseCtx = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE);
        String scriptPath = GfePyIncludeUtil.getUtilitiesLF(baseCtx).getFile()
                .getPath();
        return FileUtil.join(scriptPath, "SmartToolInterface.py");
    }

    private static String getIncludePath() {
        return PyUtil.buildJepIncludePath(PythonIncludePathUtil
                .getCommonPythonIncludePath("time", "dataaccess"),
                GfePyIncludeUtil.getVtecIncludePath(), GfePyIncludeUtil
                        .getCommonGfeIncludePath(), GfePyIncludeUtil
                        .getSmartToolsIncludePath(), GfePyIncludeUtil
                        .getUtilitiesIncludePath());
    }
}
