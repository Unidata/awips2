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
package com.raytheon.edex.plugin.gfe.smartinit;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;

/**
 * Builds SmartInitScript instances
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2008            njensen      Initial creation
 * Jul 12, 2016   5747     dgilling     Move edex_static to common_static.
 * 
 * </pre>
 * 
 * @author njensen
 */

public class SmartInitFactory {

    private static String initDir;

    static {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationContext baseCtx = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        initDir = pathMgr.getFile(baseCtx,
                LocalizationUtil.join("gfe", "smartinit")).getPath();
    }

    /**
     * Constructs a smart init script object
     * 
     * @return the script object
     * @throws JepException
     */
    public static SmartInitScript constructInit() throws JepException {
        String path = LocalizationUtil.join(initDir, "Init.py");

        // initDir must be the last one so site overrides can happen
        return new SmartInitScript(path, PyUtil.buildJepIncludePath(
                GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getCommonGfeIncludePath(), initDir),
                SmartInitScript.class.getClassLoader());
    }

}
