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

import java.util.List;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.BaseGfePyController;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Manages and runs the procedures in the system.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 05, 2008            njensen      Initial creation
 * Jan 08, 2013  1486      dgilling     Support changes to BaseGfePyController.
 * Feb 12, 2013  #1597     randerso     Added logging to support GFE Performance metrics
 * Jul 27, 2015  #4263     dgilling     Refactor and make abstract.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class ProcedureController extends BaseGfePyController {

    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    protected final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    public ProcedureController(String filePath, String anIncludePath,
            ClassLoader classLoader, DataManager dataManager)
            throws JepException {
        super(filePath, anIncludePath, classLoader, dataManager, "Procedure");

        String scriptPath = GfePyIncludeUtil.getProceduresIncludePath();

        jep.eval(INTERFACE + " = ProcedureInterface('" + scriptPath + "')");

        List<String> errors = getStartupErrors();
        if (errors.size() > 0) {
            StringBuffer sb = new StringBuffer();
            sb.append("Error importing the following procedures:\n");
            for (String s : errors) {
                sb.append(s);
                sb.append("\n");
            }
            statusHandler.handle(Priority.PROBLEM, sb.toString());
        }
        jep.eval("import TimeRange");
        jep.eval("import sys");
        jep.eval("sys.argv = ['ProcedureInterface']");
    }
}
