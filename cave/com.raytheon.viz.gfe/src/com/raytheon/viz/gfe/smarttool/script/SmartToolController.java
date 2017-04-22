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

import java.util.List;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.BaseGfePyController;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Controller for getting the information on smart tools and running them. All
 * smart tools execute through this interface.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2008            njensen     Initial creation
 * Oct 29, 2013  2476      njensen     Renamed numeric methods to numpy
 * 10/31/2013    2508      randerso    Change to use DiscreteGridSlice.getKeys()
 * Oct 14, 2014  3676      njensen     Promoted getNumpyResult() to parent class
 * Apr 23, 2015  4259      njensen     Updated for new JEP API
 * Jul 23, 2015  4263      dgilling    Refactored to abstract class.
 * 
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class SmartToolController extends BaseGfePyController {
    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    /**
     * Constructor
     * 
     * @param filePath
     *            path to the SmartToolInterface.py
     * @param anIncludePath
     *            path of directories to include
     * @param classLoader
     *            Java classloader
     * @param dataManager
     *            current DataManager
     * @throws JepException
     */
    public SmartToolController(String filePath, String anIncludePath,
            ClassLoader classLoader, DataManager dataManager)
            throws JepException {
        super(filePath, anIncludePath, classLoader, dataManager, "Tool");

        String scriptPath = GfePyIncludeUtil.getSmartToolsIncludePath();
        jep.eval(INTERFACE + " = SmartToolInterface('" + scriptPath + "')");

        List<String> errors = getStartupErrors();
        if (errors.size() > 0) {
            StringBuffer sb = new StringBuffer();
            sb.append("Error importing the following smart tools:\n");
            for (String s : errors) {
                sb.append(s);
                sb.append("\n");
            }
            statusHandler.handle(Priority.PROBLEM, sb.toString());
        }
        jep.eval("import TimeRange, numpy");
    }
}
