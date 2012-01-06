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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import jep.JepException;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Procedure interpreter to be used for running on the UI thread
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 9, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ProcedureUIController extends ProcedureController {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ProcedureUIController.class);

    private Map<String, String[]> menuItems;

    public ProcedureUIController(String scriptPath, String buildIncludePath,
            ClassLoader classLoader, DataManager dataMgr) throws JepException {
        super(scriptPath, buildIncludePath, classLoader, dataMgr);
    }

    public synchronized String[] getMenuItems(String menuName) {
        if (menuItems == null) {
            menuItems = new HashMap<String, String[]>();
            menuItems.put("Hazards", listProceduresByMenu("Hazards"));
            menuItems.put("Verify", listProceduresByMenu("Verify"));
            menuItems.put("Edit", listProceduresByMenu("Edit"));
            menuItems.put("Consistency", listProceduresByMenu("Consistency"));
            menuItems.put("Populate", listProceduresByMenu("Populate"));
        }
        return menuItems.get(menuName);
    }

    /**
     * Lists all the procedures that correspond to the menu name
     * 
     * @param menuName
     *            the name of the menu
     * @return the names of tools that apply to the parm
     */
    @SuppressWarnings(value = "unchecked")
    private String[] listProceduresByMenu(String menuName) {
        Set<String> set = null;
        String[] procs = new String[0];
        try {
            HashMap<String, Object> argMap = new HashMap<String, Object>(1);
            argMap.put("menu", menuName);
            set = (Set<String>) execute("getScripts", INTERFACE, argMap);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error determining list of or procedures for menu "
                            + menuName, e);
        }
        if (set != null) {
            procs = set.toArray(new String[set.size()]);
            Arrays.sort(procs);
        }
        return procs;
    }

    @Override
    public void fileUpdated(final FileUpdatedMessage message) {
        super.fileUpdated(message);

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                processFileUpdates();
                menuItems = null;
            }
        });
    }
}
