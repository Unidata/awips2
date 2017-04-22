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
import java.util.Map;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;
import com.raytheon.viz.gfe.smarttool.PreviewInfo;
import com.raytheon.viz.gfe.ui.runtimeui.SelectionDlg;

/**
 * Dynamic GUI for displaying VariableLists and running procedures
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 09, 2010            njensen     Initial creation
 * Dec 09, 2013  #2367     dgilling    Use new ProcedureJobPool.
 * Jul 17, 2015  4575      njensen     Changed varDict from String to Map
 * Sep 23, 2015  4871      randerso    Code clean up
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ProcedureSelectionDlg extends SelectionDlg {

    /**
     * Constructor
     * 
     * @param parent
     *            parent shell
     * @param name
     *            name of smartTool/procedure
     * @param dataMgr
     *            DataManager instance to use
     * @param fieldDefs
     *            field definitions for dialog
     */
    public ProcedureSelectionDlg(Shell parent, String name,
            DataManager dataMgr, List<FieldDefinition> fieldDefs) {
        super(parent, name, dataMgr, fieldDefs, false);
    }

    @Override
    protected void run() {
        PreviewInfo pi = ProcedureUtil.checkAndBuildPreview(dataMgr, name);
        if (pi != null) {
            ProcedureRequest req = ProcedureUtil.buildProcedureRequest(name,
                    dataMgr);
            if (req != null) {
                Map<String, Object> varDict = getValues();
                req.setVarDict(varDict);
                req.setPreview(pi);
                dataMgr.getProcedureJobPool().schedule(req);
            }
        }
    }

}
