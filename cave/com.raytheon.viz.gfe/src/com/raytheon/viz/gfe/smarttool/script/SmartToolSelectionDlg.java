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
import java.util.Map;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;
import com.raytheon.viz.gfe.smarttool.PreviewInfo;
import com.raytheon.viz.gfe.smarttool.SmartUtil;
import com.raytheon.viz.gfe.ui.runtimeui.SelectionDlg;

/**
 * Dynamic GUI for showing smart tools' Variable Lists and running the tools
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 09, 2010            njensen     Initial creation
 * Jun 25, 2013  16065     ryu         Passing outerLevel to tool job
 * Dec 10, 2013  #2367     dgilling    Use new SmartToolJobPool.
 * Jul 17, 2015  4575      njensen     Changed varDict from String to Map
 * Sep 23, 2015  4871      randerso    Code clean up
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolSelectionDlg extends SelectionDlg {

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
    public SmartToolSelectionDlg(Shell parent, String name,
            DataManager dataMgr, List<FieldDefinition> fieldDefs) {
        super(parent, name, dataMgr, fieldDefs, false);
    }

    @Override
    protected void run() {
        PreviewInfo pi = SmartUtil.checkAndBuildPreview(dataMgr, name);
        if (pi != null) {
            SmartToolRequest req = SmartUtil.buildSmartToolRequest(dataMgr, pi,
                    true);
            if (req != null) {
                Map<String, Object> varDict = getValues();
                req.setVarDict(varDict);
                dataMgr.getSmartToolJobPool().schedule(req);
            }
        }
    }

}
