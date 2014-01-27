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
 * Feb 9, 2010            njensen     Initial creation
 * Jun 25, 2013  16065    ryu         Passing outerLevel to tool job
 * Dec 10, 2013  #2367    dgilling    Use new SmartToolJobPool.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolSelectionDlg extends SelectionDlg {

    public SmartToolSelectionDlg(Shell parent, String title,
            DataManager dataMgr, List<FieldDefinition> varList) {
        super(parent, title, dataMgr, varList);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.ui.runtimeui.SelectionDlg#run()
     */
    @Override
    public void run() {
        PreviewInfo pi = SmartUtil.checkAndBuildPreview(dataMgr, name);
        if (pi != null) {
            SmartToolRequest req = SmartUtil.buildSmartToolRequest(dataMgr, pi,
                    true);
            if (req != null) {
                String varDict = dataMgr.getSmartToolInterface()
                        .transformVarDict(getValues());
                req.setVarDict(varDict);
                dataMgr.getSmartToolJobPool().schedule(req);
            }
        }
    }

}
