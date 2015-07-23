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
package com.raytheon.viz.gfe.smarttool;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.core.DataManagerUIFactory;

/**
 * Display information dialog for Smart Tool.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 09, 2010            mpduff       Initial creation
 * Jul 23, 2015  #4263     dgilling     Support SmartToolMetadataManager.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class InfoAction extends Action {

    private String item = null;

    public InfoAction(String item) {
        super("Info");
        this.item = item;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        if (item.indexOf(".") != -1) {
            item = item.substring(0, item.indexOf("."));
        }

        IWorkbenchWindow window = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        String info = DataManagerUIFactory.getInstance(window)
                .getSmartToolInterface().getInfo(item);
        Shell shell = window.getShell();
        MessageDialog.openInformation(shell, item, info);
    }
}
