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
package com.raytheon.uf.viz.derivparam.ui.actions;

import org.eclipse.jface.action.Action;

import com.raytheon.uf.viz.derivparam.ui.dialogs.DerivedParamWizard;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.dialogs.CaveSWTWizardDlg;

/**
 * Opens wizard for creating a new derived parameter. TODO: Look into having
 * New... be build in and having adapter supply a New action
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 14, 2010            mschenke     Initial creation
 * Oct 22, 2012 1229       rferrel     Changes for non-blocking CaveSWTWizardDlg.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class NewDerivedParameterAction extends Action {
    private CaveSWTWizardDlg dialog;

    /**
     * 
     */
    public NewDerivedParameterAction() {
        super("Derived Parameter...");
    }

    @Override
    public void run() {
        DerivedParamWizard wizard = new DerivedParamWizard();
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            dialog = new CaveSWTWizardDlg(VizWorkbenchManager.getInstance()
                    .getCurrentWindow().getShell(), wizard);
            dialog.open();
        } else {
            dialog.bringToTop();
        }
    }

}
