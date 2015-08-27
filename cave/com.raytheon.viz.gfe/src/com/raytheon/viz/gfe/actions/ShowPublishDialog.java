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
package com.raytheon.viz.gfe.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.dialogs.PublishDialog;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Action to launch publish to official dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 06, 2008            Eric Babin  Initial Creation
 * Oct 25, 2012 1287       rferrel     Changes for non-blocking PublishDialog.
 * Aug 27, 2015 4749       njensen     Now extends GfeShowDialogHandler
 * 
 * </pre>
 * 
 */
public class ShowPublishDialog extends GfeShowDialogHandler {

    @Override
    public boolean isEnabled() {
        if (!super.isEnabled()) {
            return false;
        }

        DataManager dm = DataManagerUIFactory.getCurrentInstance();
        if (dm != null) {
            return !dm.getParmManager().getMutableDatabase()
                    .equals(dm.getParmManager().getProductDB());
        }
        return false;
    }

    @Override
    protected CaveJFACEDialog createDialog(Shell shell, DataManager dm,
            ExecutionEvent event) {
        return new PublishDialog(shell, dm);
    }
}
