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

import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.dialogs.isc.SendISCDialog;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;

/**
 * Action to launch Send ISC grids dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2015  #4858     dgilling     Initial creation
 * Oct 01, 2015  #4888     dgilling     Refactor based on GfeShowDialogHandler.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class ShowSendIscGridsDialog extends GfeShowDialogHandler {

    @Override
    protected CaveJFACEDialog createDialog(Shell shell, DataManager dm,
            ExecutionEvent event) {
        if (!SimulatedTimeOperations.isTransmitAllowed()) {
            SimulatedTimeOperations.displayFeatureLevelWarning(shell,
                    "Send ISC grids");
            return null;
        }

        return new SendISCDialog(shell, dm);
    }

    @Override
    public boolean isEnabled() {
        DataManager dm = DataManagerUIFactory.getCurrentInstance();
        if (dm != null) {
            return (!dm.sendIscOnSave() || !dm.sendIscOnPublish())
                    && CAVEMode.getMode().equals(CAVEMode.OPERATIONAL)
                    && dm.requestISC();
        }

        return false;
    }
}
