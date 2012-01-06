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

package com.raytheon.viz.awipstools.ui.action;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.awipstools.Activator;
import com.raytheon.viz.awipstools.ui.dialog.UnitsCalculatorDialog;

/**
 * UnitsCalculatorAction.java
 * 
 * Action class called when using units calculator from tools menu.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ----------   ----------  ----------- --------------------------
 * 08/28/07     426         Eric Babin    Initial Creation.
 * </pre>
 * 
 * @author Eric Babin
 * @version 1
 */
public class UnitsCalculatorAction extends AbstractHandler {

    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        UnitsCalculatorDialog id = null;
        try {
            id = new UnitsCalculatorDialog(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell(), "Unit Calculator");
            if (id.open() == UnitsCalculatorDialog.OK) {

            }
        } catch (VizException e) {
            Status status = new Status(Status.ERROR, Activator.PLUGIN_ID, 0,
                    "Error occurred during units calculator tool action.", e);
            ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                    "ERROR",
                    "Error occurred during units calculator tool action",
                    status);
            throw new ExecutionException(
                    "Error occurred units calculator action", e);
        }
        return null;
    }

}
