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
package com.raytheon.viz.gfe.temporaleditor.actions;

import java.util.Date;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Display;

import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.temporaleditor.dialogs.TEWxSetValueDialog;

/**
 * Used by the right click menu on a WEATHER grid type, to set a discrete
 * weather element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ------------- --------------------------
 * May 28, 2009 #2159      Richard Peter Initial Creation.
 * Nov 13, 2012 #1298      rferrel       Changes for non-blocking TEWxSetValueDialog.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class SetWeatherAction extends Action {
    private Parm parm;

    private Date date;

    public SetWeatherAction(Parm parm, Date date) {
        super("Set...");
        this.parm = parm;
        this.date = date;
    }

    @Override
    public void run() {
        // The dialog being opened is modal to the parent dialog. This will
        // prevent the launching of another dialog until the modal dialog is
        // closed.
        TEWxSetValueDialog weatherPickupValueDialog = new TEWxSetValueDialog(
                Display.getCurrent().getActiveShell(), parm, date);
        weatherPickupValueDialog.setBlockOnOpen(false);
        weatherPickupValueDialog.open();
    }
}
