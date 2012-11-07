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
package com.raytheon.viz.ui.personalities.awips;

import org.eclipse.swt.widgets.Display;

import com.raytheon.viz.ui.dialogs.CaveSWTDialogBase;

/**
 * This abstract class provides a way to wait for a non-blocking dialog to
 * dispose before continuing. Since having more then one dialog blocking causes
 * problems the intended uses is to block the top level dialog of a component
 * running in stand alone mode.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2012 1229       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public abstract class AbstractCAVEDialogComponent extends AbstractCAVEComponent {
    protected void blockUntilClosed(CaveSWTDialogBase dlg) {
        while (!dlg.isDisposed()) {
            if (!Display.getCurrent().readAndDispatch()) {
                Display.getCurrent().sleep();
            }
        }
    }
}
