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

package com.raytheon.viz.texteditor;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Widget;

import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.command.ICommand;
import com.raytheon.viz.texteditor.dialogs.TextEditorDialog;

/**
 * Execute an AFOS command.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/12/2009   2191        rjpeter     Initial creation.
 * 02/14/2018   6960        dgilling    Fix NullPointerException when command
 *                                      is triggered from a sub-menu.
 * </pre>
 *
 * @author rjpeter
 *
 */
public class AfosProductAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        String afosCommand = arg0.getParameter("afoscommand");
        Object trigger = arg0.getTrigger();

        if (trigger != null && trigger instanceof Event) {
            Event e = (Event) trigger;
            Widget w = e.widget;

            Object dialog = null;
            if (w instanceof MenuItem) {
                MenuItem item = (MenuItem) w;
                Menu menu = item.getParent();
                dialog = menu.getData("Dialog");

                // recurse back up tree in case we were multiple levels deep
                while (dialog == null) {
                    item = menu.getParentItem();
                    if (item != null) {
                        menu = item.getParent();
                        if (menu != null) {
                            dialog = menu.getData("Dialog");
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }

            if ((dialog != null) && (dialog instanceof TextEditorDialog)) {
                TextEditorDialog editorDlg = (TextEditorDialog) dialog;
                editorDlg.setAfosCmdField(afosCommand);
                ICommand cmd = CommandFactory.getAfosCommand(afosCommand);
                editorDlg.executeCommand(cmd);
            }
        }
        return null;
    }

}
