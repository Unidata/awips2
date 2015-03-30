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
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.awipstools.ui.dialog.LAPSToolsDlg;
import com.raytheon.viz.ui.EditorUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2009     #          bsteffen      Initial creation
 * Nov 2013     #          mccaslin      Only one GUI dialog at a time
 * Oct 2014     #          mccaslin      Improved error handeling
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class LapsToolsAction extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LapsToolsAction.class);

    /**
     * LAPS Tools dialog.
     */
    private static LAPSToolsDlg lapsToolsDlg = null;

    public static LAPSToolsDlg getLapsToolsDlg() {
        return lapsToolsDlg;
    }

    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        if (lapsToolsDlg == null) {
            try {
                lapsToolsDlg = new LAPSToolsDlg(shell);
                lapsToolsDlg.addListener(SWT.Dispose, new Listener() {
                    @Override
                    public void handleEvent(Event event) {
                        lapsToolsDlg = null;
                    }
                });

                if (lapsToolsDlg.isLapsInstalled()) {
                    lapsToolsDlg.open();
                } else {
                    String whatLapsIs = "LAPS is not installed.           ";
                    // Note: Go through the LAPS v2.0 Scripting Interface first,
                    // if you find that LAPS is not installed.

                    MessageBox mb = new MessageBox(EditorUtil.getActiveEditor()
                            .getSite().getShell(), SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Cannot open the LAPS tool");
                    mb.setMessage(whatLapsIs);
                    mb.open();
                    lapsToolsDlg = null;

                    // int val = mb.open();
                    // if (val == SWT.OK) {
                    // AlertViz Customization Update
                    // return false;
                    // }

                }

            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error: Cannot open LAPS V2.0 Tools GUI", e);
            }

        }

        return null;
    }

}
