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
package com.raytheon.uf.viz.d2d.ui.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Draws a locator box around the cursor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 1, 2009            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class LocateCursorAction extends AbstractTool {

    private CaveJFACEDialog dialog;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        super.execute(event);
        // check if a current dialog is open... only one dialog open at a time.
        if (dialog == null || dialog.getShell() == null
                || dialog.getShell().isDisposed()) {
            IWorkbenchWindow window = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow();
            Shell shell = window.getShell();

            openDialog(shell);
        }
        return null;
    }

    private void openDialog(Shell parentShell) {
        dialog = new CursorBoxDialog(parentShell);

        dialog.setBlockOnOpen(false);
        dialog.open();

        // set up close timer
        Display display = PlatformUI.getWorkbench().getDisplay();
        int oneAndAHalfSeconds = 1500;

        display.timerExec(oneAndAHalfSeconds, new Runnable() {
            @Override
            public void run() {
                if (dialog != null && dialog.getShell() != null
                        && !dialog.getShell().isDisposed()) {
                    dialog.close();
                }
            }
        });
    }

    private static class CursorBoxDialog extends CaveJFACEDialog {
        protected CursorBoxDialog(Shell parentShell) {
            super(parentShell);
            setShellStyle(SWT.NO_TRIM | SWT.MODELESS);
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
         * .Shell)
         */
        @Override
        protected void configureShell(Shell newShell) {
            // using cursor bounds, open a dialog centered on the cursor
            // location. draw a series of 12 nested boxes in this dialog.
            final Display display = newShell.getDisplay();
            final Point cLoc = display.getCursorLocation();
            final Color white = display.getSystemColor(SWT.COLOR_WHITE);
            final Color black = display.getSystemColor(SWT.COLOR_BLACK);
            final int boxCount = 23; // 12 white boxes and 11 black boxes
            final int sideSize = 190;
            final int center = sideSize / 2;
            final int sideInc = center / boxCount;
            final int margin = sideInc * 2;
            GridData shellGD = new GridData(SWT.FILL, SWT.FILL, true, true);

            newShell.setLayout(new GridLayout(1, false));
            newShell.setLayoutData(shellGD);
            newShell.setBounds(cLoc.x - center, cLoc.y - center, sideSize,
                    sideSize);

            Canvas canvas = new Canvas(newShell, SWT.NONE);
            canvas.setLayoutData(new GridData(sideSize, sideSize));
            canvas.addPaintListener(new PaintListener() {
                @Override
                public void paintControl(PaintEvent e) {
                    for (int i = 0; i < boxCount; ++i) {
                        Color color = (i % 2 == 0) ? white : black;
                        int x = i * sideInc;
                        int y = x;
                        int boxSide = sideSize - (margin * i);

                        e.gc.setBackground(color);
                        e.gc.fillRectangle(x, y, boxSide, boxSide);
                    }
                }
            });

            super.configureShell(newShell);
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.jface.dialogs.Dialog#createButtonBar(org.eclipse.swt.
         * widgets .Composite)
         */
        @Override
        protected Control createButtonBar(Composite parent) {
            // overide this method so that the default dialog buttons are not
            // drawn.
            return null;
        }

    }
}
