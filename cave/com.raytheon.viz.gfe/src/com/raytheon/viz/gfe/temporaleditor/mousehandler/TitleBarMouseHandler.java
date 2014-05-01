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
package com.raytheon.viz.gfe.temporaleditor.mousehandler;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.gridmanager.MouseHandler;
import com.raytheon.viz.gfe.temporaleditor.AbstractTemporalEditorBar;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorUtil;
import com.raytheon.viz.gfe.temporaleditor.dialogs.DisplayAttributesDialog;
import com.raytheon.viz.gfe.temporaleditor.dialogs.MoveWeatherElementDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * MouseHandler to resize temporal editor bars.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ------------- --------------------------
 * May 28, 2009 #2159      Richard Peter Initial Creation.
 * Nov 14, 2012 #1298      rferrel       Changes for non-blocking DisplayAttributesDialog.
 *                                        Changes for non-blocking MoveWeatherElementDialog.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class TitleBarMouseHandler extends MouseHandler {
    private MenuManager menuMgr;

    AbstractTemporalEditorBar teBar;

    public TitleBarMouseHandler(AbstractTemporalEditorBar teBar) {
        this.teBar = teBar;
    }

    @Override
    public void mouseClick(MouseEvent e) {
        super.mouseClick(e);
        List<Parm> parmList = teBar.getParms();
        if (e.button == 1) {
            Parm parm = teBar.getClickedTitleBarParm(new Point(e.x, e.y), true,
                    false);

            if (parm != null) {
                teBar.toggleParmDisplayed(parm);
            }

            parm = teBar.getClickedTitleBarParm(new Point(e.x, e.y), false,
                    true);

            if (parm != null) {
                teBar.toggleParmDisplayedAsGraphic(parm);
            }
        } else if (e.button == 2) {
            // move displayed parm to start of list, only need to do if more
            // than one already in list
            if (parmList.size() > 1) {
                Parm parm = teBar.getClickedTitleBarParm(new Point(e.x, e.y));

                if (parm != null) {
                    parmList.remove(parm);
                    parmList.add(0, parm);
                    teBar.redraw();
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.gridmanager.MouseHandler#displayContextMenu ()
     */
    @Override
    public void displayContextMenu(final MouseEvent e)
            throws GFEServerException {
        super.displayContextMenu(e);

        if (menuMgr != null) {
            menuMgr.dispose();
        }

        Point pt = new Point(e.x, e.y);
        final Parm parm = teBar.getClickedTitleBarParm(pt);
        if (parm != null) {
            menuMgr = new MenuManager("#PopupMenu");

            GridType gridType = parm.getGridInfo().getGridType();

            if (GridType.SCALAR.equals(gridType)
                    || GridType.VECTOR.equals(gridType)) {
                menuMgr.add(new Action("Display Attributes") {
                    @Override
                    public void run() {
                        Shell shell = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell();
                        // The dialog being opened is modal to the parent
                        // dialog. This will prevent the launching of another
                        // dialog until the modal dialog is closed.
                        DisplayAttributesDialog dialog = new DisplayAttributesDialog(
                                shell, teBar, parm);
                        dialog.setBlockOnOpen(false);
                        dialog.setCloseCallback(new ICloseCallback() {

                            @Override
                            public void dialogClosed(Object returnValue) {
                                teBar.redraw();
                            }
                        });
                        dialog.open();
                    }
                });
            }

            final List<AbstractTemporalEditorBar> barList = teBar
                    .getTemporalEditor().getCombinableBars(parm, teBar);
            if (teBar.getParms().size() > 1 || barList.size() > 0) {
                menuMgr.add(new Action("Move "
                        + TemporalEditorUtil.getTitleBarText(parm) + "...") {
                    @Override
                    public void run() {
                        Shell shell = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell();
                        // The dialog being opened is modal to the parent
                        // dialog. This will prevent the launching of another
                        // dialog until the modal dialog is closed.
                        MoveWeatherElementDialog dialog = new MoveWeatherElementDialog(
                                shell, teBar.getTemporalEditor(), parm, teBar,
                                barList);
                        dialog.setBlockOnOpen(false);
                        dialog.open();
                    }
                });
            }

            Composite container = teBar.getContainer();
            Menu menu = menuMgr.createContextMenu(container);
            menu.setVisible(true);

            container.setMenu(menu);
        }
    }
}
