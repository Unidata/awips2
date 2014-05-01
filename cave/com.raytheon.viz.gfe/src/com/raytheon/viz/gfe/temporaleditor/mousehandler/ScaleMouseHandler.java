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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.gridmanager.MouseHandler;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorNumericBar;

/**
 * MouseHandler to interact with a numeric scale.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ------------- --------------------------
 * May 28, 2009 #2159      Richard Peter Initial Creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ScaleMouseHandler extends MouseHandler {
    private static final float ZOOM_FACTOR = 1.5f;

    private MenuManager menuMgr;

    int lastHeightProcessed = 0;

    TemporalEditorNumericBar teBar;

    public ScaleMouseHandler(TemporalEditorNumericBar teBar) {
        this.teBar = teBar;
    }

    @Override
    public void mouseClick(MouseEvent e) {
        super.mouseClick(e);
        if (e.button == 1) {
            // zoom out
            teBar.zoom(ZOOM_FACTOR, e.y);
        } else if (e.button == 2) {
            // zoom in
            teBar.zoom(1 / ZOOM_FACTOR, e.y);
        }
    }

    @Override
    public void dragStart(MouseEvent e) {
        super.dragStart(e);
        lastHeightProcessed = this.getDragAnchor().y;
    }

    @Override
    public void dragMove(MouseEvent e) {
        super.dragMove(e);
        teBar.pan(lastHeightProcessed - e.y);
        lastHeightProcessed = e.y;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.gridmanager.MouseHandler#displayContextMenu ()
     */
    @Override
    public void displayContextMenu(MouseEvent e) throws GFEServerException {
        super.displayContextMenu(e);

        if (menuMgr != null) {
            menuMgr.dispose();
        }

        menuMgr = new MenuManager("#PopupMenu");
        menuMgr.add(new Action("Full View") {
            @Override
            public void run() {
                teBar.fullView();
            }
        });

        menuMgr.add(new Action("Fit to Data") {
            @Override
            public void run() {
                teBar.fitToData();
            }
        });

        Composite container = teBar.getContainer();
        Menu menu = menuMgr.createContextMenu(container);
        menu.setVisible(true);

        container.setMenu(menu);
    }
}
