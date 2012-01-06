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
package com.raytheon.viz.gfe.smarttool;

import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.viz.gfe.smarttool.action.InfoAction;

/**
 * Listens to mouse events on the smart tools list in the EditActionsDialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 25, 2008            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolMouseListener implements MouseListener {

    private MenuManager menuMgr;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.MouseListener#mouseDoubleClick(org.eclipse.swt
     * .events.MouseEvent)
     */
    @Override
    public void mouseDoubleClick(MouseEvent e) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.MouseListener#mouseDown(org.eclipse.swt.events
     * .MouseEvent)
     */
    @Override
    public void mouseDown(MouseEvent e) {

        List list = (List) e.getSource();
        String selected = list.getItem(list.getSelectionIndex());

        // one left mouse button click
        if (e.button == 1) {
            SmartUtil.runTool(selected);
        } else if (e.button == 3) {
            if (menuMgr == null) {
                menuMgr = new MenuManager("#PopupMenu");
                // menuMgr.setRemoveAllWhenShown(true);
            } else {
                menuMgr.removeAll();
            }

            menuMgr.add(new InfoAction(selected, "Info..."));

            Menu menu = menuMgr.createContextMenu(list);

            menu.setVisible(true);
            list.setMenu(menu);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.swt.events.MouseListener#mouseUp(org.eclipse.swt.events.
     * MouseEvent)
     */
    @Override
    public void mouseUp(MouseEvent arg0) {

    }

}
