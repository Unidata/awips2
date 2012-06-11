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
package com.raytheon.viz.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.IMenuService;

/**
 * 
 * A handler for toolitems that drop down a menu when the item is clicked.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractDropDownMenuHandler extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        // This function has been unfortunately adapted from
        // CommandContributionItem.openDropDownMenu
        if (event.getTrigger() instanceof Event) {
            Event e = (Event) event.getTrigger();
            if (e.widget instanceof ToolItem) {
                ToolItem ti = (ToolItem) e.widget;
                if (ti.getData() instanceof CommandContributionItem) {
                    CommandContributionItem cci = (CommandContributionItem) ti
                            .getData();
                    final String id = cci.getId();
                    final MenuManager menuManager = new MenuManager();

                    Menu menu = menuManager.createContextMenu(ti.getParent());
                    menuManager.addMenuListener(new IMenuListener() {
                        public void menuAboutToShow(IMenuManager manager) {
                            ((IMenuService) PlatformUI.getWorkbench()
                                    .getService(IMenuService.class))
                                    .populateContributionManager(menuManager,
                                            "menu:" + id);
                        }
                    });

                    // position the menu below the drop down item
                    Point point = ti.getParent().toDisplay(
                            new Point(ti.getBounds().x, ti.getBounds().height));
                    menu.setLocation(point.x, point.y);
                    menu.setVisible(true);
                }
            }
        }
        return null;
    }
}
