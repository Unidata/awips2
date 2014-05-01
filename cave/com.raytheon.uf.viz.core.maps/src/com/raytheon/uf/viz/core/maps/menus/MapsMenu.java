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
package com.raytheon.uf.viz.core.maps.menus;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.uf.viz.core.maps.MapStore;
import com.raytheon.uf.viz.core.maps.MapStore.MapNode;
import com.raytheon.uf.viz.ui.menus.widgets.AbstractTearOffableCompoundContributionItem;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2008            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MapsMenu extends AbstractTearOffableCompoundContributionItem {

    /**
     * @param text
     * @param id
     */
    public MapsMenu() {
        super("Maps", MapsMenu.class.getName());
    }

    boolean addTear = false;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.ui.menus.widgets.
     * AbstractTearOffableCompoundContributionItem
     * #addContributionItems(org.eclipse.jface.action.IMenuManager)
     */
    @Override
    protected void addContributionItems(IMenuManager manager) {
        MapNode node = MapStore.getMapTree();
        createMenu(manager, node);
    }

    private void createMenu(IMenuManager manager, MapNode root) {
        for (MapNode node : root.getSubTree()) {
            if (node.getSubTree() == null) {
                Map<String, String> parms = new HashMap<String, String>();
                parms.put("mapName", node.getName());
                parms.put("mapPath", node.getPath());

                CommandContributionItem item = new CommandContributionItem(
                        new CommandContributionItemParameter(
                                PlatformUI.getWorkbench(),
                                null,
                                "com.raytheon.uf.viz.core.maps.actions.LoadMap",
                                parms, null, null, null, node.getName(), null,
                                null, CommandContributionItem.STYLE_CHECK,
                                null, true));
                manager.add(item);
            } else {
                IMenuManager subMenu = new MenuManager(node.getName(),
                        manager.getId() + "." + node.getName());
                createMenu(subMenu, node);
                manager.add(subMenu);
            }
        }
    }
}
