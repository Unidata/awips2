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

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.uf.viz.core.maps.MapStore;
import com.raytheon.uf.viz.core.maps.MapStore.MapNode;

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

public class MapsMenu extends CompoundContributionItem {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        MapNode node = MapStore.getMapTree();
        return createMenu(node).getItems();
    }

    private MenuManager createMenu(MapNode root) {
        MenuManager menuMgr = new MenuManager(root.getName());
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
                menuMgr.add(item);
            } else {
                menuMgr.add(createMenu(node));
            }
        }
        return menuMgr;
    }
}
