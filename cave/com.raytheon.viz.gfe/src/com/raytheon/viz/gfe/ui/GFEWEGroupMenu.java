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
package com.raytheon.viz.gfe.ui;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IWEGroupManager;

/**
 * Displays Weather Element groups (formerly Bundles)
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 9, 2008				chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GFEWEGroupMenu extends CompoundContributionItem {
    private static final String COMMAND_ID = "com.raytheon.viz.gfe.actions.WEGroup";

    private static final String NULL_COMMAND_ID = "com.raytheon.viz.ui.actions.nullAction";

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        return buildMenu().getItems();
    }

    public void fill(Menu parent) {
        buildMenu().fill(parent, -1);
    }

    private MenuManager buildMenu() {

        int cascadeNum = Integer.MAX_VALUE;
        if (GFEPreference.contains("MaxMenuItemsBeforeCascade")) {
            cascadeNum = GFEPreference
                    .getIntPreference("MaxMenuItemsBeforeCascade");
        }

        // Get the inventory from the WE Group Manager,
        // constructing one menu item with the COMMAND_ID
        // as the command, and the name of the WEGroup as the
        // "name"
        MenuManager menuMgr = new MenuManager("Weather Element Groups");
        DataManager dm = DataManager.getCurrentInstance();
        MenuManager currentPage = menuMgr;
        if (dm != null) {
            IWEGroupManager weGroupManager = dm.getWEGroupManager();
            List<String> groupList = weGroupManager.getInventory();
            int count = 0;
            for (String group : groupList) {
                Map<String, String> parms = new HashMap<String, String>();
                parms.put("name", group);
                group = group.replace("&", "&&");
                currentPage
                        .add(new CommandContributionItem(
                                new CommandContributionItemParameter(PlatformUI
                                        .getWorkbench(), null, COMMAND_ID,
                                        parms, null, null, null, group, null,
                                        null,
                                        CommandContributionItem.STYLE_PUSH,
                                        null, true)));
                // Also paginate the screen every cascadeNum items to keep the
                // menus from getting too long.
                if (++count % cascadeNum == 0) {
                    MenuManager mm1 = new MenuManager("More");
                    currentPage.add(mm1);
                    currentPage = mm1;
                }
            }
        }

        if (menuMgr.getItems().length == 0) {
            menuMgr.add(new CommandContributionItem(
                    new CommandContributionItemParameter(PlatformUI
                            .getWorkbench(), null, NULL_COMMAND_ID, null, null,
                            null, null, "<Empty>", null, null,
                            CommandContributionItem.STYLE_PUSH, null, true)));
        }
        return menuMgr;
    }
}
