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
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager;

/**
 * TODO: change this implementation to work from the server's list of edit areas
 * 
 * This is a temporary implementation that finds the edit areas in a static
 * client directory. This will eventually need to get the list from the server.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Mar 28, 2008		#1053	randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GFEEditAreaMenu extends CompoundContributionItem {
    private static final String COMMAND_ID = "com.raytheon.viz.gfe.actions.EditArea";

    private static final String NULL_COMMAND_ID = "com.raytheon.viz.ui.actions.nullAction";

    // private boolean addTear = true;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        MenuManager menuMgr = new MenuManager("Edit Areas");
        DataManager dm = DataManager.getCurrentInstance();
        if (dm != null) {
            IReferenceSetManager refMgr = dm.getRefManager();
            List<String> groupList = refMgr.getGroupInventory();
            groupList.add("Misc");
            int menuLength = GFEPreference
                    .getIntPreference("MaxMenuItemsBeforeCascade");
            menuLength = (menuLength > 1) ? menuLength : 30;
            for (String group : groupList) {
                MenuManager mm = new MenuManager(group);
                menuMgr.add(mm);

                // mm.addMenuListener(new TearOffMenuListener(mm));
                int count = 0;
                for (String ref : refMgr.getGroupData(group)) {
                    Map<String, String> parms = new HashMap<String, String>();
                    parms.put("name", ref);
                    mm.add(new CommandContributionItem(
                            new CommandContributionItemParameter(PlatformUI
                                    .getWorkbench(), null, COMMAND_ID, parms,
                                    null, null, null, ref, null, null,
                                    CommandContributionItem.STYLE_PUSH, null,
                                    true)));
                    if (++count % menuLength == 0) {
                        MenuManager mm1 = new MenuManager("More");
                        mm.add(mm1);
                        mm = mm1;
                    }
                }

                if (count == 0) {
                    mm.add(new CommandContributionItem(
                            new CommandContributionItemParameter(PlatformUI
                                    .getWorkbench(), null, NULL_COMMAND_ID,
                                    null, null, null, null, "<Empty>", null,
                                    null, CommandContributionItem.STYLE_PUSH,
                                    null, true)));
                }
            }
        }
        return menuMgr.getItems();
    }
}
