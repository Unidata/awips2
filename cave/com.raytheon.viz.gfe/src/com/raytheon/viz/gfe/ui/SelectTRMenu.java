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
import java.util.Map;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISelectTimeRangeManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SelectTRMenu extends CompoundContributionItem {

    private static final String COMMAND_ID = "com.raytheon.viz.gfe.actions.selectTimeRange";

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {

        // Get the inventory from the SelectTimeRangeManager,
        // constructing one menu item with the COMMAND_ID
        // as the command, and the name of the SelectTR as the
        // "name"
        MenuManager menuMgr = new MenuManager("Select Time Ranges");
        DataManager dm = DataManager.getCurrentInstance();
        if (dm != null) {
            ISelectTimeRangeManager selectTRManager = dm
                    .getSelectTimeRangeManager();
            String[] trList = selectTRManager.inventory();
            for (String tr : trList) {

                Map<String, String> parms = new HashMap<String, String>();
                parms.put("name", tr);
                menuMgr
                        .add(new CommandContributionItem(
                                new CommandContributionItemParameter(PlatformUI
                                        .getWorkbench(), null, COMMAND_ID,
                                        parms, null, null, null, tr, null,
                                        null,
                                        CommandContributionItem.STYLE_PUSH,
                                        null, true)));
            }
        }
        return menuMgr.getItems();
    }

}
