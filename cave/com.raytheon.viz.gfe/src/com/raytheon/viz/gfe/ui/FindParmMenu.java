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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.UIFormat;
import com.raytheon.viz.gfe.core.UIFormat.FilterType;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Menu of displayed parameters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2010      #4411 randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class FindParmMenu extends CompoundContributionItem {
    private static final String COMMAND_ID = "com.raytheon.viz.gfe.actions.findWeatherElement";

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        MenuManager menuMgr = new MenuManager("Find Weather Element");
        DataManager dm = DataManager.getCurrentInstance();
        if (dm != null) {
            IParmManager parmMgr = dm.getParmManager();
            UIFormat uiFormat = new UIFormat(parmMgr, FilterType.DISPLAYED,
                    FilterType.DISPLAYED);
            Parm[] displayedParms = parmMgr.getDisplayedParms();
            Arrays.sort(displayedParms);
            for (Parm parm : displayedParms) {
                Map<String, String> parms = new HashMap<String, String>();
                ParmID parmId = parm.getParmID();
                parms.put("parm", parmId.toString());
                menuMgr.add(new CommandContributionItem(
                        new CommandContributionItemParameter(PlatformUI
                                .getWorkbench(), null, COMMAND_ID, parms, null,
                                null, null, uiFormat.uiParmID(parmId), null,
                                null, CommandContributionItem.STYLE_PUSH, null,
                                true)));
            }
        }
        return menuMgr.getItems();
    }
}
