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

import com.raytheon.viz.gfe.GFEPreference;

/**
 * Smooth Size Menu
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 21, 2010           randerso  Initial creation
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author randerso
 */

public class SmoothSizeMenu extends CompoundContributionItem {
    private static final String COMMAND_ID = "com.raytheon.viz.gfe.actions.setSmoothSize";

    @Override
    protected IContributionItem[] getContributionItems() {
        MenuManager menuMgr = new MenuManager("Smoothing Algorithm");
        int[] smoothSizeList = GFEPreference.getIntArray("SmoothSizeList",
                new int[] { 3, 5, 7, 9 });

        for (int smoothSize : smoothSizeList) {
            Map<String, String> parms = new HashMap<>();
            parms.put("size", Integer.toString(smoothSize));
            menuMgr.add(new CommandContributionItem(
                    new CommandContributionItemParameter(
                            PlatformUI.getWorkbench(), null, COMMAND_ID, parms,
                            null, null, null,
                            String.format("%dx%d", smoothSize, smoothSize),
                            null, null, CommandContributionItem.STYLE_RADIO,
                            null, true)));
        }

        return menuMgr.getItems();
    }

}
