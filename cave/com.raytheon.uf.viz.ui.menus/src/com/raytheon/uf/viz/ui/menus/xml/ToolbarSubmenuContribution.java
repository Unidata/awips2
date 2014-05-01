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
package com.raytheon.uf.viz.ui.menus.xml;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.action.IContributionItem;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonToolbarSubmenuContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.ui.menus.widgets.ToolbarSubmenuContributionItem;

/**
 * 
 * Contribution for adding submenus to a tool bar.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Dec 11, 2013  2602     bsteffen    Update MenuXMLMap.
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public class ToolbarSubmenuContribution extends
        AbstractMenuContributionItem<CommonToolbarSubmenuContribution> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.ui.menus.xml.IContribItemProvider#getContributionItems
     * (com.raytheon.uf.viz.ui.menus.xml.VariableSubstitution[], java.util.Set)
     */
    @Override
    public IContributionItem[] getContributionItems(
            CommonAbstractMenuContribution items, VariableSubstitution[] subs,
            Set<String> removals) throws VizException {
        CommonToolbarSubmenuContribution item = (CommonToolbarSubmenuContribution) items;
        if (removals.contains(items.id))
            return new IContributionItem[0];
        List<IContributionItem> contribItemList = new ArrayList<IContributionItem>();

        for (CommonAbstractMenuContribution amc : item.contributions) {
            IContribItemProvider common = MenuXMLMap
                    .getProvider(amc.getClass());
            contribItemList.addAll(Arrays.asList(common.getContributionItems(
                    amc, subs, removals)));
        }

        final ToolbarSubmenuContributionItem smci = new ToolbarSubmenuContributionItem(
                item.menuText,
                contribItemList.toArray(new IContributionItem[contribItemList
                        .size()]), item.id);

        return new IContributionItem[] { smci };
    }
}
