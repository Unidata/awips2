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
package com.raytheon.viz.volumebrowser.xml;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.action.IContributionItem;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonToolBarContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.ui.menus.xml.AbstractMenuContributionItem;
import com.raytheon.uf.viz.ui.menus.xml.IContribItemProvider;
import com.raytheon.uf.viz.ui.menus.xml.MenuXMLMap;
import com.raytheon.viz.volumebrowser.widget.ToolBarContributionItem;

/**
 * 
 * Contribution Item for tool bars.
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
public class ToolBarContribution extends
        AbstractMenuContributionItem<CommonToolBarContribution> {

    public ToolBarContribution() {
        this.xml = new CommonToolBarContribution();
    }

    @Override
    public IContributionItem[] getContributionItems(
            CommonAbstractMenuContribution items, VariableSubstitution[] subs,
            Set<String> removals) throws VizException {
        CommonToolBarContribution item = (CommonToolBarContribution) items;
        List<IContributionItem> contribItemList = new ArrayList<IContributionItem>();

        for (CommonAbstractMenuContribution amc : item.contributions) {
            IContribItemProvider common = MenuXMLMap
                    .getProvider(amc.getClass());
            contribItemList.addAll(Arrays.asList(common.getContributionItems(
                    amc, subs, removals)));
        }
        ToolBarContribution cont = new ToolBarContribution();
        cont.xml = item;
        final ToolBarContributionItem toolBarContributionItem = new ToolBarContributionItem(
                cont, contribItemList
                        .toArray(new IContributionItem[contribItemList.size()]));

        return new IContributionItem[] { toolBarContributionItem };
    }
}
