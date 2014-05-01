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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonCommandContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.viz.core.VariableSubstitutionUtil;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Provides the ability to define menu contributions based on command ids
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 28, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class CommandContribution extends
        AbstractMenuContributionItem<CommonCommandContribution> {

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
        CommonCommandContribution item = (CommonCommandContribution) items;
        Map<String, String> parms = new HashMap<String, String>();

        if (removals.contains(items.id)) {
            return new IContributionItem[0];
        }

        if (item.parameters != null) {
            Map<String, String> s = VariableSubstitution.toMap(subs);
            for (VariableSubstitution sub : item.parameters) {
                sub.value = VariableSubstitutionUtil.processVariables(
                        sub.value, s);
                parms.put(sub.key, sub.value);
            }

        }

        CommandContributionItemParameter ccp = new CommandContributionItemParameter(
                PlatformUI.getWorkbench(), items.id, item.commandId, parms,
                null, null, null, item.text, null, null,
                CommandContributionItem.STYLE_PUSH, null, true);
        CommandContributionItem cci = new CommandContributionItem(ccp);
        return new IContributionItem[] { cci };
    }

}
