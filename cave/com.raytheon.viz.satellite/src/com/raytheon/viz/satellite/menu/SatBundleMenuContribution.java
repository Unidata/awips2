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
package com.raytheon.viz.satellite.menu;

import java.util.Set;

import org.eclipse.jface.action.IContributionItem;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonBundleMenuContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.ui.menus.xml.AbstractMenuContributionItem;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 26, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SatBundleMenuContribution extends
        AbstractMenuContributionItem<CommonBundleMenuContribution> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatBundleMenuContribution.class);

    @Override
    public IContributionItem[] getContributionItems(
            CommonAbstractMenuContribution items, VariableSubstitution[] subs,
            Set<String> removals) throws VizException {
        CommonBundleMenuContribution item = (CommonBundleMenuContribution) items;
        try {
            if (removals.contains(items.id)) {
                return new IContributionItem[0];
            }

            return new IContributionItem[] { new SatBundleContributionItem(
                    item, subs) };
        } catch (VizException e) {
            if (Boolean.valueOf(item.suppressErrors)) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error setting up menu item : " + item.text, e);
            } else {
                statusHandler.handle(Priority.VERBOSE, "Skipping menu item : "
                        + item.text);
            }
            return new IContributionItem[0];
        }
    }
}
