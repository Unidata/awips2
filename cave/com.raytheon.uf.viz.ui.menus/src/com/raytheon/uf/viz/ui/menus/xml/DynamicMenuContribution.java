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

import java.util.Set;

import org.eclipse.jface.action.IContributionItem;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonDynamicMenuContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Class to make dynamic menus.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class DynamicMenuContribution extends
        AbstractMenuContributionItem<CommonDynamicMenuContribution> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DynamicMenuContribution.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.ui.menus.xml.IContribItemProvider#getContributionItems
     * (com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution,
     * com.raytheon.uf.common.menus.xml.VariableSubstitution[], java.util.Set)
     */
    @Override
    public IContributionItem[] getContributionItems(
            CommonAbstractMenuContribution items, VariableSubstitution[] subs,
            Set<String> removals) throws VizException {

        if (removals.contains(items.id)) {
            return new IContributionItem[0];
        }

        IContributionItem[] results = null;
        CommonDynamicMenuContribution item = (CommonDynamicMenuContribution) items;

        try {
            Class<?> c = Class.forName(item.classId);
            DynamicCompoundContributionItem dcci = (DynamicCompoundContributionItem) c
                    .newInstance();
            dcci.setSubstitutions(subs);
            results = dcci.getItems();
        } catch (ClassNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to find class: "
                    + item.classId, e);
        } catch (InstantiationException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to load class: "
                    + item.classId, e);
        } catch (IllegalAccessException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to access class: "
                    + item.classId, e);
        }

        if (results == null) {
            return new IContributionItem[0];
        }
        return results;
    }

}
