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
package com.raytheon.uf.viz.ui.menus.widgets;

import java.util.Map;
import java.util.Set;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.IMenuService;
import org.eclipse.ui.menus.MenuUtil;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VariableSubstitutionUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.ui.menus.xml.IContribItemProvider;
import com.raytheon.uf.viz.ui.menus.xml.MenuXMLMap;

/**
 * Provides a submenu capability
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Mar 26, 2009           chammack    Initial creation
 * May 08, 2013  1978     bsteffen    Perform variable substitution on subMenu
 *                                    IDs.
 * Dec 11, 2013  2602     bsteffen    Update MenuXMLMap.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SubmenuContributionItem extends MenuManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubmenuContributionItem.class);

    private static JobPool getContributionItemsJob = new JobPool(
            "Preparing Menus", 1, true);

    private CommonAbstractMenuContribution[] contribs;

    protected VariableSubstitution[] subs;

    protected IContributionItem[][] contributionItems;

    protected Set<String> removals;

    /**
     * 
     * @param includeSubstitutions
     * @param name
     * @param ci
     * @param removals
     * @param mListener
     */
    public SubmenuContributionItem(VariableSubstitution[] includeSubstitutions,
            String id, String name, CommonAbstractMenuContribution[] ci,
            Set<String> removals) {
        super(processSubstitution(includeSubstitutions, name),
                processSubstitution(includeSubstitutions, id));
        this.subs = includeSubstitutions;
        this.contribs = ci;
        this.removals = removals;
    }

    private static String processSubstitution(
            VariableSubstitution[] includeSubstitutions, String name) {
        if (name != null && includeSubstitutions != null
                && includeSubstitutions.length > 0) {
            Map<String, String> map = VariableSubstitution
                    .toMap(includeSubstitutions);
            try {
                name = VariableSubstitutionUtil.processVariables(name, map);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error during menu substitution", e);
            }
        }
        return name;
    }

    @Override
    public void fill(Menu parent, int index) {
        removeAll();
        super.fill(parent, index);
        getContributionItemsJob.schedule(new GetContributionItemsRunnable());
    }

    protected synchronized IContributionItem[][] getContributionItems() {
        if (this.contributionItems == null) {
            IContributionItem[][] contributionItems = new IContributionItem[this.contribs.length][];

            for (int i = 0; i < contribs.length; i++) {
                try {
                    IContribItemProvider amc = MenuXMLMap
                            .getProvider(contribs[i].getClass());
                    contributionItems[i] = amc.getContributionItems(
                            contribs[i], this.subs, this.removals);

                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error creating menu", e);
                }
            }
            this.contributionItems = contributionItems;
        }
        return this.contributionItems;
    }

    @Override
    public boolean isVisible() {
        return visible;
    }

    // call getContributionItems using the getContributionItems JobPool.
    private class GetContributionItemsRunnable implements Runnable {

        @Override
        public void run() {
            getContributionItems();
            for (int i = 0; i < contributionItems.length; i++) {
                for (IContributionItem item : contributionItems[i]) {
                    add(item);
                }
            }
            IMenuService menuService = (IMenuService) PlatformUI.getWorkbench()
                    .getService(IMenuService.class);
            menuService.populateContributionManager(
                    SubmenuContributionItem.this, MenuUtil.menuUri(getId()));
        }
    }
}
