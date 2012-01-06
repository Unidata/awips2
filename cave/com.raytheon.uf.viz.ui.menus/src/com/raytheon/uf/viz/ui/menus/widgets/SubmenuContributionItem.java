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

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VariableSubstitutionUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.ui.menus.xml.AbstractMenuContributionItem;
import com.raytheon.uf.viz.ui.menus.xml.MenuXMLMap;

/**
 * Provides a submenu capability
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SubmenuContributionItem extends ContributionItem {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubmenuContributionItem.class);

    private static JobPool getContributionItemsJob = new JobPool(
            "Preparing Menus", 1, true);

    private CommonAbstractMenuContribution[] contribs;

    private String name;

    private MenuItem widget;

    protected Menu menu;

    protected VariableSubstitution[] subs;

    protected IContributionItem[][] contributionItems;

    protected Set<String> removals;

    public SubmenuContributionItem(VariableSubstitution[] includeSubstitutions,
            String name, CommonAbstractMenuContribution[] ci,
            Set<String> removals) {
        super();
        this.subs = includeSubstitutions;
        this.contribs = ci;
        this.removals = removals;

        if (includeSubstitutions != null && includeSubstitutions.length > 0) {
            Map<String, String> map = VariableSubstitution
                    .toMap(includeSubstitutions);
            try {
                this.name = VariableSubstitutionUtil
                        .processVariables(name, map);
            } catch (VizException e) {
                this.name = name;
                statusHandler.handle(Priority.PROBLEM,
                        "Error during menu substitution", e);
            }
        } else {
            this.name = name;
        }

    }

    @Override
    public void fill(Menu parent, int index) {

        if (widget != null && widget.isDisposed()) {
            widget = null;
        }

        if (widget != null || parent == null) {
            return;
        }

        MenuItem item = null;
        if (index >= 0) {
            item = new MenuItem(parent, SWT.CASCADE, index);
        } else {
            item = new MenuItem(parent, SWT.CASCADE);
        }

        item.setData(this);

        item.setText(this.name);

        widget = item;

        createMenu();

        update(null);
    }

    private void createMenu() {
        menu = new Menu(widget.getParent().getShell(), SWT.DROP_DOWN);
        menu.addMenuListener(new MenuListener() {
            @Override
            public void menuHidden(MenuEvent e) {
                // should Menu Items be disposed here?
            }

            @Override
            public void menuShown(MenuEvent e) {
                fillMenu();
            }
        });

        widget.setMenu(menu);

        getContributionItemsJob.schedule(new GetContributionItemsRunnable());
    }

    protected synchronized IContributionItem[][] getContributionItems() {
        if (this.contributionItems == null) {
            IContributionItem[][] contributionItems = new IContributionItem[this.contribs.length][];

            for (int i = 0; i < contribs.length; i++) {
                try {
                    AbstractMenuContributionItem<?> amc = MenuXMLMap.xmlMapping
                            .get(contribs[i].getClass());
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

    private void fillMenu() {
        getContributionItems();
        if (this.menu.getItemCount() == 0) {
            for (int i = 0; i < this.contributionItems.length; i++) {
                for (IContributionItem item : this.contributionItems[i]) {
                    if (item.isVisible()) {
                        item.fill(this.menu, -1);
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.ContributionItem#dispose()
     */
    @Override
    public void dispose() {
        super.dispose();
        if (this.menu != null && !this.menu.isDisposed()) {
            this.menu.dispose();
        }
    }

    // call getContributionItems using the getContributionItems JobPool.
    private class GetContributionItemsRunnable implements Runnable {

        @Override
        public void run() {
            getContributionItems();
        }
    }

}
