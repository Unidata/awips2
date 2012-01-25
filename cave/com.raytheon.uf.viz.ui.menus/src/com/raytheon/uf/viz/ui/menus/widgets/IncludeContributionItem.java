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

import java.util.Set;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.ToolBar;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.ui.menus.xml.IncludeMenuItem;

/**
 * 
 * Waits to load included files until the menu is filled.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class IncludeContributionItem extends ContributionItem {
    static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IncludeContributionItem.class);

    private final IncludeMenuItem imc;

    private final CommonAbstractMenuContribution contribs;

    private final VariableSubstitution[] incomingSubs;

    private final Set<String> removalsIn;

    private IContributionItem[] items = null;

    public IncludeContributionItem(IncludeMenuItem imc,
            CommonAbstractMenuContribution contribs,
            VariableSubstitution[] incomingSubs, Set<String> removalsIn) {
        super();
        this.imc = imc;
        this.contribs = contribs;
        this.incomingSubs = incomingSubs;
        this.removalsIn = removalsIn;
    }

    public synchronized IContributionItem[] getContributionItems() {
        if (items == null) {
            try {
                items = imc.getAllContributionItems(contribs, incomingSubs,
                        removalsIn);
            } catch (VizException e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Error setting up menus", e);
            }
        }
        return items;
    }

    @Override
    public void fill(Menu menu, int index) {
        IContributionItem[] items = getContributionItems();
        if (index == -1) {
            index = menu.getItemCount();
        }
         getContributionItems();
        for (int i = 0; i < items.length; i++) {
            IContributionItem item = items[i];
            int oldItemCount = menu.getItemCount();
            if (item.isVisible()) {
                item.fill(menu, index);
            }
            int newItemCount = menu.getItemCount();
            int numAdded = newItemCount - oldItemCount;
            index += numAdded;
        }
    }

    @Override
    public void fill(ToolBar toolbar, int index) {
        IContributionItem[] items = getContributionItems();
        if (index == -1) {
            index = toolbar.getItemCount();
        }
         getContributionItems();
        for (int i = 0; i < items.length; i++) {
            IContributionItem item = items[i];
            int oldItemCount = toolbar.getItemCount();
            if (item.isVisible()) {
                item.fill(toolbar, index);
            }
            int newItemCount = toolbar.getItemCount();
            int numAdded = newItemCount - oldItemCount;
            index += numAdded;
        }
    }

    @Override
    public boolean isDynamic() {
        return true;
    }

    @Override
    public void dispose() {
        super.dispose();
        if (items != null) {
            for (IContributionItem item : items) {
                item.dispose();
            }
        }
    }
}