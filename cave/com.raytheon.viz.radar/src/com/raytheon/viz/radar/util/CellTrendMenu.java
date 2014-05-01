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
package com.raytheon.viz.radar.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;

import com.raytheon.uf.common.menus.xml.CommonBundleMenuContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.ui.menus.widgets.BundleContributionItem;
import com.raytheon.uf.viz.ui.menus.xml.DynamicCompoundContributionItem;

/**
 * This class generates the Cell Trend cascading menus organizing the points by
 * groups.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 2, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class CellTrendMenu extends DynamicCompoundContributionItem {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CellTrendMenu.class);

    private static final String CI_PRODUCT_KEY = "product";

    private static final String CI_PRODUCT_VALUE = "62";

    private static final String CI_ELEVATION_KEY = "elevation";

    private static final String CI_ELEVATION_VALUE = "0";

    private static final String MI_BUNDLE = "bundles/DefaultCellTrend.xml";

    private static final String MI_TEXT_PREFIX = "point ";

    private static final String MI_ID_PREFIX = "${icao}point";

    private static final String MI_EDITOR_TYPE = "com.raytheon.viz.radar.ui.xy.RadarGraphEditor";

    private static final String MI_EDITOR_KEY = "pointid";

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        List<IContributionItem> items = null;
        int subsLastIndex = getSubstitutions().length;
        VariableSubstitution[] subs = Arrays.copyOf(getSubstitutions(),
                subsLastIndex + 2);
        VariableSubstitution sub = new VariableSubstitution();
        sub.key = CI_PRODUCT_KEY;
        sub.value = CI_PRODUCT_VALUE;
        subs[subsLastIndex++] = sub;
        sub = new VariableSubstitution();
        sub.key = CI_ELEVATION_KEY;
        sub.value = CI_ELEVATION_VALUE;
        subs[subsLastIndex++] = sub;
        items = createMenuItems(null, subs);
        return items.toArray(new IContributionItem[0]);
    }

    /**
     * Create the menu items for a group node.
     * 
     * @param root
     *            - get children of this group node
     * @param subs
     *            -
     * @return
     */
    private List<IContributionItem> createMenuItems(IPointNode root,
            VariableSubstitution[] subs) {
        List<IContributionItem> items = new ArrayList<IContributionItem>();
        for (IPointNode node : PointsDataManager.getInstance()
                .getChildren(root)) {
            if (node.isGroup()) {
                IMenuManager subMenu = new MenuManager(node.getName(),
                        this.getId() + "." + node.getName());
                items.add(subMenu);
                List<IContributionItem> children = createMenuItems(node, subs);
                for (IContributionItem child : children) {
                    subMenu.add(child);
                }
            } else {
                CommonBundleMenuContribution cont = new CommonBundleMenuContribution();
                cont.bundleFile = MI_BUNDLE;
                cont.text = MI_TEXT_PREFIX + node.getName();
                cont.id = MI_ID_PREFIX + node.getName();

                cont.editorType = MI_EDITOR_TYPE;
                VariableSubstitution sub = new VariableSubstitution();
                sub.key = MI_EDITOR_KEY;
                sub.value = node.getName();
                cont.substitutions = Arrays.copyOf(subs, subs.length + 1);
                cont.substitutions[cont.substitutions.length - 1] = sub;

                BundleContributionItem item = null;
                try {
                    item = new BundleContributionItem(cont, subs);
                    items.add(item);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "CellTrendMenu unable to create menu item", e);
                }
            }
        }
        return items;
    }
}
