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
package com.raytheon.viz.volumebrowser.vbui;

import java.util.List;
import java.util.Set;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.ToolItem;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.viz.volumebrowser.vbui.DataListsProdTableComp.DataSelection;
import com.raytheon.viz.volumebrowser.widget.MenuContributionItem;
import com.raytheon.viz.volumebrowser.xml.MenuContribution;

/**
 * Create menus of points organized by groups.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2012            rferrel     Initial creation
 * Sep 25, 2012 1215       rferrel     Clicking anywhere on the Point button
 *                                      now opens the menu.
 * Sep 26, 2012 1216       rferrel     resetMenu method added.
 * Jan 24, 2013 1516       rferrel     Change Active Data Selection prior to filling menus.
 * Dec 06, 2013 2271       mpduff      Disable plane menu items for already selected items.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class PointToolAction extends Action implements IMenuCreator {

    /**
     * Menu to display the parentNode's children.
     */
    private Menu menu;

    /**
     * Type of name to prepend to a point's name.
     */
    private final String pointNames;

    /**
     * The Data Selection type that requested the menu.
     */
    private final DataSelection dataSelection;

    /**
     * The group node associated with this action.
     */
    private final IPointNode parentNode;

    /**
     * Manager to get point information.
     */
    private final PointsDataManager dataManager;

    /**
     * Constructor.
     * 
     * @param text
     * @param pointNames
     * @param dataSelection
     */
    public PointToolAction(String text, String pointNames,
            DataSelection dataSelection) {
        this(text, pointNames, dataSelection, null);
    }

    /**
     * Private constructor used recursively to generate menus for groups.
     * 
     * @param text
     * @param pointNames
     * @param dataSelecion
     * @param parentNode
     */
    private PointToolAction(String text, String pointNames,
            DataSelection dataSelecion, IPointNode parentNode) {
        super(text, SWT.DROP_DOWN);
        this.pointNames = pointNames;
        this.dataSelection = dataSelecion;
        this.parentNode = parentNode;
        this.dataManager = PointsDataManager.getInstance();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.
     * Event)
     */
    @Override
    public void runWithEvent(Event event) {
        if (menu == null) {
            ToolItem item = (ToolItem) event.widget;
            getMenu(item.getParent());
        }
        menu.setVisible(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.IMenuCreator#dispose()
     */
    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Control)
     */
    @Override
    public Menu getMenu(Control parent) {
        if (menu == null) {
            MenuItemManager.getInstance().setActiveDataSelection(dataSelection);
            menu = new Menu(parent);
            fillMenu(menu);
        }

        return menu;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Menu)
     */
    @Override
    public Menu getMenu(Menu parent) {
        if (menu == null) {

            MenuItemManager.getInstance().setActiveDataSelection(dataSelection);
            menu = new Menu(parent);
            fillMenu(menu);
        }
        return menu;
    }

    /**
     * This forces the drop down menu to to be recreated the next time the menu
     * is needed.
     */
    public void resetMenu() {
        if (menu != null) {
            final Menu oldMenu = menu;
            menu = null;
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    oldMenu.dispose();
                }
            });
        }
    }

    /**
     * Populates the menu with the children of the parentNode.
     * 
     * @param menu
     */
    private void fillMenu(final Menu menu) {
        List<IPointNode> nodes = dataManager.getChildren(parentNode);

        // Create the menu items
        for (IPointNode node : nodes) {
            if (node.isGroup()) {
                if (dataManager.getChildren(node).size() > 0) {
                    final PointToolAction submenu = new PointToolAction(
                            node.getName(), pointNames, dataSelection, node);
                    submenu.menu = new Menu(menu);
                    menu.addDisposeListener(new DisposeListener() {

                        @Override
                        public void widgetDisposed(DisposeEvent e) {
                            submenu.menu.dispose();
                        }
                    });
                    submenu.fillMenu(submenu.menu);
                    ActionContributionItem item = new ActionContributionItem(
                            submenu);
                    item.fill(menu, -1);
                }
            } else {
                TitleContributionItem cci = new TitleContributionItem();
                cci.setText(node.getName());
                cci.fill(menu, -1);

                // disable menu items for already selected items
                if (dataSelection == DataSelection.PLANES) {
                    Set<String> planeItems = MenuItemManager.getInstance()
                            .getSelectedPlaneItems();
                    for (String s : planeItems) {
                        MenuItemManager.getInstance().disableMenuItem(s,
                                DataSelection.PLANES);
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getMenuCreator()
     */
    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    /**
     * This class the details for placing a point in the desired menu.
     */
    private class TitleContributionItem extends ContributionItem {

        private String text;

        /**
         * 
         */
        public TitleContributionItem() {
            super();
        }

        /**
         * @param text
         *            the text to set
         */
        public void setText(String text) {
            this.text = text;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.jface.action.ContributionItem#fill(org.eclipse.swt.widgets
         * .Composite)
         */
        @Override
        public void fill(Menu menu, int index) {
            MenuContribution mContrib = new MenuContribution();
            mContrib.xml.key = "Point" + text;
            mContrib.xml.menuText = pointNames + " " + text;
            MenuContributionItem item = new MenuContributionItem(mContrib);
            item.fill(menu, index);
            update(null);
        }
    }
}
