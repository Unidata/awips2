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
package com.raytheon.viz.volumebrowser.widget;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.IMenuService;
import org.eclipse.ui.menus.MenuUtil;

import com.raytheon.viz.core.map.GeoUtil;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.SpaceTimeMenu;
import com.raytheon.viz.volumebrowser.vbui.VbUtil;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserAction;
import com.raytheon.viz.volumebrowser.xml.MenuContribution;
import com.raytheon.viz.volumebrowser.xml.TitleImgContribution;
import com.raytheon.viz.volumebrowser.xml.ToolBarContribution;
import org.locationtech.jts.geom.Coordinate;

/**
 *
 * Contribution item for tool bar buttons.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Dec 11, 2013  2602     bsteffen    Set the id of menu items.
 * Dec 06, 2017  6355     nabowle     Allow dynamic contribution. Hide when empty.
 * Feb 08, 2018  6717     bsteffen    Do not add more lats or lons in space mode.
 *
 * </pre>
 *
 */
public class ToolBarContributionItem extends ContributionItem {

    protected Menu menu;

    protected ToolItem widget;

    protected ToolBarContribution toolBarContribution;

    private IContributionItem[] contribs;

    public ToolBarContributionItem(ToolBarContribution toolBarContribution,
            IContributionItem[] contribs) {
        this.contribs = contribs;
        this.toolBarContribution = toolBarContribution;
        addLatLon();
        addDynamic();

        if (this.contribs == null || this.contribs.length == 0) {
            this.setVisible(false);
        }
    }

    public String getToolItemText() {
        return toolBarContribution.xml.toolItemText;
    }

    private void addContrib(IContributionItem contributionItem) {
        IContributionItem[] tmpContribs = new IContributionItem[contribs.length
                + 1];
        System.arraycopy(contribs, 0, tmpContribs, 0, contribs.length);
        tmpContribs[contribs.length] = contributionItem;
        contribs = tmpContribs;

        if (menu != null) {
            // make sure to refresh the menu
            contribs[contribs.length - 1].fill(menu, -1);
        }
    }

    private void addLatLon() {
        boolean isLat = "Lat".equals(toolBarContribution.xml.toolItemText);
        boolean isLon = "Lon".equals(toolBarContribution.xml.toolItemText);

        if (!isLat && !isLon) {
            return;
        }
        if (VolumeBrowserAction.getVolumeBrowserDlg().getDialogSettings()
                .getSpaceTimeSelection() == SpaceTimeMenu.SPACE) {
            return;
        }
        String keyPrefixString = toolBarContribution.xml.toolItemText;
        String idPrefixString = "xSect" + toolBarContribution.xml.toolItemText;

        java.awt.Rectangle coverageRectangle = VbUtil.getMapCoverageRectangle();

        double lowerPosition = 0.0;
        double upperPosition = 0.0;
        int coordinateIndex = 0;

        if (isLat) {
            lowerPosition = coverageRectangle.getMinY();
            upperPosition = coverageRectangle.getMaxY();
        } else if (isLon) {
            lowerPosition = coverageRectangle.getMinX();
            upperPosition = coverageRectangle.getMaxX();
            coordinateIndex = 1;
        }

        double scaleResolution = 16;
        double increment = Math.abs(lowerPosition - upperPosition)
                / scaleResolution;

        TitleImgContribution titleImgContribution = new TitleImgContribution();
        titleImgContribution.xml.titleText = "Dynamic";
        titleImgContribution.xml.displayDashes = true;
        addContrib(new TitleImgContributionItem(titleImgContribution));

        for (double i = lowerPosition; i <= upperPosition; i += increment) {

            MenuContribution menuContribution = new MenuContribution();
            menuContribution.xml.menuText = String.format("%7s",
                    GeoUtil.formatCoordinate(new Coordinate(i, i))
                            .split(" ")[coordinateIndex]);
            menuContribution.xml.key = keyPrefixString + i;
            menuContribution.xml.id = idPrefixString + i;

            MenuContributionItem menuContributionItem = new MenuContributionItem(
                    menuContribution);
            addContrib(menuContributionItem);

        }
    }

    /**
     * Adds dynamic contributions to this menu.
     *
     * The dynamic contributions are configured with installTo="menu:this.id" or
     * installTo="menu:this.toolBarContribution.xml.id".
     */
    private void addDynamic() {
        String id = getId();
        if (id == null) {
            id = this.toolBarContribution.xml.id;
        }
        if (id == null) {
            return;
        }

        MenuManager mgr = new MenuManager();
        IWorkbenchWindow window = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        String uri = MenuUtil.menuUri(id);
        IMenuService menuService = window.getService(IMenuService.class);
        menuService.populateContributionManager(mgr, uri);
        IContributionItem[] items = mgr.getItems();
        if (items != null) {
            for (IContributionItem item : items) {
                addContrib(item);
            }
        }
    }

    @Override
    public void fill(final ToolBar parentToolBar, int index) {
        super.fill(parentToolBar, index);

        final ToolItem ti = new ToolItem(parentToolBar, SWT.DROP_DOWN);
        ti.setText(toolBarContribution.xml.toolItemText);

        ti.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleWidgetSelected(event);
            }

        });

        widget = ti;

        widget.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                menu.dispose();
                menu = null;
                for (int i = 0; i < contribs.length; i++) {
                    contribs[i].dispose();
                }
            }
        });

        if (menu == null) {
            createPopupMenu();
        }
    }

    private void handleWidgetSelected(SelectionEvent event) {
        ToolItem tItem = (ToolItem) event.getSource();

        Rectangle rect = tItem.getBounds();
        Point pt = new Point(rect.x, rect.y + rect.height);
        pt = tItem.getParent().toDisplay(pt);
        menu.setLocation(pt.x, pt.y);
        menu.setVisible(true);
    }

    private void createPopupMenu() {
        menu = new Menu(widget.getParent().getShell(), SWT.POP_UP);

        for (int i = 0; i < contribs.length; i++) {
            contribs[i].fill(menu, -1);
        }
    }

    @Override
    public void dispose() {
        super.dispose();
        if (widget != null) {
            widget.dispose();
            widget = null;
        }
    }

}
