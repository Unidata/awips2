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

import com.raytheon.viz.core.map.GeoUtil;
import com.raytheon.viz.volumebrowser.vbui.VbUtil;
import com.raytheon.viz.volumebrowser.xml.MenuContribution;
import com.raytheon.viz.volumebrowser.xml.TitleImgContribution;
import com.raytheon.viz.volumebrowser.xml.ToolBarContribution;
import com.vividsolutions.jts.geom.Coordinate;

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
 * 
 * </pre>
 * 
 * @author unkown
 * @version 1.0
 */
public class ToolBarContributionItem extends ContributionItem {

    protected Menu menu;

    protected ToolItem widget;

    protected ToolBarContribution toolBarContribution;

    private IContributionItem[] contribs;

    public String getToolItemText() {
        return toolBarContribution.xml.toolItemText;
    }

    public void addContrib(IContributionItem contributionItem) {

        IContributionItem[] tmpContribs = new IContributionItem[contribs.length + 1];
        System.arraycopy(contribs, 0, tmpContribs, 0, contribs.length);
        tmpContribs[contribs.length] = contributionItem;
        contribs = tmpContribs;

        if (menu != null) {
            // make sure to refresh the menu
            contribs[contribs.length - 1].fill(menu, -1);
        }
    }

    public ToolBarContributionItem(ToolBarContribution toolBarContribution,
            IContributionItem[] contribs) {
        this.contribs = contribs;
        this.toolBarContribution = toolBarContribution;
        addLatLon();
    }

    private void addLatLon() {
        boolean isLat = (toolBarContribution.xml.toolItemText.equals("Lat"));
        boolean isLon = (toolBarContribution.xml.toolItemText.equals("Lon"));

        if (!isLat && !isLon) {
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
            menuContribution.xml.menuText = String
                    .format("%7s",
                            GeoUtil.formatCoordinate(new Coordinate(i, i))
                                    .split(" ")[coordinateIndex]);
            menuContribution.xml.key = keyPrefixString + i;
            menuContribution.xml.id = idPrefixString + i;

            MenuContributionItem menuContributionItem = new MenuContributionItem(
                    menuContribution);
            addContrib(menuContributionItem);

        }
    }

    @Override
    public void fill(final ToolBar parentToolBar, int index) {
        // TODO Auto-generated method stub
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
